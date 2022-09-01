{***************************************************************************}
{                                                                           }
{  Copyright (c) 1999-2015 Sergiy Kurinny                                   }
{                                                                           }
{  This library is free software; you can redistribute it and/or            }
{  modify it under the terms of the GNU Lesser General Public               }
{  License version 2.1 as published by the Free Software Foundation         }
{  and appearing in the file license.txt which is included in the root      }
{  folder of the package.                                                   }
{                                                                           }
{  This library is distributed in the hope that it will be useful,          }
{  but WITHOUT ANY WARRANTY; without even the implied warranty of           }
{  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU        }
{  Lesser General Public License for more details.                          }
{                                                                           }
{***************************************************************************}
Unit psc_calendar;

Interface
{$I psc_defines.inc}

Uses
  graphics,
  dialogs,
  stdctrls,
  menus,
  System.UITypes,
  Winapi.Windows,
  Winapi.Messages,
  Controls,
  Forms,
  System.Types,
  Printers,
  db,
  Sysutils,
  classes,

  myla_system,
  myla_interfaces,

  psc_edit,
  psc_parser_date,
  psc_wrapper,
  psc_procs,
  psc_const;

const
  clCalendarWeekLine = clPSCWindowText;
  clCalendarHeaderBorder = clPSCWindowText;
  clCalendarGrayBk = clPSCWindow;

Type
  TPSCHolidayItem = Class(TPSCNamedItem)
  private
    FDayConst: TPSCDayConst;
    FTag: Integer;
    Procedure SetWeekDay(Value: TPSCWeekDay);
    Procedure SetKind(Value: TPSCDayConstKind);
    Procedure SetDay(Value: TPSCDay);
    Procedure SetMonth(Value: TPSCMonth);
    Procedure SetDayConst(Const ADateConst: TPSCDayConst);

    Function GetWeekDay: TPSCWeekDay;
    Function GetKind: TPSCDayConstKind;
    Function GetDay: TPSCDay;
    Function GetMonth: TPSCMonth;
    Function IsDayNumber: boolean;
  public
    Procedure Assign(Source: TPersistent); override;
    Constructor Create(Collection: TCollection); override;

    Function HolidayToDateTime(Const BaseDate: TDateTime): TDateTime;

    Property DayConst: TPSCDayConst read FDayConst write SetDayConst;
  published
    Property Name;
    Property KindOfDay: TPSCDayConstKind read GetKind write SetKind default
      dckDate;
    Property WeekDay: TPSCWeekDay read GetWeekDay write SetWeekDay stored
      IsDayNumber;
    Property Day: TPSCDay read GetDay write SetDay;
    Property Month: TPSCMonth read GetMonth write SetMonth;
    Property Tag: Integer Read FTag Write FTag;
  End;

  TPSCHolidayItemClass = Class Of TPSCHolidayItem;

  TPSCOnGetHolidayDate = Procedure(Sender: TObject;
    AHoliday: TPSCHolidayItem; Const ABaseDate: TDateTime; Var ADate: TDateTime)
      Of Object;

  TPSCHolidayItems = Class(TPSCNamedItems)
  private
    FOnGetHolidayDate: TPSCOnGetHolidayDate;
    Function GetHolidayItem(Index: integer): TPSCHolidayItem;
  public
    Function GetItemDate(Const BaseDate: TDateTime; Item: TPSCHolidayItem):
      TDateTime; virtual;
    Function AddHolidayNames(ADate: TDateTime; const HolidayNames: IPSCStrings): boolean;
    Property Holidays[Index: integer]: TPSCHolidayItem read GetHolidayItem;
    Property OnGetHolidayDate: TPSCOnGetHolidayDate read FOnGetHolidayDate write
      FOnGetHolidayDate;
  End;

{------------------------------}

Const
  cPSCDefMonthsSelectedColor = clPSCBtnFace;
  cPSCScrollSpeed: Array[1..4] Of Cardinal = (500,250,100,50);
  clHolidayTextColor = clPSCMaroon;
  clArrowColor = clPSCWindowText;
  clMSMoneySelection = $004E5257;
  clMSMoneyBkColor = clPSCWindowText;
  cPSCFullWeek: TPSCWeekDays = [dwSunday,dwMonday,dwTuesday,
  dwWednesday,dwThursday,dwFriday,dwSaturday];
Type
  TPSCMonthHitTest = (mhtNowhere,mhtHead,mhtCaption,mhtMonths,
    mhtLeftArrow,mhtRightArrow,mhtYear,mhtMonth);
  TPSCMonthHitTests = Set Of TPSCMonthHitTest;

  TPSCArrowStyle = (astOutlook,astMSMoney,astCustom,astMonthCalendar);

Const
  cPSCDefCalendarBorderStyle = bsSingle;
  cPSCDefNavButtonsTimer = True;

  cArrowClickTime = 170;

  clBorderColor = clPSCBtnFace;
  clMonthHeaderColor = clPSCBtnFace;
{
  clSelectedColor = clPSCBtnFace;
  clSelectedTextColor = clPSCWindow;
}

  clSelectedColor = clHighlight;
  clSelectedTextColor = clHighlightText;

  clGrayedColor = clPSCGrayText;
  clWeekEndTextColor = clPSCWindowText;
  clNowRectColor = clPSCMaroon;
  clDayOutlineColor = clPSCBtnShadow;
  clCalendarColor = clPSCWindow;
  psDayOutlineStyle = PenStyle_Solid;
  clCalendarTextColor = clPSCWindowText;

Type
  TPSCCalendarHitTest = (chtNowhere,chtLeftArrow,chtRightArrow,chtMonthHead,
    chtWeekDays,chtWeekLeft,chtWeekRight,chtDay,chtLeftGray,chtRightGray,
    chtHidden,chtFooter);

  TPSCCalendarDateType = (cdtGray,cdtNormal,cdtSelected,cdtHidden,
    cdtWeek,cdtSelectedWeek);

  TPSCMonthPos = Set Of (mpLeft,mpTop,mpRight,mpBottom);

  TPSCCalSelectKind = (skFree,skProgressive{,skWeekSel});

  TPSCWeekDaysLength = (wdlOne,wdlTwo,wdlThree,wdlShort,wdlWhole);

const
  CPSCCalColorIndex_Border        = 0;
  CPSCCalColorIndex_MonthHeader   = 1;
  CPSCCalColorIndex_WeekHeader    = 3;
  CPSCCalColorIndex_Days          = 4;
  CPSCCalColorIndex_Selected      = 5;
  CPSCCalColorIndex_SelectedText  = 6;
  CPSCCalColorIndex_Grayed        = 7;
  CPSCCalColorIndex_WeekEndText   = 8;
  CPSCCalColorIndex_NowRect       = 9;
  CPSCCalColorIndex_WeekSide      = 10;
  CPSCCalColorIndex_DayLines      = 11;
  CPSCCalColorIndex_Arrow         = 12;
  CPSCCalColorIndex_WeekLine      = 13;
  CPSCCalColorIndex_HeaderBorder  = 14;
  CPSCCalColorIndex_GrayedBk      = 15;
type
  TPSCCustomCalendar = Class;

  TPSCPrintOption = (poHeader,poShowProgress,poPrintPageNumber);
  TPSCPrintOptions = Set Of TPSCPrintOption;
  TPSCCalendarOption = (caopGreyOutsideDays, caopScrollWhenGreyClicked);
  TPSCCalendarOptions = Set of TPSCCalendarOption;

  TPSCCalendarPrintOptions = Class(TPersistent)
  private
    FHeader: String;
    FHeaderFont: TPSCFont;
    FOptions: TPSCPrintOptions;
    FParentHeaderFont: Boolean;
    FTitle: String;
    FCalendar: TPSCCustomCalendar;
    FOverlap: Integer;

    Function IsHeaderFontStored: boolean;
    Procedure SetHeaderFont(Value: TPSCFont);
    Procedure SetParentHeaderFont(Value: Boolean);
  public
    Constructor Create(AOwner: TPSCCustomCalendar);
    Destructor Destroy; override;

    Procedure Assign(Source: TPersistent); override;
  published
    Property Header: String read FHeader write FHeader;
    Property HeaderFont: TPSCFont read FHeaderFont write SetHeaderFont stored
      IsHeaderFontStored;
    Property Title: String read FTitle write FTitle;
    Property Options: TPSCPrintOptions read FOptions write FOptions default
      [poHeader];
    Property Overlap: Integer read FOverlap write FOverlap default 0;

    Property ParentHeaderFont: Boolean read FParentHeaderFont write
      SetParentHeaderFont default True;
  End;


  TPSCCalendarColors = Class(TPSCIntfPersistent)
  private
    FBorder: TPSCColor;
    FMonthHeader: TPSCColor;
    FWeekHeader: TPSCColor;
    FDays: TPSCColor;
    FDayLines: TPSCColor;
    FSelected: TPSCColor;
    FSelectedText: TPSCColor;
    FGrayed: TPSCColor;
    FWeekEndText: TPSCColor;
    FNowRect: TPSCColor;
    FWeekSide: TPSCColor;
    FWeekNumbersFont: TPSCFont;
    FWeekDaysFont: TPSCFont;
    FHeaderFont: TPSCFont;
    FDayLinesStyle: TPenStyle;
    FDaysFont : TPSCFont;

    Procedure SetWeekNumbersFont(Value: TPSCFont);
    Procedure SetWeekDaysFont(Value: TPSCFont);
    Procedure SetHeaderFont(Value: TPSCFont);
    Procedure SetDayLinesStyle(Value: TPenStyle);
    Procedure SetDaysFont(Value: TPSCFont);

    Function GetOwnerAsCalendar:TPSCCustomCalendar;
    Function IsColorStored(Index: Integer): Boolean;
    Function IsWeekNumbersFontStored:Boolean;
    Function IsWeekDaysFontStored:Boolean;
    Function IsHeaderFontStored:Boolean;
  protected
    Procedure FontChanged(Sender: TObject);
    procedure Changed; override;
  public
    Procedure SetColor(Index: Integer; Value: TPSCColor);
    Function GetColor(Index: Integer): TPSCColor;
    Constructor Create(AOwner: TPSCCustomCalendar);
    Destructor Destroy; override;
    Procedure Assign(Source: TPersistent); override;

    Property Owner: TPSCCustomCalendar read GetOwnerAsCalendar;
  published
    Property Border: TPSCColor index CPSCCalColorIndex_Border read FBorder write SetColor
      stored IsColorStored;
    Property MonthHeader: TPSCColor index CPSCCalColorIndex_MonthHeader read FMonthHeader write SetColor;
    Property WeekHeader: TPSCColor index CPSCCalColorIndex_WeekHeader read FWeekHeader write SetColor
      stored IsColorStored;
    Property Days: TPSCColor index CPSCCalColorIndex_Days read FDays write SetColor
      stored IsColorStored;
    Property DayLines: TPSCColor index CPSCCalColorIndex_DayLines read FDayLines write SetColor;
    Property Selected: TPSCColor index CPSCCalColorIndex_Selected read FSelected write SetColor;
    Property SelectedText: TPSCColor index CPSCCalColorIndex_SelectedText read FSelectedText write SetColor;
    Property Grayed: TPSCColor index CPSCCalColorIndex_Grayed read FGrayed write SetColor;
    Property WeekEndText: TPSCColor index CPSCCalColorIndex_WeekEndText read FWeekEndText write SetColor;
    Property NowRect: TPSCColor index CPSCCalColorIndex_NowRect read FNowRect write SetColor;
    Property WeekSide: TPSCColor index CPSCCalColorIndex_WeekSide read FWeekSide write SetColor
      stored IsColorStored;
    Property WeekNumbersFont: TPSCFont read FWeekNumbersFont
      write SetWeekNumbersFont Stored IsWeekNumbersFontStored;
    Property WeekDaysFont: TPSCFont read FWeekDaysFont write SetWeekDaysFont
      Stored IsWeekDaysFontStored;
    Property HeaderFont: TPSCFont read FHeaderFont write SetHeaderFont
      Stored IsHeaderFontStored;
    Property DayLinesStyle: TPenStyle read FDayLinesStyle write SetDayLinesStyle
      default psDayOutlineStyle;
    property DaysFont : TPSCFont read FDaysFont write SetDaysFont;
  End;

  TPSCCalendGetPaintParamsEvent = Procedure(Sender: TObject;
    const ADate: TDateTime; Var AColor,ABkColor: TPSCColor;
    var AIsBold, AIsItalic, AIsUnderline, AIsStrikeOut:WordBool;
    ADateType: TPSCCalendarDateType) Of Object;

  TPSCCalendCanSelectDateEvent = Procedure(Sender: TObject;
    const ADate: TDateTime; Var ACanChange: WordBool) Of Object;

  TPSCCalendMeasureItemEvent = Procedure(Sender: TObject;
    AArea: TPSCCalendarHitTest; Var AWidth,AHeight: Integer) Of Object;

  TPSCCalendDrawItemEvent = Procedure(Sender: TObject;
    const ADate: TDateTime; AArea: TPSCCalendarHitTest;
    ADateType: TPSCCalendarDateType;
    ALeft, ATop, ARight, ABottom: Integer; Var ADefaultDrawing: WordBool) Of Object;

  TPSCDrawDayOutlineParams=packed record
    DrawLines : Boolean;
    ShowToday : Boolean;
    SetTodayColor : Boolean;
  end;

  TPSCOnDrawDayOutline = Procedure(Sender: TObject; Const ADate: TDateTime; X,Y:
    Integer; Const ARect: TRect; Var AParams: TPSCDrawDayOutlineParams) Of Object;

  IPSCDateArray=interface
    ['{197FD8A6-CEE7-4F41-BF78-C00122F5D049}']
    function GetDateInfo(const Date:TDateTime):Cardinal;
    procedure SetDateInfo(const Date:TDateTime; Value:Cardinal);

    function GetStartAssignedDate:TDateTime;
    function GetEndAssignedDate:TDateTime;
    function GetBaseYear:Word;
    function GetAssignedCount:Integer;

    property DateInfo[const Date:TDateTime]:Cardinal Read GetDateInfo Write SetDateInfo;
  end;

  TPSCBackgroundMode=(
    BackgroundMode_None,
    BackgroundMode_Color,
    BackgroundMode_Brush,
    BackgroundMode_BitmapCenter,
    BackgroundMode_BitmapStretch,
    BackgroundMode_BitmapTile,
    BackgroundMode_GradientHorz,
    BackgroundMode_GradientVert
  );

  TPSCCalendarStyle = (cstOutlook,cstMSMoney,cstMonthCalendar,
    cstWhiteOnBlack,cstBlackOnWhite,cstMSMoney2002,cstForPrinting,
    cstOutlookMonthView);

  TPSCCustomCalendar = Class(TPSCCustomControlAncestor)
  private
    FMonthListBox:TObject;
    FUserData:IPSCDateArray;
    FOnStartDateChange: TPSCNotifyEvent;
    FCancelPrinting: Boolean;
    FPrintedPageCurrent: Integer;
    FPrintedPageTotal: Integer;
    FOnPrintProgress: TPSCNotifyEvent;
    FOnDrawDayOutline: TPSCOnDrawDayOutline;
    FShiftState: TShiftState;
    FNavButtonsTimer: boolean;
    FUpdateCount: Integer;
    FModified: Boolean;
    FBorderEdges: TBevelEdges;
    FTimer: TObject;
    FColors: TPSCCalendarColors;
    FWeekCursor: TCursor;
    FDragDate: TDateTime;
    FCursorDate: TDateTime;
    FMultiSelect: Boolean;
    FBorderStyle: TBorderStyle;
    FShowMonthPopup: Boolean;
    FShowNavButtons: Boolean;
    FShowToday: Boolean;
    FSideWeekSelect: Boolean;
    FShowMonthDividers: Boolean;
    FShowWeekNumbers: Boolean;
    FShowHorzLines: Boolean;
    FShowVertLines: Boolean;
    FWeekDayNames: TPSCWeekDaysLength;
    FFirstWeekOfYear: TPSCFirstWeekOfYear;
    FFirstDayOfWeek: TPSCFirstDayOfWeek;
    FWorkDays: TPSCWeekDays;
    FMinDateLimit: Boolean;
    FMinDate: TDateTime;
    FMaxDateLimit: Boolean;
    FMaxDate: TDateTime;
    FSelStart: TDateTime;
    FSelFinish: TDateTime;
    FStartDate: TDateTime;
    FMaxSelDates: Integer;
    FSelCount: Integer;
    FWeeksSelected: Boolean;
    FSelDates: TList;
    FSelectKind: TPSCCalSelectKind;
    FExtendedSelect: Boolean;
    FReadOnly: Boolean;
    FClearOnKeyMoving: Boolean;
    FSelShiftState: TShiftState;
    FOnDateChanged: TPSCNotifyEvent;
    FOnGetPaintParams: TPSCCalendGetPaintParamsEvent;
    FOnCanSelectDate: TPSCCalendCanSelectDateEvent;
    FOnMeasureItem: TPSCCalendMeasureItemEvent;
    FOnDrawItem: TPSCCalendDrawItemEvent;
    FPrintOptions: TPSCCalendarPrintOptions;
    FMonthShiftX: Integer;
    FMonthShiftY: Integer;
    FVersion: String;
    FNavButtonsSpeed: Integer;
    FFlat: Boolean;
    FOptions : TPSCCalendarOptions;

    procedure SetOptions(V:TPSCCalendarOptions);

    Procedure MonthPopupClosed(Sender: TObject; Canceled: Boolean);
    Procedure SetVersion(const AValue:String);
    Procedure SetWeeksSelected(V: boolean);
    Procedure KillTimer;
    Procedure SetMultiSelect(Value: Boolean);
    Procedure SetSelStart(Value: TDateTime);
    Procedure SetSelFinish(Value: TDateTime);
    Procedure SetMaxSelDates(Value: Integer);
    Procedure SetMinDateLimit(Value: Boolean);
    Procedure SetMinDate(Const Value: TDateTime);
    Procedure SetMaxDateLimit(Value: Boolean);
    Procedure SetMaxDate(Const Value: TDateTime);
    Procedure SetShowNavButtons(Value: Boolean);
    Procedure SetShowToday(Value: Boolean);
    Procedure SetShowMonthDividers(Value: Boolean);
    Procedure SetFirstWeekOfYear(Value: TPSCFirstWeekOfYear);
    Procedure SetShowWeekNumbers(Value: Boolean);
    Procedure SetShowHorzLines(Value: Boolean);
    Procedure SetShowVertLines(Value: Boolean);
    Procedure SetWeekDayNames(Value: TPSCWeekDaysLength);
    Procedure SetCursorDate(Value: TDateTime);
    Procedure SetColors(Value: TPSCCalendarColors);
    Procedure SetFirstDayOfWeek(Value: TPSCFirstDayOfWeek);
    Procedure SetWorkDays(Value: TPSCWeekDays);
    Procedure SetFlat(Value: Boolean);
    Procedure SetBorderStyle(Value: TBorderStyle);
    Procedure SetBorderEdges(Value: TBevelEdges);
    Procedure SetExtendedSelect(Value: Boolean);
    Procedure SetSelectKind(Value: TPSCCalSelectKind);
    Procedure InternalSetCursor(Value: TDateTime);
    Procedure InvalidateSelection;
    Procedure UpdateSelection;
    Procedure UpdateLimits;
    Procedure TimerEvent(Timer: TObject; EventID: Integer);
    Procedure SetPrintOptions(Const Value: TPSCCalendarPrintOptions);

    Function GetEndDate: TDateTime;
    Function GetAutoSize: boolean;
    Function StartDateStored: Boolean;
    Function FindMonth(Date: TDateTime; Var Index: Integer): Boolean;
    Function SetSelDate(Const Date: TDateTime; Value: Boolean): Boolean;
    Function GetWeekRect(Date: TDateTime; Side: TPSCCalendarHitTest;
      Var Rect: TRect): Boolean;
    Function CheckWeekSelected(Date: TDateTime): Boolean;
    Function CorrectStartDate(Value: TDateTime) : TDateTime;
  protected
    FPopupDate: TDateTime;
    FFooterHeight: Integer;
    FColWidth: Integer;
    FSideWidth: Integer;
    FRowHeight: Integer;
    FHeaderHeight: Integer;
    FDaysOfWeekHeight: Integer;
    Procedure WMGetDlgCode(Var Message: TMessage);message WM_GETDLGCODE;
    Procedure WMWindowPosChanging(Var Message: TWMWindowPosChanging);
      message WM_WINDOWPOSCHANGING;
    Procedure CMBorderChanged(Var Message: TMessage); message CM_BORDERCHANGED;
    Procedure WMEraseBkgnd(Var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    Procedure WMSize(Var Message: TWMSize); message WM_SIZE;
    Procedure WMSetCursor(Var Message: TWMSetCursor); message WM_SETCURSOR;
    Procedure CMCtl3DChanged(Var Message: TMessage); message CM_CTL3DCHANGED;
    Procedure CMParentColorChanged(Var Message: TMessage); message
      CM_PARENTCOLORCHANGED;
    Procedure CMColorChanged(Var Message: TMessage); message CM_COLORCHANGED;
    Procedure CMFontChanged(Var Message: TMessage); message CM_FONTCHANGED;

    Function GetMonthStartDate(MonthCol,MonthRow: Integer;
      DecFirstWeek: Boolean): TDateTime;
    function GetTopLeftCorner:TPoint;virtual;
    Function CorrectNCRect(Const R: TRect): TRect;virtual;
    Function UpdateTopLeftCorner(Const R: TRect): TRect;virtual;
    Function GetArrowRect(Arrow: TPSCCalendarHitTest): TRect;virtual;
    Function GetHeaderRect(Const Date: TDateTime; Var Rect: TRect): Boolean;virtual;
    Function CanSelectionChange(Const Date: TDateTime): WordBool; virtual;
    Function GetReadOnly: Boolean; virtual;
    Function CreateCalendarColors: TPSCCalendarColors; virtual;
    Function GetHeaderText(Const Date: TDateTime): String; virtual;
    Function DateInRange(Const ADate: TDateTime): Boolean; virtual;
    function GetDayName(ADayIndex:Integer;ALength:TPSCWeekDaysLength):String;virtual;

    Function DoDrawItem(const ADate: TDateTime;
      AArea: TPSCCalendarHitTest;ADateType: TPSCCalendarDateType;
      ARect: TRect):WordBool;
    Procedure UpdateWidthHeight(Var AWidth,AHeight: Integer);virtual;
    Procedure BorderChanged;virtual;
    Procedure FontChanged;virtual;
    Procedure DoOnDrawDayOutline(Const ADate: TDateTime; X,Y: Integer;
      Const ARect: TRect; Var AParams: TPSCDrawDayOutlineParams);virtual;
    Procedure InvalidateDateAndSiblings(const ADate:TDateTime);virtual;
    procedure DoMeasureItem(AArea: TPSCCalendarHitTest;
      Var AWidth,AHeight: Integer);virtual;
    Procedure SetAutoSize(V: boolean);{$IFDEF D6} override;{$ELSE}virtual;{$ENDIF}
    Procedure SetStartDate(Value: TDateTime); virtual;
    Procedure DrawDays(const ABounds:TRect;AMonthPos: TPSCMonthPos;
      const ABegDate,ABegMonth,AEndMonth:TDateTime); virtual;
    Procedure CreateWnd; override;
    Procedure InvalidateArrow(Arrow: TPSCCalendarHitTest);virtual;
    Procedure DateChanged; virtual;
    Procedure SetReadOnly(Value: Boolean); virtual;
    Procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    Procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X,Y: Integer); override;
    Procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X,Y: Integer); override;
    Procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    Procedure KeyDown(Var Key: Word; Shift: TShiftState); override;
    Procedure MouseHitTest(Var HitTest: TPSCCalendarHitTest); virtual;
    Procedure PaintMonth(MonthCol,MonthRow: Integer; Const Bounds: TRect);
    Procedure Paint; override;
    Procedure DoGetPaintParams(Const Date: TDateTime;
      Var Color,BkColor: TPSCColor; Var FontStyle: TFontStyles;
      DateType: TPSCCalendarDateType); virtual;
    Procedure OpenMonthPopup(Const Date: TDateTime); virtual;
    Procedure InvalidateWeekSelection;virtual;
    Procedure InvalidateWeek(Const Date: TDateTime);virtual;
    Procedure ClearWeekSelection;virtual;
    Procedure MoveSelection(Delta: Integer);virtual;
    Procedure Select(Delta: Integer; Kind: Boolean);virtual;
    procedure AlignDayName(const ARect:TRect;const ASize:TSize;
      var APoint:TPoint);virtual;
    Property Color default clPSCWindow;
    Property ParentColor default false;
    Property MultiSelect: Boolean read FMultiSelect write SetMultiSelect
      default true;
    Property ClearOnKeyMoving: Boolean read FClearOnKeyMoving
      write FClearOnKeyMoving default false;
    Property MaxSelDates: Integer read FMaxSelDates write SetMaxSelDates
      default 42;
    Property MinDateLimit: Boolean read FMinDateLimit write SetMinDateLimit
      default false;
    Property MinDate: TDateTime read FMinDate write SetMinDate;
    Property MaxDateLimit: Boolean read FMaxDateLimit write SetMaxDateLimit
      default false;
    Property MaxDate: TDateTime read FMaxDate write SetMaxDate;
    Property AutoSize: boolean read GetAutoSize write SetAutoSize stored False;
    Property StartDate: TDateTime read FStartDate write SetStartDate
      stored StartDateStored;
    Property Flat: Boolean read FFlat write SetFlat default true;
    Property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle;
    Property BorderEdges: TBevelEdges read FBorderEdges write SetBorderEdges
      default [beLeft,beTop,beRight,beBottom];
    Property ShowMonthPopup: Boolean read FShowMonthPopup
      write FShowMonthPopup default true;
    Property ShowNavButtons: Boolean read FShowNavButtons
      write SetShowNavButtons default true;
    Property ShowToday: Boolean read FShowToday write SetShowToday default true;
    Property ShowWeekNumbers: Boolean read FShowWeekNumbers
      write SetShowWeekNumbers default false;
    Property WeekDayNames: TPSCWeekDaysLength read FWeekDayNames
      write SetWeekDayNames default wdlOne;
    Property SideWeekSelect: Boolean read FSideWeekSelect write FSideWeekSelect
      default true;
    Property ShowMonthDividers: Boolean read FShowMonthDividers
      write SetShowMonthDividers;
    Property ShowHorzLines: Boolean read FShowHorzLines write SetShowHorzLines
      default false;
    Property ShowVertLines: Boolean read FShowVertLines write SetShowVertLines
      default false;
    Property FirstWeekOfYear: TPSCFirstWeekOfYear read FFirstWeekOfYear
      write SetFirstWeekOfYear default fwJanFirst;
    Property FirstDayOfWeek: TPSCFirstDayOfWeek read FFirstDayOfWeek
      write SetFirstDayOfWeek default dwLocaleDefault;
    Property WorkDays: TPSCWeekDays read FWorkDays write SetWorkDays
      default [dwMonday..dwFriday];
    Property Colors: TPSCCalendarColors read FColors write SetColors;
    Property WeekCursor: TCursor read FWeekCursor write FWeekCursor default crDefault;
    Property SelectKind: TPSCCalSelectKind read FSelectKind write SetSelectKind;
    Property ExtendedSelect: Boolean read FExtendedSelect
      write SetExtendedSelect; 
    Property ReadOnly: Boolean read GetReadOnly write SetReadOnly default false;
    Property OnDateChanged: TPSCNotifyEvent read FOnDateChanged
      write FOnDateChanged;
    Property OnGetPaintParams: TPSCCalendGetPaintParamsEvent read
      FOnGetPaintParams
      write FOnGetPaintParams;
    Property OnCanSelectDate: TPSCCalendCanSelectDateEvent read FOnCanSelectDate
      write FOnCanSelectDate;
    Property OnMeasureItem: TPSCCalendMeasureItemEvent read FOnMeasureItem
      write FOnMeasureItem;
    Property OnDrawItem: TPSCCalendDrawItemEvent read FOnDrawItem write
      FOnDrawItem;
  public
    Property ShiftStateValue:TShiftState Read FShiftState;
    Property UserData:IPSCDateArray Read FUserData;
    Procedure DrawHeader(Const Date: TDateTime; MonthPos: TPSCMonthPos;
      Var Rect: TRect); virtual;
    Procedure DrawArrow(Arrow: TPSCCalendarHitTest; Const Rect: TRect); virtual;
    Procedure DrawWeekLine(Var Rect: TRect); virtual;
    Procedure DrawDaysHeader(MonthPos: TPSCMonthPos;
      Const Rect: TRect); virtual;
    Procedure DrawDay(Const Date: TDateTime; BkgndColor,TextColor: TPSCColor;
      FontStyle: TFontStyles; Area: TPSCCalendarHitTest;
      DateType: TPSCCalendarDateType;
      X,Y: Integer; Const Rect: TRect); virtual;
    Procedure DrawDayOutline(Const Date: TDateTime; X,Y: Integer;
      Const Rect: TRect); virtual;
    Procedure DrawDividers(MonthPos: TPSCMonthPos; Const Rect: TRect); virtual;
    Procedure DrawWeekSide(Const Date: TDateTime; Side: TPSCCalendarHitTest;
      DateType: TPSCCalendarDateType; Const Rect: TRect); virtual;
    Procedure SelectDate(Const Date: TDateTime; Value: Boolean);
    Procedure DefaultDrawDay(Const Date: TDateTime;
      BkgndColor,TextColor: TPSCColor; FontStyle: TFontStyles;
      DateType: TPSCCalendarDateType; X,Y: Integer; Const Rect: TRect);
    Procedure UpdateWeekSelection(NeedIvalidate: Boolean);
    Function SelectionIsInOneWeek: boolean;
    Function WorkDaysSelected: boolean;
    Procedure SelectionToView;
    Procedure SelectWorkWeek(Const ADate: TDateTime);
    Procedure RemeasureCalendar;
    Procedure BeginUpdate; virtual;
    Procedure EndUpdate; virtual;

    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    Function GetSelDate(Const Date: TDateTime): Boolean;
    Function GetNotSelDate(Const Date: TDateTime): Boolean;
    Function MonthColCount: Integer;virtual;
    Function MonthRowCount: Integer;virtual;
    Function GetMonthWidth: Integer;virtual;
    Function GetMonthHeight: Integer;virtual;
    Function DateToPos(Const Date: TDateTime; Var MonthCol,
      MonthRow: Integer): Boolean;virtual;
    Function GetHitTest(P: TPoint; Var Date: TDateTime): TPSCCalendarHitTest;overload;virtual;
    Function GetHitTest(X,Y: Integer; Var Date: TDateTime): TPSCCalendarHitTest;overload;
    Function DateToRect(Const Date: TDateTime; Var Rect: TRect): Boolean;overload;
    Function DateToRect(Const ADate: TDateTime;
      Var ALeft, ATop, ARight, ABottom: Integer): Boolean;overload;virtual;
    Function GetBeginOfWeek(Const Date: TDateTime): TDateTime;
    Function GetEndOfWeek(Const Date: TDateTime): TDateTime;
    Function GetWeekNumber(Const Date: TDateTime): Integer;
    Function StartFromNextMonth: Boolean;
    Function StartFromPrevMonth: Boolean;
    Function StartFromNextYear: Boolean;
    Function StartFromPrevYear: Boolean;
    Function SelectDay(Const Date: TDateTime; ClearSelection: Boolean): Boolean;
    Function SelectWeek(Date: TDateTime; ClearSelection: Boolean): Boolean;
    Function SelectMonth(Date: TDateTime; ClearSelection: Boolean): Boolean;
    Function ClearSelection: Boolean;

    Procedure SelectToPrevDay;
    Procedure SelectToNextDay;
    Procedure SelectToPrevWeek;
    Procedure SelectToNextWeek;
    Procedure SelectToWeekBeg;
    Procedure SelectToWeekEnd;
    Procedure SelectToMonthBeg;
    Procedure SelectToMonthEnd;
    Procedure SelectToCalendBeg;
    Procedure SelectToCalendEnd;
    Procedure ChangeSelection(Const ASelStart,ASelFinish: TDateTime);
    Procedure Print;
    procedure PrintWithDialog;

    Property UpdateCount: Integer read FUpdateCount;
    Property Canvas;
    Property CursorDate: TDateTime read FCursorDate write SetCursorDate;
    Property EndDate: TDateTime read GetEndDate;
    Property SelStart: TDateTime read FSelStart write SetSelStart;
    Property SelFinish: TDateTime read FSelFinish write SetSelFinish;
    Property SelDate[Const Date: TDateTime]: Boolean read GetSelDate
    write SelectDate;
    Property SelCount: Integer read FSelCount;
    Property WeeksSelected: Boolean read FWeeksSelected write SetWeeksSelected;
    Property ColWidth: Integer read FColWidth;
    Property SideWidth: Integer read FSideWidth;
    Property RowHeight: Integer read FRowHeight;
    Property HeaderHeight: Integer read FHeaderHeight;
    Property DaysOfWeekHeight: Integer read FDaysOfWeekHeight;
    Property SelShiftState: TShiftState read FSelShiftState;
    Property Modified: Boolean read FModified;
    Property PrintOptions: TPSCCalendarPrintOptions read FPrintOptions write
      SetPrintOptions;
    Property PrintedPageCurrent: Integer read FPrintedPageCurrent;
    Property PrintedPageTotal: Integer read FPrintedPageTotal;
    Property CancelPrinting: Boolean read FCancelPrinting write FCancelPrinting;
    property Options : TPSCCalendarOptions read FOptions write SetOptions;
  public
    Property OnStartDateChange: TPSCNotifyEvent Read FOnStartDateChange Write FOnStartDateChange;
    Property TabStop default True;
    Property ParentShowHint default False;
    Property ShowHint default True;
    Property Version: String read FVersion write SetVersion stored false;
    Property NavButtonsTimer: boolean read FNavButtonsTimer write
      FNavButtonsTimer default cPSCDefNavButtonsTimer;
    Property NavButtonsSpeed: Integer Read FNavButtonsSpeed Write FNavButtonsSpeed;
    Property OnDrawDayOutline: TPSCOnDrawDayOutline read FOnDrawDayOutline write
      FOnDrawDayOutline;
    Property OnPrintProgress: TPSCNotifyEvent read FOnPrintProgress write
      FOnPrintProgress;
    Property BevelEdges;
    Property BevelInner;
    Property BevelOuter;
    Property BevelKind;
    Property BevelWidth;
  End;

  TPSCMonthPopupType = (ptStandard,ptMSMoney,ptMonthCalendar);
  TPSCSelectedArrow = (saNotSelected,saLeftArrow,saRightArrow);

  TPSCCalendarColorsPro = Class(TPSCCalendarColors)
  private
    FHolidaysFont: TPSCFont;
    FArrowColor: TPSCColor;
    FWeekLineColor : TPSCColor;
    FHeaderBorderColor : TPSCColor;
    FGrayedBkColor : TPSCColor;
    Function IsArrowColorStored : boolean;
    Function IsWeekLineColorStored : boolean;
    Function IsGrayedBkColorStored : boolean;
    Function IsDayLinesStyleStored : boolean;
    Function IsBorderStored(Index : integer) : boolean;
    Function IsMonthHeaderStored(Index : integer) : boolean;
    Function IsWeekHeaderStored(Index : integer) : boolean;
    Function IsDaysStored(Index : integer) : boolean;
    Function IsDayLinesStored(Index : integer) : boolean;
    Function IsSelectedStored(Index : integer) : boolean;
    Function IsSelectedTextStored(Index : integer) : boolean;
    Function IsHeaderBorderColorStored : boolean;
    Function IsGrayedStored(Index : integer) : boolean;
    Function IsWeekEndTextStored(Index : integer) : boolean;
    Function IsNowRectStored(Index : integer) : boolean;
    Function IsWeekSideStored(Index : integer) : boolean;
    Function IsDaysFontStored : boolean;
    Function IsHolidaysFontStored: boolean;

    Procedure SetHeaderBorderColor(AValue : TPSCColor);
    Procedure SetWeekLineColor(AValue : TPSCColor);
    Procedure SetGrayedBkColor(AValue : TPSCColor);
    Procedure SetArrowColor(Value: TPSCColor);
    Procedure SetHolidaysFont(Value: TPSCFont);
  public
    Constructor Create(AOwner: TPSCCustomCalendar);
    Destructor Destroy; override;
    Procedure Assign(Source: TPersistent); override;
  published
    Property HolidaysFont: TPSCFont read FHolidaysFont write SetHolidaysFont
      Stored IsHolidaysFontStored;
    property ArrowColor: TPSCColor  read FArrowColor write SetArrowColor
     stored IsArrowColorStored;
    property WeekLineColor : TPSCColor read FWeekLineColor write SetWeekLineColor
     stored IsWeekLineColorStored;
    property HeaderBorderColor : TPSCColor read FHeaderBorderColor
     write SetHeaderBorderColor stored IsHeaderBorderColorStored;
    property GrayedBkColor : TPSCColor read FGrayedBkColor write SetGrayedBkColor
     stored IsGrayedBkColorStored;
    property DayLinesStyle stored IsDayLinesStyleStored;
    property Border stored IsBorderStored;
    property MonthHeader stored IsMonthHeaderStored;
    property WeekHeader stored IsWeekHeaderStored;
    property Days stored IsDaysStored;
    property DayLines stored IsDayLinesStored;
    property Selected stored IsSelectedStored;
    property SelectedText stored IsSelectedTextStored;
    property WeekDaysFont;
    property WeekNumbersFont;
    property HeaderFont;
    property Grayed stored IsGrayedStored;
    property WeekEndText stored IsWeekEndTextStored;
    property NowRect stored IsNowRectStored;
    property WeekSide stored IsWeekSideStored;
    property DaysFont stored IsDaysFontStored;
  End;

  TPSCShowCountryInHoliday = (cihAlways,cihNever,cihSmart);

  TPSCCustomCalendarPro = Class(TPSCCustomCalendar)
  private
    FForceShowHint: boolean;
    FHolidayNames: IPSCStrings;
    FDefaultHolidays: boolean;
    FOnGetHolidayDate: TPSCOnGetHolidayDate;
    FCalendarStyle: TPSCCalendarStyle;
    FPopupType: TPSCMonthPopupType;
    FMonths: TPSCPopupForm;
    FShowArrowEdges: Boolean;
    FArrowStyle: TPSCArrowStyle;
    FImages: TImageList;
    FLeftArrow: Integer;
    FLeftArrowHot: Integer;
    FRightArrow: Integer;
    FRightArrowHot: Integer;
    FShowHolidays: Boolean;
    FCountries: Array of TPSCCountryID;
    FHeaderCase: TPSCTextCase;
    FShortMonthName: Boolean;
    FHolidays: TPSCHolidayItems;
    FSelectedArrow: TPSCSelectedArrow;
    FCountryInHoliday:TPSCShowCountryInHoliday;

    Procedure PopupClosed(Sender: TObject; Canceled: Boolean);
    Function IsCustomHolidaysStored: boolean;
    Procedure SetDefaultHolidays(V: boolean);
    Procedure SetColors(Value: TPSCCalendarColorsPro);
    Function GetColors: TPSCCalendarColorsPro;
    Procedure SetShowArrowEdges(Value: Boolean);
    Procedure SetImages(Value: TImageList);
    Procedure SetLeftArrow(Value: Integer);
    Procedure SetLeftArrowHot(Value: Integer);
    Procedure SetRightArrow(Value: Integer);
    Procedure SetRightArrowHot(Value: Integer);
    Procedure SetShowHolidays(Value: Boolean);
    Procedure SetHolidays(Value: TPSCHolidayItems);
    Procedure SetHeaderCase(Value: TPSCTextCase);
    Procedure SetShortMonthName(Value: Boolean);
    Procedure HolidaysChange(Sender: TObject);
    Procedure WMCaptureChanged(Var Message: TMessage);
      message WM_CAPTURECHANGED;
    Procedure ItemsOnGetHolidayDate(Sender: TObject; Holiday: TPSCHolidayItem;
      Const BaseDate: TDateTime; Var ADate: TDateTime);
  protected
    Procedure OpenMonthPopup(Const Date: TDateTime);override;
    Procedure SetCalendarStyle(Value: TPSCCalendarStyle);virtual;
    Procedure SetArrowStyle(Value: TPSCArrowStyle);virtual;
    property SelectedArrow: TPSCSelectedArrow Read FSelectedArrow;

    Procedure CMHintShow(Var Message: TCMHintShow); message CM_HINTSHOW;
    Procedure CMMouseLeave(Var Message: TMessage); message CM_MOUSELEAVE;

    Function GetHintStr(Var HintStr: String; CursorPos: TPoint;
      Var CursorRect: TRect): Integer; virtual;
    Procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    Procedure MouseHitTest(Var HitTest: TPSCCalendarHitTest); override;
    Function GetHeaderText(Const Date: TDateTime): String; override;
    Procedure DoOnGetHolidayDate(Holiday: TPSCHolidayItem; Const BaseDate:
      TDateTime;
      Var ADate: TDateTime); virtual;
    Function CreateCalendarColors: TPSCCalendarColors; override;
  public
    procedure SetHolidayCountries(const ACountries:Array of TPSCCountryID);overload;
    procedure EnumHolidayCountries(const S:IPSCStrings);
    procedure SetHolidayCountries(const ACountries:IPSCStrings);overload;

    Procedure DrawHeader(Const Date: TDateTime; MonthPos: TPSCMonthPos;
      Var Rect: TRect); override;
    Procedure DrawArrow(Arrow: TPSCCalendarHitTest;
      Const Rect: TRect); override;
    Procedure DrawWeekLine(Var Rect: TRect); override;
    Procedure DrawDay(Const Date: TDateTime; BkgndColor,TextColor: TPSCColor;
      FontStyle: TFontStyles; Area: TPSCCalendarHitTest;
      DateType: TPSCCalendarDateType;
      X,Y: Integer; Const Rect: TRect); override;
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    Property HolidayNames: IPSCStrings read FHolidayNames;
    Function GetHolidayNames(Const ADate: TDateTime;
      const HolidayNames: IPSCStrings): boolean;virtual;
  protected
    Property CalendarStyle: TPSCCalendarStyle read FCalendarStyle
      write SetCalendarStyle default cstOutlook;
    Property Colors: TPSCCalendarColorsPro read GetColors write SetColors;
    Property PopupType: TPSCMonthPopupType read FPopupType write FPopupType
      default ptStandard;
    Property ShowArrowEdges: Boolean read FShowArrowEdges
      write SetShowArrowEdges default false;
    Property ArrowStyle: TPSCArrowStyle read FArrowStyle write SetArrowStyle;
    Property Images: TImageList read FImages write SetImages;
    Property LeftArrow: Integer read FLeftArrow write SetLeftArrow
      default -1;
    Property LeftArrowHot: Integer read FLeftArrowHot write SetLeftArrowHot
      default -1;
    Property RightArrow: Integer read FRightArrow write SetRightArrow
      default -1;
    Property RightArrowHot: Integer read FRightArrowHot write SetRightArrowHot
      default -1;
    Property ShowHolidays: Boolean read FShowHolidays write SetShowHolidays
      default false;
    Property CustomHolidays: TPSCHolidayItems read FHolidays write SetHolidays
      stored IsCustomHolidaysStored;
    Property HeaderCase: TPSCTextCase read FHeaderCase write SetHeaderCase
      default tcDefault;
    Property ShortMonthName: Boolean read FShortMonthName
      write SetShortMonthName default false;
    Property OnGetHolidayDate: TPSCOnGetHolidayDate read FOnGetHolidayDate
      write FOnGetHolidayDate;
    Property DefaultHolidays: boolean read FDefaultHolidays write
      SetDefaultHolidays default True;
    Property ForceShowHint: boolean read FForceShowHint write FForceShowHint
      default False;
    property CountryInHoliday:TPSCShowCountryInHoliday Read FCountryInHoliday
      Write FCountryInHoliday Default cihAlways;
  End;

type
  TPSCCalendarSelStyle = (
    sslFillRect,
    sslEllipse,
    sslFrameRect
  );
  
  TPSCCalendarDayHeaderStyle =(
    dhsOutlook,
    dhsOutlookMonthView
  );

const
  CPSCDefCalSelectStyle = sslFillRect;
  CPSCDefCalVertAlign = vaCenter;
  CPSCDefCalHorzAlign = haCenter;
  CPSCDefPastDaysAsGrayed = false;
  CPSCDefShowFocusRect = false;
  CPSCDefShowFooter = false;
  CPSCDefShowHeader = true;
  CPSCDefWantEsc = true;
  CPSCDefShowDayDelimiters = false;
  CPSCDefDayHeaderStyle = dhsOutlook;
  CPSCDefSelectionAndBorderDists = 0;
  CPSCDefAlterEvenOdd = false;

  clPSCMonthViewBorder = $00F9FEFF;
  clPSCMonthViewHeader = $00E3EFF3;
  clPSCMonthViewDays = $00EFF7F7;
  clPSCMonthViewSelected = $00DEDEDE;
  clPSCMonthViewWeekLine = $00B3BFC3;
type

  TPSCCalendarSelShape = (
    sstEllipse,
    sstRightEllipse,
    sstLeftEllipse,
    sstRectangle
  );

  TPSCCalendarWeekLineStyle = (
    wlsIndentedLine,
    wlsFullLine
  );

  TPSCCalendarTodayStyle = (
    ctsSquare,
    ctsHelix
  );

  TPSCCalendarHeaderStyle = (
    chsOutlook,
    chsMonthCalendar,
    chsWhiteOnBlack,
    chsBlackOnWhite,
    chsMSMoney2002,
    chsForPrinting,
    chsOutlookMonthView
  );

  TPSCCalendarSizeStyle = (
    cssOutlook,
    cssMonthCalendar
  );

  TPSCFontAttr = packed record
    Color : TPSCColor;
    Size : integer;
    Style : TFontStyles;
  end;

  TPSCCalendarState = packed record
    Color : TPSCColor;
    CalendarsInHeight : integer;
    CalendarsInWidth : integer;
    WantEsc : boolean;
    SelectionAndBorderDists : integer;
    ShortMonthName : boolean;
    ShowNavButtons : boolean;
    ShowMonthDividers : boolean;
    DayHeaderStyle : TPSCCalendarDayHeaderStyle;
    AlterEvenOdd : boolean;
    FirstDayFormat : string;
    ShowFocusRect : boolean;
    HeaderCase : TPSCTextCase;
    PastDaysAsGrayed : boolean;
    NavButtonsSpeed : integer;
    TodayStyle : TPSCCalendarTodayStyle;
    ShowArrowEdges : boolean;
    HeaderFormat : string;
    DaysFont : TPSCFontAttr;
    ArrowColor : TPSCColor;
    Border : TPSCColor;
    MonthHeader : TPSCColor;
    WeekHeader : TPSCColor;
    Days : TPSCColor;
    DayLines : TPSCColor;
    Selected : TPSCColor;
    SelectedText : TPSCColor;
    WeekDaysFont : TPSCFontAttr;
    WeekNumbersFont : TPSCFontAttr;
    HolidaysFont : TPSCFontAttr;
    HeaderFont : TPSCFontAttr;
    Grayed : TPSCColor;
    WeekEndText : TPSCColor;
    NowRect : TPSCColor;
    WeekSide : TPSCColor;
    WeekDayNames : TPSCWeekDaysLength;
    WeekLineColor : TPSCColor;
    HeaderBorderColor : TPSCColor;
    GrayedBkColor : TPSCColor;
    Flat : boolean;
    WeekLineStyle : TPSCCalendarWeekLineStyle;
    SelectStyle : TPSCCalendarSelStyle;
    PopupType : TPSCMonthPopupType;
    ShowHeader : boolean;
    HeaderStyle : TPSCCalendarHeaderStyle;
    ArrowStyle : TPSCArrowStyle;
    WeekDayCase : TPSCTextCase;
    SizeStyle : TPSCCalendarSizeStyle;
    ShowFooter : boolean;
    ShowToday : boolean;
    DayVertAlign : TPSCVertAlign;
    DayHorzAlign : TPSCHorzAlign;
    DayHeaderHorzAlign : TPSCHorzAlign;
    BorderStyle : TBorderStyle;
    ShowDayDelimiters : boolean;
    SelectKind : TPSCCalSelectKind;
    ShowVertLines : boolean;
    ShowHorzLines : boolean;
    ShowMonthPopup : boolean;
    WeekCursor : TCursor;
    ExtendedSelect : boolean;
    DayLinesStyle : TPenStyle;
    Options : TPSCCalendarOptions;
  End;

  PPSCCalendarState = ^TPSCCalendarState;

  TPSCCustomCalendarPro2 = class(TPSCCustomCalendarPro)
  private
    FCalendarState: TPSCCalendarState;

    FYearPopupMonthNumber : integer;  
    FHeaderFormat : string;
    FFirstDayFormat : string;
    FWeekLineStyle : TPSCCalendarWeekLineStyle;
    FSelectStyle : TPSCCalendarSelStyle;
    FPopupHeaderYear : TObject;
    FWantEsc : boolean;
    FMonthsPopup : TPopupMenu;
    FShowFocusRect : boolean;
    FTodayStyle : TPSCCalendarTodayStyle;
    FShowHeader : boolean;
    FHeaderStyle : TPSCCalendarHeaderStyle;
    FShowFooter : boolean;
    FCalendarsInWidth : integer;
    FCalendarsInHeight : integer;
    FSizeStyle : TPSCCalendarSizeStyle;
    FPastDaysAsGrayed : boolean;
    FWeekDayCase : TPSCTextCase;
    FDayVertAlign : TPSCVertAlign;
    FDayHorzAlign : TPSCHorzAlign;
    FDayHeaderHorzAlign : TPSCHorzAlign;
    FIsLeftMouseDown : boolean;
    FAlterEvenOdd : boolean;
    FShowDayDelimiters : boolean;
    FDayHeaderStyle : TPSCCalendarDayHeaderStyle;
    FSelectionAndBorderDists : integer;
    FMouseDate : TDateTime;

    Function IsOptionsStored:Boolean;
    function GetFirstDayText(const ADate : TDateTime; const AText : string;
     const ARect : TRect; AFirstDayFormat : string) : string;
    function GetSelectShape(const ADate : TDateTime) : TPSCCalendarSelShape;
    function IsHeaderFormatStored : boolean;
    function IsFirstDayFormatStored : boolean;
    function IsTodayStyleStored : boolean;
    function IsHeaderStyleStored : boolean;
    function IsSizeStyleStored : boolean;
    function IsDayVertAlignStored  : boolean;
    function IsDayHorzAlignStored : boolean;
    function IsDayHeaderHorzAlignStored : boolean;
    function IsAlterEvenOddStored : boolean;
    function IsWeekDayCaseStored : boolean;
    function IsPastDaysAsGrayedStored : boolean;
    function IsShowFocusRectStored : boolean;
    function IsShowFooterStored : boolean;
    function IsShowHeaderStored : boolean;
    function IsWeekLineStyleStored : boolean;
    function IsSelectStyleStored : boolean;
    function IsWantEscStored : boolean;
    function IsShowDayDelimitersStored : boolean;
    function IsDayHeaderStyleStored : boolean;
    function IsSelectionAndBorderDistsStored: Boolean;
    function IsCalendarsInHeightStored : boolean;
    function IsCalendarsInWidthStored : boolean;
    function IsColorStored : boolean;
    function IsShortMonthNameStored : boolean;
    function IsShowNavButtonsStored : boolean;
    function IsShowMonthDividersStored : boolean;
    function IsHeaderCaseStored : boolean;
    function IsNavButtonsSpeedStored : boolean;
    function IsShowArrowEdgesStored : boolean;
    function IsWeekDayNamesStored : boolean;
    function IsFlatStored : boolean;
    function IsPopupTypeStored : boolean;
    function IsArrowStyleStored : boolean;
    function IsShowTodayStored : boolean;
    function IsBorderStyleStored : boolean;
    function  IsSelectKindStored : boolean;
    function IsShowVertLinesStored : boolean;
    function IsShowHorzLinesStored : boolean;
    function IsShowMonthPopupStored : boolean;
    function IsWeekCursorStored : boolean;
    function IsExtendedSelectStored : boolean;

    function GetBkColor(const ADate : TDateTime; AArea: TPSCCalendarHitTest;
     const BkgndColor :TPSCColor): TPSCColor;
    function GetMouseDate : TDateTime;
    function GetCalDateRect(ADate : TDateTime) : TRect;
    procedure SetHeaderFormat(const AValue : string);
    procedure SetFirstDayFormat(const AValue : string);
    procedure SetWeekLineStyle(AValue : TPSCCalendarWeekLineStyle);
    procedure SetSelectStyle(AValue : TPSCCalendarSelStyle);
    procedure SetShowFocusRect(AValue : boolean);
    procedure SetShowDayDelimiters(AValue : boolean);
    procedure SetSelectionAndBorderDists(AValue : integer);
    procedure DrawArrowAsMonth(AArea: TPSCCalendarHitTest;
      const AArrowRect, ARect :TRect; AIsArrowDown : boolean);
    procedure CustomSelect(const ADate : TDateTime;
      ARect : TRect; const AText : string);
    procedure DoYearChange(Sender : TObject);
    procedure PopupAsMonthInit;
    procedure DoPopupAsMonthClick(Sender: TObject);
    procedure SetTodayStyle(AValue: TPSCCalendarTodayStyle);
    procedure SetShowHeader(AValue : boolean);
    procedure SetHeaderStyle(AValue : TPSCCalendarHeaderStyle);
    procedure SetShowFooter(AValue : boolean);
    procedure SetCalendarsInWidth(AValue : integer);
    procedure SetCalendarsInHeight(AValue : integer);
    procedure SetSizeStyle(AValue : TPSCCalendarSizeStyle);
    procedure SetAlterEvenOdd(AValue : boolean);
    procedure DoPopupClosed(Sender : TObject; ACanceled : boolean);
    procedure CustomDayDraw(const ADate: TDateTime; ASelect: boolean;
      ADateType : TPSCCalendarDateType; const ARect: TRect;
      const AText: string;  AArea: TPSCCalendarHitTest; BkgndColor,
      TextColor: TPSCColor; FontStyle: TFontStyles; X, Y: Integer);
    procedure ShowYearPopup(ALeft, ATop : integer; const ADate : TDateTime);
    procedure SetPastDaysAsGrayed(AValue : boolean);
    procedure SetWeekDayCase(AValue : TPSCTextCase);
    procedure SetDayVertAlign(AValue : TPSCVertAlign);
    procedure SetDayHorzAlign(AValue : TPSCHorzAlign);
    procedure SetDayHeaderHorzAlign(AValue : TPSCHorzAlign);
    procedure SetIsLeftMouseDown(AValue : boolean);
    procedure SetDayHeaderStyle(AValue : TPSCCalendarDayHeaderStyle);
    procedure GetDayXYAligned(const ARect : TRect; const ASize:TSize;
     var ASlipX, ASlipY : integer);
    procedure UpdateDaySelectRect(const ASourceRect : TRect; const ASize:TSize;
     var ADestRect : TRect);
    procedure UpdateDayRect(const ASourceRect : TRect; const AText : string;
     var ADestRect : TRect);
    procedure DoMouseOnHitTest(const ADate : TDateTime; AHitTest: TPSCCalendarHitTest;
      AP : TPoint; var AYearInHeader: TRect; var AMonthInHeader : TRect;
      var ALeft : integer; var ATop : integer);
    procedure UpdateMeasure;
    procedure DoMeasureItemAsMonthCalendar(AArea: TPSCCalendarHitTest;
     var AWidth, AHeight: Integer);
    property PopupHeaderYear : TObject read FPopupHeaderYear write FPopupHeaderYear;
    property MonthsPopup : TPopupMenu read FMonthsPopup write FMonthsPopup;
    procedure SetMouseDate(const Value: TDateTime);
    procedure SetOtherProperties;
  protected
    function GetTextSize(AText : string) : TSize;
    function GetArrowRect(Arrow: TPSCCalendarHitTest): TRect; override;
    function GetDayName(ADayIndex:Integer;ALength:TPSCWeekDaysLength):String; override;
    function CorrectNCRect(Const R: TRect): TRect; override;
    procedure AlignDayName(const ARect:TRect;const ASize:TSize;
      var APoint:TPoint);override;
    procedure DrawFooterHelix(ACanvas : TPSCCanvas; ARect : TRect);
    procedure UpdateWidthHeight(var AWidth,AHeight: Integer); override;
    procedure Changed; virtual;
    procedure KeyDown(Var Key: Word; Shift: TShiftState); override;
    procedure DrawFooterMonthCalendar(const ARect: TRect);
    procedure DrawFooter(const ARect: TRect); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X,Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X,Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;

    procedure DoMeasureItem(AArea: TPSCCalendarHitTest;
      Var AWidth,AHeight: Integer); override;
    procedure DoOnDrawDayOutline(Const ADate: TDateTime; X,Y: Integer;
      Const ARect: TRect; Var AParams: TPSCDrawDayOutlineParams); override;
    procedure Paint; override;
    procedure FontChanged; override;
    procedure OpenMonthPopup(Const Date: TDateTime); override;
  public
    property IsLeftMouseDown : boolean read FIsLeftMouseDown write SetIsLeftMouseDown;
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    function GetHeaderText(Const Date: TDateTime): String; override;
    function GetHitTest(P: TPoint; Var Date: TDateTime): TPSCCalendarHitTest; overload; override;
    function GetCalendarWidth : integer;
    function GetCalendarHeight : integer;
    procedure DrawDaysHeader(MonthPos: TPSCMonthPos;
      Const Rect: TRect); override;
    procedure DrawHeader(Const Date: TDateTime; MonthPos: TPSCMonthPos;
      Var Rect: TRect); override;
    procedure DrawArrow(Arrow: TPSCCalendarHitTest; const Rect: TRect); override;
    procedure SetCalendarStyle(Value: TPSCCalendarStyle); override;
    procedure DrawDay(Const Date: TDateTime; BkgndColor,TextColor: TPSCColor;
      FontStyle: TFontStyles; Area: TPSCCalendarHitTest;
      DateType: TPSCCalendarDateType; X,Y: Integer; Const Rect: TRect); override;
    procedure DrawWeekLine(Var Rect: TRect); override;
    Procedure DrawDividers(MonthPos: TPSCMonthPos; Const Rect: TRect); override;
    Procedure DrawWeekSide(Const Date: TDateTime; Side: TPSCCalendarHitTest;
      DateType: TPSCCalendarDateType; Const Rect: TRect); override;
    property MouseDate : TDateTime read GetMouseDate write SetMouseDate;
    property Color stored IsColorStored nodefault;
    property ShortMonthName stored IsShortMonthNameStored;
    property ShowNavButtons stored IsShowNavButtonsStored;
    property ShowMonthDividers stored IsShowMonthDividersStored;
    property HeaderCase stored IsHeaderCaseStored;
    property NavButtonsSpeed stored IsNavButtonsSpeedStored;
    property ShowArrowEdges stored IsShowArrowEdgesStored;
    property WeekDayNames stored IsWeekDayNamesStored;
    property Flat stored IsFlatStored;
    property PopupType stored IsPopupTypeStored;
    property ArrowStyle stored IsArrowStyleStored;
    property ShowToday stored IsShowTodayStored;
    property BorderStyle stored IsBorderStyleStored;
    property SelectKind stored IsSelectKindStored;
    property ShowVertLines stored IsShowVertLinesStored;
    property ShowHorzLines stored IsShowHorzLinesStored;
    property ShowMonthPopup stored IsShowMonthPopupStored;
    property WeekCursor stored IsWeekCursorStored;
    property ExtendedSelect stored IsExtendedSelectStored;
    property Options Stored IsOptionsStored;

    property HeaderFormat: string read FHeaderFormat write SetHeaderFormat stored IsHeaderFormatStored;
    property FirstDayFormat : string read FFirstDayFormat write SetFirstDayFormat stored IsFirstDayFormatStored;
    property WeekLineStyle : TPSCCalendarWeekLineStyle read FWeekLineStyle write SetWeekLineStyle stored IsWeekLineStyleStored;
    property SelectStyle : TPSCCalendarSelStyle read FSelectStyle write SetSelectStyle stored IsSelectStyleStored;
    property WantEsc : boolean read FWantEsc write FWantEsc stored IsWantEscStored;
    property TodayStyle : TPSCCalendarTodayStyle read FTodayStyle write SetTodayStyle stored IsTodayStyleStored;
    property ShowHeader : boolean read FShowHeader write SetShowHeader stored IsShowHeaderStored;
    property HeaderStyle : TPSCCalendarHeaderStyle read FHeaderStyle write SetHeaderStyle stored IsHeaderStyleStored;
    property ShowFooter : boolean read FShowFooter write SetShowFooter stored IsShowFooterStored;
    property CalendarsInHeight : integer read FCalendarsInHeight write SetCalendarsInHeight stored IsCalendarsInHeightStored;
    property CalendarsInWidth : integer read FCalendarsInWidth write SetCalendarsInWidth stored IsCalendarsInWidthStored;
    property ShowFocusRect : boolean read FShowFocusRect write SetShowFocusRect stored IsShowFocusRectStored;
    property PastDaysAsGrayed : boolean read FPastDaysAsGrayed write SetPastDaysAsGrayed stored IsPastDaysAsGrayedStored;
    property SizeStyle : TPSCCalendarSizeStyle read FSizeStyle write SetSizeStyle stored IsSizeStyleStored;
    property WeekDayCase : TPSCTextCase read FWeekDayCase write SetWeekDayCase stored IsWeekDayCaseStored;
    property DayVertAlign : TPSCVertAlign read FDayVertAlign write SetDayVertAlign stored IsDayVertAlignStored;
    property DayHorzAlign : TPSCHorzAlign read FDayHorzAlign write SetDayHorzAlign stored IsDayHorzAlignStored;
    property DayHeaderHorzAlign : TPSCHorzAlign read FDayHeaderHorzAlign write SetDayHeaderHorzAlign stored IsDayHeaderHorzAlignStored;
    property AlterEvenOdd : boolean read FAlterEvenOdd write SetAlterEvenOdd stored IsAlterEvenOddStored;
    property ShowDayDelimiters : boolean read FShowDayDelimiters write SetShowDayDelimiters stored IsShowDayDelimitersStored;
    property DayHeaderStyle : TPSCCalendarDayHeaderStyle read FDayHeaderStyle write SetDayHeaderStyle stored IsDayHeaderStyleStored;
    property SelectionAndBorderDists : integer read FSelectionAndBorderDists write SetSelectionAndBorderDists stored IsSelectionAndBorderDistsStored;
  end;

  TPSCCalendar = Class(TPSCCustomCalendarPro2)
  published
    Property CalendarStyle;
    Property Options;

    property HeaderFormat;
    property FirstDayFormat;
    property WeekLineStyle;
    property SelectStyle;
    property WantEsc;
    property TodayStyle;
    property ShowHeader;
    property HeaderStyle;
    property ShowFooter;
    property CalendarsInHeight;
    property CalendarsInWidth;
    property ShowFocusRect;
    property PastDaysAsGrayed;
    property SizeStyle;
    property WeekDayCase;
    property DayVertAlign;
    property DayHorzAlign;
    property DayHeaderHorzAlign;
    property AlterEvenOdd;
    property ShowDayDelimiters;
    property DayHeaderStyle;
    property SelectionAndBorderDists;

    property CountryInHoliday;
    Property OnStartDateChange;
    Property Version;
    Property NavButtonsTimer;
    Property NavButtonsSpeed;
    Property OnDrawDayOutline;
    Property OnPrintProgress;
    Property BevelEdges;
    Property BevelInner;
    Property BevelOuter;
    Property BevelKind;
    Property BevelWidth;

    Property Align;
    Property Anchors;
    Property BiDiMode;
    Property DockSite;
    Property DragCursor;
    Property DragKind;
    Property ParentBiDiMode;
    Property Color default clPSCWindow;
    Property Constraints;
    Property DragMode;
    Property Enabled;
    Property Font;
    Property ParentColor default false;
    Property ParentFont;
    Property ParentShowHint;
    Property PopupMenu;
    Property ShowHint;
    Property TabOrder;
    Property TabStop;
    Property Visible;
    Property OnClick;
    Property OnDblClick;
    Property OnDragDrop;
    Property OnDockDrop;
    Property OnDockOver;
    Property OnEndDock;
    Property OnGetSiteInfo;
    Property OnStartDock;
    Property OnUnDock;
    Property OnDragOver;
    Property OnEndDrag;
    Property OnEnter;
    Property OnExit;
    Property OnKeyDown;
    Property OnKeyPress;
    Property OnKeyUp;
    Property OnMouseDown;
    Property OnMouseMove;
    Property OnMouseUp;
    Property OnStartDrag;

    Property ForceShowHint;
    Property AutoSize;
    Property MultiSelect;
    Property ClearOnKeyMoving;
    Property PrintOptions;
    Property ShowMonthPopup;
    Property ShowNavButtons;
    Property ShowToday;
    Property SideWeekSelect;
    Property ShowMonthDividers;
    Property ShowWeekNumbers;
    Property ShowHorzLines;
    Property ShowVertLines;
    Property WeekDayNames;
    Property FirstWeekOfYear;
    Property FirstDayOfWeek;
    Property WorkDays;
    Property MinDateLimit;
    Property MinDate;
    Property MaxDateLimit;
    Property MaxDate;
    Property StartDate;
    Property Flat;
    Property BorderStyle;
    Property BorderEdges;
    Property MaxSelDates;
    Property WeekCursor;
    Property SelectKind;
    Property ExtendedSelect;
    Property ReadOnly;
    Property Colors;
    Property OnDateChanged;
    Property OnGetPaintParams;
    Property OnCanSelectDate;
    Property OnMeasureItem;
    Property OnDrawItem;

    Property ArrowStyle;
    Property PopupType;
    Property Images;
    Property ShowArrowEdges;
    Property LeftArrow;
    Property LeftArrowHot;
    Property RightArrow;
    Property RightArrowHot;
    Property ShowHolidays;
    Property CustomHolidays;
    Property HeaderCase;
    Property ShortMonthName;
    Property OnGetHolidayDate;
    Property DefaultHolidays;
  End;

Function PSCCalcWorkDays(Const StartDate,FinishDate: TDateTime;
  const Countries:Array of TPSCCountryID; CustomHolidays: TPSCHolidayItems;
  DefaultHolidays: boolean; WorkDays: TPSCWeekDays;
  ExcludeEvent: TPSCDateSelectedProc): Integer;
Function PSCAddAllHolidays(Const ADate: TDateTime;
  const ACountries:Array of TPSCCountryID;
  const HolidayList:IPSCStrings;DefaultHolidays: boolean): boolean;

function PSCDateToDay (const DT: TDateTime): Word;
function PSCWorkDaysToWeekEnds(WorkDays:TPSCWeekDays):TPSCWeekDays;
function PSCWeekEndsToWorkDays(WeekEnds:TPSCWeekDays):TPSCWeekDays;
function PSCIncDayEx(Const Date: TDateTime; NumberOfDays:Integer;
  ExcludeEvent:TPSCDateSelectedProc): TDateTime;
function PSCDateToStrY2k(const Date:TDateTime):String;

type
  PPSCGetCalendarFilterStrParams=^TPSCGetCalendarFilterStrParams;
  TPSCGetCalendarFilterStrParams=packed record
    DateSelectedProc: TPSCDateSelectedProc;
    OrStr: String;
    AndStr: String;
  end;
  
function PSCGetCalendarFilterStrEx(const SelStart,SelFinish:TDateTime;
  const DateField:String;AParams:PPSCGetCalendarFilterStrParams):String;

type
  TPSCCalendarClass=class of TPSCCalendar;

  TPSCCustomDBCalendar = Class(TPSCCustomCalendarPro2)
  private
    FDataLink: TObject;
    FDataChanging: Boolean;

    Procedure SetDataSource(Value: TDataSource);
    Procedure SetDataSet(Value: TDataSet);
    Procedure SetBitsFieldName(Const Value: String);
    Procedure SetStartFieldName(Const Value: String);
    Procedure SetFinishFieldName(Const Value: String);
    Procedure DataChange(Sender: TObject);
    Procedure UpdateData(Sender: TObject);
    Procedure CMExit(Var Message: TCMExit); message CM_EXIT;

    Function GetDataSource: TDataSource;
    Function GetDataSet: TDataSet;
    Function GetBitsFieldName: String;
    Function GetStartFieldName: String;
    Function GetFinishFieldName: String;
    Function GetBitsField: TField;
    Function GetStartField: TField;
    Function GetFinishField: TField;
  protected
    Procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    Procedure KeyDown(Var Key: Word; Shift: TShiftState); override;
    Procedure DateChanged; override;
    Procedure SetReadOnly(Value: Boolean); override;

    Function GetReadOnly: Boolean; override;
    Function CanSelectionChange(Const Date: TDateTime): WordBool; override;
  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    Function ExecuteAction(Action: TBasicAction): Boolean; override;
    Function UpdateAction(Action: TBasicAction): Boolean; override;
    Procedure Reset;

    Property BitsField: TField read GetBitsField;
    Property StartField: TField read GetStartField;
    Property FinishField: TField read GetFinishField;
  protected
    Property DataSource: TDataSource read GetDataSource write SetDataSource;
    Property BitsFieldName: String read GetBitsFieldName write SetBitsFieldName;
    Property StartFieldName: String read GetStartFieldName
      write SetStartFieldName;
    Property FinishFieldName: String read GetFinishFieldName
      write SetFinishFieldName;
  public
    Property DataSet: TDataSet read GetDataSet write SetDataSet;
  End;

  TPSCDBCalendar = Class(TPSCCustomDBCalendar)
  published
    Property CalendarStyle;
    Property AlterEvenOdd;
    Property BevelEdges;
    Property BevelInner;
    Property BevelKind;
    Property BevelOuter;
    Property BevelWidth;
    Property CalendarsInHeight;
    Property CalendarsInWidth;
    Property CountryInHoliday;
    Property DayHeaderHorzAlign;
    Property DayHeaderStyle;
    Property DayHorzAlign;
    Property DayVertAlign;
    Property FirstDayFormat;
    Property HeaderFormat;
    Property HeaderStyle;
    Property NavButtonsSpeed;
    Property NavButtonsTimer;
    Property PastDaysAsGrayed;
    Property SelectionAndBorderDists;
    Property SelectStyle;
    Property ShowDayDelimiters;
    Property ShowFocusRect;
    Property ShowFooter;
    Property ShowHeader;
    Property SizeStyle;
    Property TodayStyle;
    Property Version;
    Property WantEsc;
    Property WeekDayCase;
    Property WeekLineStyle;

    Property Options;
    Property Align;
    Property Anchors;
    Property BiDiMode;
    Property DragCursor;
    Property DockSite;
    Property DragKind;
    Property ParentBiDiMode;
    Property Color;
    Property Constraints;
    Property DragMode;
    Property Enabled;
    Property Font;
    Property ParentColor default false;
    Property ParentFont;
    Property ParentShowHint;
    Property PopupMenu;
    Property ShowHint;
    Property TabOrder;
    Property TabStop;
    Property Visible;
    Property OnClick;
    Property OnDblClick;
    Property OnDragDrop;
    Property OnDockDrop;
    Property OnDockOver;
    Property OnEndDock;
    Property OnGetSiteInfo;
    Property OnStartDock;
    Property OnUnDock;
    Property OnDragOver;
    Property OnEndDrag;
    Property OnEnter;
    Property OnExit;
    Property OnMouseDown;
    Property OnMouseMove;
    Property OnMouseUp;
    Property OnStartDrag;
    Property ForceShowHint;
    Property AutoSize;
    Property MultiSelect Stored False;
    Property ClearOnKeyMoving;
    Property PrintOptions;
    Property ShowMonthPopup;
    Property ShowNavButtons;
    Property ShowToday;
    Property SideWeekSelect;
    Property ShowMonthDividers;
    Property ShowWeekNumbers;
    Property ShowHorzLines;
    Property ShowVertLines;
    Property WeekDayNames;
    Property FirstWeekOfYear;
    Property FirstDayOfWeek;
    Property WorkDays;
    Property MinDateLimit;
    Property MinDate;
    Property MaxDateLimit;
    Property MaxDate;
    Property StartDate Stored False;
    Property Flat;
    Property BorderStyle;
    Property BorderEdges;
    Property MaxSelDates;
    Property WeekCursor;
    Property SelectKind;
    Property ExtendedSelect Stored False;
    Property ReadOnly;
    Property Colors;
    Property OnDateChanged;
    Property OnGetPaintParams;
    Property OnCanSelectDate;
    Property OnMeasureItem;
    Property OnDrawItem;

    Property ArrowStyle;
    Property PopupType;
    Property Images;
    Property ShowArrowEdges;
    Property LeftArrow;
    Property LeftArrowHot;
    Property RightArrow;
    Property RightArrowHot;
    Property ShowHolidays;
    Property CustomHolidays;
    Property HeaderCase;
    Property ShortMonthName;
    Property OnGetHolidayDate;
    Property DefaultHolidays;

    Property DataSource;
    Property BitsFieldName;
    Property StartFieldName;
    Property FinishFieldName;

    Property OnDrawDayOutline;
    Property OnKeyDown;
    Property OnKeyPress;
    Property OnKeyUp;
    Property OnPrintProgress;
    Property OnStartDateChange;
  End;

  TPSCMonthsBox = Class(TPSCCustomControlAncestor)
  private
    FAllowedHitTests: TPSCMonthHitTests;
    FDown: Boolean;
    FYearStep: Integer;
    FStart: Integer;
    FTimer: TObject;
    FYearsPopup: TWinControl;
    FMonth: Integer;
    FYear: Integer;
    FSelected: TPSCMonthHitTest;
    FSelButton: TPSCMonthHitTest;
    FCaptionColor: TPSCColor;
    FCaptionTextColor: TPSCColor;
    FSelectedColor: TPSCColor;
    FSelectedTextColor: TPSCColor;
    FHeaderColor: TPSCColor;
    FArrowColor: TPSCColor;
    FHeaderFont: TPSCFont;
    FInheritHeaderFont: Boolean;
    FHeaderHeight: Integer;
    FArrowStyle: TPSCArrowStyle;
    FImages: TPSCImageList;
    FLeftArrow: Integer;
    FLeftArrowHot: Integer;
    FRightArrow: Integer;
    FRightArrowHot: Integer;
    FShowEdges: Boolean;
    FShowVertLines: Boolean;
    FShowHorzLines: Boolean;
    FYearsPopupCtl3D: Boolean;
    FOnChangeDate: TPSCNotifyEvent;
    Procedure KillTimer;
    Procedure SetMonth(Value: Integer);
    Procedure SetYear(Value: Integer);
    Procedure SetSelDate(Value: TDateTime);
    Function GetSelDate: TDateTime;
    Procedure SetSelected(Value: TPSCMonthHitTest);
    Procedure SetCaptionColor(Value: TPSCColor);
    Procedure SetCaptionTextColor(Value: TPSCColor);
    Procedure SetSelectedColor(Value: TPSCColor);
    Procedure SetSelectedTextColor(Value: TPSCColor);
    Procedure SetHeaderColor(Value: TPSCColor);
    Procedure SetArrowColor(Value: TPSCColor);
    Procedure SetShowEdges(Value: Boolean);
    Procedure SetArrowStyle(Value: TPSCArrowStyle);
    Procedure SetHeaderHeight(Value: Integer);
    Procedure SetImages(Value: TPSCImageList);
    Procedure SetLeftArrow(Value: Integer);
    Procedure SetLeftArrowHot(Value: Integer);
    Procedure SetRightArrow(Value: Integer);
    Procedure SetRightArrowHot(Value: Integer);
    Procedure SetShowVertLines(Value: Boolean);
    Procedure SetShowHorzLines(Value: Boolean);
    Procedure SetYearsPopupCtl3D(Value: Boolean);
    Procedure SetHeaderFont(Value: TPSCFont);
    Procedure SetFlat(Value: Boolean);
    Function GetFlat: Boolean;
    Function IsCaptionColorStored: Boolean;
    Function IsCaptionTextColorStored: Boolean;
    Function IsHeaderFontStored: Boolean;
    Procedure DoSelect;
    Procedure SelectYear;
    Procedure OnFontChanged(Sender: TObject);
    Procedure YearsPopupClosed(Sender: TObject; Canceled: Boolean);
    Procedure CMCtl3DChanged(Var Message: TMessage); message CM_CTL3DCHANGED;
    Procedure WMEraseBkgnd(Var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    Procedure TimerEvent(Timer: TObject; EventID: Integer);
  protected
    Function GetHitTest(P: TPoint; Var Month: Integer): TPSCMonthHitTest;virtual;

    Procedure WMGetDlgCode(Var Message: TMessage); message WM_GETDLGCODE;
    Procedure CMMouseLeave(Var Message: TMessage); message CM_MOUSELEAVE;
    Procedure CMColorChanged(Var Message: TMessage); message CM_COLORCHANGED;
    Procedure CMFontChanged(Var Message: TMessage); message CM_FONTCHANGED;
    Procedure CMTextChanged(Var Message: TMessage); message CM_TEXTCHANGED;
    Procedure CreateParams(Var Params: TCreateParams); override;
    Procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    Procedure Paint; override;
    Procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    Procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X,Y: Integer); override;
    Procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X,Y: Integer); override;
    Procedure KeyDown(Var Key: Word; Shift: TShiftState); override;
    Procedure KeyUp(Var Key: Word; Shift: TShiftState); override;
    Procedure InvalidateArea(Area: TPSCMonthHitTest; Month: Integer);
    Procedure GetRect(Area: TPSCMonthHitTest; Month: Integer; Var Rect: TRect);
    Procedure DrawHeader;
    Procedure DrawYear;
    Procedure DrawArrow(Arrow: TPSCMonthHitTest);
    Procedure DrawCaption;
    Procedure DrawMonths;
    Procedure DrawMonth(Month: Integer);
  public
    Property AllowedHitTests: TPSCMonthHitTests read FAllowedHitTests write
      FAllowedHitTests;
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    Property SelDate: TDateTime read GetSelDate write SetSelDate stored false;
    Property Selected: TPSCMonthHitTest read FSelected write SetSelected;
  published
    Property Align;
    Property Anchors;
    Property BiDiMode;
    Property DockSite;
    Property DragCursor;
    Property DragKind;
    Property ParentBiDiMode;
    Property Color default clPSCWindow;
    Property Constraints;
    Property DragMode;
    Property Enabled;
    Property Font;
    Property ParentColor default false;
    Property ParentFont;
    Property ParentShowHint;
    Property PopupMenu;
    Property ShowHint;
    Property TabOrder;
    Property TabStop default True;
    Property Visible;
    Property OnClick;
    Property OnDblClick;
    Property OnDragDrop;
    Property OnDragOver;
    Property OnEndDrag;
    Property OnEnter;
    Property OnExit;
    Property OnMouseDown;
    Property OnMouseMove;
    Property OnMouseUp;
    Property OnStartDrag;
    Property OnDockDrop;
    Property OnDockOver;
    Property OnEndDock;
    Property OnGetSiteInfo;
    Property OnStartDock;
    Property OnUnDock;

    Property Month: Integer read FMonth write SetMonth;
    Property Year: Integer read FYear write SetYear;
    Property CaptionColor: TPSCColor read FCaptionColor write SetCaptionColor
      stored IsCaptionColorStored;
    Property CaptionTextColor: TPSCColor read FCaptionTextColor
      write SetCaptionTextColor stored IsCaptionTextColorStored;
    Property SelectedColor: TPSCColor read FSelectedColor write SetSelectedColor
      default cPSCDefMonthsSelectedColor;
    Property SelectedTextColor: TPSCColor read FSelectedTextColor
      write SetSelectedTextColor default clPSCWindowText;
    Property HeaderColor: TPSCColor read FHeaderColor write SetHeaderColor
      default clPSCBtnFace;
    Property ShowEdges: Boolean read FShowEdges write SetShowEdges default
      False;
    Property ArrowColor: TPSCColor read FArrowColor write SetArrowColor
      default clArrowColor;
    Property ArrowStyle: TPSCArrowStyle read FArrowStyle write SetArrowStyle
      default astOutlook;
    Property HeaderFont: TPSCFont read FHeaderFont write SetHeaderFont
      stored IsHeaderFontStored;
    Property InheritHeaderFont: Boolean read FInheritHeaderFont
      write FInheritHeaderFont default true;
    Property HeaderHeight: Integer read FHeaderHeight write SetHeaderHeight
      default 0;
    Property Images: TPSCImageList read FImages write SetImages;
    Property LeftArrow: Integer read FLeftArrow write SetLeftArrow
      default -1;
    Property LeftArrowHot: Integer read FLeftArrowHot write SetLeftArrowHot
      default -1;
    Property RightArrow: Integer read FRightArrow write SetRightArrow
      default -1;
    Property RightArrowHot: Integer read FRightArrowHot write SetRightArrowHot
      default -1;
    Property ShowVertLines: Boolean read FShowVertLines write SetShowVertLines
      default false;
    Property ShowHorzLines: Boolean read FShowHorzLines write SetShowHorzLines
      default false;
    Property YearsPopupCtl3D: Boolean read FYearsPopupCtl3D
      write SetYearsPopupCtl3D default false;
    Property Flat: Boolean read GetFlat write SetFlat default true;
    Property OnChangeDate: TPSCNotifyEvent read FOnChangeDate write FOnChangeDate;
  End;

  TPSCMonthsPopup = Class(TPSCPopupForm)
  private
    FMonths: TPSCMonthsBox;
    Procedure SelectMonth(Sender: TObject);
    Procedure WMEraseBkgnd(Var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  protected
    Procedure CreateParams(Var Params: TCreateParams); override;
  public
    Procedure ResizeFormControls; override;
    constructor CreateNew(AOwner: TComponent; Dummy: Integer=0); override;
    Property Months: TPSCMonthsBox read FMonths;
  End;

  TListBoxClass = class of TListbox;

  TPSCPopupTListBox = Class(TPSCPopupForm)
  private
    FListBox: TListBox;
    Procedure ListBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X,Y: Integer);
  protected
    Function GetListBoxClass: TListBoxClass; virtual;
  public
    constructor CreateNew(AOwner: TComponent; Dummy: Integer=0); override;
    Property ListBox: TListBox read FListBox;
  End;

  TPSCYearsPopup = Class(TPSCPopupTListBox)
  private
    Procedure SelectYear(Sender: TObject);
  protected
    Function GetListBoxClass: TListBoxClass; override;
    Procedure CreateParams(Var Params: TCreateParams); override;
  public
    Procedure ResizeFormControls; override;
    constructor CreateNew(AOwner: TComponent; Dummy: Integer=0); override;
  End;

Procedure PSCDrawMSMoneyArrow(Canvas: TPSCCanvas; LeftArrow: Boolean;
  Const Rect: TRect);
Procedure PSCDrawOutlookArrow(Canvas: TPSCCanvas; LeftArrow: Boolean;
  Const Rect: TRect);

function PSCCreateDateArray:IPSCDateArray;
Function PSCIsDateInDays(Const Date: TDateTime; Days: TPSCWeekDays): boolean;
Function PSCGetWeekNumber(Const Date: TDateTime; FirstDay:
  TPSCFirstDayOfWeek;
  FirstWeek: TPSCFirstWeekOfYear): Integer;
Function PSCGetWorkDaysCount(WorkDays: TPSCWeekDays): Integer;

procedure PSCDrawDayHelix(ACanvas : TPSCCanvas; const ARect : TRect);
procedure PSCDrawBlackOnWhiteArrow(Canvas: TPSCCanvas; LeftArrow: Boolean;
  Const Rect: TRect);

const
  CPSCDefaultCalendarClass:TPSCCalendarClass=TPSCCalendar;

  CPSCDayNumbers : array[1..31] of string[2] =
   ('1', '2', '3', '4', '5', '6', '7', '8', '9', '10',
    '11', '12', '13', '14', '15', '16', '17', '18', '19', '20',
    '21', '22', '23', '24', '25', '26', '27', '28', '29', '30', '31');

  CPSCCalendarState_Outlook : TPSCCalendarState  =
  (
    Color : clPSCWindow;
    CalendarsInHeight : 0;
    CalendarsInWidth : 0;
    WantEsc : CPSCDefWantEsc;
    SelectionAndBorderDists : 0;
    ShortMonthName : false;
    ShowNavButtons : true;
    ShowMonthDividers : false;
    DayHeaderStyle : dhsOutlook;
    AlterEvenOdd : CPSCDefAlterEvenOdd;
    FirstDayFormat : '';
    ShowFocusRect : false;
    HeaderCase : tcDefault;
    PastDaysAsGrayed : CPSCDefPastDaysAsGrayed;
    NavButtonsSpeed : 350;
    TodayStyle : ctsSquare;
    ShowArrowEdges : false;
    HeaderFormat : '';
    DaysFont : (Color : clPSCWindowText);
    ArrowColor : clArrowColor;
    Border : clBorderColor;
    MonthHeader : clMonthHeaderColor;
    WeekHeader : clPSCWindow;
    Days : clPSCWindow;
    DayLines : clDayOutlineColor;
    Selected : clSelectedColor;
    SelectedText : clSelectedTextColor;
    WeekDaysFont : (Color : clPSCWindowText);
    WeekNumbersFont : (Color : clPSCWindowText);
    HolidaysFont : (Color : clHolidayTextColor; Style : [FontStyle_Bold]);
    HeaderFont : (Color : clPSCWindowText; Size : 8);
    Grayed : clGrayedColor;
    WeekEndText : clWeekEndTextColor;
    NowRect : clNowRectColor;
    WeekSide : clPSCWindow;
    WeekDayNames : wdlOne;
    WeekLineColor : clPSCGrayText;
    HeaderBorderColor : clPSCBtnFace;
    GrayedBkColor : clPSCWindow;
    Flat : true;
    WeekLineStyle : wlsFullLine;
    SelectStyle : sslFillRect;
    PopupType : ptMsMoney;//ptStandard;
    ShowHeader : CPSCDefShowHeader;
    HeaderStyle : chsOutlook;
    ArrowStyle : astOutlook;
    WeekDayCase : tcDefault;
    SizeStyle : cssOutlook;
    ShowFooter : false;
    ShowToday : true;
    DayVertAlign : CPSCDefCalVertAlign;
    DayHorzAlign : CPSCDefCalHorzAlign;
    DayHeaderHorzAlign : CPSCDefCalHorzAlign;
    BorderStyle : bsSingle;
    ShowDayDelimiters : false;
    SelectKind : skProgressive;
    ShowVertLines : false;
    ShowHorzLines : false;
    ShowMonthPopup : true;
    WeekCursor : crDefault;
    ExtendedSelect : true;
    DayLinesStyle : psDayOutlineStyle;
    Options : [caopGreyOutsideDays, caopScrollWhenGreyClicked];
  );

  CPSCCalendarState_MSMoney : TPSCCalendarState  =
  (
    Color : clMSMoneyBkColor;
    CalendarsInHeight : 0;
    CalendarsInWidth : 0;
    WantEsc : CPSCDefWantEsc;
    SelectionAndBorderDists : 0;
    ShortMonthName : true;
    ShowNavButtons : true;
    ShowMonthDividers : false;
    DayHeaderStyle : dhsOutlook;
    AlterEvenOdd : CPSCDefAlterEvenOdd;
    FirstDayFormat : '';
    ShowFocusRect : false;
    HeaderCase : tcUpper;
    PastDaysAsGrayed : CPSCDefPastDaysAsGrayed;
    NavButtonsSpeed : 350;
    TodayStyle : ctsSquare;
    ShowArrowEdges : false;
    HeaderFormat : '';
    DaysFont : (Color : clPSCBtnHighlight);
    ArrowColor : clPSCWhite;
    Border : clMSMoneyBkColor;
    MonthHeader : clMSMoneyBkColor;
    WeekHeader : clMSMoneySelection;
    Days : clMSMoneyBkColor;
    DayLines : clDayOutlineColor;
    Selected : clMSMoneySelection;
    SelectedText : clPSCHighLightText;
    WeekDaysFont : (Color : clPSCBtnHighlight);
    WeekNumbersFont : (Color : clPSCBtnHighlight);
    HolidaysFont : (Color : clHolidayTextColor);
    HeaderFont : (Color : clPSCBtnHighlight; Size : 8);
    Grayed : clPSCBtnShadow;
    WeekEndText : clPSCBtnHighlight;
    NowRect : clPSCRed;
    WeekSide : clMSMoneyBkColor;
    WeekDayNames : wdlTwo;
    WeekLineColor : clPSCWindow;
    HeaderBorderColor : clPSCWindowText;
    GrayedBkColor : clPSCWindowText;
    Flat : true;
    WeekLineStyle : wlsIndentedLine;
    SelectStyle : sslFillRect;
    PopupType : ptMSMoney;
    ShowHeader : CPSCDefShowHeader;
    HeaderStyle : chsOutlook;
    ArrowStyle : astMSMoney;
    WeekDayCase : tcDefault;
    SizeStyle : cssOutlook;
    ShowFooter : false;
    ShowToday : true;
    DayVertAlign : CPSCDefCalVertAlign;
    DayHorzAlign : CPSCDefCalHorzAlign;
    DayHeaderHorzAlign : CPSCDefCalHorzAlign;
    BorderStyle : bsNone;
    ShowDayDelimiters : false;
    SelectKind : skProgressive;
    ShowVertLines : false;
    ShowHorzLines : true;
    ShowMonthPopup : true;
    WeekCursor : crDefault;
    ExtendedSelect : true;
    DayLinesStyle : psDayOutlineStyle;
    Options : [caopGreyOutsideDays, caopScrollWhenGreyClicked];
  );

  CPSCCalendarState_MonthCalendar : TPSCCalendarState  =
  (
    CalendarsInHeight : 0;
    CalendarsInWidth : 0;
    WantEsc : CPSCDefWantEsc;
    SelectionAndBorderDists : 0;
    ShortMonthName : false;
    ShowNavButtons : true;
    ShowMonthDividers : false;
    DayHeaderStyle : dhsOutlook;
    AlterEvenOdd : CPSCDefAlterEvenOdd;
    FirstDayFormat : '';
    ShowFocusRect : true;
    HeaderCase : tcDefault;
    PastDaysAsGrayed : false;
    NavButtonsSpeed : 350;
    TodayStyle : ctsHelix;
    ShowArrowEdges : false;
    HeaderFormat : '';
    DaysFont : (Color : clPSCWindowText);
    ArrowColor : clPSCWindow;
    Border : clPSCBtnFace;
    MonthHeader : clPSCActiveCaption;
    WeekHeader : clPSCWindow;
    Days : clPSCWindow;
    DayLines : clPSCBtnFace;
    Selected : clPSCActiveCaption;
    SelectedText : clPSCWindow;
    WeekDaysFont : (Color : clPSCActiveCaption);
    WeekNumbersFont : (Color : clPSCActiveCaption{clPSCWindowText});
    HolidaysFont : (Color : clPSCMaroon; Style : [FontStyle_Bold]);
    HeaderFont : (Color : clPSCWindow; Size : 8; Style : [FontStyle_Bold]);
    Grayed : clPSCInactiveCaptionText;
    WeekEndText : clPSCWindowText;
    NowRect : clPSCRed;
    WeekSide : clPSCWindow;
    WeekDayNames : wdlShort;
    WeekLineColor : clPSCWindowText;
    HeaderBorderColor : clPSCActiveCaption;
    GrayedBkColor : clPSCWindow;
    Flat : true;
    WeekLineStyle : wlsFullLine;
    SelectStyle : sslEllipse;
    PopupType : ptMonthCalendar;
    ShowHeader : CPSCDefShowHeader;
    HeaderStyle : chsMonthCalendar;
    ArrowStyle : astMonthCalendar;
    WeekDayCase : tcDefault;
    SizeStyle : cssMonthCalendar;
    ShowFooter : true;
    ShowToday : true;
    DayVertAlign : CPSCDefCalVertAlign;
    DayHorzAlign : CPSCDefCalHorzAlign;
    DayHeaderHorzAlign : CPSCDefCalHorzAlign;
    BorderStyle : bsNone;
    ShowDayDelimiters : false;
    SelectKind : skFree;
    ShowVertLines : false;
    ShowHorzLines : false;
    ShowMonthPopup : true;
    WeekCursor : crDefault;
    ExtendedSelect : true;
    DayLinesStyle : psDayOutlineStyle;
    Options : [caopGreyOutsideDays, caopScrollWhenGreyClicked];
  );

  CPSCCalendarState_WhiteOnBlack : TPSCCalendarState  =
  (
    Color  : clPSCWindowText;
    CalendarsInHeight : 0;
    CalendarsInWidth : 0;
    WantEsc : CPSCDefWantEsc;
    SelectionAndBorderDists : 0;
    ShortMonthName : false;
    ShowNavButtons : true;
    ShowMonthDividers : false;
    DayHeaderStyle : dhsOutlook;
    AlterEvenOdd : CPSCDefAlterEvenOdd;
    FirstDayFormat : '';
    ShowFocusRect : true;
    HeaderCase : tcDefault;
    PastDaysAsGrayed : CPSCDefPastDaysAsGrayed;
    NavButtonsSpeed : 350;
    TodayStyle : ctsSquare;
    ShowArrowEdges : false;
    HeaderFormat : '';
    DaysFont : (Color : clPSCWindow);
    ArrowColor : clPSCWindow;
    Border : clPSCWindowText;
    MonthHeader : clPSCWindowText;
    WeekHeader : clPSCWindowText;
    Days : clPSCWindowText;
    DayLines : clPSCWindow;
    Selected : clPSCMonthViewSelected;
    SelectedText : clPSCWindow;
    WeekDaysFont : (Color : clPSCWindow);
    WeekNumbersFont : (Color : clPSCWindow);
    HolidaysFont : (Color : clPSCWindow; Style : [FontStyle_Bold]);
    HeaderFont : (Color : clPSCWindow; Size : 8);
    Grayed : clPSC3DDkShadow;
    WeekEndText : clPSCWindow;
    NowRect : clPSCWindow;
    WeekSide : clPSCWindowText;
    WeekDayNames : wdlOne;
    WeekLineColor : clPSCWindow;
    HeaderBorderColor : clPSCWindow;
    GrayedBkColor : clPSCWindowText;
    Flat : true;
    WeekLineStyle : wlsFullLine;
    SelectStyle : sslFillRect;
    PopupType : ptStandard;
    ShowHeader : CPSCDefShowHeader;
    HeaderStyle : chsWhiteOnBlack;
    ArrowStyle : astMonthCalendar;
    WeekDayCase : tcLower;
    SizeStyle : cssMonthCalendar;
    ShowFooter : false;
    ShowToday : false;
    DayVertAlign : CPSCDefCalVertAlign;
    DayHorzAlign : haRight;
    DayHeaderHorzAlign : haRight;
    BorderStyle : bsNone;
    ShowDayDelimiters : false;
    SelectKind : skFree;
    ShowVertLines : false;
    ShowHorzLines : false;
    ShowMonthPopup : true;
    WeekCursor : crDefault;
    ExtendedSelect : true;
    DayLinesStyle : psDayOutlineStyle;
    Options : [caopGreyOutsideDays, caopScrollWhenGreyClicked];
  );

  CPSCCalendarState_BlackOnWhite : TPSCCalendarState  =
  (
    Color : clPSCWindow;
    CalendarsInHeight : 0;
    CalendarsInWidth : 0;
    WantEsc : CPSCDefWantEsc;
    SelectionAndBorderDists : 0;
    ShortMonthName : false;
    ShowNavButtons : true;
    ShowMonthDividers : false;
    DayHeaderStyle : dhsOutlook;
    AlterEvenOdd : CPSCDefAlterEvenOdd;
    FirstDayFormat : '';
    ShowFocusRect : true;
    HeaderCase : tcDefault;
    PastDaysAsGrayed : CPSCDefPastDaysAsGrayed;
    NavButtonsSpeed : 350;
    TodayStyle : ctsSquare;
    ShowArrowEdges : false;
    HeaderFormat : '';
    DaysFont : (Color : clPSCWindowText);
    ArrowColor : clPSCWindowText;
    Border : clPSCWindow;
    MonthHeader : clPSCWindow;
    WeekHeader : clPSCWindow;
    Days : clPSCWindow;
    DayLines : clPSCWindowText;
    Selected : clPSCMonthViewSelected;
    SelectedText : clPSCWindowText;
    WeekDaysFont : (Color : clPSCWindowText);
    WeekNumbersFont : (Color : clPSCWindowText);
    HolidaysFont : (Color : clPSCWindowText; Style : [FontStyle_Bold]);
    HeaderFont : (Color : clPSCWindowText; Size : 8);
    Grayed : clPSCInactiveBorder;
    WeekEndText : clPSCWindowText;
    NowRect : clPSCWindowText;
    WeekSide : clPSCWindow;
    WeekDayNames : wdlOne;
    WeekLineColor : clPSCWindowText;
    HeaderBorderColor : clPSCWindowText;                
    GrayedBkColor : clPSCWindow;                        
    Flat : true;                                        
    WeekLineStyle : wlsFullLine;                        
    SelectStyle : sslFillRect;                          
    PopupType : ptStandard;
    ShowHeader : CPSCDefShowHeader;
    HeaderStyle : chsBlackOnWhite;
    ArrowStyle : astMonthCalendar;
    WeekDayCase : tcLower;
    SizeStyle : cssMonthCalendar;                       
    ShowFooter : false;
    ShowToday : false;                                  
    DayVertAlign : CPSCDefCalVertAlign;                 
    DayHorzAlign : haRight;
    DayHeaderHorzAlign : haRight;
    BorderStyle : bsNone;
    ShowDayDelimiters : false;
    SelectKind : skFree;
    ShowVertLines : false;
    ShowHorzLines : false;
    ShowMonthPopup : true;
    WeekCursor : crDefault;
    ExtendedSelect : true;
    DayLinesStyle : psDayOutlineStyle;
    Options : [caopGreyOutsideDays, caopScrollWhenGreyClicked];
  );

  CPSCCalendarState_MSMoney2002 : TPSCCalendarState  =
  (
    Color : clPSCMonthViewDays;
    CalendarsInHeight : 0;
    CalendarsInWidth : 0;
    WantEsc : CPSCDefWantEsc;
    SelectionAndBorderDists : 0;
    ShortMonthName : false;
    ShowNavButtons : true;
    ShowMonthDividers : false;
    DayHeaderStyle : dhsOutlook;
    AlterEvenOdd : CPSCDefAlterEvenOdd;
    FirstDayFormat : '';
    ShowFocusRect : true;
    HeaderCase : tcDefault;
    PastDaysAsGrayed : CPSCDefPastDaysAsGrayed;
    NavButtonsSpeed : 350;
    TodayStyle : ctsSquare;
    ShowArrowEdges : false;
    HeaderFormat : '';
    DaysFont : (Color : clPSCWindowText);
    ArrowColor : clPSCWindowText;
    Border : clPSCMonthViewBorder;
    MonthHeader : clPSCMonthViewHeader;
    WeekHeader : clPSCMonthViewDays;
    Days : clPSCMonthViewDays;
    DayLines : clPSCWindowText;
    Selected : clPSCMonthViewSelected;
    SelectedText : clPSCWindowText;
    WeekDaysFont : (Color : clPSCWindowText);
    WeekNumbersFont : (Color : clPSCWindowText);
    HolidaysFont : (Color : clPSCWindowText; Style : [FontStyle_Bold]);
    HeaderFont : (Color : clPSCWindowText; Size : 8);
    Grayed : clPSCSilver;
    WeekEndText : clPSCWindowText;
    NowRect : clPSCRed;
    WeekSide : clPSCMonthViewDays;
    WeekDayNames : wdlOne;
    WeekLineColor : clPSCMonthViewWeekLine;
    HeaderBorderColor : clPSCMonthViewWeekLine;
    GrayedBkColor : clPSCMonthViewDays;
    Flat : true;
    WeekLineStyle : wlsFullLine;
    SelectStyle : sslFillRect;
    PopupType : ptStandard;
    ShowHeader : CPSCDefShowHeader;
    HeaderStyle : chsMSMoney2002;
    ArrowStyle : astMonthCalendar;
    WeekDayCase : tcLower;
    SizeStyle : cssMonthCalendar;
    ShowFooter : false;
    ShowToday : true;
    DayVertAlign : CPSCDefCalVertAlign;
    DayHorzAlign : haRight;
    DayHeaderHorzAlign : haRight;
    BorderStyle : bsNone;
    ShowDayDelimiters : false;
    SelectKind : skFree;
    ShowVertLines : false;
    ShowHorzLines : false;
    ShowMonthPopup : true;
    WeekCursor : crDefault;
    ExtendedSelect : true;
    DayLinesStyle : psDayOutlineStyle;
    Options : [caopGreyOutsideDays, caopScrollWhenGreyClicked];
  );

  CPSCCalendarState_ForPrinting : TPSCCalendarState  =
  (
    Color : clPSCWindow;
    CalendarsInHeight : 0;
    CalendarsInWidth : 0;
    WantEsc : CPSCDefWantEsc;
    SelectionAndBorderDists : 0;
    ShortMonthName : false;
    ShowNavButtons : false;
    ShowMonthDividers : false;
    DayHeaderStyle : dhsOutlook;
    AlterEvenOdd : CPSCDefAlterEvenOdd;
    FirstDayFormat : '';
    ShowFocusRect : true;
    HeaderCase : tcDefault;
    PastDaysAsGrayed : CPSCDefPastDaysAsGrayed;
    NavButtonsSpeed : 350;
    TodayStyle : ctsSquare;
    ShowArrowEdges : false;
    HeaderFormat : '';
    DaysFont : (Color : clPSCWindowText);
    ArrowColor : clPSCWindow;
    Border : clPSCWindow;
    MonthHeader : clPSCWindow;
    WeekHeader : clPSCWindow;
    Days : clPSCWindow;
    DayLines : clPSCWindow;
    Selected : clPSC3DLight;
    SelectedText : clPSCWindowText;
    WeekDaysFont : (Color : clPSCWindowText);
    WeekNumbersFont : (Color : clPSCWindowText);
    HolidaysFont : (Color : clHolidayTextColor; Style : [FontStyle_Bold]);
    HeaderFont : (Color : clPSCNavy; Size : 10);
    Grayed : clPSCWindow;
    WeekEndText : clPSCMaroon;
    NowRect : clPSCWindow;
    WeekSide : clPSCWindow;
    WeekDayNames : wdlShort;                         
    WeekLineColor : clPSCWindow;
    HeaderBorderColor : clPSCWindow;
    GrayedBkColor : clPSCWindow;                     
    Flat : true;                                     
    WeekLineStyle : wlsFullLine;                     
    SelectStyle : sslFillRect;                       
    PopupType : ptStandard;                          
    ShowHeader : CPSCDefShowHeader;                  
    HeaderStyle : chsForPrinting;
    ArrowStyle : astMonthCalendar;
    WeekDayCase : tcDefault;                         
    SizeStyle : cssMonthCalendar;
    ShowFooter : false;
    ShowToday : false;
    DayVertAlign : CPSCDefCalVertAlign;
    DayHorzAlign : CPSCDefCalHorzAlign;
    DayHeaderHorzAlign : CPSCDefCalHorzAlign;
    BorderStyle : bsNone;
    ShowDayDelimiters : false;
    SelectKind : skFree;
    ShowVertLines : false;
    ShowHorzLines : false;
    ShowMonthPopup : true;
    WeekCursor : crDefault;
    ExtendedSelect : true;
    DayLinesStyle : psDayOutlineStyle;
    Options : [caopGreyOutsideDays, caopScrollWhenGreyClicked];
  );

  CPSCCalendarState_OutlookMonthView : TPSCCalendarState  =
  (
    Color : clPSCWindow;
    CalendarsInHeight : 1;
    CalendarsInWidth : 1;
    WantEsc : CPSCDefWantEsc;
    SelectionAndBorderDists : 1;
    ShortMonthName : false;
    ShowNavButtons : false;
    ShowMonthDividers : false;
    DayHeaderStyle : dhsOutlookMonthView;
    AlterEvenOdd : true;
    FirstDayFormat : 'MMMM dd';
    ShowFocusRect : true;
    HeaderCase : tcDefault;
    PastDaysAsGrayed : CPSCDefPastDaysAsGrayed;
    NavButtonsSpeed : 350;
    TodayStyle : ctsSquare;
    ShowArrowEdges : false;
    HeaderFormat : '';
    DaysFont : (Color : clPSCWindowText);
    ArrowColor : clPSCWindowText;
    Border : clPSCBtnFace;
    MonthHeader : clPSCBtnFace;
    WeekHeader : clPSCBtnFace;
    Days : clPSCWindow;
    DayLines : clPSCWindowText;
    Selected : clPSCHighLight;
    SelectedText : clPSCWindow;
    WeekDaysFont : (Color : clPSCWindowText);
    WeekNumbersFont : (Color : clPSCWindowText);
    HolidaysFont : (Color : clPSCWindowText);
    HeaderFont : (Color : clPSCWindowText; Size : 10);
    Grayed : clPSCWindowText;
    WeekEndText : clPSCWindowText;
    NowRect : clPSCHighLight;
    WeekSide : clPSCWindow;
    WeekDayNames : wdlWhole;
    WeekLineColor : clPSCBtnShadow;
    HeaderBorderColor : clPSCGrayText;
    GrayedBkColor : clPSCScrollBar;
    Flat : true;
    WeekLineStyle : wlsFullLine;
    SelectStyle : sslFillRect;                        
    PopupType : ptStandard;
    ShowHeader : false;                               
    HeaderStyle : chsOutlookMonthView;
    ArrowStyle : astMonthCalendar;
    WeekDayCase : tcDefault;
    SizeStyle : cssMonthCalendar;
    ShowFooter : false;
    ShowToday : false;
    DayVertAlign : vaTop;
    DayHorzAlign : haRight;
    DayHeaderHorzAlign : CPSCDefCalHorzAlign;
    BorderStyle : bsSingle;
    ShowDayDelimiters : true;
    SelectKind : skFree;
    ShowVertLines : true;
    ShowHorzLines : true;
    ShowMonthPopup : true;
    WeekCursor : crDefault;
    ExtendedSelect : true;
    DayLinesStyle : psDayOutlineStyle;
    Options : [caopGreyOutsideDays];
  );

function PSCGetMonthsBetween(const ANow, AThen: TDateTime) : integer;

{------------------------------------------------------------------}

Implementation

uses
  psc_holidays,
  psc_edit_date;

{------------------------------------------------------------------}

type

  THackControl=class(TControl)
  end;

  TPSCParentedFont = class(TPSCFont)
  private
    FOwner: TControl;
    FDefFontAttr: TPSCFontAttr;
    function IsCharsetStored : boolean;
    function IsColorStored : boolean;
    function IsHeightStored : boolean;
    function IsNameStored : boolean;
    function IsPitchStored : boolean;
    function IsSizeStored : boolean;
    function IsStyleStored : boolean;
  protected
    function GetOwner: TPersistent; override;
    function IsFontStored:Boolean;
  public
    constructor Create(AOwner : TControl);
  published
    property Charset stored IsCharsetStored;
    property Color stored IsColorStored;
    property Height stored IsHeightStored;
    property Name stored IsNameStored;
    property Pitch stored IsPitchStored;
    property Size stored IsSizeStored;
    property Style stored IsStyleStored;
  end;

  TPSCCalendarDataLink = Class(TDataLink)
  private
    FStartField: TField;
    FFinishField: TField;
    FBitsField: TField;
    FBitsFieldName: String;
    FStartFieldName: String;
    FFinishFieldName: String;
    FControl: TComponent;
    FEditing: Boolean;
    FModified: Boolean;
    FOnDataChange: TPSCNotifyEvent;
    FOnEditingChange: TPSCNotifyEvent;
    FOnUpdateData: TPSCNotifyEvent;
    FOnActiveChange: TPSCNotifyEvent;

    Function GetCanModify: Boolean;
    Procedure SetEditing(Value: Boolean);
    Procedure SetBitsField(Value: TField);
    Procedure SetStartField(Value: TField);
    Procedure SetFinishField(Value: TField);
    Procedure SetBitsFieldName(Const Value: String);
    Procedure SetStartFieldName(Const Value: String);
    Procedure SetFinishFieldName(Const Value: String);
    Procedure UpdateBitsField;
    Procedure UpdateStartField;
    Procedure UpdateFinishField;
    Procedure UpdateFields;
  protected
    Procedure ActiveChanged; override;
    Procedure EditingChanged; override;
    Procedure FocusControl(Field: TFieldRef); override;
    Procedure LayoutChanged; override;
    Procedure RecordChanged(Field: TField); override;
    Procedure UpdateData; override;
  public
    Constructor Create;
    Function Edit: Boolean;
    Procedure Modified;
    Procedure Reset;
    Property CanModify: Boolean read GetCanModify;
    Property Control: TComponent read FControl write FControl;
    Property Editing: Boolean read FEditing;
    Property BitsField: TField read FBitsField;
    Property StartField: TField read FStartField;
    Property FinishField: TField read FFinishField;
    Property BitsFieldName: String read FBitsFieldName write SetBitsFieldName;
    Property StartFieldName: String read FStartFieldName
      write SetStartFieldName;
    Property FinishFieldName: String read FFinishFieldName
      write SetFinishFieldName;
    Property OnDataChange: TPSCNotifyEvent read FOnDataChange write FOnDataChange;
    Property OnEditingChange: TPSCNotifyEvent read FOnEditingChange
      write FOnEditingChange;
    Property OnUpdateData: TPSCNotifyEvent read FOnUpdateData write FOnUpdateData;
    Property OnActiveChange: TPSCNotifyEvent read FOnActiveChange
      write FOnActiveChange;
  End;

{------------------------------------------------------------------------------}

Constructor TPSCCalendarDataLink.Create;
Begin
  Inherited Create;
  VisualControl := True;
End;

{------------------------------------------------------------------------------}

Procedure TPSCCalendarDataLink.SetEditing(Value: Boolean);
Begin
  If FEditing <> Value Then
    Begin
      FEditing := Value;
      FModified := False;
      If Assigned(FOnEditingChange) Then
        FOnEditingChange(Self)
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCalendarDataLink.SetBitsFieldName(Const Value: String);
Begin
  If FBitsFieldName <> Value Then
    Begin
      FBitsFieldName := Value;
      UpdateBitsField
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCalendarDataLink.SetStartFieldName(Const Value: String);
Begin
  If FStartFieldName <> Value Then
    Begin
      FStartFieldName := Value;
      UpdateStartField
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCalendarDataLink.SetFinishFieldName(Const Value: String);
Begin
  If FFinishFieldName <> Value Then
    Begin
      FFinishFieldName := Value;
      UpdateFinishField
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCalendarDataLink.SetBitsField(Value: TField);
Begin
  If FBitsField <> Value Then
    Begin
      FBitsField := Value;
      EditingChanged;
      RecordChanged(Nil)
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCalendarDataLink.SetStartField(Value: TField);
Begin
  If FStartField <> Value Then
    Begin
      FStartField := Value;
      EditingChanged;
      RecordChanged(Nil)
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCalendarDataLink.SetFinishField(Value: TField);
Begin
  If FFinishField <> Value Then
    Begin
      FFinishField := Value;
      EditingChanged;
      RecordChanged(Nil)
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCalendarDataLink.UpdateBitsField;
Begin
  SetBitsField(Nil);
  If Active And (FBitsFieldName <> '') Then
    SetBitsField(DataSet.FieldByName(FBitsFieldName))
End;

{------------------------------------------------------------------------------}

Procedure TPSCCalendarDataLink.UpdateStartField;
Begin
  SetStartField(Nil);
  If Active And (FStartFieldName <> '') Then
    SetStartField(DataSet.FieldByName(FStartFieldName))
End;

{------------------------------------------------------------------------------}

Procedure TPSCCalendarDataLink.UpdateFinishField;
Begin
  SetFinishField(Nil);
  If Active And (FFinishFieldName <> '') Then
    SetFinishField(DataSet.FieldByName(FFinishFieldName))
End;

{------------------------------------------------------------------------------}

Procedure TPSCCalendarDataLink.UpdateFields;
Begin
  FStartField := Nil;
  FFinishField := Nil;
  FBitsField := Nil;
  If Active Then
    Begin
      If FStartFieldName <> '' Then
        FStartField := DataSet.FieldByName(FStartFieldName);
      If FFinishFieldName <> '' Then
        FFinishField := DataSet.FieldByName(FFinishFieldName);
      If FBitsFieldName <> '' Then
        FBitsField := DataSet.FieldByName(FBitsFieldName);
      EditingChanged;
      RecordChanged(Nil)
    End
End;

{------------------------------------------------------------------------------}

Function TPSCCalendarDataLink.Edit: Boolean;
Begin
  If CanModify Then
    Inherited Edit;
  Result := FEditing
End;

{------------------------------------------------------------------------------}

Function TPSCCalendarDataLink.GetCanModify: Boolean;
Begin
  Result := Not ReadOnly And
    ((FBitsField = Nil) Or FBitsField.CanModify) And
    (FStartField <> Nil) And FStartField.CanModify And
    ((FFinishField = Nil) Or FFinishField.CanModify)
End;

{------------------------------------------------------------------------------}

Procedure TPSCCalendarDataLink.Modified;
Begin
  FModified := true
End;

{------------------------------------------------------------------------------}

Procedure TPSCCalendarDataLink.Reset;
Begin
  RecordChanged(Nil)
End;

{------------------------------------------------------------------------------}

Procedure TPSCCalendarDataLink.ActiveChanged;
Begin
  UpdateFields;
  If Assigned(FOnActiveChange) Then
    FOnActiveChange(Self)
End;

{------------------------------------------------------------------------------}

Procedure TPSCCalendarDataLink.EditingChanged;
Begin
  SetEditing(Inherited Editing And CanModify)
End;

{------------------------------------------------------------------------------}

Procedure TPSCCalendarDataLink.FocusControl(Field: TFieldRef);
Begin
  If (Field^ <> Nil) And
    ((Field^ = FBitsField) Or (Field^ = FStartField) Or (Field^ = FFinishField))
      And
    (FControl Is TWinControl) And TWinControl(FControl).CanFocus Then
    Begin
      Field^ := Nil;
      TWinControl(FControl).SetFocus
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCalendarDataLink.RecordChanged(Field: TField);
Begin
  If (Field = Nil) Or (Field = FBitsField) Or (Field = FStartField) Or
    (Field = FFinishField) Then
    Begin
      If Assigned(FOnDataChange) Then
        FOnDataChange(Self);
      FModified := false
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCalendarDataLink.LayoutChanged;
Begin
  UpdateFields
End;

{------------------------------------------------------------------------------}

Procedure TPSCCalendarDataLink.UpdateData;
Begin
  If FModified Then
    Begin
      If (FBitsField <> Nil) Or (FStartField <> Nil) Or (FFinishField <> Nil)
        And
        Assigned(FOnUpdateData) Then
        FOnUpdateData(Self);
      FModified := false
    End
End;

{------------------------------------------------------------------------------}

Constructor TPSCCustomDBCalendar.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);
  FDataLink := TPSCCalendarDataLink.Create;
  TPSCCalendarDataLink(FDataLink).Control := Self;
  TPSCCalendarDataLink(FDataLink).OnDataChange := DataChange;
  TPSCCalendarDataLink(FDataLink).OnUpdateData := UpdateData
End;

{------------------------------------------------------------------------------}

Destructor TPSCCustomDBCalendar.Destroy;
Begin
  FDataLink.Free;
  FDataLink := Nil;
  Inherited Destroy
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomDBCalendar.Notification(AComponent: TComponent;
  Operation: TOperation);
Begin
  Inherited Notification(AComponent,Operation);
  If (Operation = opRemove) And (FDataLink <> Nil) And
    (AComponent = TPSCCalendarDataLink(FDataLink).DataSource) Then
    TPSCCalendarDataLink(FDataLink).DataSource := Nil
End;

{------------------------------------------------------------------------------}

Function TPSCCustomDBCalendar.CanSelectionChange(Const Date: TDateTime):WordBool;
Begin
  Result := Inherited CanSelectionChange(Date) And
    (FDataChanging Or (FDataLink = Nil) Or Not TPSCCalendarDataLink(FDataLink).Active Or
    TPSCCalendarDataLink(FDataLink).CanModify And Not (csDesigning In ComponentState))
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomDBCalendar.DateChanged;
Begin
  Inherited DateChanged;
  If Not FDataChanging And (FDataLink <> Nil) And TPSCCalendarDataLink(FDataLink).Active And
    TPSCCalendarDataLink(FDataLink).CanModify And Not (csDesigning In ComponentState) Then
    Begin
      FDataChanging := true;
      Try
        TPSCCalendarDataLink(FDataLink).Edit;
        TPSCCalendarDataLink(FDataLink).Modified
      Finally
        FDataChanging := false
      End
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomDBCalendar.SetReadOnly(Value: Boolean);
Begin
  TPSCCalendarDataLink(FDataLink).ReadOnly := Value;
End;

{------------------------------------------------------------------------------}

Function TPSCCustomDBCalendar.GetReadOnly: Boolean;
Begin
  Result := TPSCCalendarDataLink(FDataLink).ReadOnly;
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomDBCalendar.CMExit(Var Message: TCMExit);
Begin
  Inherited;
  If Not FDataChanging Then
    Begin
      FDataChanging := true;
      Try
        TPSCCalendarDataLink(FDataLink).UpdateData
      Finally
        FDataChanging := false
      End
    End
End;

{------------------------------------------------------------------------------}

Function TPSCCustomDBCalendar.ExecuteAction(Action: TBasicAction): Boolean;
Begin
  Result := Inherited ExecuteAction(Action) Or (FDataLink <> Nil) And
    TPSCCalendarDataLink(FDataLink).ExecuteAction(Action)
End;

{------------------------------------------------------------------------------}

Function TPSCCustomDBCalendar.UpdateAction(Action: TBasicAction): Boolean;
Begin
  Result := Inherited UpdateAction(Action) Or (FDataLink <> Nil) And
    TPSCCalendarDataLink(FDataLink).UpdateAction(Action)
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomDBCalendar.Reset;
Begin
  TPSCCalendarDataLink(FDataLink).Reset;
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomDBCalendar.SetDataSource(Value: TDataSource);
Begin
  With TPSCCalendarDataLink(FDataLink) Do
    If Not (DataSourceFixed And (csLoading In ComponentState)) Then
      Begin
        If (DataSource <> Nil) And (DataSource.Owner = Self) Then
          DataSource.Destroy;
        DataSource := Value;
        If Value <> Nil Then
          Value.FreeNotification(Self)
      End
End;

{------------------------------------------------------------------------------}

Function TPSCCustomDBCalendar.GetDataSource: TDataSource;
Begin
  Result := TPSCCalendarDataLink(FDataLink).DataSource;
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomDBCalendar.SetDataSet(Value: TDataSet);
Begin
  With FDataLink Do
    Begin
      If DataSource = Nil Then
        DataSource := TDataSource.Create(Self);
      DataSource.DataSet := Value
    End
End;

{------------------------------------------------------------------------------}

Function TPSCCustomDBCalendar.GetDataSet: TDataSet;
Begin
  With FDataLink Do
    If DataSource <> Nil Then
      Result := DataSource.DataSet
    Else
      Result := Nil
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomDBCalendar.SetBitsFieldName(Const Value: String);
Begin
  TPSCCalendarDataLink(FDataLink).BitsFieldName := Value;
End;

{------------------------------------------------------------------------------}

Function TPSCCustomDBCalendar.GetBitsFieldName: String;
Begin
  Result := TPSCCalendarDataLink(FDataLink).BitsFieldName;
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomDBCalendar.SetStartFieldName(Const Value: String);
Begin
  TPSCCalendarDataLink(FDataLink).StartFieldName := Value
End;

{------------------------------------------------------------------------------}

Function TPSCCustomDBCalendar.GetStartFieldName: String;
Begin
  Result := TPSCCalendarDataLink(FDataLink).StartFieldName;
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomDBCalendar.SetFinishFieldName(Const Value: String);
Begin
  TPSCCalendarDataLink(FDataLink).FinishFieldName := Value
End;

{------------------------------------------------------------------------------}

Function TPSCCustomDBCalendar.GetFinishFieldName: String;
Begin
  Result := TPSCCalendarDataLink(FDataLink).FinishFieldName;
End;

{------------------------------------------------------------------------------}

Function TPSCCustomDBCalendar.GetBitsField: TField;
Begin
  Result := TPSCCalendarDataLink(FDataLink).BitsField;
End;

{------------------------------------------------------------------------------}

Function TPSCCustomDBCalendar.GetStartField: TField;
Begin
  Result := TPSCCalendarDataLink(FDataLink).StartField;
End;

{------------------------------------------------------------------------------}

Function TPSCCustomDBCalendar.GetFinishField: TField;
Begin
  Result := TPSCCalendarDataLink(FDataLink).FinishField;
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomDBCalendar.DataChange(Sender: TObject);

  Procedure SetDates(Const Value: String);
  Var
    I: integer;
  Begin
    For I := 1 To Length(Value) Do
      SelDate[SelStart + I - 1] := Value[I] <> '0';
    UpdateWeekSelection(true)
  End;

Begin
  If FDataChanging Then
    Exit;
  FDataChanging := true;
  Try
    With TPSCCalendarDataLink(FDataLink) Do
      Begin
        If Not Active Then
          Exit;
        BeginUpdate;
        Try
          If StartField = Nil Then
            Exit;
          ClearSelection;
          MultiSelect := FinishField <> Nil;
          ExtendedSelect := BitsField <> Nil;
          If Not StartField.IsNull Then
            SelStart := PSCDateOf(StartField.AsDateTime);
          StartDate := SelStart;
          If FinishField = Nil Then
            Exit;
          If Not FinishField.IsNull Then
            SelFinish := PSCDateOf(FinishField.AsDateTime);
          If (BitsField <> Nil) And Not FinishField.IsNull Then
            SetDates(BitsField.AsString)
        Finally
          EndUpdate
        End
      End
  Finally
    FDataChanging := false
  End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomDBCalendar.UpdateData(Sender: TObject);

  Function MakeBitsString: String;
  Var
    I: Integer;
    MyCount:Integer;
  Begin
    Result:='';
    MyCount:=Trunc(SelFinish - SelStart + 1);
    For I := 1 To MyCount Do
      If SelDate[SelStart + I - 1] Then
        Result:= Result + '1'
      Else
        Result:= Result + '0';
  End;

Begin
  With TPSCCalendarDataLink(FDataLink) Do
    If Editing Then
      Begin
        If StartField <> Nil Then
          If (SelCount = 0) And Not StartField.Required Then
            StartField.Clear
          Else
            StartField.AsDateTime := PSCDateOf(SelStart) +
              PSCTimeOf(StartField.AsDateTime);
        If FinishField <> Nil Then
          If (SelCount = 0) And Not FinishField.Required Then
            FinishField.Clear
          Else
            FinishField.AsDateTime := PSCDateOf(SelFinish) +
              PSCTimeOf(FinishField.AsDateTime);
        If BitsField <> Nil Then
          If (SelCount = 0) And Not BitsField.Required Then
            BitsField.Clear
          Else
            BitsField.AsString := MakeBitsString
      End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomDBCalendar.KeyDown(Var Key: Word; Shift: TShiftState);
Begin
  Case Key Of
    VK_ESCAPE:
      Begin
        Reset;
        Key := 0
      End
  End;
  Inherited KeyDown(Key,Shift);
End;

{-------------------------------------}

type
  TPSCDrawArrowKind = array[boolean, boolean] of Cardinal;

  THackPopupMenu = class(TPopupMenu)
  end;

{-------------------------------------}

const
  WhiteArrowPoints: Array[0..2] Of TPoint = (
    (x: 0; y: - 5),
    (x: 5; y: 0),
    (x: 0; y: 5));
    
  CPSCDrawArrowKind : TPSCDrawArrowKind = (
    (
      PSC_DFCS_SCROLLRIGHT,
      PSC_DFCS_SCROLLRIGHT or PSC_DFCS_PUSHED
    ),
    (
      PSC_DFCS_SCROLLLEFT,
      PSC_DFCS_SCROLLLEFT or PSC_DFCS_PUSHED
    )
  );

{-------------------------------------}

constructor TPSCCustomCalendarPro2.Create(AOwner: TComponent);
begin
  FCalendarState:=CPSCCalendarState_Outlook;
  inherited;
  CalendarStyle := cstOutlook;
end;

{-------------------------------------}

destructor TPSCCustomCalendarPro2.Destroy;
begin
  FPopupHeaderYear.Free;
  FMonthsPopup.Free;
  inherited;
end;

{-------------------------------------}

procedure TPSCCustomCalendarPro2.CustomSelect(const ADate: TDateTime; ARect: TRect;
 const AText : string);
var
  MyLeftRect, MyRightRect : TRect;
  MyOldPenColor : TPSCColor;
  MyOldBrushColor : TPSCColor;
  MyRect : TRect;
  MySize : TSize;
  MyDate : TDateTime;
  MyHitTest : TPSCCalendarHitTest;
begin
  MySize := GetTextSize(AText);
  MyRect := ARect;
  UpdateDaySelectRect(ARect, GetTextSize(AText), MyRect);
  with Canvas, MyRect, Colors do
  begin
    MyOldBrushColor := Brush.Color;
    MyHitTest := GetHitTest(ARect.TopLeft, MyDate);
    Brush.Color := GetBkColor(ADate, MyHitTest, Colors.Days);
    FillRect(ARect);
    MyLeftRect := Rect(Left, Top, (Right + Left) div 2, Bottom);
    MyRightRect := Rect((Left + Right) div 2, Top, Right, Bottom);
    MyOldPenColor := Pen.Color;
    Pen.Width := 1;
    Pen.Color := GetBkColor(ADate, MyHitTest, Colors.Days);
    Brush.Color := Selected;
    case SelectStyle of
      sslEllipse :
        case GetSelectShape(ADate) of
          sstEllipse : Ellipse(MyRect);
          sstRightEllipse :
          begin
            Ellipse(MyRect);
            FillRect(MyRightRect);
          end;
          sstLeftEllipse :
          begin
            Ellipse(MyRect);
            FillRect(MyLeftRect);
          end;
          sstRectangle : FillRect(MyRect)
        end;
      sslFillRect: FillRect(MyRect);
      sslFrameRect: FrameRect(MyRect);
    else
      FillRect(MyRect);
    end;
    Pen.Color := MyOldPenColor;
    Brush.Color := MyOldBrushColor;
  end;
end;

{-------------------------------------}

procedure PSCDrawDayHelix(ACanvas : TPSCCanvas; const ARect : TRect);
begin
  with ACanvas, ARect do
  begin
    MoveTo(Left, Top + 3);
    LineTo((Left + Right) div 2 + 2, Top + 3);
    Arc(Left + 1, Top + 3, Right, Bottom, Left,
     (Top + Bottom) div 2 + 5, (Right + Left) div 2 + 3, Top);
    PolyBezier([Point(Left + 1, (Top + Bottom) div 2 + 4), Point(Left + 3, (2 * Top + Bottom) div 3 + 4),
     Point(Left + 4, (3 * Top + Bottom) div 4 + 4), Point((Left + Right) div 2, (4 * Top + Bottom) div 5 + 4)]);
  end;
end;

{-------------------------------------}

procedure TPSCCustomCalendarPro2.DrawArrow(Arrow: TPSCCalendarHitTest;
  const Rect: TRect);
var
  MyRect : TRect;
  MyArrowRect : TRect;
  MyLeft, MyTop : integer;
  MyWidth, MyHeight : integer;
  MyArrowDown : boolean;
begin
  if Rect.Bottom - Rect.Top = 0 then
    exit;
  if ArrowStyle <> astMSMoney then{= astMonthCalendar then}
  begin
    MyArrowDown := false;
    MyArrowRect := Rect;//GetArrowRect(Arrow);
    with MyArrowRect do
    begin
      MyWidth := PSCGetSystemMetrics(SM_CXHSCROLL) + 6;
      MyHeight := PSCGetSystemMetrics(SM_CYHSCROLL);
      MyLeft := 0;
      if Arrow = chtRightArrow then
      begin
        MyLeft := Right - MyWidth - 4;
        MyArrowDown := (csLButtonDown in ControlState) and (SelectedArrow = saRightArrow);
      end;
      if Arrow = chtLeftArrow then
      begin
        MyLeft := Left + 4;
        MyArrowDown := (csLButtonDown in ControlState) and (SelectedArrow = saLeftArrow);
      end;
        MyTop := (Bottom + Top - MyHeight) div 2;
        MyRect := Classes.Rect(MyLeft, MyTop, MyLeft + MyWidth, MyTop + MyHeight);
    end;
    DrawArrowAsMonth(Arrow, MyArrowRect, MyRect, MyArrowDown);
  end
  else
    inherited;
end;

{-------------------------------------}

procedure TPSCCustomCalendarPro2.DrawDay(const Date: TDateTime; BkgndColor,
  TextColor: TPSCColor; FontStyle: TFontStyles; Area: TPSCCalendarHitTest;
  DateType: TPSCCalendarDateType; X, Y: Integer; const Rect: TRect);
var
  MyDay, MyMonth, MyYear : word;
  MySelect : boolean;
begin
  PSCDecodeDate(Date, MyYear, MyMonth, MyDay);
  MySelect := DateType in [cdtSelected, cdtSelectedWeek];
  if MySelect then
    CustomSelect(Date, Rect, CPSCDayNumbers[MyDay]);
  if (Area in [chtDay, chtLeftGray, chtRightGray]) then
    CustomDayDraw(Date, MySelect, DateType, Rect, CPSCDayNumbers[MyDay], Area,
    BkgndColor, TextColor, FontStyle, X, Y);
end;

{-------------------------------------}

function TPSCCustomCalendarPro2.GetHeaderText(
  const Date: TDateTime): String;
Var
  MyYear, MyMonth, MyDay: Word;
Begin
  if HeaderFormat = '' then
    result := inherited GetHeaderText(Date)
  else
  begin
    PSCDecodeDate(Date, MyYear, MyMonth, MyDay);
    If ShortMonthName Then
      result := PSCShortMonthNames(MyMonth)
    Else
      result := PSCLongMonthNames(MyMonth);
    result := FormatDateTime(HeaderFormat, Date);
    Case HeaderCase Of
      tcUpper: Result := PSCUpperCase(Result);
      tcLower: Result := PSCLowerCase(Result)
    End
  end;
End;

{-------------------------------------}

procedure TPSCCustomCalendarPro2.SetHeaderFormat(const AValue: string);
begin
  if FHeaderFormat <> AValue then
  begin
    FHeaderFormat := AValue;
    Changed;
  end;
end;

{-------------------------------------}

function TPSCCustomCalendarPro2.GetSelectShape(const ADate: TDateTime): TPSCCalendarSelShape;
const
  CPSCArray : array[boolean, boolean] of TPSCCalendarSelShape =
  ((sstEllipse, sstRightEllipse), (sstLeftEllipse, sstRectangle));
begin
  result := CPSCArray[SelDate[ADate - 1], SelDate[ADate + 1]];
end;

{-------------------------------------}

procedure TPSCCustomCalendarPro2.SetCalendarStyle(
  Value: TPSCCalendarStyle);
begin
  begin
    FCalendarStyle := Value;
    case FCalendarStyle of
      cstMSMoney:
        FCalendarState:=CPSCCalendarState_MSMoney;
      cstMonthCalendar:
        FCalendarState:=CPSCCalendarState_MonthCalendar;
      cstWhiteOnBlack:
        FCalendarState:=CPSCCalendarState_WhiteOnBlack;
      cstBlackOnWhite:
        FCalendarState:=CPSCCalendarState_BlackOnWhite;
      cstMSMoney2002:
        FCalendarState:=CPSCCalendarState_MSMoney2002;
      cstForPrinting:
        FCalendarState:=CPSCCalendarState_ForPrinting;
      cstOutlookMonthView:
        FCalendarState:=CPSCCalendarState_OutlookMonthView;
      else
        FCalendarState:=CPSCCalendarState_Outlook;
    end;
    Invalidate;
    SetOtherProperties;
  end;
end;

{-------------------------------------}

procedure TPSCCustomCalendarPro2.DrawArrowAsMonth(AArea: TPSCCalendarHitTest;
  const AArrowRect, ARect: TRect; AIsArrowDown : boolean);
var
  MyState : Cardinal;

  Procedure _DrawArrow(Canvas: TPSCCanvas; LeftArrow: Boolean;
    Const Rect: TRect);
  Var
    I: Integer;
    Points: Array[0..High(WhiteArrowPoints)] Of TPoint;
    P: TPoint;
  Begin
    With Canvas,Rect Do
      Begin
        For I := Low(WhiteArrowPoints) To High(WhiteArrowPoints) Do
          With Points[I] Do
            Begin
              P := WhiteArrowPoints[I];
              If LeftArrow Then
                P.x := -P.x;
              x := (Left + Right) Div 2 + P.x * (Right - Left) Div 32;
              y := (Top + Bottom) Div 2 + P.y * (Bottom - Top) Div 24
            End;
        Polygon(Points);
      End
  End;

begin
    MyState := CPSCDrawArrowKind[AArea = chtLeftArrow, AIsArrowDown];
    case HeaderStyle of
      chsMonthCalendar:
        DrawFrameControl(Canvas.Handle, ARect, PSC_DFC_SCROLL, MyState);
    else
      begin
        with Canvas do
        begin
          if Brush.Color <> Colors.ArrowColor then
            Brush.Color := Colors.ArrowColor;
          if Pen.Color <> Colors.ArrowColor then
            Pen.Color := Colors.ArrowColor;
        end;
        if AArea = chtRightArrow then
          _DrawArrow(Canvas, false, ARect);
        if AArea = chtLeftArrow then
          _DrawArrow(Canvas, true, ARect);
      end;
    end;
end;

{-------------------------------------}

procedure TPSCCustomCalendarPro2.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  MyDate : TDateTime;
  MyYearPopupShow : boolean;
  MyPopupAsMonthShow : boolean;
  MyP : TPoint;
  MyHitTest : TPSCCalendarHitTest;
  MyLeft, MyTop : integer;
  MyYearInHeader, MyMonthInHeader : TRect;
begin
  MyP := Point(X, Y);
  MyHitTest := GetHitTest(Point(x, y), MyDate);
  if (ssLeft in Shift) and (MyHitTest = chtDay) then
    IsLeftMouseDown := true;
  MyLeft:= 0;
  MyTop := 0;
  DoMouseOnHitTest(MyDate, MyHitTest, MyP, MyYearInHeader, MyMonthInHeader, MyLeft, MyTop);
  MyYearPopupShow :=  false;
  MyPopupAsMonthShow := false;

  MyYearPopupShow :=  PtInRect(MyYearInHeader, Point(X, Y));

  if (MyHitTest = chtMonthHead) and (PopupType = ptMonthCalendar) then
  begin
    MyPopupAsMonthShow := not PtInRect(MyYearInHeader, MyP);
  end;
  if MyYearPopupShow then
  begin
    FYearPopupMonthNumber := PSCGetMonthsBetween(MyDate, StartDate);
    ShowYearPopup(MyLeft, MyTop, MyDate);
  end
  else
    inherited;
  if MyPopupAsMonthShow then
  begin
    MyP := ClientToScreen(Point(X, Y));
    if MonthsPopup = nil then
    begin
      MonthsPopup := THackPopupMenu.Create(Self);
      PopupAsMonthInit;
    end;
    MonthsPopup.Popup(MyP.X, MyP.Y);
  end;
end;

{-------------------------------------}

procedure TPSCCustomCalendarPro2.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  IsLeftMouseDown := false;
end;

{-------------------------------------}

procedure TPSCCustomCalendarPro2.DoMeasureItem(AArea: TPSCCalendarHitTest;
  var AWidth, AHeight: Integer);
begin
  inherited;
  DoMeasureItemAsMonthCalendar(AArea, AWidth, AHeight);
end;

{-------------------------------------}

function TPSCCustomCalendarPro2.IsHeaderFormatStored: boolean;
begin
  result := FCalendarState.HeaderFormat <> HeaderFormat;
end;

{-------------------------------------}

procedure TPSCCustomCalendarPro2.DrawWeekLine(var Rect: TRect);
var
  MyOldPenColor : TPSCColor;
  MyOldPenWidth : integer;
  Delta : integer;
begin
  with Canvas do
  begin
    if WeekLineStyle = wlsIndentedLine then
      inherited
    else
    begin
      if CalendarStyle = cstMonthCalendar then
        Delta := 4
      else
        Delta := 0;
      MyOldPenColor := Pen.Color;
      MyOldPenWidth := Pen.Width;
      Pen.Color := Colors.WeekLineColor;
      Pen.Width := 1;
      MoveTo(Rect.Left + Delta, Rect.Bottom - 1);
      LineTo(Rect.Right - Delta, Rect.Bottom - 1);
      Pen.Color := MyOldPenColor;
      Pen.Width := MyOldPenWidth;
    end;
  end;
end;

{-------------------------------------}

procedure TPSCCustomCalendarPro2.SetWeekLineStyle(
  AValue: TPSCCalendarWeekLineStyle);
begin
  if FWeekLineStyle <> AValue then
  begin
    FWeekLineStyle := AValue;
    Changed;
  end;
end;

{-------------------------------------}

procedure TPSCCustomCalendarPro2.SetSelectStyle(AValue: TPSCCalendarSelStyle);
begin
  if FSelectStyle <> AValue then
  begin
    FSelectStyle := AValue;
    Changed;
  end;
end;

{-------------------------------------}

procedure TPSCCustomCalendarPro2.DrawHeader(const Date: TDateTime;
  MonthPos: TPSCMonthPos; var Rect: TRect);
Const
  LeftEdge: Array[Boolean] Of Cardinal = (0,BF_LEFT);
  TopEdge: Array[Boolean] Of Cardinal = (0,BF_TOP);
  RightEdge: Array[Boolean] Of Cardinal = (0,BF_RIGHT);
var
  MySize : TSize;
  MyS : string;
  MyOldBrushColor : TPSCColor;
  MyRect : TRect;
  MyArrowRect : TRect;
  Myx,MyY:Integer;
begin
  if Rect.Bottom - Rect.Top = 0 then
    exit;
  with Canvas, Colors do
  begin
  if HeaderStyle <> chsOutlook then
    begin
      MyRect := Rect;
      MyArrowRect := GetArrowRect(chtRightArrow);
      if Rect.Right > MyArrowRect.Right then
        with Rect do
          MyRect := Classes.Rect(Left, Top, Right - (MyArrowRect.Right - MyArrowRect.Left), Bottom);
      MyArrowRect := GetArrowRect(chtLeftArrow);
      if Rect.Left < MyArrowRect.Left then
        with Rect do
          MyRect := Classes.Rect(Left + (MyArrowRect.Right - MyArrowRect.Left), Top, MyRect.Right, Bottom);
      MyOldBrushColor := Brush.Color;
      Brush.Color := MonthHeader;
      Font := Self.Font;
      Font.Color := HeaderFont.Color;
      Font.Style := HeaderFont.Style;
      MyS := GetHeaderText(Date);
      MySize:=Canvas.TextExtent(MyS);

      if ShowNavButtons then
        begin
          MyX:=(MyRect.Right + MyRect.Left - MySize.cx) Div 2;
          MyY:=(MyRect.Top + MyRect.Bottom - MySize.cy) Div 2;
          Canvas.TextRect(MyRect,MyX,MyY,MyS);
        end
      else
        begin
          MyX:=(Rect.Right + Rect.Left - MySize.cx) Div 2;
          MyY:=(Rect.Top + Rect.Bottom - MySize.cy) Div 2;
          Canvas.TextRect(Rect,MyX,MyY,MyS);
        end;

      if HeaderStyle = chsMonthCalendar then
        DrawEdge(Canvas.Handle,Rect, BDR_RAISEDINNER,BF_ADJUST Or
          LeftEdge[Not (mpLeft In MonthPos)] Or
          RightEdge[Not (mpRight In MonthPos)] Or BF_MONO);
      Pen.Color := HeaderBorderColor;
      Brush.Color := HeaderBorderColor;
      if HeaderStyle <> chsMonthCalendar then
        if BorderStyle = bsNone then
          with MyRect do
        begin
          Pen.Width := 1;
          MoveTo(MyRect.Left, MyRect.Bottom - 1);
          LineTo(MyRect.Right - 1, MyRect.Bottom - 1);
          LineTo(Right - 1, Top);
          LineTo(Left, Top);
          LineTo(Left, Bottom);
//          FrameRect(MyRect)
        end
        else
          with MyRect do
          begin
            MoveTo(MyRect.Left, MyRect.Bottom - 1);
            LineTo(MyRect.Right - 1, MyRect.Bottom - 1);
            MoveTo(MyRect.Left, MyRect.Bottom - 1);
            Pen.Color := clPSCWindow;
            LineTo(Left, Top);
            LineTo(Right, Top);
          end;
      Brush.Color := MyOldBrushColor;
    end
    else
      if ShowHeader then
        inherited;
  end;
end;

{-------------------------------------}

type
  TPSCYearEditPopupForm = class(TPSCPopupForm)
  private
    FYearEdit : TPSCDateTimeUpDown;
  public
    Constructor CreateNew(AOwner : TComponent; Dummy: Integer=0); override;
    property YearEdit : TPSCDateTimeUpDown read FYearEdit write FYearEdit;
  end;

{-------------------------------------}

procedure TPSCCustomCalendarPro2.DoPopupClosed(Sender: TObject;
  ACanceled: boolean);
var
  MyNewYear : word;
  MyDate : TDateTime;
  MyYear, MyMonth, MyDay : word;
begin
  MyDate := StartDate;
  PSCDecodeDate(MyDate, MyYear, MyMonth, MyDay);
  MyDate := PSCIncMonth(MyDate, FYearPopupMonthNumber);
  PSCDecodeDate(MyDate, MyYear, MyMonth, MyDay);
  MyNewYear := TPSCYearEditPopupForm(PopupHeaderYear).YearEdit.TextParts.Items[0].PartData.GetValue;
  if MyNewYear <> MyYear then
  begin
    MyDate := PSCEncodeDate(MyNewYear, MyMonth, MyDay);
    MyDate := PSCIncMonth(MyDate, -FYearPopupMonthNumber);
    StartDate := MyDate;
  end;
end;

{-------------------------------------}

procedure TPSCCustomCalendarPro2.DoYearChange(Sender: TObject);
begin
  DoPopupClosed(Sender, false);
end;

{-------------------------------------}

procedure TPSCCustomCalendarPro2.PopupAsMonthInit;
const
  CQuickChars:Array[1..12] of String=(
   '1','2','3','4','5','6','7','8','9','0','!','@');
var
  i : integer;
  MyNewItem: TMenuItem;
begin
  with MonthsPopup do
  begin
    for i := 1 to 12 do
    begin
      MyNewItem := TMenuItem.Create(MonthsPopup);
      with MyNewItem do
      begin
        Caption := PSCLongMonthNames(i);
        Caption := '&'+CQuickChars[i]+'. '+Caption;
        Tag := i;
        OnClick := DoPopupAsMonthClick;
      end;
      Items.Add(MyNewItem);
    end;
  end;
end;

{-------------------------------------}

procedure TPSCCustomCalendarPro2.DoPopupAsMonthClick(Sender: TObject);
var
  MyR : TRect;
  MyDate : TDateTime;
  MyYear, MyMonth, MyDay : word;
  MyX, MyY : integer;
  MyMonthCol, MyMonthRow : integer;
  MyMonthWidth, MyMonthHeight : integer;
  MyP : TPoint;
  MyNewMonth, MyNewYear : integer;
begin
  MyP := ScreenToClient(THackPopupMenu(MonthsPopup).PopupPoint);
  MyMonthCol := 0;
  MyMonthRow := 0;
  MyMonthWidth := GetMonthWidth;
  MyMonthHeight := GetMonthHeight;
  MyR := UpdateTopLeftCorner(Rect(0,0,ClientWidth,ClientHeight));
  MyX := MyP.X - MyR.Left;
  MyY := MyP.Y - MyR.Top;
  MyY := MyY - MyMonthHeight;
  while MyY > 0 do
  begin
    MyMonthRow := MyMonthRow + 1;
    MyY := MyY - MyMonthHeight;
  end;
  MyX := MyX - MyMonthWidth;
  while MyX > 0 do
  begin
    MyMonthCol := MyMonthCol + 1;
    MyX := MyX - MyMonthWidth;
  end;
  MyDate := GetMonthStartDate(MyMonthCol,MyMonthRow,false);
  PSCDecodeDate(MyDate, MyYear, MyMonth, MyDay);
  MyNewMonth := (Sender as TMenuItem).Tag - (MyMonthCol + MyMonthRow * MonthColCount);
  MyNewYear := MyYear;
  if MyNewMonth <= 0 then
  begin
    MyNewMonth := MyNewMonth + 12;
    MyNewYear := MyNewYear - 1;
    if MyNewYear < 0 then
      MyNewYear := 0;
  end;
  if MyNewMonth > 12 then
  begin
    MyNewMonth := MyNewMonth - 12;
    MyNewYear := MyNewYear + 1;
  end;
  MyMonth := MyNewMonth;
  MyYear := MyNewYear;
  StartDate := PSCEncodeDate(MyYear, MyMonth, MyDay);
end;

{-------------------------------------}

procedure TPSCCustomCalendarPro2.SetShowFocusRect(AValue: boolean);
begin
  if FShowFocusRect <> AVAlue then
  begin
    FShowFocusRect := AValue;
    Changed;
  end;
end;

{-------------------------------------}

procedure TPSCCalendarColorsPro.SetGrayedBkColor(AValue: TPSCColor);
begin
  if FGrayedBkColor <> AValue then
  begin
    FGrayedBkColor := AValue;
    Changed;
  end;
end;

{-------------------------------------}

procedure TPSCCalendarColorsPro.SetHeaderBorderColor(AValue: TPSCColor);
begin
  if FHeaderBorderColor <> AValue then
  begin
    FHeaderBorderColor := AValue;
    Changed;
  end;
end;

{-------------------------------------}

procedure TPSCCalendarColorsPro.SetWeekLineColor(AValue: TPSCColor);
begin
  if FWeekLineColor <> AValue then
  begin
    FWeekLineColor := AValue;
    Changed;
  end;
end;

{-------------------------------------}

procedure TPSCCustomCalendarPro2.SetTodayStyle(
  AValue: TPSCCalendarTodayStyle);
begin
  if FTodayStyle <> AValue then
  begin
    FTodayStyle := AValue;
    Changed;
  end;
end;

{-------------------------------------}

function TPSCCustomCalendarPro2.IsTodayStyleStored: boolean;
begin
  result := FCalendarState.TodayStyle <> TodayStyle;
end;

{-------------------------------------}

procedure TPSCCustomCalendarPro2.SetShowHeader(AValue: boolean);
begin
  if FShowHeader <> AValue then
  begin
    FShowHeader := AValue;
    Changed;
  end;
end;

{-------------------------------------}

procedure TPSCCustomCalendarPro2.DoOnDrawDayOutline(const ADate: TDateTime;
  X, Y: Integer; const ARect: TRect;
  var AParams: TPSCDrawDayOutlineParams);
begin
  AParams.ShowToday := false;
  inherited;
end;

{-------------------------------------}

procedure TPSCCustomCalendarPro2.SetHeaderStyle(AValue: TPSCCalendarHeaderStyle);
begin
  if FHeaderStyle <> AValue then
  begin
    FHeaderStyle := AValue;
    Changed;
  end;
end;

{-------------------------------------}

function TPSCCustomCalendarPro2.IsHeaderStyleStored: boolean;
begin
  result := FCalendarState.HeaderStyle <> HeaderStyle;
end;

{-------------------------------------}

procedure TPSCCustomCalendarPro2.SetShowFooter(AValue: boolean);
begin
  if FShowFooter <> AValue then
  begin
    FShowFooter := AValue;
    Changed;
  end;
end;

{-------------------------------------}

procedure TPSCCustomCalendarPro2.DrawFooter(const ARect: TRect);
begin
  DrawFooterMonthCalendar(ARect);
end;

{-------------------------------------}

procedure TPSCCustomCalendarPro2.Paint;
var
  MyRect : TRect;
begin
  inherited;
  if ShowFooter then
  begin
    MyRect := CorrectNCRect(ClientRect);
    with MyRect do
      MyRect := Rect(Left, Bottom - FFooterHeight, Right, Bottom);
    DrawFooter(MyRect);
  end;
end;

{-------------------------------------}

procedure TPSCCustomCalendarPro2.FontChanged;
begin
  inherited;
  UpdateMeasure;
  BorderChanged;
end;

{-------------------------------------}

procedure TPSCCustomCalendarPro2.KeyDown(var Key: Word;
  Shift: TShiftState);
begin
  if (not WantEsc) and (Key = VK_ESCAPE) then
    exit;
  inherited;
end;

{-------------------------------------}

function TPSCCustomCalendarPro2.GetHitTest(P: TPoint;
  var Date: TDateTime): TPSCCalendarHitTest;
var
  MyRect : TRect;
  MyR : TRect;
begin
  result := inherited GetHitTest(P, Date);
  MyR := CorrectNCRect(ClientRect);
  with MyR do
  begin
    if ShowFooter then
      MyRect := Rect(Left, Bottom - FFooterHeight, Right, Bottom)
    else
      MyRect := MyR;
  end;
  if PtInRect(MyRect, P) and ShowFooter then
    result := chtFooter;
  if (P.Y >= MyRect.Bottom) then
    result := chtNowhere;
end;

{-------------------------------------}

procedure TPSCCustomCalendarPro2.SetCalendarsInHeight(AValue: integer);
begin
  if FCalendarsInHeight <> AValue then
  begin
    FCalendarsInHeight := AValue;
    Changed;
  end;
end;

{-------------------------------------}

procedure TPSCCustomCalendarPro2.SetCalendarsInWidth(AValue: integer);
begin
  if FCalendarsInWidth <> AValue then
  begin
    FCalendarsInWidth := AValue;
    Changed;
  end;
end;

{-------------------------------------}

procedure TPSCCustomCalendarPro2.Changed;
begin
  FontChanged;
  Invalidate;
end;

{-------------------------------------}

procedure TPSCCustomCalendarPro2.OpenMonthPopup(const Date: TDateTime);
begin
  if PopupType <> ptMonthCalendar then
    inherited;
end;

{-------------------------------------}

function TPSCCustomCalendarPro2.GetBkColor(const ADate : TDateTime;
 AArea: TPSCCalendarHitTest; const BkgndColor :TPSCColor) : TPSCColor;
var
  MyYear, MyMonth, MyDay : word;
begin
  result := BkgndColor;
  with Canvas do
  begin
    PSCDecodeDate(ADate, MyYear, MyMonth, MyDay);
    if AlterEvenOdd then
    begin
      if MyMonth mod 2 = 0 then
        result := Colors.GrayedBkColor;
    end
    else
    begin
      if (AArea in [chtLeftGray,chtRightGray]) or (PastDaysAsGrayed and
       (ADate < PSCDateOf(PSCNow))) then
        result := Colors.GrayedBkColor;
    end;
  end;
end;

{-------------------------------------}
type
  THackCanvas = class(TPSCCanvas)
  end;

procedure TPSCCustomCalendarPro2.CustomDayDraw(const ADate: TDateTime;
 ASelect: boolean; ADateType : TPSCCalendarDateType; const ARect: TRect;
 const AText: string; AArea: TPSCCalendarHitTest; BkgndColor,
 TextColor: TPSCColor; FontStyle: TFontStyles; X, Y: Integer);
var
  MySlipX, MySlipY :integer;
  MyOldBrushColor : TPSCColor;
  MyOldBrushStyle : TBrushStyle;
  MyOldPenColor : TPSCColor;
  MyOldPenWidth : integer;
  MyOldFontColor : TPSCColor;
  MyRect : TRect;
  MyTodayRect : TRect;
  i : integer;
  MyText : string;
  MySize : TSize;

  procedure DrawFrameRect;
  begin
    with Canvas do
      if (ADate = PSCDateOf(PSCNow)) and ShowToday then
        case TodayStyle of
          ctsHelix:
          begin
            MyOldPenColor := Pen.Color;
            Pen.Color := Colors.NowRect;
            MyOldPenWidth := Pen.Width;
            Pen.Width := 2;
            PSCDrawDayHelix(Canvas, MyRect);
            Pen.Color := MyOldPenColor;
            Pen.Width := MyOldPenWidth;
          end;
          ctsSquare:
          begin
            with MyRect do
            MyTodayRect := Rect(Left, Top, Right, Bottom);
            MyOldBrushColor := Brush.Color;
            Brush.Color := Colors.NowRect;
            Canvas.FrameRect(MyTodayRect);
            Brush.Color := MyOldBrushColor;
          end;
        end;
  end;

  function GetFontColor : TPSCColor;
  begin
    i := PSCDayOfWeek(ADate);
    if TPSCWeekDay(i) in WorkDays then
      result := TextColor
    else
      result := Colors.WeekEndText;
    if ((PastDaysAsGrayed and (ADate < PSCDateOf(PSCNow))) or (ADateType = cdtGray)) or
     (MinDateLimit and (ADate < MinDate))  or (MaxDateLimit and (ADate > MaxDate)) then
      result := Colors.Grayed;
    if ASelect then
    begin
      if SelectStyle = sslFrameRect then
        result := Colors.DaysFont.Color
      else
        result := Colors.SelectedText;
    end;
  end;

begin
  with Canvas, ARect do
  begin
    Font.Style := FontStyle;
    MyText := AText;
    if not ASelect then
    begin
      MyOldBrushColor := Brush.Color;
      Brush.Color := GetBkColor(ADate, AArea, BkgndColor);
      FillRect(ARect);
      Brush.Color := MyOldBrushColor;
    end;
      MyOldFontColor := Font.Color;
    Font.Color := GetFontColor;
    if FShowHolidays and not (AArea in [chtLeftGray,chtRightGray]) and
      GetHolidayNames(ADate,HolidayNames) then
    begin
      Font := Colors.HolidaysFont;
      if ASelect and (SelectStyle <> sslFrameRect) then
        Font.Color := Colors.SelectedText;
      FontStyle := Font.Style;
      TextColor := Font.Color;
    end;
    if ((PSCStrToInt(AText) = 1) or (ADate = GetMonthStartDate(0, 0, true)))
     and (FirstDayFormat <> '') then
      MyText := GetFirstDayText(ADate, MyText, ARect, FirstDayFormat);
    UpdateDayRect(ARect, MyText, MyRect);
    GetDayXYAligned(ARect, GetTextSize(MyText), MySlipX, MySlipY);
    PSCUpdateCanvasState(Canvas);
    MySize := TextExtent(MyText);
    MyOldBrushStyle := Brush.Style;
    Brush.Style := BrushStyle_Clear;
    Canvas.TextRect(MyRect,MyRect.Left + MySlipX, MyRect.Top + MySlipY,MyText);
    Brush.Style := MyOldBrushStyle;
    Font.Color := MyOldFontColor;
    DrawDayOutline(ADate, X, Y, ARect);
    DrawFrameRect;
    if ShowFocusRect and (AArea = chtDay) then
      if (MouseDate = ADate) and IsLeftMouseDown and (not ReadOnly) then
        DrawFocusRect(MyRect);
  end;
end;

{-------------------------------------}

procedure TPSCCustomCalendarPro2.ShowYearPopup(ALeft, ATop : integer;
  const ADate : TDateTime);
const
  CIncValue=2;
begin
  ControlState := [];
  if PopupHeaderYear = nil then
    PopupHeaderYear := TPSCYearEditPopupForm.CreateNew(nil, 0);
  with TPSCYearEditPopupForm(PopupHeaderYear) do
  begin
    BorderStyle := bsNone;
    Font:=Self.Font;
    Color := Colors.Days;

    With YearEdit do
    begin
      OnChange := nil;
      YearEdit.DateTime:=ADate;
      Parent := TWinControl(PopupHeaderYear);
      Flat := True;
      AutoSize := true;
      HandleNeeded;
      Width := GetMaxTextWidth(True);
      OnChange := DoYearChange;
    end;
    ClientWidth := YearEdit.Width;
    ClientHeight := YearEdit.Height;
    if Height > (HeaderHeight + 2) then
    begin
      Height := HeaderHeight + 8;
      YearEdit.Height := ClientHeight - 2;
    end;
    PopupExact(Self, ALeft - 1, ATop - (Height - HeaderHeight) div 2, nil);
  end;
end;

{-------------------------------------}

procedure TPSCCustomCalendarPro2.SetPastDaysAsGrayed(AValue: boolean);
begin
  if FPastDaysAsGrayed <> AValue then
  begin
    FPastDaysAsGrayed := AValue;
    Changed;
  end;
end;

{-------------------------------------}

procedure TPSCCustomCalendarPro2.SetSizeStyle(
  AValue: TPSCCalendarSizeStyle);
begin
  if FSizeStyle <> AValue then
  begin
    FSizeStyle := AValue;
    Changed;
  end;
end;

{-------------------------------------}

function TPSCCustomCalendarPro2.IsSizeStyleStored: boolean;
begin
  result := FCalendarState.SizeStyle <> SizeStyle;
end;

{-------------------------------------}

Procedure PSCDrawBlackOnWhiteArrow(Canvas: TPSCCanvas; LeftArrow: Boolean;
  Const Rect: TRect);
Var
  I: Integer;
  Points: Array[0..High(WhiteArrowPoints)] Of TPoint;
  P: TPoint;
Begin
  With Canvas,Rect Do
    Begin
      For I := Low(WhiteArrowPoints) To High(WhiteArrowPoints) Do
        With Points[I] Do
          Begin
            P := WhiteArrowPoints[I];
            If LeftArrow Then
              P.x := -P.x;
            x := (Left + Right) Div 2 + P.x * (Right - Left) Div 32;
            y := (Top + Bottom) Div 2 + P.y * (Bottom - Top) Div 24
          End;
      Polygon(Points)
    End
End;

{-------------------------------------}

procedure TPSCCustomCalendarPro2.SetWeekDayCase(AValue: TPSCTextCase);
begin
  if FWeekDayCase <> AValue then
  begin
    FWeekDayCase := AValue;
    Changed;
  end;
end;

{-------------------------------------}

function TPSCCustomCalendarPro2.GetDayName(ADayIndex: Integer; ALength:TPSCWeekDaysLength): string;
const
  MyWeekDaysLength : array[0..4] of
    TPSCWeekDaysLength = (wdlOne, wdlOne, wdlTwo, wdlOne, wdlThree);
var
  MySize : TSize;
  MyOldLength : TPSCWeekDaysLength;
  MyWasteWidth : integer;
begin
  with Canvas do
  begin
    case DayHeaderStyle of
      dhsOutlook: MyWasteWidth := 2;
      dhsOutlookMonthView: MyWasteWidth := 20;
    else
      MyWasteWidth := 2;
    end;
    result := inherited GetDayName(ADayIndex, ALength);
    MySize := GetTextSize(result);
    if ((ColWidth - MyWasteWidth) < MySize.cx) and (Length(result) > 1) then
    begin
      MyOldLength := ALength;
      ALength := MyWeekDaysLength[integer(ALength)];
      if ALength <> MyOldLength then
      begin
        result := GetDayName(ADayIndex, ALength);
        MySize := GetTextSize(result);
      end;
    end;
    case WeekDayCase of
      tcUpper: result := PSCUpperCase(result);
      tcLower: result := PSCLowerCase(result);
    end;
  end;
end;

{-------------------------------------}

procedure TPSCCustomCalendarPro2.SetDayHorzAlign(AValue: TPSCHorzAlign);
begin
  if FDayHorzAlign <> AValue then
  begin
    FDayHorzAlign := AValue;
    Changed;
  end;
end;

{-------------------------------------}

procedure TPSCCustomCalendarPro2.SetDayVertAlign(AValue: TPSCVertAlign);
begin
  if FDayVertAlign <> AValue then
  begin
    FDayVertAlign := AValue;
    Changed;
  end;
end;

{-------------------------------------}

procedure TPSCCustomCalendarPro2.GetDayXYAligned(const ARect : TRect;
 const ASize:TSize; var ASlipX, ASlipY: integer);
var
  MySize : TSize;
  MyMaxSize : TSize;
  MyRect : TRect;
  MyControlSlip : integer;
begin
  UpdateDaySelectRect(ARect, ASize, MyRect);
  with MyRect, Canvas do
  begin
    MySize := ASize;//GetTextSize(AText);
    MyMaxSize := GetTextSize('30');
    MyControlSlip := ((Right - Left) - MyMaxSize.cx) div 2;
    ASlipX := ((Right - Left) - MySize.cx) div 2;
    ASlipY := ((Bottom - Top) - MySize.cy) div 2 + 1;
    case DayHorzAlign of
      haLeft:
        if (ASlipX > 4) and (MyControlSlip > 4) then
          ASlipX := 4;
      haCenter: ASlipX := ((Right - Left) - MySize.cx) div 2;
      haRight:
        if (ASlipX > 4) and (MyControlSlip > 4) then
          ASlipX := (Right - Left) - MySize.cx - 4;
    else
      ASlipX := ((Right - Left) - MySize.cx) div 2;
    end;
    if FontStyle_Italic in Font.Style then
      ASlipX := ASlipX - (GetTextSize('g').cx div 2);
  end;
end;

{-------------------------------------}

procedure TPSCCustomCalendarPro2.UpdateDaySelectRect(const ASourceRect: TRect;
 const ASize:TSize; var ADestRect: TRect);
var
  MyDelta : integer;
begin
  with ASourceRect, Canvas do
  begin
    MyDelta := SelectionAndBorderDists;
    if ((Bottom - Top) - 2 * ASize.cy) > 0 then
    begin
      case DayVertAlign of
        vaTop: ADestRect := Rect(Left + MyDelta + 1, Top + MyDelta + 1,
         Right - MyDelta, Top + 3 + ASize.cy + MyDelta);
        vaCenter: ADestRect := Rect(Left + MyDelta + 1, (Bottom + Top - ASize.cy)
         div 2, Right - MyDelta,(Bottom + Top + ASize.cy) div 2 + 2);
        vaBottom: ADestRect := Rect(Left + MyDelta + 1, Bottom - 4 - ASize.cy -
         MyDelta, Right + MyDelta, Bottom - MyDelta);
      else
        ADestRect := Rect(Left + MyDelta + 1, (Bottom + Top - ASize.cy) div 2 -
         2, Right + MyDelta,(Bottom + Top + ASize.cy) div 2 + 2);
      end;
    end
    else
      if SelectStyle = sslEllipse then
        ADestRect := Rect(Left, Top + 2, Right, Bottom - 1)
      else
        ADestRect := Rect(Left + MyDelta, Top,
         Right - MyDelta, Bottom);
  end;
end;

{-------------------------------------}

function TPSCCustomCalendarPro2.CorrectNCRect(const R: TRect): TRect;
var
  MyRect : TRect;
begin
  result := inherited CorrectNCRect(R);
  if ShowFooter then
  begin
    MyRect := result;
    with MyRect do
    begin
      if FFooterHeight div 2 < Top then
        result := Rect(Left, Top - FFooterHeight div 2, Right, Bottom + FFooterHeight div 2)
      else
        result := Rect(Left, 0, Right, Bottom - Top + FFooterHeight);
    end;
  end;
end;

{-------------------------------------}

function TPSCCustomCalendarPro2.GetCalendarHeight: integer;
begin
  if CalendarsInHeight > 0 then
    result := GetMonthHeight * CalendarsInHeight
  else
    result := GetMonthHeight;
  if ShowFooter then
    result := result + FFooterHeight;
end;

{-------------------------------------}

function TPSCCustomCalendarPro2.GetCalendarWidth: integer;
begin
  if CalendarsInWidth > 0 then
    result := GetMonthWidth * CalendarsInWidth
  else
    result := GetMonthWidth;
end;

{-------------------------------------}

procedure TPSCCustomCalendarPro2.DoMouseOnHitTest(const ADate : TDateTime;
  AHitTest: TPSCCalendarHitTest; AP : TPoint; var AYearInHeader,
  AMonthInHeader: TRect; var ALeft : integer; var ATop : integer);
var
  MyMonthWidth, MyMonthHeight : integer;
  MyR : TRect;
  MyRect : TRect;
  MyS, MySS : string;
  MySize, MyWasteSize : TSize;
  MyRight : integer;
begin
  case AHitTest of
    chtFooter:
    begin
      StartDate := PSCDateOf(PSCNow);
      ClearSelection;
      SelDate[PSCDateOf(PSCNow)] := true;
    end;
    chtMonthHead:
    begin
      MyMonthWidth := GetMonthWidth;
      MyMonthHeight := GetMonthHeight;
      MyR := CorrectNCRect(ClientRect);
      MyRect := Rect(MyR.Left + MyMonthWidth * ((AP.X - MyR.Left) Div MyMonthWidth),
       MyR.Top + MyMonthHeight * ((AP.Y - MyR.Top) Div MyMonthHeight), MyR.Left
       + MyMonthWidth * ((AP.X - MyR.Left) div MyMonthWidth + 1), MyR.Top +
       MyMonthHeight * ((AP.Y - MyR.Top) Div MyMonthHeight) + HeaderHeight);
      MyS := GetHeaderText(ADate);
      MySS := Copy(MyS, 1, Length(MyS) - 4);
      MyWasteSize:=Canvas.TextExtent(MySS);
      MySize:=Canvas.TextExtent(MyS);
      with MyRect do
      begin
        ALeft := (Right + Left - MySize.cx) Div 2 + MyWasteSize.cx;
        MyRight := (Right + Left + MySize.cx) Div 2;
        ATop := Top;
        AYearInHeader := Classes.Rect(ALeft, Top, MyRight, Bottom);
        AMonthInHeader := Classes.Rect(Left, Top, ALeft, Bottom);
      end;
    end;

  end;
end;

{-------------------------------------}

procedure TPSCCustomCalendarPro2.UpdateWidthHeight(var AWidth,
  AHeight: Integer);
begin
  inherited;
  UpdateMeasure;
end;

{-------------------------------------}

procedure TPSCCustomCalendarPro2.UpdateMeasure;
var
  MyNewHeight, MyNewWidth : integer;
  MyCalendarsInHeight : integer;
begin
  if HandleAllocated then
  begin
    if CalendarsInHeight > 0 then
    begin
      if ShowFooter then
        MyCalendarsInHeight := (ClientHeight - FFooterHeight) div CalendarsInHeight
      else
        MyCalendarsInHeight := ClientHeight div CalendarsInHeight;
      MyNewHeight := (MyCalendarsInHeight - (HeaderHeight + DaysOfWeekHeight)) div 6;
      if (MyNewHeight > RowHeight) or (CalendarsInHeight = 1) then
        FRowHeight := MyNewHeight;
    end;
    if CalendarsInWidth > 0 then
    begin
      MyNewWidth := (ClientWidth div CalendarsInWidth - SideWidth * 2) div 7;
      if (MyNewWidth > ColWidth) or (CalendarsInWidth = 1) then
        FColWidth := MyNewWidth;
    end;
  end;
end;

{-------------------------------------}

procedure TPSCCustomCalendarPro2.DrawDaysHeader(MonthPos: TPSCMonthPos;
  const Rect: TRect);
var
  i : integer;
  MyOldFont : TPSCFont;
  Delta : integer;
begin
  MyOldFont := TPSCFont.Create;
  MyOldFont.Assign(Canvas.Font);
  Canvas.Font := Colors.WeekDaysFont;
  inherited;
  with Canvas, Rect do
  begin
    If ShowWeekNumbers Then
      Delta := 4
    Else
      Delta := 0;
    if ShowDayDelimiters and (BorderStyle = bsSingle) then
    begin
      for i := 1 to 7 do
      begin
        Pen.Width := 1;
        Pen.Color := clPSCWindow;
        MoveTo(Left + (i - 1) * ColWidth, Top);
        LineTo(Left + 2 * SideWidth + i * ColWidth, Top);
        if i <> 7 then
        begin
          MoveTo(Left + Delta + SideWidth + 1 + i * ColWidth, Top + 2);
          LineTo(Left + Delta + SideWidth + 1 + i * ColWidth, Bottom - 3);
        end;
        Pen.Color := Colors.HeaderBorderColor;
        if i <> 7 then
        begin
          MoveTo(Left + Delta + SideWidth + i * ColWidth, Top + 2);
          LineTo(Left + Delta + SideWidth + i * ColWidth, Bottom - 3);
        end;
      end;
    end;
  end;
  Canvas.Font.Assign(MyOldFont);
  MyOldFont.Free;
end;

{-------------------------------------}

function TPSCCustomCalendarPro2.IsCalendarsInHeightStored: boolean;
begin
  result := FCalendarState.CalendarsInHeight <> CalendarsInHeight;
end;

{-------------------------------------}

function TPSCCustomCalendarPro2.IsCalendarsInWidthStored: boolean;
begin
  result := FCalendarState.CalendarsInWidth <> CalendarsInWidth;
end;

{-------------------------------------}

procedure TPSCCustomCalendarPro2.SetIsLeftMouseDown(AValue: boolean);
var
  MyRect : TRect;
begin
  if FIsLeftMouseDown <> AValue then
  begin
    FIsLeftMouseDown := AValue;
    DateToRect(CursorDate, MyRect);
    InvalidateRect(MyRect);
  end;
end;

{-------------------------------------}

procedure TPSCCustomCalendarPro2.DoMeasureItemAsMonthCalendar(AArea: TPSCCalendarHitTest;
  var AWidth, AHeight: Integer);
var
  MyNewSize : integer;
  MySizeXY : TSize;
begin
  with Colors, Canvas do
  begin
    MyNewSize := Self.Font.Size;
    if SizeStyle = cssMonthCalendar then
    case AArea of
      chtFooter:
      begin
        Font.Size := MyNewSize;
        MySizeXY := GetTextSize('T');
        AHeight := MySizeXY.cy + 5;
      end;
      chtMonthHead:
      begin
        Font := HeaderFont;
        Font.Size := MyNewSize;
        MySizeXY := GetTextSize('s');
        if ShowHeader then
        begin
          case HeaderStyle of
            chsOutlook, chsWhiteOnBlack, chsBlackOnWhite, chsMSMoney2002, chsOutlookMonthView:
              AHeight := MySizeXY.cy + 4;
            chsMonthCalendar, chsForPrinting:
              AHeight := MySizeXY.cy * 2 + 4;
           end;
        end
        else
          AHeight := 0;
      end;
      chtWeekDays:
      begin
        Font.Size := WeekDaysFont.Size;
        MySizeXY := GetTextSize('T');
        if CalendarStyle = cstOutlookMonthView then
          AHeight := MySizeXY.cy + 2
        else
          AHeight := MySizeXY.cy;
      end;
      chtDay:
      begin
        Font.Size := MyNewSize;
        Font.Style := [];
        MySizeXY := GetTextSize('221 ');
        AWidth := (MySizeXY.cx + 8);
        AHeight := (MySizeXY.cy + 2);
        if FontStyle_Italic in Self.Font.Style then
          AWidth := AWidth + (GetTextSize('2').cx div 7);
      end;
      chtWeekRight: AWidth := 0;
      chtWeekLeft:
      begin
        Font.Size := WeekNumbersFont.Size;
        MySizeXY := GetTextSize('11');
        if ShowWeekNumbers then
        begin
          MySizeXY := GetTextSize('22');
          AWidth := MySizeXY.cx;
        end
        else
          if ShowMonthDividers then
            AWidth := 1
          else
            AWidth := 0;
      end;
    end
    else
    case AArea of
      chtMonthHead:
      begin
        Font := HeaderFont;
        MySizeXY := GetTextSize('s');
        if ShowHeader then
        begin
          case HeaderStyle of
            chsOutlook, chsWhiteOnBlack, chsBlackOnWhite, chsMSMoney2002, chsOutlookMonthView:
              AHeight := MySizeXY.cy + 4;
            chsMonthCalendar, chsForPrinting:
              AHeight := MySizeXY.cy * 2 + 4;
           end;
        end
        else
          AHeight := 0;
      end;
      chtWeekDays:
      begin
        Font.Size := WeekDaysFont.Size;
        MySizeXY := GetTextSize('T');
        AHeight := MySizeXY.cy;
      end;
    end;
  end;
end;

{-------------------------------------}

function TPSCCustomCalendarPro2.GetFirstDayText(const ADate: TDateTime;
  const AText: string; const ARect : TRect; AFirstDayFormat : string): string;
var
  MyP : integer;
  MySize : TSize;
  MyString : string;

  function IsToLongText(AText : string) : boolean;
  begin
    MySize := GetTextSize(AText);
    with ARect do
      result := (Right - Left - 4 - 2 * SelectionAndBorderDists) <= MySize.cx;
  end;

begin
  result := AText;
  if FirstDayFormat = '' then
    exit;
  DateTimeToString(MyString, AFirstDayFormat, ADate);
  MyP := PSCPosIgnoreCase('mmmm', AFirstDayFormat);
  if (MyP <> 0) and IsToLongText(MyString) then
  begin
    Delete(AFirstDayFormat,MyP,1);
    MyString := GetFirstDayText(ADate, AText, ARect, AFirstDayFormat);
  end;
  result := MyString;
end;

{-------------------------------------}

function TPSCCustomCalendarPro2.IsFirstDayFormatStored: boolean;
begin
  result := FCalendarState.FirstDayFormat <> FirstDayFormat;
end;

{-------------------------------------}

procedure TPSCCustomCalendarPro2.SetFirstDayFormat(const AValue: string);
begin
  if FFirstDayFormat <> AValue then
  begin
    FFirstDayFormat := AValue;
    Changed;
  end;
end;

{-------------------------------------}

procedure TPSCCustomCalendarPro2.SetAlterEvenOdd(AValue: boolean);
begin
  if FAlterEvenOdd <> AValue then
  begin
    FAlterEvenOdd := AValue;
    Changed;
  end;
end;

{-------------------------------------}

function TPSCCustomCalendarPro2.GetArrowRect(
  Arrow: TPSCCalendarHitTest): TRect;
var
  MyArrowWidth: Integer;
begin
  if ArrowStyle = astMonthCalendar then
  begin
    with result do
    begin
      MyArrowWidth := PSCGetSystemMetrics(SM_CXHSCROLL) + 10;
      if Arrow = chtLeftArrow then
        Left := 0
      else
        Left := GetMonthWidth * MonthColCount - MyArrowWidth;
      Top := 0;
      Right := Left + MyArrowWidth;
      Bottom := Top + FHeaderHeight
    end;
    result := UpdateTopLeftCorner(Result);
  end
  else
     result := inherited GetArrowRect(Arrow);
end;

{-------------------------------------}

procedure TPSCCustomCalendarPro2.DrawFooterMonthCalendar(const ARect: TRect);
var
  MyImageRect : TRect;
  MySizeXY : TSize;
  MyS : string;
  MyBkMode : integer;
begin
  with Canvas do
  begin
    if ShowFooter then
    begin
      FillRect(ARect);
      MySizeXY := GetTextSize('11');
      MyS := PSCRemoveCharSet(['&'], PSCConsts.TodayButton) + ': ' +
        DateToStr(PSCDateOf(PSCNow));
      MyBkMode := GetBkMode(Canvas.Handle);
      SetBkMode(Handle, TRANSPARENT);
      with ARect do
      begin
        if TodayStyle = ctsHelix then
          MyImageRect := Classes.Rect(Left + 2, Top + 1, Left + MySizeXY.cx + 15, Bottom - 2)
        else
          MyImageRect := Classes.Rect(Left + 2, Top + 1, Left + 2, Bottom - 2);
        MySizeXY:=Canvas.TextExtent(MyS);
        Font.Style := [FontStyle_Bold];
        Canvas.TextRect(ARect,MyImageRect.Right + 5,
         (Top + Bottom - MySizeXY.cy) Div 2,MyS);
      end;
      if BorderStyle = bsSingle then
        with ARect do
        begin
          Pen.Width := 1;
          Pen.Color := clPSCGrayText;
          MoveTo(Left - 1, Top);
          LineTo(Left - 1, Bottom);
          Pen.Color := Colors.Days;
          MoveTo(Right, Top);
          LineTo(Right, Bottom);
        end;
      if TodayStyle = ctsHelix then
        DrawFooterHelix(Canvas, MyImageRect);
      SetBkMode(Handle, MyBkMode);
    end;
  end;
end;

{-------------------------------------}

function TPSCCustomCalendarPro2.IsDayVertAlignStored: boolean;
begin
  result := FCalendarState.DayVertAlign <> DayVertAlign;
end;

{-------------------------------------}

function TPSCCustomCalendarPro2.IsDayHorzAlignStored: boolean;
begin
  result := FCalendarState.DayHorzAlign <> DayHorzAlign;
end;

{-------------------------------------}

function TPSCCustomCalendarPro2.IsAlterEvenOddStored: boolean;
begin
  result := FCalendarState.AlterEvenOdd <> AlterEvenOdd;
end;

{-------------------------------------}

function TPSCCustomCalendarPro2.IsWeekDayCaseStored: boolean;
begin
  result := FCalendarState.WeekDayCase <> WeekDayCase;
end;

{-------------------------------------}

function TPSCCustomCalendarPro2.IsPastDaysAsGrayedStored: boolean;
begin
  result := FCalendarState.PastDaysAsGrayed <> PastDaysAsGrayed;
end;

{-------------------------------------}

function TPSCCustomCalendarPro2.IsShowFocusRectStored: boolean;
begin
  result := FCalendarState.ShowFocusRect <> ShowFocusRect;
end;

{-------------------------------------}

function TPSCCustomCalendarPro2.IsShowFooterStored: boolean;
begin
  result := FCalendarState.ShowFooter <> ShowFooter;
end;

{-------------------------------------}

function TPSCCustomCalendarPro2.IsShowHeaderStored: boolean;
begin
  result := FCalendarState.ShowHeader <> ShowHeader;
end;

{-------------------------------------}

function TPSCCustomCalendarPro2.IsWeekLineStyleStored: boolean;
begin
  result := FCalendarState.WeekLineStyle <> WeekLineStyle;
end;

{-------------------------------------}

function TPSCCustomCalendarPro2.IsSelectStyleStored: boolean;
begin
  result := FCalendarState.SelectStyle <> SelectStyle;
end;

{-------------------------------------}

function TPSCCustomCalendarPro2.IsWantEscStored: boolean;
begin
  result := FCalendarState.WantEsc <> WantEsc;
end;

{-------------------------------------}

procedure TPSCCustomCalendarPro2.SetShowDayDelimiters(AValue: boolean);
begin
  if FShowDayDelimiters <> AValue then
  begin
    FShowDayDelimiters := AValue;
    Changed;
  end;
end;

{-------------------------------------}

function TPSCCustomCalendarPro2.IsShowDayDelimitersStored: boolean;
begin
  result := FCalendarState.ShowDayDelimiters <> ShowDayDelimiters;
end;

{-------------------------------------}

constructor TPSCYearEditPopupForm.CreateNew(AOwner: TComponent; Dummy: Integer=0);
begin
  inherited;
  YearEdit := TPSCDateTimeUpDown.Create(Self);
  YearEdit.Kind:=cpkDate;
  YearEdit.DateTimeFormat.DateKind:=dfkShort;
  YearEdit.DateTimeFormat.ShortDateFormat:='yyyy';
end;

{-------------------------------------}

procedure TPSCCustomCalendarPro2.DrawFooterHelix(ACanvas: TPSCCanvas;
  ARect: TRect);
var
  MyOldPenColor : TPSCColor;
begin
  with ARect, ACanvas do
  begin
    MyOldPenColor := Pen.Color;
    Pen.Color := Colors.NowRect;
    Pen.Width := 2;
     MoveTo(Left, Top + 3);
     LineTo((Left + Right) div 2 + 2, Top + 3);
    Arc(Left + 1, Top + 3, Right, Bottom, Left,
     (Top + Bottom) div 2 + 5, (Right + Left) div 2 + 3, Top);
    PolyBezier([Point(Left + 1, (Top + Bottom) div 2 + 4), Point(Left + 3,
     (2 * Top + Bottom) div 3 + 4), Point(Left + 4, (3 * Top + Bottom) div 4 + 4),
     Point((Left + Right) div 2, (4 * Top + Bottom) div 5 + 4)]);
    Pen.Color := MyOldPenColor;
    Pen.Width := 1;
  end;
end;

{-------------------------------------}

function TPSCCustomCalendarPro2.IsDayHeaderStyleStored: boolean;
begin
  result := FCalendarState.DayHeaderStyle <> DayHeaderStyle;
end;

{-------------------------------------}

procedure TPSCCustomCalendarPro2.SetDayHeaderStyle(
  AValue: TPSCCalendarDayHeaderStyle);
begin
  if FDayHeaderStyle <> AValue then
  begin
    FDayHeaderStyle := AValue;
    Changed;
  end;
end;

{-------------------------------------}

procedure TPSCCustomCalendarPro2.DrawDividers(MonthPos: TPSCMonthPos;
  const Rect: TRect);
begin
  if ShowMonthDividers then
    begin
      with Canvas, Rect do
      begin
        Pen.Color := Colors.Border;
        if not (mpLeft in MonthPos) then
        begin
          MoveTo(Left, Top);
          LineTo(Left, Bottom);
        end;
        if not (mpRight in MonthPos) then
        begin
          MoveTo(Right - 1, Top);
          LineTo(Right - 1, Bottom);
        end;
      end;
    end;
end;

{-------------------------------------}

function TPSCCustomCalendarPro2.IsSelectionAndBorderDistsStored: Boolean;
begin
  result := FCalendarState.SelectionAndBorderDists <> SelectionAndBorderDists;
end;

{-------------------------------------}

procedure TPSCCustomCalendarPro2.SetSelectionAndBorderDists(
  AValue: integer);
begin
  if FSelectionAndBorderDists <> AValue then
  begin
    FSelectionAndBorderDists := AValue;
    Changed;
  end;
end;

{-------------------------------------}

procedure TPSCCustomCalendarPro2.UpdateDayRect(const ASourceRect: TRect;
  const AText: string; var ADestRect: TRect);
var
  MySize : TSize;
  MyDelta : integer;
begin
  with ASourceRect, Canvas do
  begin
    MyDelta := SelectionAndBorderDists;
    MySize := GetTextSize(AText);
    if ((Bottom - Top) - 2 * MySize.cy) > 0 then
    begin
      case DayVertAlign of
        vaTop: ADestRect := Rect(Left + MyDelta + 1, Top + MyDelta + 1,
         Right - MyDelta, Top + 3 + MySize.cy + MyDelta);
        vaCenter: ADestRect := Rect(Left + MyDelta + 1, (Bottom + Top - MySize.cy)
         div 2, Right - MyDelta,(Bottom + Top + MySize.cy) div 2 + 2);
        vaBottom: ADestRect := Rect(Left + MyDelta + 1, Bottom - 4 - MySize.cy -
        MyDelta, Right + MyDelta, Bottom - MyDelta);
      else
        ADestRect := Rect(Left + MyDelta + 1, (Bottom + Top - MySize.cy) div 2 - 2,
         Right + MyDelta,(Bottom + Top + MySize.cy) div 2 + 2);
      end;
    end
    else
      with ASourceRect do
      ADestRect := Rect(Left, Top, Right, Bottom);
  end;
end;

{-------------------------------------}

function TPSCCustomCalendarPro2.GetTextSize(AText: string): TSize;
begin
  result := Canvas.TextExtent(AText);
end;

{-------------------------------------}

function TPSCCustomCalendarPro2.GetMouseDate: TDateTime;
var
  MyCursorPos : TPoint;
  MyHitTest : TPSCCalendarHitTest;
  MyStartDate : TDateTime;
begin
  MyStartDate := GetMonthStartDate(0,0,true);
  GetCursorPos(MyCursorPos);
  MyCursorPos := ScreenToClient(MyCursorPos);
  MyHitTest := GetHitTest(MyCursorPos, result);
  if (not (MyHitTest in [chtDay,chtLeftGray,chtRightGray])) or (result > EndDate) or
   (result < MyStartDate) or (MyCursorPos.X < GetCalDateRect(MyStartDate).Left) or
   (MyCursorPos.X > GetCalDateRect(EndDate - 1).Right) then
    result := CursorDate;
end;

{-------------------------------------}

procedure TPSCCustomCalendarPro2.AlignDayName(const ARect:TRect;
  const ASize:TSize;var APoint:TPoint);
var
  MyY : integer;
  MyDelta : integer;
begin
  With ARect do
  begin
    if ShowDayDelimiters then
      MyDelta := 2
    else
      MyDelta := 0;  
    case DayHeaderHorzAlign of
      haLeft:
        APoint.X := Left + MyDelta;
      haCenter:
        APoint.X := Left + (Right - Left - ASize.cx) div 2;
      haRight:
        APoint.X := Left +(Right - Left) - ASize.cx - MyDelta;
    end;
    if (DayHeaderHorzAlign = DayHorzAlign)and (DayHorzAlign <> haCenter) then
    begin
      GetDayXYAligned(ARect, ASize, APoint.X, MyY);
      APoint.X := APoint.X + Left;
    end;
  end;
end;

{------------------------------------------------------------------}

type
  TPSCDateArrayItem=class(TInterfacedObject)
  private
    FDays:Array[1..12] of Array[1..31] of Cardinal;
  public
    function GetDateInfo(Month,Day:Word):Cardinal;
    procedure SetDateInfo(Month,Day:Word;Value:Cardinal);
  end;

  TPSCDateArray=class(TInterfacedObject,IPSCDateArray)
  private
    FYear:Word;
    FBeforeYear,FAfterYear:IPSCObjectList;
    FYearArrayItem:TPSCDateArrayItem;
    FStartAssignedDate,FEndAssignedDate:TDateTime;
    FAssignedCount:Integer;
    procedure UpdateAssignedStartEnd(const Date:TDateTime; Value:Cardinal);
    function GetStartAssignedDate:TDateTime;
    function GetEndAssignedDate:TDateTime;
    function GetBaseYear:Word;
    function GetAssignedCount:Integer;
  protected
    function GetDateArrayItem(Year:Word;CreateItem:boolean):TPSCDateArrayItem;virtual;
  public
    function GetDateInfo(const Date:TDateTime):Cardinal;virtual;
    procedure SetDateInfo(const Date:TDateTime; Value:Cardinal);virtual;
    constructor Create(BaseYear:Word);
    destructor Destroy;override;
    property DateInfo[const Date:TDateTime]:Cardinal Read GetDateInfo Write SetDateInfo;
    property StartAssignedDate:TDateTime Read GetStartAssignedDate;
    property EndAssignedDate:TDateTime Read GetEndAssignedDate;
    property BaseYear:Word Read GetBaseYear;
    property AssignedCount:Integer Read GetAssignedCount;
  end;

{------------------------------}

function PSCCreateDateArray:IPSCDateArray;
begin
  Result:=TPSCDateArray.Create(PSCGetDateYear(PSCNow));
end;

{------------------------------}

function TPSCDateArray.GetStartAssignedDate:TDateTime;
begin
  Result:=FStartAssignedDate;
end;

{------------------------------}

function TPSCDateArray.GetEndAssignedDate:TDateTime;
begin
  Result:=FEndAssignedDate;
end;

{------------------------------}

function TPSCDateArray.GetBaseYear:Word;
begin
  Result:=FYear;
end;

{------------------------------}

function TPSCDateArray.GetAssignedCount:Integer;
begin
  Result:=FAssignedCount;
end;

{------------------------------}

constructor TPSCDateArray.Create(BaseYear:Word);
begin
  inherited Create;
  FYear:=BaseYear;
  FBeforeYear:=PSCCreateObjectList(ioOwned);
  FAfterYear:=PSCCreateObjectList(ioOwned);
  FYearArrayItem:=TPSCDateArrayItem.Create;
end;

{------------------------------}

destructor TPSCDateArray.Destroy;
begin
  FYearArrayItem.Free;
  inherited;
end;

{------------------------------}

function TPSCDateArray.GetDateInfo(const Date:TDateTime):Cardinal;
var
  Item:TPSCDateArrayItem;
  Year,Month,Day:Word;
begin
  PSCDecodeDate(Date,Year,Month,Day);
  Item:=GetDateArrayItem(Year,False);
  If Item<>nil then
    Result:=Item.GetDateInfo(Month,Day)
  else
    Result:=0;
end;

{------------------------------}

procedure TPSCDateArray.UpdateAssignedStartEnd(const Date:TDateTime; Value:Cardinal);

  procedure ValueNotNil;
  begin
    if FStartAssignedDate=0 then
      FStartAssignedDate:=Date
    else
      FStartAssignedDate:=PSCMin(FStartAssignedDate,Date);

    If FEndAssignedDate=0 then
      FEndAssignedDate:=Date
    else
      FEndAssignedDate:=PSCMax(FStartAssignedDate,Date);
  end;

  procedure ValueIsNil;

    procedure SearchSmallerEnd;
    var
      DateIterator:TDateTime;
    begin
      DateIterator:=PSCDateOf(FEndAssignedDate);
      While DateIterator>=PSCDateOf(FStartAssignedDate) do
      begin
        If DateInfo[DateIterator]<>0 then
        begin
          FEndAssignedDate:=DateIterator;
          exit;
        end;
        DateIterator:=DateIterator-1;
      end;
    end;

    procedure SearchBiggerStart;
    var
      DateIterator:TDateTime;
    begin
      DateIterator:=PSCDateOf(FStartAssignedDate);
      While DateIterator<=PSCDateOf(FEndAssignedDate) do
      begin
        If DateInfo[DateIterator]<>0 then
        begin
          FStartAssignedDate:=DateIterator;
          exit;
        end;
        DateIterator:=DateIterator+1;
      end;
    end;

  begin
    If (FStartAssignedDate=FEndAssignedDate) and (Date=FEndAssignedDate) then
    begin
      FStartAssignedDate:=0;
      FEndAssignedDate:=0;
      exit;
    end;

    If FStartAssignedDate=Date then
      SearchBiggerStart
    else
      if FEndAssignedDate=Date then
        SearchSmallerEnd;
  end;

begin
  If Value<>0 then
    ValueNotNil
  else
    ValueIsNil;
end;
{------------------------------}

procedure TPSCDateArray.SetDateInfo(const Date:TDateTime; Value:Cardinal);
var
  Item:TPSCDateArrayItem;
  Year,Month,Day:Word;
begin
  If GetDateInfo(Date)<>Value then
  begin
    PSCDecodeDate(Date,Year,Month,Day);
    Item:=GetDateArrayItem(Year,True);
    Item.SetDateInfo(Month,Day,Value);
    UpdateAssignedStartEnd(Date,Value);
    If Value<>0 then
      inc(FAssignedCount)
    else
      dec(FAssignedCount);
  end;
end;

{------------------------------}

function TPSCDateArray.GetDateArrayItem(Year:Word;CreateItem:boolean):TPSCDateArrayItem;
var
  FDeltaYear:Integer;

  function GetItemFromList(const List:IPSCObjectList;
    Index:Integer):TPSCDateArrayItem;
  begin
    If CreateItem then
    begin
      While List.Count<=Index do
        List.Add(nil);
      If List[Index]=nil then
        List[Index]:=TPSCDateArrayItem.Create;
    end;
    If Index<List.Count then
      Result:=TPSCDateArrayItem(List[Index])
    else
      Result:=nil;
  end;

begin
  FDeltaYear:=Year-FYear;
  If FDeltaYear=0 then
    Result:=FYearArrayItem
  else
    If FDeltaYear<0 then
      Result:=GetItemFromList(FBeforeYear,Abs(FDeltaYear)-1)
    else
      Result:=GetItemFromList(FAfterYear,FDeltaYear-1);
end;

{------------------------------}

function TPSCDateArrayItem.GetDateInfo(Month,Day:Word):Cardinal;
begin
  Result:=FDays[Month][Day];
end;

{------------------------------}

procedure TPSCDateArrayItem.SetDateInfo(Month,Day:Word;Value:Cardinal);
begin
  FDays[Month][Day]:=Value;
end;

{-------------------------------------------------------------------------}

function PSCDateToStrY2k(const Date:TDateTime):String;
begin
  DateTimeToString(Result, PSCYearFormatToYear2k(PSCShortDateFormat), Date);
end;

{------------------------------}

function PSCGetCalendarFilterStrEx(const SelStart,SelFinish:TDateTime;
  const DateField:String;AParams:PPSCGetCalendarFilterStrParams):String;
Var
  I        : TDateTime;
  LastDate : TDateTime;
  InPeriod : Boolean;

  function _And:String;
  begin
    if (AParams=nil) or (AParams.AndStr='') then
      Result:=SPSCOperationFltAnd
    else
      Result:=AParams.AndStr;
  end;

  function _Or:String;
  begin
    if (AParams=nil) or (AParams.OrStr='') then
      Result:=SPSCOperationFltOr
    else
      Result:=AParams.OrStr;
  end;

  function _DateToStr(const ADate:TDateTime):String;
  begin
    Result:=PSCDateToStrY2k(ADate);
  end;

  Function GetPeriod : String;
  begin
    Result := '';
    If InPeriod Then
    begin
      If PSCDateOf( I ) - PSCDateOf( LastDate ) = 1 Then
        Result := ' '+_Or+' (' + DateField + '=''' +
          _DateToStr( PSCIncDay( I, -1 ) ) + '''' + ')'
      Else
        Result := ' '+_Or+' (('#39 + _DateToStr( LastDate ) + #39'<=' +
          DateField + ') '+_And+' (' + DateField + '<'#39 +
          _DateToStr( I ) + #39'))';
      InPeriod := False;
    end;
  end;

begin
  I := SelStart;
  InPeriod := False;
  Result := '';
  While I <= SelFinish Do
  begin
    If Not (((AParams=nil) or not Assigned(AParams.DateSelectedProc)) or AParams.DateSelectedProc(i)) Then
      Result := Result + GetPeriod
    Else
      If Not InPeriod Then
      begin
        InPeriod := True;
        LastDate := I;
      end;
    I := PSCIncDay( I, 1 );
  end;
  Result := Result + GetPeriod;
  Delete( Result, 1, 4 );
end;

{-------------------------------------------}

function PSCIncDayEx(Const Date: TDateTime; NumberOfDays:Integer;
  ExcludeEvent:TPSCDateSelectedProc): TDateTime;
begin
  If not Assigned(ExcludeEvent) then
  begin
    Result:=PSCIncDay(Date,NumberOfDays);
    exit;
  end;

  Result:=Date;
  While NumberofDays>0 do
  begin
    Result:=Result+1;
    if not ExcludeEvent(Result) then
      dec(NumberofDays);
  end;
  While NumberOfDays<0 do
  begin
    Result:=Result-1;
    if not ExcludeEvent(Result) then
      inc(NumberOfDays);
  end;
end;

{-------------------------------------------}

function PSCWorkDaysToWeekEnds(WorkDays:TPSCWeekDays):TPSCWeekDays;
begin
  Result:=cPSCFullWeek-WorkDays;
end;

{-------------------------------------------}

function PSCWeekEndsToWorkDays(WeekEnds:TPSCWeekDays):TPSCWeekDays;
begin
  Result:=cPSCFullWeek-WeekEnds;
end;

{-------------------------------------------}

function PSCDateToDay (const DT: TDateTime): Word;
var
  M, Y : Word;
begin
  PSCDecodeDate (DT, Y, M, Result);
end;

{----------------------------------------}

Procedure PSCDrawOutlookArrow(Canvas: TPSCCanvas; LeftArrow: Boolean;
  Const Rect: TRect);
Const
  ArrowKind: Array[Boolean] Of Cardinal = (PSC_DFCS_SCROLLRIGHT,PSC_DFCS_SCROLLLEFT);
Var
  Bitmap: TPSCBitmap;
Begin
  Bitmap := TPSCBitmap.Create;
  Try
    With Bitmap Do
      Begin
        Transparent := true;
        TransparentColor := clPSCBtnFace;

        Width := Rect.Right - Rect.Left - 2;
        Height := Rect.Bottom - Rect.Top - 2;
        DrawFrameControl(Canvas.Handle,Classes.Rect(-1, -1,Width + 1,Height + 1),
          PSC_DFC_SCROLL,ArrowKind[LeftArrow] Or PSC_DFCS_ADJUSTRECT Or PSC_DFCS_FLAT Or
          PSC_DFCS_TRANSPARENT)
      End;
    Canvas.Draw(Rect.Left + 1,Rect.Top + 1,Bitmap)
  Finally
    Bitmap.Free;
  End
End;

{------------------------------------------------------------------------------}

Constructor TPSCCalendarColors.Create(AOwner: TPSCCustomCalendar);
Begin
  Inherited Create(AOwner);
  FWeekNumbersFont := TPSCParentedFont.Create(AOwner);
  FWeekDaysFont := TPSCParentedFont.Create(AOwner);
  FHeaderFont := TPSCParentedFont.Create(AOwner);
  FDaysFont := TPSCParentedFont.Create(AOwner);
  With DaysFont Do
  Begin
    Assign(TPSCCustomCalendarPro(Owner).Font);
    OnChange := FontChanged;
  End;
  With FWeekNumbersFont Do
    Begin
      Assign(AOwner.Font);
      {Height := Height * 3 Div 4;}
      OnChange := FontChanged;
    End;
  With FWeekDaysFont Do
    Begin
      Assign(AOwner.Font);
      OnChange := FontChanged;
    End;
  With FHeaderFont Do
    Begin
      Assign(AOwner.Font);
      OnChange := FontChanged;
    End;
  FBorder := clBorderColor;
  FMonthHeader := clMonthHeaderColor;
  FWeekHeader := AOwner.Color;
  FDays := AOwner.Color;
  FDayLines := clDayOutlineColor;
  FSelected := clSelectedColor;
  FSelectedText := clSelectedTextColor;
  FGrayed := clGrayedColor;
  FWeekEndText := clWeekEndTextColor;
  FNowRect := clNowRectColor;
  FWeekSide := AOwner.Color;
  FDayLinesStyle := psDayOutlineStyle
End;

{------------------------------------------------------------------------------}

Destructor TPSCCalendarColors.Destroy;
Begin
  FHeaderFont.Free;
  FWeekNumbersFont.Free;
  FWeekDaysFont.Free;
  Inherited Destroy
End;

{------------------------------------------------------------------------------}

Procedure TPSCCalendarColors.Assign(Source: TPersistent);
Begin
  inherited;
  If Owner<>nil then
  begin
    Owner.BorderChanged;
    Owner.Invalidate;
  end;
End;

{------------------------------------------------------------------------------}

Function TPSCCalendarColors.GetColor(Index: Integer): TPSCColor;
Begin
  Case Index Of
    CPSCCalColorIndex_Border        : Result := FBorder;
    CPSCCalColorIndex_MonthHeader   : Result := FMonthHeader;
    CPSCCalColorIndex_WeekHeader    : Result := FWeekHeader;
    CPSCCalColorIndex_Days          : Result := FDays;
    CPSCCalColorIndex_Selected      : Result := FSelected;
    CPSCCalColorIndex_SelectedText  : Result := FSelectedText;
    CPSCCalColorIndex_Grayed        : Result := FGrayed;
    CPSCCalColorIndex_WeekEndText   : Result := FWeekEndText;
    CPSCCalColorIndex_NowRect       : Result := FNowRect;
    CPSCCalColorIndex_WeekSide      : Result := FWeekSide;
    CPSCCalColorIndex_DayLines      : Result := FDayLines;
  else
    Result:=clPSCBtnFace;
  End;
End;

{------------------------------------------------------------------------------}

Procedure TPSCCalendarColors.SetColor(Index: Integer; Value: TPSCColor);
Begin
  Case Index Of
    CPSCCalColorIndex_Border:
      Begin
        FBorder := Value;

        If Owner<>nil then
        begin
          Owner.ParentColor := false;
          Owner.BorderChanged;
        end;
        Exit;
      End;
    CPSCCalColorIndex_MonthHeader   : FMonthHeader := Value;
    CPSCCalColorIndex_WeekHeader    : FWeekHeader := Value;
    CPSCCalColorIndex_Days          : FDays := Value;
    CPSCCalColorIndex_Selected      : FSelected := Value;
    CPSCCalColorIndex_SelectedText  : FSelectedText := Value;
    CPSCCalColorIndex_Grayed        : FGrayed := Value;
    CPSCCalColorIndex_WeekEndText   : FWeekEndText := Value;
    CPSCCalColorIndex_NowRect       : FNowRect := Value;
    CPSCCalColorIndex_WeekSide      : FWeekSide := Value;
    CPSCCalColorIndex_DayLines      : FDayLines := Value
  End;
  Changed;
End;

{------------------------------------------------------------------------------}

Procedure TPSCCalendarColors.SetWeekNumbersFont(Value: TPSCFont);
Begin
  If FWeekNumbersFont <> Value Then
    FWeekNumbersFont.Assign(Value)
End;

{------------------------------------------------------------------------------}

Procedure TPSCCalendarColors.SetWeekDaysFont(Value: TPSCFont);
Begin
  If FWeekDaysFont <> Value Then
    FWeekDaysFont.Assign(Value)
End;

{------------------------------------------------------------------------------}

Procedure TPSCCalendarColors.SetHeaderFont(Value: TPSCFont);
Begin
  If FHeaderFont <> Value Then
    FHeaderFont.Assign(Value)
End;

{------------------------------------------------------------------------------}

Function TPSCCalendarColors.GetOwnerAsCalendar:TPSCCustomCalendar;
begin
  Result:=TPSCCustomCalendar(GetOwner);
end;

{------------------------------------------------------------------------------}

Function TPSCCalendarColors.IsWeekNumbersFontStored:Boolean;
begin
  Result:=TPSCParentedFont(WeekNumbersFont).IsFontStored;
end;

{------------------------------------------------------------------------------}

Function TPSCCalendarColors.IsWeekDaysFontStored:Boolean;
begin
  Result:=TPSCParentedFont(WeekDaysFont).IsFontStored;
end;

{------------------------------------------------------------------------------}

Function TPSCCalendarColors.IsHeaderFontStored:Boolean;
begin
  Result:=TPSCParentedFont(HeaderFont).IsFontStored;
end;

{------------------------------------------------------------------------------}

Function TPSCCalendarColors.IsColorStored(Index: Integer): Boolean;
Begin
  If Owner=nil then
  begin
    Result:=True;
    exit;
  end;
  Case Index Of
    0: Result := Not Owner.ParentColor;
    3: Result := FWeekHeader <> Owner.Color;
    4: Result := FDays <> Owner.Color;
    10: Result := WeekSide <> Owner.Color;
  Else
    Result := false;
  End
End;

{-------------------------------------}

procedure TPSCCalendarColors.Changed;
begin
  If Owner<>nil then
    Owner.Invalidate;
end;

{------------------------------------------------------------------------------}

Procedure TPSCCalendarColors.FontChanged(Sender: TObject);
Begin
  If Owner<>nil then
    Owner.FontChanged;
End;

{------------------------------------------------------------------------------}

Procedure TPSCCalendarColors.SetDayLinesStyle(Value: TPenStyle);
Begin
  If FDayLinesStyle <> Value Then
    Begin
      FDayLinesStyle := Value;
      Changed;
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.SetVersion(const AValue:String);
begin
end;

{------------------------------------------------------------------------------}

Constructor TPSCCustomCalendar.Create(AOwner: TComponent);
Begin
  FUserData:=PSCCreateDateArray;
  FVersion:=SPSCProductVerNo;
  FNavButtonsSpeed:=cArrowClickTime;
  Inherited Create(AOwner);
  ParentShowHint := False;
  ShowHint := True;
  FNavButtonsTimer := cPSCDefNavButtonsTimer;

  FFlat:=True;
  FPrintOptions := TPSCCalendarPrintOptions.Create(Self);

  ControlStyle := ControlStyle + [csOpaque,csReflector];
  FColors := CreateCalendarColors;
  FSelDates := TList.Create;
  FMultiSelect := true;
  StartDate := PSCDateOf(PSCNow);
  FSelectKind := skProgressive;

  FMinDate := 0;
  FMaxDate := 0;
  CursorDate := PSCDateOf(PSCNow);
  FMaxSelDates := 42;
  FShowMonthPopup := true;
  FShowNavButtons := true;
  FShowToday := true;
  FSideWeekSelect := true;
  FWorkDays := [dwMonday..dwFriday];
  Color := clCalendarColor;
  ParentColor := false;
  FWeekCursor := crDefault;
  Flat := true;
  FBorderStyle := cPSCDefCalendarBorderStyle;
  FBorderEdges := [beLeft,beTop,beRight,beBottom];
  FExtendedSelect := true;
  Width := 170;
  Height := 170;
  TabStop := True;
  FFirstWeekOfYear := fwJanFirst;
End;

{------------------------------------------------------------------------------}

Type
  PMonthSelection = ^TMonthSelection;
  TMonthSelection = Record
    Base: TDateTime;
    Dates: UINT;
  End;

Destructor TPSCCustomCalendar.Destroy;
Begin
  FPrintOptions.Free;
  FPrintOptions := Nil;
  Colors.Free;
  FSelCount := 0;
  While FSelDates.Count > 0 Do
    Begin
      Dispose(PMonthSelection(FSelDates[FSelDates.Count - 1]));
      FSelDates.Delete(FSelDates.Count - 1)
    End;
  FSelDates.Free;
  Inherited Destroy
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.BeginUpdate;
Begin
  FUpdateCount := FUpdateCount + 1;
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.EndUpdate;
Begin
  FUpdateCount := FUpdateCount - 1;
  UpdateSelection;
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.UpdateSelection;
Begin
  If (FUpdateCount = 0) And FModified Then
    Begin
      FModified := false;
      DateChanged
    End
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendar.CreateCalendarColors: TPSCCalendarColors;
Begin
  Result := TPSCCalendarColors.Create(Self)
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendar.ClearSelection: Boolean;
Var
  Rect: TRect;
  OldCount: Integer;
Begin
  Result := true;
  If HandleAllocated Then
    ClearWeekSelection;
  If FMultiSelect Then
    Begin
      If FSelCount > 0 Then
        Begin
          BeginUpdate;
          Try
            Repeat
              If HandleAllocated And DateToRect(FSelStart,Rect) Then
                InvalidateRect(Rect);
              OldCount := FSelCount;
              SelDate[FSelStart] := false;
              If OldCount <= FSelCount Then
                Begin
                  Result := false;
                  Break
                End
            Until FSelCount = 0
          Finally
            EndUpdate
          End
        End
    End
  Else
    If FSelCount > 0 Then
      If Not CanSelectionChange(FCursorDate) Then
        Result := false
      Else
        Begin
          InvalidateSelection;
          FSelStart := 0;
          FSelFinish := 0;
          FSelCount := 0;
          FModified := true;
          UpdateSelection
        End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.ClearWeekSelection;
Begin
  If FWeeksSelected Then
    Begin
      FWeeksSelected := false;
      InvalidateWeekSelection
    End
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendar.StartDateStored: Boolean;
Begin
  Result := PSCDateOf(StartDate) <> CorrectStartDate(PSCDateOf(PSCNow));
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.SetMultiSelect(Value: Boolean);
Var
  Date: TDateTime;
  Selected: Boolean;
Begin
  If FMultiSelect <> Value Then
    Begin
      Selected := FSelCount > 0;
      Date := FSelStart;
      If Not Selected Or ClearSelection Then
        Begin
          FMultiSelect := Value;
          If Selected Then
            CursorDate := Date
        End
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.ChangeSelection(Const ASelStart,ASelFinish:
  TDateTime);
Begin
  BeginUpdate;
  Try
    ClearSelection;
    SelStart := ASelStart;
    SelFinish := ASelFinish
  Finally
    EndUpdate
  End
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendar.SelectDay(Const Date: TDateTime;
  ClearSelection: Boolean): Boolean;
Begin
  BeginUpdate;
  Try
    If ClearSelection Then
      Self.ClearSelection;
    Result := SetSelDate(Date,true)
  Finally
    EndUpdate
  End
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendar.SelectWeek(Date: TDateTime;
  ClearSelection: Boolean): Boolean;
Var
  I: Integer;
  Value: Boolean;
Begin
  Result := false;
  If Not FMultiSelect Then
    Exit;
  BeginUpdate;
  Try
    If ClearSelection Then
      Self.ClearSelection;
    Date := GetBeginOfWeek(Date);
    If Not FExtendedSelect And (FSelCount > 0) And
      Not ((Date <= FSelStart) And (Date + 7 >= FSelStart) Or
      (Date - 1 <= FSelFinish) And (Date + 6 >= FSelFinish)) Then
      Exit;
    FDragDate := Date;
    FCursorDate := Date + 6;
    Value := Not CheckWeekSelected(Date);
    Result := true;
    For I := 0 To 6 Do
      If Not SetSelDate(Date + I,Value) Then
        Result := false;
    If FSelCount = 7 Then
      FWeeksSelected := true;
    InvalidateWeek(Date)
  Finally
    EndUpdate
  End;
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendar.SelectMonth(Date: TDateTime;
  ClearSelection: Boolean): Boolean;
Var
  Week: Integer;
  Finish: TDateTime;
Begin
  BeginUpdate;
  Try
    Result := false;
    If Not FMultiSelect Then
      Exit;
    If ClearSelection Then
      Self.ClearSelection;
    Finish := GetEndOfWeek(PSCGetMonthEnd(Date));
    Date := GetBeginOfWeek(PSCGetMonthStart(Date));
    If Not FExtendedSelect And (FSelCount > 0) And
      ((FSelStart > Finish) Or (FSelFinish < Date)) Then
      Exit;
    FDragDate := Finish;
    FCursorDate := Date;
    Result := true;
    Week := 0;
    BeginUpdate;
    Try
      Repeat
        If Week < 6 Then
          Week := Week + 1
        Else
          Begin
            Week := 0;
            InvalidateWeek(Date)
          End;
        If Not SetSelDate(Date,true) Then
          Result := false;
        Date := Date + 1
      Until Date > Finish
    Finally
      EndUpdate
    End;
    If Not SelDate[FCursorDate] Then
      FCursorDate := FSelStart;
    If Not SelDate[FDragDate] Then
      FDragDate := FSelFinish
  Finally
    EndUpdate;
  End;
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendar.CheckWeekSelected(Date: TDateTime): Boolean;
Var
  I: Integer;
Begin
  Date := GetBeginOfWeek(Date);
  Result := false;
  If (Date >= FSelStart) And (Date + 6 <= FSelFinish) Then
    Begin
      If FMultiSelect Then
        For I := 0 To 6 Do
          If Not SelDate[Date + I] Then
            Exit;
      Result := true
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.InvalidateWeek(Const Date: TDateTime);
Var
  Rect: TRect;
Begin
  If not HandleAllocated then
    exit;
  If Not FShowWeekNumbers Then
    Begin
      If GetWeekRect(Date,chtWeekLeft,Rect) Then
        InvalidateRect(Rect);
      If GetWeekRect(Date,chtWeekRight,Rect) Then
        InvalidateRect(Rect)
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.InvalidateWeekSelection;
Var
  BeginDate,EndDate: TDateTime;
Begin
  BeginDate := GetMonthStartDate(0,0,true);
  EndDate := GetMonthStartDate(MonthColCount - 1,MonthRowCount - 1,true) + (7 *
    6);
  While BeginDate < EndDate Do
    Begin
      InvalidateWeek(BeginDate);
      BeginDate := BeginDate + 7
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.UpdateWeekSelection(NeedIvalidate: Boolean);
Var
  Date: TDateTime;
  OldWeeksSel: Boolean;
Begin
  Date := FSelStart;
  OldWeeksSel := FWeeksSelected;
  FWeeksSelected := true;
  While Date <= FSelFinish Do
    If CheckWeekSelected(Date) Then
      Date := Date + 7
    Else
      Begin
        FWeeksSelected := false;
        Break
      End;
  If NeedIvalidate Or (FWeeksSelected <> OldWeeksSel) Then
    InvalidateWeekSelection
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.InvalidateArrow(Arrow: TPSCCalendarHitTest);
Var
  R: TRect;
Begin
  If FShowNavButtons And HandleAllocated Then
    Begin
      R := GetArrowRect(Arrow);
      InvalidateRect(R);
    End
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendar.GetArrowRect(Arrow: TPSCCalendarHitTest): TRect;
Var
  ArrowWidth: Integer;
Begin
  With Result Do
    Begin
      ArrowWidth := FHeaderHeight * 3 Div 2;
      If Arrow = chtLeftArrow Then
        Left := 0
      Else
        Left := GetMonthWidth * MonthColCount - ArrowWidth;
      Top := 0;
      Right := Left + ArrowWidth;
      Bottom := Top + FHeaderHeight
    End;
  Result := UpdateTopLeftCorner(Result);
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendar.GetMonthStartDate(MonthCol,MonthRow: Integer;
  DecFirstWeek: Boolean): TDateTime;
Var
  Date: TDateTime;
Begin
  Result := PSCIncMonth(FStartDate,MonthCol + MonthRow * MonthColCount);
  If DecFirstWeek Then
    Begin
      Date := Result;
      Result := GetBeginOfWeek(Result);
      If (Date = Result) And (Date = FStartDate) Then
        Result := Result - 7
    End
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendar.GetHitTest(X,Y: Integer;
  Var Date: TDateTime): TPSCCalendarHitTest;
begin
  Result:=GetHitTest(Point(X,Y),Date);
end;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendar.GetHitTest(P: TPoint;
  Var Date: TDateTime): TPSCCalendarHitTest;
Const
  WeekSide: Array[Boolean] Of TPSCCalendarHitTest = (chtWeekRight,chtWeekLeft);
Var
  X,Y,xa,ya: Integer;
  MonthWidth,MonthHeight: Integer;
  BeginDate,BeginMonth,EndMonth: TDateTime;
  R: TRect;
Begin
  R := UpdateTopLeftCorner(Rect(0,0,ClientWidth,ClientHeight));
  If Not PtInRect(R,P) Then
    Begin
      Result := chtNowhere;
      Date := FCursorDate;
      Exit
    End;
  X := P.X - R.Left;
  Y := P.Y - R.Top;
  MonthWidth := GetMonthWidth;
  MonthHeight := GetMonthHeight;
  xa := X Div MonthWidth;
  ya := Y Div MonthHeight;
  X := X - xa * MonthWidth;
  Y := Y - ya * MonthHeight - FHeaderHeight;
  If Y < 0 Then
    Begin
      Result := chtMonthHead;
      Date := GetMonthStartDate(xa,ya,false);
      If FShowNavButtons Then
        If PtInRect(GetArrowRect(chtLeftArrow),P) Then
          Result := chtLeftArrow
        Else
          If PtInRect(GetArrowRect(chtRightArrow),P) Then
            Result := chtRightArrow
    End
  Else
    Begin
      Y := Y - FDaysOfWeekHeight - 2;
      If Y < 0 Then
        Begin
          Result := chtWeekDays;
          Date := GetMonthStartDate(xa,ya,false)
        End
      Else
        Begin
          Y := Y Div FRowHeight;
          X := X - FSideWidth;
          If FShowWeekNumbers Then
            X := X - 4;
          If (X < 0) Or (X >= FColWidth * 7) Then
            Begin
              Result := WeekSide[X < 0];
              Date := GetMonthStartDate(xa,ya,true) + Y * 7
            End
          Else
            Begin
              X := X Div FColWidth;
              BeginMonth := GetMonthStartDate(xa,ya,false);
              BeginDate := GetMonthStartDate(xa,ya,true);
              EndMonth := PSCGetMonthEnd(BeginMonth);
              Date := BeginDate + Y * 7 + X;
              If Date < BeginMonth Then
                If (xa = 0) And (ya = 0) Then
                  Result := chtLeftGray
                Else
                  Result := chtHidden
              Else
                If Date > EndMonth Then
                  If (xa = MonthColCount - 1) And (ya = MonthRowCount - 1) Then
                    Result := chtRightGray
                  Else
                    Result := chtHidden
                Else
                  Result := chtDay
            End
        End
    End
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendar.DateToPos(Const Date: TDateTime;
  Var MonthCol,MonthRow: Integer): Boolean;
Var
  Year,Month,Day,Year1,Month1: Word;
  Distance: Integer;
  Col,Row: Integer;
Begin
  Result := false;
  PSCDecodeDate(FStartDate,Year,Month,Day);
  PSCDecodeDate(Date,Year1,Month1,Day);
  Distance := (Year1 - Year) * 12 + (Month1 - Month);
  Col := MonthColCount;
  Row := MonthRowCount;
  If (Distance >= -1) Or (Distance <= Col * Row) Then
    Begin
      If Distance = -1 Then
        If Date >= GetMonthStartDate(0,0,true) Then
          Distance := 0
        Else
          Exit
      Else
        If Distance = Col * Row Then
          If Date < GetMonthStartDate(Col - 1,Row - 1,true) + (7 * 6) Then
            Distance := Col * Row - 1
          Else
            Exit;
      MonthRow := Distance Div Col;
      MonthCol := Distance Mod Col;
      Result := true
    End
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendar.DateToRect(Const ADate: TDateTime;
  Var ALeft, ATop, ARight, ABottom: Integer): Boolean;
var
  MyRect:TRect;
begin
  Result:=DateToRect(ADate,MyRect);
  With MyRect do
  begin
    ALeft   := Left  ;
    ATop    := Top   ;
    ARight  := Right ;
    ABottom := Bottom;
  end;
end;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendar.DateToRect(Const Date: TDateTime;
  Var Rect: TRect): Boolean;
Var
  MRow,MCol,DRow,DCol: Integer;
  Day: Word;
Begin
  If Not DateToPos(Date,MCol,MRow) Then
    Result := false
  Else
    Begin
      Result := true;
      Day := Trunc(Date - GetMonthStartDate(MCol,MRow,true));
      DRow := Day Div 7;
      DCol := Day Mod 7;
      With Rect Do
        Begin
          Left := MCol * GetMonthWidth + FSideWidth + DCol * FColWidth;
          If FShowWeekNumbers Then
            Left := Left + 4;
          Top := MRow * GetMonthHeight + FDaysOfWeekHeight + 2 + FHeaderHeight +
            DRow * FRowHeight;
          Right := Left + FColWidth;
          Bottom := Top + FRowHeight
        End;
      Rect := UpdateTopLeftCorner(Rect);
    End
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendar.GetWeekRect(Date: TDateTime;
  Side: TPSCCalendarHitTest; Var Rect: TRect): Boolean;
Begin
  Date := GetBeginOfWeek(Date);
  Case Side Of
    chtWeekLeft:
      Begin
        Result := DateToRect(Date,Rect);
        OffsetRect(Rect, -FSideWidth,0)
      End;
    chtWeekRight:
      Begin
        Result := DateToRect(Date + 6,Rect);
        OffsetRect(Rect,FSideWidth,0)
      End
  Else
    Result := false
  End;
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendar.CanSelectionChange(Const Date: TDateTime): WordBool;
Begin
  Result := true;
  If Assigned(FOnCanSelectDate) Then
    FOnCanSelectDate(Self,Date,Result)
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendar.GetBeginOfWeek(Const Date: TDateTime): TDateTime;
Begin
  Result := PSCGetWeekStartEx(Date,FFirstDayOfWeek)
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendar.GetEndOfWeek(Const Date: TDateTime): TDateTime;
Begin
  Result := PSCGetWeekEndEx(Date,FFirstDayOfWeek)
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendar.GetWeekNumber(Const Date: TDateTime): Integer;
Begin
  Result := PSCGetWeekNumber(Date,FFirstDayOfWeek,FFirstWeekOfYear)
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.KillTimer;
Begin
  PSCKillTimer(FTimer);
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendar.StartFromNextMonth: Boolean;
Var
  OldStartDate: TDateTime;
Begin
  OldStartDate := FStartDate;
  StartDate := PSCIncMonth(FStartDate,1);
  Result := FStartDate <> OldStartDate
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendar.StartFromPrevMonth: Boolean;
Var
  OldStartDate: TDateTime;
Begin
  OldStartDate := FStartDate;
  StartDate := PSCIncMonth(FStartDate, -1);
  Result := FStartDate <> OldStartDate
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendar.StartFromNextYear: Boolean;
Var
  OldStartDate: TDateTime;
Begin
  OldStartDate := FStartDate;
  StartDate := PSCIncMonth(FStartDate,12);
  Result := FStartDate <> OldStartDate
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendar.StartFromPrevYear: Boolean;
Var
  OldStartDate: TDateTime;
Begin
  OldStartDate := FStartDate;
  StartDate := PSCIncMonth(FStartDate, -12);
  Result := FStartDate <> OldStartDate
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.TimerEvent(Timer: TObject; EventID: Integer);
Var
  Shift: TShiftState;
  Value: Boolean;
  HitTest: TPSCCalendarHitTest;
  P: TPoint;
  Date: TDateTime;
Begin
  Shift := FShiftState;
  GetCursorPos(P);
  HitTest := GetHitTest(ScreenToClient(P),Date);
  Case EventID Of
    1:
      If HitTest = chtLeftArrow Then
        Begin
          If ssCtrl In Shift Then
            Value := StartFromPrevYear
          Else
            Value := StartFromPrevMonth;
          If Not Value Then
            KillTimer;
        End;
    2:
      If HitTest = chtRightArrow Then
        Begin
          If ssCtrl In Shift Then
            Value := StartFromNextYear
          Else
            Value := StartFromNextMonth;
          If Not Value Then
            KillTimer;
        End
  End
End;

{-------------------------------------------}

Type
  TPSCPopupMonthListBox = Class(TPSCPopupTListBox)
  private
    Procedure ListBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X,Y: Integer);
  protected
    Procedure Activate; override;
    Function GetListBoxClass: TListBoxClass; override;
    Procedure CreateParams(Var Params: TCreateParams); override;
  public
    Procedure ResizeFormControls; override;
    constructor CreateNew(AOwner: TComponent; Dummy: Integer=0); override;
  End;

  TPSCMonthListBox = Class(TListBox)
  private
    FTimer: TObject;
    FTimerId: Integer;
    FSelDate: TDateTime;
    Procedure SetSelDate(Value: TDateTime);
    Procedure KillTimer;
    Function GetItemDate: TDateTime;

    Procedure TimerEvent(Timer: TObject; EventID: Integer);
  protected
    Procedure CNDrawItem(Var Message: TWMDrawItem); message cn_DrawItem;
    Procedure DrawItem(Index: Integer; Rect: TRect;
      State: TOwnerDrawState); override;
    Procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    Procedure KeyDown(Var Key: Word; Shift: TShiftState); override;

  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    Property SelDate: TDateTime read FSelDate write SetSelDate;
    Property ItemDate: TDateTime read GetItemDate;
  End;

Const
  cPSCNumberOfMonth = 7; {number of month in a popup month listbox}

{------------------------------------------------------------------------------}

Destructor TPSCMonthListBox.Destroy;
Begin
  KillTimer;
  Inherited;
End;

{------------------------------------------------------------------------------}

Constructor TPSCMonthListBox.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);
  ControlStyle := ControlStyle + [csCaptureMouse];
  Ctl3d := false;
  BorderStyle := bsNone;
  Style := lbOwnerDrawFixed;
  IntegralHeight := true;
  Height := ItemHeight * 7;
End;

{------------------------------------------------------------------------------}

Function TPSCMonthListBox.GetItemDate: TDateTime;
Begin
  If ItemIndex >= 0 Then
    Result := PSCIncMonth(SelDate,ItemIndex - (cPSCNumberOfMonth Div 2))
  Else
    Result := SelDate
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthListBox.KillTimer;
Begin
  PSCKillTimer(FTimer);
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthListBox.TimerEvent(Timer: TObject; EventID: Integer);
Begin
  SelDate := PSCIncMonth(SelDate,1 - Ord(Integer(EventID) < 0) * 2);
  If EventID < 0 Then
    ItemIndex := 0
  Else
    ItemIndex := Items.Count - 1;
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthListBox.KeyDown(Var Key: Word; Shift: TShiftState);
Var
  N,I: Integer;

  Procedure ModifySelDate(IncValue: Integer);
  Begin
    SelDate := PSCIncMonth(SelDate,IncValue);
    ItemIndex := I;
  End;

Begin
  N := ClientHeight Div ItemHeight;
  I := ItemIndex;
  Case Key Of
    VK_UP:
      Begin
        If I = 0 Then
          ModifySelDate(-1)
        Else
          ItemIndex := ItemIndex - 1;
        Key := 0;
      End;
    VK_DOWN:
      Begin
        If I = N - 1 Then
          ModifySelDate(1)
        Else
          ItemIndex := ItemIndex + 1;
        Key := 0;
      End;
    VK_PRIOR:
      Begin
        If I = 0 Then
          ModifySelDate(-N)
        Else
          ItemIndex := 0;
        Key := 0;
      End;
    VK_NEXT:
      Begin
        If I = N - 1 Then
          ModifySelDate(N)
        Else
          ItemIndex := N - 1;
        Key := 0;
      End;
  End;
  Inherited KeyDown(Key,Shift)
End;
{------------------------------------------------------------------------------}

Procedure TPSCMonthListBox.MouseMove(Shift: TShiftState; X,Y: Integer);
Var
  NewTimerId: Integer;
Begin
  Inherited;
  If (Y >= 0) And (Y < Height) Then
    Begin
      KillTimer;
      ItemIndex := Y Div ItemHeight;
      Exit
    End;
  ItemIndex := -1;
  If Y < 0 Then
    NewTimerId := Y
  Else
    NewTimerId := Y - ClientHeight;
  NewTimerId := NewTimerId Div ItemHeight + 1 - Ord(NewTimerId < 0) * 2;
  If NewTimerId > 4 Then
    NewTimerId := 4
  Else
    If NewTimerId < -4 Then
      NewTimerId := -4;
  If (FTimer = nil) Or (NewTimerId <> FTimerId) Then
    Begin
      KillTimer;
      FTimerId := NewTimerId;
      FTimer :=
        PSCSetTimer(Cardinal(FTimerId),cPSCScrollSpeed[Abs(FTimerId)],TimerEvent)
    End
End;

{------------------------------------------------------------------------------}

Function DateToMonthYear(Date: TDateTime): String;
Var
  Year,Month,Day: Word;
Begin
  PSCDecodeDate(Date,Year,Month,Day);
  Result := PSCFormat('%s %d', [PSCLongMonthNames(Month),Year]) //don't resource
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthListBox.SetSelDate(Value: TDateTime);
Var
  I: Integer;
Begin
  If FSelDate <> Value Then
    Begin
      FSelDate := Value;
      With Items Do
        Begin
          BeginUpdate;
          Try
            Clear;
            For I := 0 To cPSCNumberOfMonth - 1 Do
              Add(DateToMonthYear(PSCIncMonth(Value,I - (cPSCNumberOfMonth Div
                2))));
          Finally
            EndUpdate
          End
        End
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthListBox.CNDrawItem(Var Message: TWMDrawItem);
Begin
  With Message.DrawItemStruct^ Do
    Begin
      itemState := itemState And (Not ods_Focus);
      Inherited;
    End;
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthListBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);

Const
  UsedColors: Array[Boolean] Of TPSCColor = (clPSCWindow,clPSCWindowText);
Var
  ASelected: Boolean;
  S: String;
Begin
  With Canvas Do
    Begin
      Font := Self.Font;
      Brush := Self.Brush;
      ASelected := (odSelected In State);
      If FTimer <> nil Then
        ASelected := false;
      Brush.Color := UsedColors[ASelected];
      Font.Color := UsedColors[Not ASelected];
      Brush.Style := BrushStyle_Solid;
      FillRect(Rect);
      If (Index >= cPSCNumberOfMonth) Or (Index < 0) Then
        Exit;
      S := Items[Index];
      PSCDrawText(Canvas,S,Length(S),Rect,DT_CENTER Or DT_VCENTER);
    End;
End;

{------------------------------------------------------------------------------}

Function PSCGetMonthListBoxWidth(ACanvas: TPSCCanvas): Integer;
Var
  DateTime: TDateTime;
  I: Integer;
Begin
  Result := 0;
  For I := 1 To 12 Do
    Begin
      DateTime := PSCEncodeDate(2000,I,1);
      Result := PSCMax(Result,ACanvas.TextWidth('XX' + //don't resource
        DateToMonthYear(DateTime) + 'XX')) //don't resource
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCPopupMonthListBox.ResizeFormControls;
Begin
  Inherited;
  Canvas.Font := Font;
  With ListBox Do
    Begin
      ItemHeight := Self.Canvas.TextHeight('X') + 4;
      ClientHeight := ItemHeight * cPSCNumberOfMonth;
      ClientWidth := PSCGetMonthListBoxWidth(Self.Canvas);
    End;
  ClientHeight := ListBox.Height;
  ClientWidth := ListBox.Width;
End;

{------------------------------------------------------------------------------}

constructor TPSCPopupMonthListBox.CreateNew(AOwner: TComponent; Dummy: Integer=0);
Begin
  Inherited;
  With TPSCMonthListBox(ListBox) Do
    Begin
      OnMouseUp := ListBoxMouseUp;
      BorderStyle := bsSingle;
      Ctl3d := False;
      Align := alNone;
      ParentColor := true;
      ParentFont := True;
      Parent := Self;
      ResizeFormControls;
    End;
  ActiveControl := ListBox;
End;

{------------------------------------------------------------------------------}

Function TPSCPopupMonthListBox.GetListBoxClass: TListBoxClass;
Begin
  Result := TPSCMonthListBox;
End;

{------------------------------------------------------------------------------}

Procedure TPSCPopupMonthListBox.CreateParams(Var Params: TCreateParams);
Begin
  Inherited CreateParams(Params);
  With Params Do
    Begin
      Style := Style And Not (WS_DLGFRAME Or WS_BORDER Or WS_THICKFRAME);
      ExStyle := ExStyle And Not WS_EX_CLIENTEDGE
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCPopupMonthListBox.Activate;
Begin
  TPSCMonthListBox(ListBox).MouseCapture := true
End;

{------------------------------------------------------------------------------}

Procedure TPSCPopupMonthListBox.ListBoxMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
Begin
  If Button = mbLeft Then
    ClosePopup(false,True);
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.OpenMonthPopup(Const Date: TDateTime);
Var
  R: TRect;
Begin
  If FMonthListBox <> Nil Then
    FMonthListBox.Free;
  FMonthListBox := TPSCPopupMonthListBox.CreateNew(Nil,0);
  With TPSCPopupMonthListBox(FMonthListBox) Do
    Begin
      Font := Self.Font;

      With TPSCMonthListBox(ListBox) Do
        Begin
          Font := Self.Font;
          SelDate := Date;
          ItemIndex := 3;
        End;
      GetHeaderRect(Date,R);
      FPopupDate := Date;

      PopupEx(Self,R,MonthPopupClosed,pskCentered,
        [poParentColor,poParentFontColor]);
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.MonthPopupClosed(Sender: TObject;
  Canceled: Boolean);
Var
  Col,Row: Integer;
  Date: TDateTime;
  List: TPSCMonthListBox;
  Index: Integer;
Begin
  If (Not Canceled) And (FMonthListBox <> Nil) Then
    Begin
      List := TPSCMonthListBox(TPSCPopupMonthListBox(FMonthListBox).ListBox);
      Index := List.ItemIndex;
      If Index >= 0 Then
        Begin
          Date := List.ItemDate;
          If DateToPos(FPopupDate,Col,Row) Then
            Date := PSCIncMonth(Date, -(Col + Row * MonthColCount));
          StartDate := Date
        End;
    End;
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X,Y: Integer);
Var
  Date: TDateTime;
  Value: Boolean;
  HitTest: TPSCCalendarHitTest;
  Rect: TRect;
Begin
  FShiftState := Shift;
  Inherited MouseDown(Button,Shift,X,Y);
  If Not (csDesigning In ComponentState) And CanFocus Then
    SetFocus;
  If (Button = mbLeft) Then
    Begin
      KillTimer;
      HitTest := GetHitTest(Point(X,Y),Date);
      If Not FSideWeekSelect And (HitTest = chtWeekLeft) Or
        ReadOnly And Not (HitTest In [chtLeftArrow,chtRightArrow,chtMonthHead])
          Then
        Exit;
      If ({FSelectKind = skWeekSel} False) And
        (HitTest In [chtDay,chtLeftGray,chtRightGray]) Then
        HitTest := chtWeekLeft;
      Case HitTest Of
        chtLeftArrow:
          If FShowNavButtons Then
            Begin
              If ssCtrl In Shift Then
                Value := StartFromPrevYear
              Else
                Value := StartFromPrevMonth;
              If Value And (FTimer = nil) And Not (ssDouble In Shift) And
                NavButtonsTimer Then
                FTimer := PSCSetTimer(1,FNavButtonsSpeed,TimerEvent);
            End;
        chtRightArrow:
          If FShowNavButtons Then
            Begin
              If ssCtrl In Shift Then
                Value := StartFromNextYear
              Else
                Value := StartFromNextMonth;
              If Value And (FTimer = nil) And Not (ssDouble In Shift) And
                NavButtonsTimer Then
                FTimer := PSCSetTimer(2,FNavButtonsSpeed,TimerEvent);
            End;
        chtMonthHead:
          If FShowMonthPopup Then
            OpenMonthPopup(Date);
        chtWeekDays:
          If FMultiSelect Then
            Begin
              If SelectMonth(Date, Not (ssCtrl In Shift)) And
                (Not (ssCtrl In Shift) Or ({FSelectKind = skWeekSel}False)) Then
                FWeeksSelected := true
            End;
        chtWeekLeft:
          If DateToRect(Date,Rect) Then
            Begin
              With Rect Do
                Begin
                  Right := Left;
                  Left := Left - FSideWidth;
                  If FShowWeekNumbers Then
                    Left := Left - 4
                End;
              If Not PtInRect(Rect,Point(X,Y)) Then
                Exit;
              If FMultiSelect Then
                SelectWeek(Date, Not (ssCtrl In Shift))
              Else
                If {FSelectKind = skWeekSel} False Then
                  CursorDate := Date
            End;
        chtDay,chtLeftGray,chtRightGray:
          If Not FMultiSelect Then
            InternalSetCursor(Date)
          Else
            Begin
              BeginUpdate;
              Try
                If (ssShift In Shift) And (FSelCount > 0) Then
                  Begin
                    If Not (ssCtrl In Shift) Then
                      Begin
                        ClearSelection;
                        CursorDate := FDragDate
                      End;
                    MouseMove(Shift,X,Y);
                    Exit
                  End;

                If (MinDateLimit And (Date < MinDate)) Or
                  (MaxDateLimit And (Date > MaxDate)) Then
                  Begin
                    PSCBeep;
                    Exit;
                  End;

                FSelShiftState := Shift;
                If ssCtrl In Shift Then
                  Value := Not SelDate[Date]
                Else
                  Begin
                    ClearSelection;
                    Value := true
                  End;
                FDragDate := Date;
                FCursorDate := Date;
                If Not FExtendedSelect And (FSelCount > 0) Then
                  If Not Value Then
                    If Date = FSelStart Then
                      FDragDate := FSelFinish
                    Else
                      If Date = FSelFinish Then
                        FDragDate := FSelStart
                      Else
                        Begin
                          MouseCapture := false;
                          Exit
                        End
                    Else
                      If (Date <> FSelStart - 1) And (Date <> FSelFinish + 1)
                        Then
                        Begin
                          MouseCapture := false;
                          Exit
                        End;
                      If SelDate[Date] <> Value Then
                        If ((FMaxSelDates = 0) Or (FSelCount + Ord(Value) * 2 - 1
                          <= FMaxSelDates)) Then
                          Begin
                            ClearWeekSelection;
                            SelDate[Date] := Value
                          End
                        Else
                          PSCBeep;
              Finally
                EndUpdate
              End
            End
      End
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X,Y: Integer);
Var
  Date: TDateTime;
Begin
  FShiftState := Shift;
  KillTimer;
  If (Button = mbLeft) and (caopScrollWhenGreyClicked in Options) Then
    Case GetHitTest(Point(X,Y),Date) Of
        chtLeftGray:
          If FSelFinish <= PSCGetMonthEnd(Date) Then
            StartFromPrevMonth;
        chtRightGray:
          If FSelStart >= PSCGetMonthStart(Date) Then
            StartFromNextMonth
      End;
  Inherited MouseUp(Button,Shift,X,Y)
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.MouseMove(Shift: TShiftState; X,Y: Integer);
Type
  TSelectKind = (skSelect,skUnselect,slInvert);
Const
  CtrlToSelectKind: Array[Boolean] Of TSelectKind = (skSelect,slInvert);
Var
  TmpDate,Date,CurDate: TDateTime;
  Week: Integer;
  Value,DateCompare: Boolean;
  HitTest: TPSCCalendarHitTest;
  Rect: TRect;

  Function Select(First,Second: TDateTime; Kind: TSelectKind): Boolean;
  Var
    FDate: TDateTime;
    Value: Boolean;
  Begin
    If First > Second Then
      Begin
        FDate := Second;
        Second := First;
        First := FDate
      End;
    Result := true;
    BeginUpdate;
    Try
      While First <= Second Do
        Begin
          Case Kind Of
            skSelect: Value := true;
            skUnselect: Value := false;
          Else
            Value := Not SelDate[First]
          End;
          If Not SetSelDate(First,Value) Then
            Begin
              Result := false;
              Break
            End;
          First := First + 1
        End
    Finally
      EndUpdate
    End
  End;

  Procedure RoundSelectionToWeeks;
  Begin
    If Date > FDragDate Then
      Begin
        CurDate := FDragDate;
        TmpDate := Date
      End
    Else
      Begin
        CurDate := Date;
        TmpDate := FDragDate
      End;
    If (TmpDate - CurDate > 7) Or (TmpDate - CurDate >= 6) And
      (CurDate = GetBeginOfWeek(CurDate)) Then
      Begin
        If Date >= FDragDate Then
          Begin
            FDragDate := GetBeginOfWeek(FDragDate);
            CurDate := FDragDate;
            TmpDate := FDragDate + 6
          End
        Else
          Begin
            FDragDate := GetEndOfWeek(FDragDate);
            CurDate := FDragDate - 6;
            TmpDate := FDragDate
          End;
        If Select(CurDate,TmpDate,CtrlToSelectKind[ssCtrl In Shift]) Then
          Begin
            HitTest := chtWeekLeft;
            Date := GetBeginOfWeek(Date);
            UpdateWeekSelection(false)
          End
      End
  End;

  Procedure SelectBlock;
  Var
    OldCursor: TDateTime;
  Begin
    OldCursor := FCursorDate;
    FCursorDate := Date;
    BeginUpdate;
    Try
      While CurDate <> Date Do
        Begin
          DateCompare := CurDate < Date;
          If DateCompare Then
            Begin
              Value := CurDate >= FDragDate;
              If Value Then
                CurDate := CurDate + 1
            End
          Else
            Begin
              Value := CurDate <= FDragDate;
              If Value Then
                CurDate := CurDate - 1;
            End;

          If ExtendedSelect And (ssCtrl In Shift) Then
            Value := Not SelDate[CurDate];
          SelDate[CurDate] := Value;

          If DateCompare Then
            Begin
              If CurDate < FDragDate Then
                CurDate := CurDate + 1
            End
          Else
            If CurDate > FDragDate Then
              CurDate := CurDate - 1
        End;
      If Not FExtendedSelect And (FSelCount > 0) Then
        SelDate[OldCursor] := (OldCursor >= FSelStart) And (OldCursor <=
          FSelFinish)
    Finally
      EndUpdate
    End
  End;

  Procedure SelectFreeBlock;
  Var
    Earlier,Later,CDate: TDateTime;
    WeekDelta,I: Integer;

    Procedure GetDateRect(First,Second: TDateTime);
    Begin
      If First < Second Then
        Begin
          Earlier := First;
          Later := Second
        End
      Else
        Begin
          Earlier := Second;
          Later := First
        End;
      WeekDelta := Trunc((Later - GetBeginOfWeek(Later)) -
        (Earlier - GetBeginOfWeek(Earlier)));
      If WeekDelta < 0 Then
        Begin
          WeekDelta := -WeekDelta;
          Later := Later + WeekDelta;
          Earlier := Earlier - WeekDelta
        End
    End;

  Begin
    FCursorDate := Date;
    BeginUpdate;
    Try
      GetDateRect(FDragDate,CurDate);
      CDate := Earlier;
      While CDate <= Later Do
        Begin
          For I := 0 To WeekDelta Do
            Begin
              If ssCtrl In Shift Then
                Value := Not SelDate[CDate + I]
              Else
                Value := false;
              SelDate[CDate + I] := Value
            End;
          CDate := CDate + 7
        End;
      GetDateRect(FDragDate,Date);
      CDate := Earlier;
      While CDate <= Later Do
        Begin
          For I := 0 To WeekDelta Do
            Begin
              If ssCtrl In Shift Then
                Value := Not SelDate[CDate + I]
              Else
                Value := true;
              SelDate[CDate + I] := Value
            End;
          CDate := CDate + 7
        End
    Finally
      EndUpdate
    End
  End;

  Procedure SelectWeekBlock;
  Begin
    BeginUpdate;
    Try
      TmpDate := GetBeginOfWeek(FDragDate);
      If TmpDate <= Date Then
        Begin
          Date := Date + 6;
          If FDragDate <> TmpDate Then
            Begin
              Select(FDragDate,TmpDate,CtrlToSelectKind[ssCtrl In Shift]);
              UpdateWeekSelection(false);
              FDragDate := TmpDate
            End
        End
      Else
        If FDragDate <> TmpDate + 6 Then
          Begin
            Select(FDragDate,TmpDate + 6,CtrlToSelectKind[ssCtrl In Shift]);
            UpdateWeekSelection(false);
            FDragDate := TmpDate + 6
          End;
      FCursorDate := Date;
      Week := 6;
      While CurDate <> Date Do
        Begin
          If Week < 6 Then
            Week := Week + 1
          Else
            Begin
              Week := 0;
              InvalidateWeek(CurDate)
            End;
          DateCompare := CurDate < Date;
          If DateCompare Then
            Begin
              Value := CurDate >= FDragDate;
              If Value Then
                CurDate := CurDate + 1
            End
          Else
            Begin
              Value := CurDate <= FDragDate;
              If Value Then
                CurDate := CurDate - 1
            End;
          If (CurDate < TmpDate) Or (CurDate > TmpDate + 6) Then
            If Not SetSelDate(CurDate,Value) Then
              Break;
          If Not Value Then
            If DateCompare Then
              CurDate := CurDate + 1
            Else
              CurDate := CurDate - 1
        End
    Finally
      EndUpdate
    End;
    InvalidateWeek(CurDate)
  End;

Begin
  HitTest := GetHitTest(Point(X,Y),Date);
  MouseHitTest(HitTest);
  If (ssLeft In Shift) And Not ReadOnly Then
    Begin
      Case HitTest Of
        chtDay,chtLeftGray,chtRightGray:
          If ({FSelectKind = skWeekSel} False) Or FWeeksSelected Then
            Begin
              HitTest := chtWeekLeft;
              Date := GetBeginOfWeek(Date)
            End
          Else
            If FMultiSelect And
            Not ((FSelectKind = skFree) Or (Shift * [ssCtrl,ssShift] <> []))
              Then
              RoundSelectionToWeeks;
        chtWeekLeft:
          If Not FSideWeekSelect Then
            Exit
          Else
            If DateToRect(Date,Rect) Then
              Begin
                With Rect Do
                  Begin
                    Right := Left;
                    Left := Left - FSideWidth;
                    If FShowWeekNumbers Then
                      Left := Left - 4
                  End;
                If Not PtInRect(Rect,Point(X,Y)) Then
                  Exit;
                CurDate := GetBeginOfWeek(FDragDate);
                If Abs(FCursorDate - FDragDate) < 6 Then
                  SelectWeek(FDragDate,false)
              End
      End;
      CurDate := FCursorDate;
      FSelShiftState := Shift;
      If (FSelCount > 0) And (Date <> CurDate) Then
        Case HitTest Of
          chtDay,chtLeftGray,chtRightGray:
            If Not FMultiSelect Then
              InternalSetCursor(Date)
            Else
              If FExtendedSelect And (SelectKind = skFree) And (ssAlt In Shift)
                Then
                SelectFreeBlock
              Else
                SelectBlock;
          chtWeekLeft:
            If FMultiSelect Then
              SelectWeekBlock
        End
    End;
  Inherited MouseMove(Shift,X,Y)
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.MouseHitTest(Var HitTest: TPSCCalendarHitTest);
Begin
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.KeyDown(Var Key: Word; Shift: TShiftState);
Var
  Delta: Integer;
  Date: TDateTime;
Begin
  FShiftState := Shift;
  Inherited KeyDown(Key,Shift);
  Case Key Of
    VK_RETURN:
      If ssCtrl In Shift Then
        Begin
          If ssShift In Shift Then
            Date := StartDate
          Else
            Date := PSCDateOf(PSCNow);
          StartDate := Date;
          If Not ReadOnly And (Not FMultiSelect Or (FSelCount = 0)) Then
            If CanSelectionChange(Date) Then
              CursorDate := Date
            Else
              CursorDate := StartDate;
          Key := 0
        End
      Else
        If ssShift In Shift Then
          Begin
            If FMultiSelect And (FSelCount > 0) Then
              StartDate := FSelStart;
            Key := 0
          End;
  End;
  If Not ReadOnly Then
    If (ssShift In Shift) And FMultiSelect Then
      Case Key Of
        VK_LEFT:
          Begin
            Select(-1,false);
            Key := 0
          End;
        VK_RIGHT:
          Begin
            Select(1,false);
            Key := 0
          End;
        VK_UP:
          Begin
            Select(-7,false);
            Key := 0
          End;
        VK_DOWN:
          Begin
            Select(7,false);
            Key := 0
          End;
        VK_HOME:
          Begin
            If ssCtrl In Shift Then
              Delta := Trunc(PSCGetMonthStart(FCursorDate) - FCursorDate)
            Else
              Delta := Trunc(GetBeginOfWeek(FCursorDate) - FCursorDate);
            Select(Delta,true);
            Key := 0
          End;
        VK_END:
          Begin
            If ssCtrl In Shift Then
              Delta := Trunc(PSCGetMonthEnd(FCursorDate) - FCursorDate)
            Else
              Delta := Trunc(GetEndOfWeek(FCursorDate) - FCursorDate);
            Select(Delta,true);
            Key := 0
          End;
        VK_PRIOR:
          If ssCtrl In Shift Then
            Begin
              Delta := Trunc(GetBeginOfWeek(PSCGetMonthStart(FCursorDate)) -
                FCursorDate);
              Select(Delta,true);
              Key := 0
            End;
        VK_NEXT:
          If ssCtrl In Shift Then
            Begin
              Delta := Trunc(GetBeginOfWeek(PSCGetMonthStart(FCursorDate)) + (7
                * 6 - 1) -
                FCursorDate);
              Select(Delta,true);
              Key := 0
            End
      End
    Else
      Begin
        If FClearOnKeyMoving And FMultiSelect And (FSelCount > 1) And
          (PSCIntIsInArray(Key, [VK_LEFT,VK_RIGHT,VK_UP,VK_DOWN])) Then
          Begin
            If {SelectKind = skWeekSel} False Then
              Begin
                Date := GetBeginOfWeek(SelStart);
                ClearSelection;
                SelStart := Date;
                SelFinish := Date + 6;
                UpdateWeekSelection(true)
              End
            Else
              Begin
                SelFinish := FSelStart;
                CursorDate := FSelStart;
                ClearWeekSelection
              End
          End;
        Case Key Of
          VK_LEFT:
            Begin
              If FWeeksSelected Then
                SelectToPrevWeek
              Else
                SelectToPrevDay;
              Key := 0
            End;
          VK_RIGHT:
            Begin
              If FWeeksSelected Then
                SelectToNextWeek
              Else
                SelectToNextDay;
              Key := 0
            End;
          VK_UP:
            Begin
              SelectToPrevWeek;
              Key := 0
            End;
          VK_DOWN:
            Begin
              SelectToNextWeek;
              Key := 0
            End;
          VK_HOME:
            Begin
              If ssCtrl In Shift Then
                SelectToMonthBeg
              Else
                SelectToWeekBeg;
              Key := 0
            End;
          VK_END:
            Begin
              If ssCtrl In Shift Then
                SelectToMonthEnd
              Else
                SelectToWeekEnd;
              Key := 0
            End;
          VK_PRIOR:
            If ssCtrl In Shift Then
              Begin
                SelectToCalendBeg;
                Key := 0
              End;
          VK_NEXT:
            If ssCtrl In Shift Then
              Begin
                SelectToCalendEnd;
                Key := 0
              End;
          VK_ESCAPE:
            Begin
              ClearSelection;
              Key := 0
            End
        End
      End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.WMGetDlgCode(Var Message: TMessage);
Begin
  Inherited;
  With Message Do
    Result := Result Or DLGC_WANTARROWS
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.WMEraseBkgnd(Var Message: TWMEraseBkgnd);
Begin
  Message.Result := 1
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.DateChanged;
Begin
  If Assigned(FOnDateChanged) Then
    FOnDateChanged(Self)
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.SetMaxSelDates(Value: Integer);
Begin
  If Value < 0 Then
    Value := 0;
  If ({SelectKind = skWeekSel} False) And (Value < 7) Then
    Value := 7;
  FMaxSelDates := Value;
  If Value > 0 Then
    Begin
      BeginUpdate;
      Try
        While (FSelCount > Value) And (FSelStart <> FSelFinish) Do
          SelDate[FSelFinish] := false
      Finally
        EndUpdate
      End;
      UpdateWeekSelection(true)
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.UpdateLimits;
Begin
  If FSelCount > 0 Then
    Begin
      If FMinDateLimit Then
        If FSelFinish < FMinDate Then
          ClearSelection
        Else
          If FSelStart < FMinDate Then
            SelStart := FMinDate;
      If FMaxDateLimit Then
        If FSelStart > FMaxDate Then
          ClearSelection
        Else
          If FSelFinish > FMaxDate Then
            SelFinish := FMaxDate;
      UpdateWeekSelection(true);
    End;
  InvalidateArrow(chtLeftArrow);
  InvalidateArrow(chtRightArrow)
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.SetMinDateLimit(Value: Boolean);
Begin
  If FMinDateLimit <> Value Then
    Begin
      FMinDateLimit := Value;
      UpdateLimits
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.SetMinDate(Const Value: TDateTime);
Begin
  If FMinDate <> PSCDateOf(Value) Then
    Begin
      FMinDate := PSCDateOf(Value);
      UpdateLimits
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.SetMaxDateLimit(Value: Boolean);
Begin
  If FMaxDateLimit <> Value Then
    Begin
      FMaxDateLimit := Value;
      UpdateLimits
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.SetMaxDate(Const Value: TDateTime);
Begin
  If FMaxDate <> PSCDateOf(Value) Then
    Begin
      FMaxDate := PSCDateOf(Value);
      UpdateLimits
    End
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendar.FindMonth(Date: TDateTime;
  Var Index: Integer): Boolean;
Var
  C: TDateTime;
  L,H: Integer;
Begin
  Result := false;
  H := FSelDates.Count;
  If H = 0 Then
    Index := 0
  Else
    Begin
      Date := PSCGetMonthStart(Date);
      If Date < PMonthSelection(FSelDates[0]).Base Then
        Index := 0
      Else
        If Date > PMonthSelection(FSelDates[H - 1]).Base Then
          Index := H
        Else
          Begin
            L := 0;
            H := H - 1;
            While L <= H Do
              Begin
                Index := (L + H) Shr 1;
                C := PMonthSelection(FSelDates[Index]).Base - Date;
                If C < 0 Then
                  L := Index + 1
                Else
                  If C > 0 Then
                    H := Index - 1
                  Else
                    Begin
                      Result := true;
                      Exit
                    End
              End;
            Index := L
          End
    End
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendar.GetNotSelDate(Const Date: TDateTime): Boolean;
Begin
  Result := Not GetSelDate(Date);
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendar.GetSelDate(Const Date: TDateTime): Boolean;
Var
  Index: Integer;
Begin
  Result := false;
  If (Date >= FSelStart) And (Date <= FSelFinish) Then
    Begin
      If Not MultiSelect Then
        Result := true
      Else
        If FindMonth(Date,Index) Then
          With PMonthSelection(FSelDates[Index])^ Do
            Result := Dates And (1 Shl Trunc(Date - Base)) <> 0
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.SelectDate(Const Date: TDateTime; Value: Boolean);
Begin
  SetSelDate(Date,Value)
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.InvalidateDateAndSiblings(const ADate:TDateTime);
var
  MyRect: TRect;
  MyExpandRect: Boolean;

  procedure ExpandRect(const ADate:TDateTime);
  var
    MyThisRect:TRect;
  begin
    If DateToRect(ADate,MyThisRect) then
    begin
      If MyExpandRect then
        begin
          MyRect.Left    := PSCMin(MyRect.Left,MyThisRect.Left);
          MyRect.Top     := PSCMin(MyRect.Top,MyThisRect.Top);
          MyRect.Right   := PSCMax(MyRect.Right,MyThisRect.Right);
          MyRect.Bottom  := PSCMax(MyRect.Bottom,MyThisRect.Bottom);
        end
      else
        MyRect:=MyThisRect;
      MyExpandRect:=True;
    end;
  end;

begin
  If HandleAllocated then
    begin
      MyExpandRect:=False;
      ExpandRect(ADate);
      ExpandRect(ADate+1);
      ExpandRect(ADate-1);
      If MyExpandRect then
        InvalidateRect(MyRect);
    end;
end;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendar.SetSelDate(Const Date: TDateTime;
  Value: Boolean): Boolean;
Var
  Index: Integer;
  MonthSelection: PMonthSelection;
  NewDates: Cardinal;

  Procedure UpdateSelStart(Index: Integer);
  Var
    I,J: Cardinal;
  Begin
    With PMonthSelection(FSelDates[Index])^ Do
      Begin
        J := 1;
        For I := 0 To SizeOf(Cardinal) * 8 - 1 Do
          If Dates And J = 0 Then
            J := J * 2
          Else
            Begin
              FSelStart := Base + I;
              Break;
            End
      End
  End;

  Procedure UpdateSelFinish(Index: Integer);
  Var
    I,J: Cardinal;
  Begin
    With PMonthSelection(FSelDates[Index])^ Do
      Begin
        J := $80000000;
        For I := SizeOf(Cardinal) * 8 - 1 Downto 0 Do
          If Dates And J = 0 Then
            J := J Div 2
          Else
            Begin
              FSelFinish := Base + I;
              Break;
            End
      End
  End;

Begin
  If Not MultiSelect Then
    Begin
      Result := Value;
      If Value Then
        SetCursorDate(Date)
    End
  Else
    If Value And ((FMaxSelDates > 0) And (FSelCount = FMaxSelDates) Or
      FMinDateLimit And (Date < FMinDate) Or FMaxDateLimit And (Date > FMaxDate))
        Then
      Begin
        Result := false;
        Exit
      End
    Else
      Result := true;
  If FindMonth(Date,Index) Then
    Begin
      MonthSelection := PMonthSelection(FSelDates[Index]);
      With MonthSelection^ Do
        Begin
          If Value Then
            Begin
              NewDates := Dates Or (1 Shl Trunc(Date - Base));
              If NewDates <> Dates Then
                Begin
                  If Not CanSelectionChange(Date) Then
                    Begin
                      Result := false;
                      Exit
                    End;
                  Dates := NewDates Or (1 Shl Trunc(Date - Base));
                  FSelCount := FSelCount + 1;
                  If FSelStart > Date Then
                    FSelStart := Date
                  Else
                    If FSelFinish < Date Then
                      FSelFinish := Date;
                  FModified := true;
                  UpdateSelection
                End;
            End
          Else
            Begin
              NewDates := Dates And Not (1 Shl Trunc(Date - Base));
              If NewDates <> Dates Then
                Begin
                  If Not CanSelectionChange(Date) Then
                    Begin
                      Result := false;
                      Exit
                    End;
                  Dates := NewDates;
                  FSelCount := FSelCount - 1;
                  If Dates = 0 Then
                    Begin
                      If FSelCount > 0 Then
                        If Date = FSelStart Then
                          UpdateSelStart(Index + 1)
                        Else
                          If Date = FSelFinish Then
                            UpdateSelFinish(Index - 1);
                      Dispose(MonthSelection);
                      FSelDates.Delete(Index)
                    End
                  Else
                    If FSelStart = Date Then
                      UpdateSelStart(Index)
                    Else
                      If FSelFinish = Date Then
                        UpdateSelFinish(Index);
                  FModified := true;
                  UpdateSelection
                End
            End
        End
    End
  Else
    If Value Then
      Begin
        If Not CanSelectionChange(Date) Then
          Begin
            Result := false;
            Exit
          End;
        If Index = 0 Then
          FSelStart := Date;
        If Index = FSelDates.Count Then
          FSelFinish := Date;
        New(MonthSelection);
        With MonthSelection^ Do
          Begin
            Base := PSCGetMonthStart(Date);
            Dates := 1 Shl Trunc(Date - Base)
          End;
        Try
          FSelDates.Insert(Index,MonthSelection);
        Except
          Dispose(MonthSelection);
          Raise
        End;
        FSelCount := FSelCount + 1;
        FModified := true;
        UpdateSelection
      End;

  InvalidateDateAndSiblings(Date);
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.SetSelStart(Value: TDateTime);
Var
  OldStart: TDateTime;
Begin
  If Value > FSelFinish Then
    SetSelFinish(Value)
  Else
    Begin
      Value := Int(Value);
      If FMinDateLimit And (Value < FMinDate) Then
        Value := FMinDate;
      If (FSelStart <> Value) Or (SelCount = 0) Then
        If FMultiSelect Then
          If FSelCount = 0 Then
            SelDate[Value] := true
          Else
            Begin
              BeginUpdate;
              Try
                Repeat
                  OldStart := FSelStart;
                  If FSelStart < Value Then
                    SelDate[FSelStart] := false
                  Else
                    SelDate[FSelStart - 1] := true
                Until (FSelStart = Value) Or (FSelStart = OldStart)
              Finally
                EndUpdate
              End
            End
        Else
          SetCursorDate(Value)
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.SetSelFinish(Value: TDateTime);
Var
  OldFinish: TDateTime;
Begin
  If Value < FSelStart Then
    SetSelStart(Value)
  Else
    Begin
      Value := Int(Value);
      If FMaxDateLimit And (Value > FMaxDate) Then
        Value := FMaxDate;
      If (FSelFinish <> Value) Or (SelCount = 0) Then
        If FMultiSelect Then
          If FSelCount = 0 Then
            SelDate[Value] := true
          Else
            Begin
              BeginUpdate;
              Try
                Repeat
                  OldFinish := FSelFinish;
                  If FSelFinish > Value Then
                    SelDate[FSelFinish] := false
                  Else
                    SelDate[FSelFinish + 1] := true
                Until (FSelFinish = Value) Or (FSelFinish = OldFinish)
              Finally
                EndUpdate
              End
            End
        Else
          SetCursorDate(Value)
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.WMSize(Var Message: TWMSize);
Begin
  Inherited;
  If HandleAllocated Then
    SetStartDate(FStartDate);
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.SetWeeksSelected(V: boolean);
Begin
  If FWeeksSelected <> V Then
    Begin
      FWeeksSelected := V;
      InvalidateWeekSelection;
    End;
End;

{------------------------------------------------------------------------------}

function TPSCCustomCalendar.GetTopLeftCorner:TPoint;
Var
  R:TRect;
begin
  R:=CorrectNCRect(Rect(0,0,ClientWidth,ClientHeight));
  Result.X:=R.Left;
  Result.Y:=R.Top;
end;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendar.UpdateTopLeftCorner(Const R: TRect): TRect;
Var
  P:TPoint;
begin
  Result:=R;
  P:=GetTopLeftCorner;
  OffsetRect(Result,P.X,P.Y);
end;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.CMCtl3DChanged(Var Message: TMessage);
Begin
  Inherited;
  BorderChanged;
  Invalidate;
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.CMColorChanged(Var Message: TMessage);
Begin
  Inherited;
  With FColors Do
    Begin
      FWeekHeader := Color;
      FDays := Color;
      FWeekSide := Color
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.CMParentColorChanged(Var Message: TMessage);
Var
  Color: TPSCColor;
Begin
  Inherited;
  If ParentColor Then
    Begin
      If Message.WParam <> 0 Then
        Color := TPSCColor(Message.lParam)
      Else
        Color := TPSCCustomCalendar(Parent).Color;
      If Colors.Border <> Color Then
        Begin
          Colors.Border := Color;
          ParentColor := True
        End
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.SetFlat(Value: Boolean);
Begin
  If FFlat<>Value then
  begin
    FFlat:=Value;
    Invalidate;
  end;
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.SetBorderStyle(Value: TBorderStyle);
Begin
  If FBorderStyle <> Value Then
    Begin
      FBorderStyle := Value;
      BorderChanged;
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.SetBorderEdges(Value: TBevelEdges);
Begin
  If FBorderEdges <> Value Then
    Begin
      FBorderEdges := Value;
      AdjustSize;
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.SetExtendedSelect(Value: Boolean);
Begin
  If FExtendedSelect <> Value Then
    Begin
      If FExtendedSelect And FMultiSelect Then
        SelFinish := SelStart;
      UpdateWeekSelection(true);
      FExtendedSelect := Value
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.SetSelectKind(Value: TPSCCalSelectKind);
Begin
  If FSelectKind <> Value Then
    Begin
      If FMultiSelect And (FSelCount > 0) Then
        Begin
          If True {Value <> skWeekSel} Then
            SelFinish := SelStart
          Else
            Begin
              SelStart := GetBeginOfWeek(SelStart);
              SelFinish := SelStart + 6;
              FWeeksSelected := true
            End;
          InvalidateWeekSelection
        End;
      FSelectKind := Value
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.SetReadOnly(Value: Boolean);
Begin
  FReadOnly := Value
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendar.GetReadOnly: Boolean;
Begin
  Result := FReadOnly
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.WMSetCursor(Var Message: TWMSetCursor);
Var
  P: TPoint;
  Date: TDateTime;
Begin
  With Message Do
    If Not (csDesigning In ComponentState) And FSideWeekSelect And
      FMultiSelect And Not ReadOnly And (CursorWnd = Handle) Then
      Case Smallint(HitTest) Of
        HTCLIENT:
          Begin
            GetCursorPos(P);
            P := ScreenToClient(P);
            Case GetHitTest(P,Date) Of
              chtWeekLeft:
                Begin
                  winapi.Windows.SetCursor(Screen.Cursors[FWeekCursor]);
                  Result := 1;
                  Exit
                End
            End
          End
      End;
  Inherited
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.CreateWnd;
Begin
  Inherited;
  FontChanged;
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.Notification(AComponent: TComponent;
  Operation: TOperation);
Begin
  Inherited Notification(AComponent,Operation);
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendar.MonthColCount: Integer;
Var
  MonthWidth,Width: Integer;
Begin
  If Not HandleAllocated Then
    Result := 1
  Else
    Begin
      MonthWidth := GetMonthWidth;
      Width := ClientWidth;
      If (MonthWidth = 0) Or (Width < MonthWidth) Then
        Result := 1
      Else
        Result := Width Div MonthWidth
    End
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendar.MonthRowCount: Integer;
Var
  MonthHeight,Height: Integer;
Begin
  If Not HandleAllocated Then
    Result := 1
  Else
    Begin
      MonthHeight := GetMonthHeight;
      Height := ClientHeight;
      If (MonthHeight = 0) Or (Height < MonthHeight) Then
        Result := 1
      Else
        Result := ClientHeight Div MonthHeight
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.UpdateWidthHeight(Var AWidth,AHeight: Integer);
Var
  Value,Delta: Integer;
Begin
  Delta := 4;

  If FBorderEdges * [beLeft,beRight] = [] Then
    Begin
      Value := GetMonthWidth;
      If Value > 0  Then
      begin
        Dec(AWidth,Width-ClientWidth+Delta);
        AWidth:=PSCMax(Value,AWidth);
        AWidth := (AWidth Div Value) * Value + Delta +
          (Width-ClientWidth);
      end;
    End;

  If FBorderEdges * [beTop,beBottom] = [] Then
    Begin
      Value := GetMonthHeight;
      If Value > 0 Then
      begin
        Dec(AHeight,Height-ClientHeight+Delta);
        AHeight:=PSCMax(Value,AHeight);
        AHeight := (AHeight Div Value) * Value + Delta +
          (Height-ClientHeight);
      end;
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.WMWindowPosChanging(
  Var Message: TWMWindowPosChanging);
Begin
  With Message.WindowPos^ Do
    If flags And SWP_NOSIZE = 0 Then
      UpdateWidthHeight(cx,cy);
  Inherited;
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendar.CorrectNCRect(Const R: TRect): TRect;
Var
  delta,delta2: Integer;
Begin
  Result := R;
  InflateRect(Result,-2,-2);
  With Result Do
    Begin
      If FBorderEdges * [beLeft,beRight] <> [] Then
        Begin
          delta := GetMonthWidth;
          If delta <> 0 Then
            Begin
              delta2 := Right - Left;
              If delta2 < delta Then
                delta := 0
              Else
                delta := delta2 Mod delta
            End;
          If FBorderEdges * [beLeft,beRight] = [beLeft,beRight] Then
            delta2 := delta Div 2
          Else
            If beLeft In FBorderEdges Then
              delta2 := delta
            Else
              delta2 := 0;
          Left := Left + delta2;
          Right := Right - (delta - delta2);
        End;
      If FBorderEdges * [beTop,beBottom] <> [] Then
        Begin
          delta := GetMonthHeight;
          If delta <> 0 Then
            Begin
              delta2 := Bottom - Top;
              If delta2 < delta Then
                delta := 0
              Else
                delta := delta2 Mod delta
            End;
          If FBorderEdges * [beTop,beBottom] = [beTop,beBottom] Then
            delta2 := delta Div 2
          Else
            If beTop In FBorderEdges Then
              delta2 := delta
            Else
              delta2 := 0;
          Top := Top + delta2;
          Bottom := Bottom - (delta - delta2);
        End;
    End;
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.BorderChanged;
begin
  If HandleAllocated Then
  begin
    SetWindowPos(Handle,0,0,0,Width,Height,SWP_NOACTIVATE Or
      SWP_NOZORDER Or SWP_NOMOVE Or SWP_FRAMECHANGED);
    AdjustSize;  
    Invalidate;  
  end;
end;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.CMBorderChanged(Var Message: TMessage);
Begin
  Inherited;
  BorderChanged;
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendar.GetHeaderRect(Const Date: TDateTime;
  Var Rect: TRect): Boolean;
Var
  Col,Row: Integer;
Begin
  If Not DateToPos(Date,Col,Row) Then
    Result := false
  Else
    With Rect Do
      Begin
        Result := true;
        Right := GetMonthWidth;
        Left := Col * Right;
        Top := Row * GetMonthHeight;
        Right := Left + Right;
        Bottom := Top + FHeaderHeight
      End;
  Rect := UpdateTopLeftCorner(Rect);
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendar.GetHeaderText(Const Date: TDateTime): String;
Begin
  Result := DateToMonthYear(Date);
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.DrawHeader(Const Date: TDateTime; MonthPos:
  TPSCMonthPos;
  Var Rect: TRect);
Const
  LeftEdge: Array[Boolean] Of Cardinal = (0,BF_LEFT);
  TopEdge: Array[Boolean] Of Cardinal = (0,BF_TOP);
  RightEdge: Array[Boolean] Of Cardinal = (0,BF_RIGHT);
Var
  ASize: TSize;
  S: String;
Begin
  With Canvas Do
    Begin
      If not Flat Then
        DrawEdge(Canvas.Handle,Rect,BDR_SUNKENOUTER,BF_ADJUST Or BF_MONO Or BF_BOTTOM
          Or TopEdge[Not (mpTop In MonthPos)] Or RightEdge[mpRight In
            MonthPos]);
      DrawEdge(Canvas.Handle,Rect,BDR_RAISEDINNER,BF_ADJUST Or BF_TOP Or BF_BOTTOM
        Or
        LeftEdge[mpLeft In MonthPos] Or RightEdge[mpRight In MonthPos]);

      S := GetHeaderText(Date);
      Brush.Color := FColors.MonthHeader;

      Font := FColors.HeaderFont;
      Try
        ASize:=Canvas.TextExtent(S);
        With Rect Do
          Canvas.TextRect(Rect,(Right + Left - ASize.cx) Div 2,
            (Top + Bottom - ASize.cy) Div 2,S);
      Finally
        Font := Self.Font
      End;
      InflateRect(Rect,0, -1);

      DrawEdge(Canvas.Handle,Rect,BDR_RAISEDINNER,BF_ADJUST Or
        LeftEdge[Not (mpLeft In MonthPos)] Or
        RightEdge[Not (mpRight In MonthPos)]);

    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.DrawArrow(Arrow: TPSCCalendarHitTest;
  Const Rect: TRect);
Begin
  PSCDrawOutlookArrow(Canvas,Arrow = chtLeftArrow,Rect);
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.DrawWeekLine(Var Rect: TRect);
Const
  EdgeStyle: Array[Boolean] Of Cardinal = (EDGE_ETCHED,BDR_RAISEDOUTER);
Begin
  DrawEdge(Canvas.Handle,Rect,EdgeStyle[not Flat],BF_BOTTOM);
End;

{------------------------------------------------------------------------------}

function TPSCCustomCalendar.GetDayName(ADayIndex:Integer;
  ALength:TPSCWeekDaysLength):String;
var
  L: Integer;
begin
  If ALength In [wdlOne,wdlTwo,wdlShort] Then
    Result := FormatSettings.ShortDayNames[ADayIndex]
  Else
    Result := FormatSettings.LongDayNames[ADayIndex];
  L := Length(Result);
  Case ALength Of
    wdlOne: L := 1;
    wdlTwo:
      If L >= 2 Then
        L := 2;
    wdlThree:
      If L >= 3 Then
        L := 3
  End;

  Result:=Copy(Result,1,L);
end;

{------------------------------------------------------------------------------}

procedure TPSCCustomCalendar.AlignDayName(const ARect:TRect;
  const ASize:TSize;var APoint:TPoint);
begin
end;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.DrawDaysHeader(MonthPos: TPSCMonthPos;
  Const Rect: TRect);
Var
  I,J,H: Integer;
  R: TRect;
  S: String;
  aSize: TSize;
  MyPoint:TPoint;
Begin
  With Canvas Do
    Begin
      If FWeekDayNames <> wdlOne Then
        Font := FColors.WeekDaysFont
      Else
        Font.Color := FColors.WeekDaysFont.Color;
      Try
        Brush.Color := FColors.WeekHeader;
        If Not DoDrawItem(0,chtWeekDays,cdtNormal,Rect) Then
          Exit;
        FillRect(Rect);
        DrawDividers(MonthPos,Rect);
        H := Rect.Left + FSideWidth;
        If FShowWeekNumbers Then
          H := H + 4;
        J := Ord(PSCGetRealFirstDayOfWeek(FFirstDayOfWeek));
        With R Do
          Begin
            For I := 0 To 6 Do
              Begin
                Left := H;
                Top := Rect.Top;
                H := H + FColWidth;
                Right := H;
                Bottom := Rect.Bottom;

                S:=GetDayName(j,WeekDayNames);

                aSize:=Canvas.TextExtent(S);

                MyPoint.X:=Left+ (Right - Left - aSize.cx) Div 2;
                MyPoint.Y:=Top + (Bottom - Top - aSize.cy) Div 2;

                AlignDayName(R,aSize,MyPoint);

                Canvas.TextRect(R,MyPoint.X,MyPoint.Y,S);

                If J = 7 Then
                  J := 1
                Else
                  J := J + 1
              End;
            Left := Rect.Left + FSideWidth;
            Right := Rect.Right - FSideWidth;
            If FShowWeekNumbers Then
              Begin
                Left := Left + 2;
                Right := Right + 4
              End;
            Top := Rect.Top;
            Bottom := Rect.Bottom
          End;
        DrawWeekLine(R)
      Finally
        If FWeekDayNames <> wdlOne Then
          Font := Self.Font
      End
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.DefaultDrawDay(Const Date: TDateTime;
  BkgndColor,TextColor: TPSCColor; FontStyle: TFontStyles;
  DateType: TPSCCalendarDateType; X,Y: Integer; Const Rect: TRect);
Var
  R: TRect;
  Size: TSize;
  S: String;
Begin
  With Canvas Do
    Begin
      S := PSCIntToStr(PSCGetDateDay(Date));
      Size:=Canvas.TextExtent(S);
      R := Rect;
      Canvas.TextRect(R,(R.Left + R.Right - Size.CX) Div 2,
        (R.Top + R.Bottom - Size.CY) Div 2,S);
      DrawDayOutline(Date,X,Y,Rect)
    End;
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.DrawDay(Const Date: TDateTime;
  BkgndColor,TextColor: TPSCColor; FontStyle: TFontStyles;
  Area: TPSCCalendarHitTest; DateType: TPSCCalendarDateType;
  X,Y: Integer; Const Rect: TRect);
Begin
  With Canvas Do
    Begin
      Font.Color := TextColor;
      If Font.Style <> FontStyle Then
        Font.Style := FontStyle;
      If Brush.Color <> BkgndColor Then
        Brush.Color := BkgndColor;
      If DoDrawItem(Date,Area,DateType,Rect) Then
        DefaultDrawDay(Date,BkgndColor,TextColor,FontStyle,DateType,X,Y,Rect);
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.DrawDayOutline(Const Date: TDateTime; X,Y: Integer;
  Const Rect: TRect);

  Procedure DrawLines;
  Begin
    With Canvas,Rect Do
      Begin
        If FShowHorzLines And (Y > 0) Then
          Begin
            If Pen.Color <> FColors.FDayLines Then
              Pen.Color := FColors.FDayLines;
            Pen.Style := FColors.DayLinesStyle;
            MoveTo(Left,Top);
            LineTo(Right,Top)
          End;
        If FShowVertLines And (X > 0) Then
          Begin
            If Pen.Color <> FColors.FDayLines Then
              Pen.Color := FColors.FDayLines;
            Pen.Style := FColors.DayLinesStyle;
            MoveTo(Left,Top);
            LineTo(Left,Bottom)
          End
      End
  End;

Var
  AParams: TPSCDrawDayOutlineParams;
Begin
  FillChar(AParams,SizeOf(AParams),0);
  AParams.DrawLines:=True;
  AParams.SetTodayColor:=True;

  If FShowToday And (Date = PSCDateOf(PSCNow)) Then
    AParams.ShowToday:=True;

  DoOnDrawDayOutline(Date,X,Y,Rect,AParams);

  With Canvas Do
    Begin
      If AParams.DrawLines Then
        DrawLines;
      If AParams.ShowToday Then
        Begin
          If AParams.SetTodayColor Then
            Brush.Color := FColors.NowRect;
          With Rect Do
            Canvas.FrameRect(PSCRect(Left + Ord(FShowVertLines),
              Top + Ord(FShowHorzLines),Right,Bottom));
        End
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.DoOnDrawDayOutline(Const ADate: TDateTime; X,Y: Integer;
  Const ARect: TRect; Var AParams: TPSCDrawDayOutlineParams);
begin
  If Assigned(FOnDrawDayOutline) Then
    FOnDrawDayOutline(Self,ADate,X,Y,ARect,AParams);
end;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.DrawDividers(MonthPos: TPSCMonthPos;
  Const Rect: TRect);
Var
  R: TRect;
Begin
  If FShowMonthDividers Then
    Begin
      If Not (mpLeft In MonthPos) Then
        Begin
          R := Rect;
          DrawEdge(Canvas.Handle,R,BDR_RAISEDOUTER,BF_LEFT Or BF_SOFT);
        End;
      If Not (mpRight In MonthPos) Then
        Begin
          R := Rect;
          DrawEdge(Canvas.Handle,R,BDR_RAISEDINNER,BF_RIGHT Or BF_SOFT)
        End
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.DrawWeekSide(Const Date: TDateTime;
  Side: TPSCCalendarHitTest; DateType: TPSCCalendarDateType; Const Rect: TRect);
Const
  FlatBounds: Array[Boolean] Of Integer = (1,0);
Var
  R: TRect;
  Size: TSize;
  S: String;
Begin
  With Canvas Do
    Begin
      FillRect(Rect);
      If (Side = chtWeekLeft) And FShowWeekNumbers And (DateType <> cdtHidden)
        Then
        Begin
          Font := FColors.WeekNumbersFont;
          Try
            S := PSCIntToStr(GetWeekNumber(Date));
            R := Rect;
            Size:=Canvas.TextExtent(S);
            Canvas.TextRect(R,R.Left + (FSideWidth - Size.cx) + 1,
              (R.Top + R.Bottom - Size.cy) Div 2,S);
            With Rect Do
              R := Classes.Rect(Left,Top - FlatBounds[not Flat],Right - 2,Bottom);
            MoveTo(R.Right, R.Top);
            LineTo(R.Right, R.Bottom);
          Finally
            Font := Self.Font;
          End
        End
    End
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendar.DoDrawItem(const ADate: TDateTime;
  AArea: TPSCCalendarHitTest;ADateType: TPSCCalendarDateType;
  ARect: TRect):WordBool;
begin
  Result := true;
  If Assigned(FOnDrawItem) Then
    FOnDrawItem(Self,0,AArea,ADateType,
      ARect.Left, ARect.Top, ARect.Right,ARect.Bottom,Result);
end;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.DrawDays(const ABounds:TRect;
  AMonthPos: TPSCMonthPos;const ABegDate,ABegMonth,AEndMonth:TDateTime);
Var
  RTop: Integer;
  RLeft: Integer;
  Y: Integer;
  R: TRect;
  PaintDate: TDateTime;
  PaintType: TPSCCalendarDateType;
  HiddenWeek: Boolean;
  DateSelected: Boolean;
  BkGndColor: TPSCColor;
  FontColor: TPSCColor;
  TextColor: TPSCColor;
  BkColor: TPSCColor;
  CustomFontStyle: TFontStyles;
  FontStyle: TFontStyles;
  WD: TPSCWeekDay;
  Area: TPSCCalendarHitTest;

  Procedure DrawWeekSides(Date: TDateTime);
  Var
    IsWeekSelected: Boolean;
    IsWeekSelected2: Boolean;
    RT: TRect;

    Function GetWeekColor: TPSCColor;
    Const
      WeekType: Array[Boolean] Of TPSCCalendarDateType = (
        cdtWeek,cdtSelectedWeek);
    Begin
      If IsWeekSelected Then
        Result := FColors.Selected
      Else
        Result := FColors.FWeekSide;
      TextColor := FontColor;
      DoGetPaintParams(PaintDate,TextColor,Result,CustomFontStyle,
        WeekType[IsWeekSelected])
    End;

  Begin
    R := Rect(ABounds.Left,RTop - FRowHeight,ABounds.Right,RTop);
    With Canvas Do
      Begin
        IsWeekSelected2 := Not FShowWeekNumbers And FWeeksSelected And
          CheckWeekSelected(Date);
        If (Date < ABegMonth) And (AMonthPos * [mpLeft,mpTop] <> [mpLeft,mpTop])
          Or
          (Date > AEndMonth) And
          (AMonthPos * [mpRight,mpBottom] <> [mpRight,mpBottom]) Then
          IsWeekSelected := false
        Else
          IsWeekSelected := IsWeekSelected2;
        BkColor := GetWeekColor;
        If Brush.Color <> BkColor Then
          Brush.Color := BkColor;
        RT := Rect(R.Left,R.Top,ABounds.Left + FSideWidth,R.Bottom);
        If FShowWeekNumbers Then
          RT.Right := RT.Right + 4;
        If HiddenWeek Then
          PaintType := cdtHidden
        Else
        If IsWeekSelected Then
            PaintType := cdtSelectedWeek
          Else
            PaintType := cdtWeek;
        If DoDrawItem(Date,chtWeekLeft,PaintType,RT) Then
          DrawWeekSide(Date,chtWeekLeft,PaintType,RT);
        If (Date + 6 < ABegMonth) And (AMonthPos * [mpLeft,mpTop] <>
          [mpLeft,mpTop]) Or
          (Date + 6 > AEndMonth) And
          (AMonthPos * [mpRight,mpBottom] <> [mpRight,mpBottom]) Then
          IsWeekSelected := false
        Else
          IsWeekSelected := IsWeekSelected2;
        BkColor := GetWeekColor;
        Brush.Color := BkColor;
        RT := Rect(ABounds.Right - FSideWidth,R.Top,R.Right,R.Bottom);
        If FShowWeekNumbers Then
          RT.Left := RT.Left + 4;
        If HiddenWeek Then
          PaintType := cdtHidden
        Else
        If IsWeekSelected Then
            PaintType := cdtSelectedWeek
          Else
            PaintType := cdtWeek;
        If DoDrawItem(Date,chtWeekRight,PaintType,RT) Then
          DrawWeekSide(Date,chtWeekRight,PaintType,RT);
      End;
    DrawDividers(AMonthPos,R);
  End;

  procedure MyDrawDaysRow;
  var
    X: Integer;
  begin
    With Canvas,R Do
    For X := 0 To 6 Do
      Begin
        Left := RLeft;
        RLeft := RLeft + FColWidth;
        Top := RTop - FRowHeight;
        Right := RLeft;
        Bottom := RTop;
        If WD = High(TPSCWeekDay) Then
          WD := Low(TPSCWeekDay);
        WD := Succ(WD);
        If Not RectVisible(Handle,R) Then
          Continue;
        PaintDate := ABegDate + X + Y * 7;
        Area := chtNowhere;
        DateSelected := SelDate[PaintDate];
        If (PaintDate < ABegMonth) Then
          If AMonthPos * [mpLeft,mpTop] = [mpLeft,mpTop] Then
            Begin
              PaintType := cdtGray;
              Area := chtLeftGray
            End
          Else
            PaintType := cdtHidden
        Else
          If (PaintDate > AEndMonth) Then
            If AMonthPos * [mpBottom,mpRight] = [mpBottom,mpRight] Then
              Begin
                PaintType := cdtGray;
                Area := chtRightGray
              End
            Else
              PaintType := cdtHidden
          Else
            PaintType := cdtNormal;
        If PaintType = cdtHidden Then
          Begin
            BkColor := BkGndColor;
            DoGetPaintParams(PaintDate,TextColor,BkColor,
              CustomFontStyle,cdtHidden);
            Brush.Color := BkColor;
            If DoDrawItem(PaintDate,chtHidden,cdtHidden,R) Then
              Begin
                FillRect(R);
                DrawDayOutline(0,X,Y,R)
              End;
            Continue
          End;
        HiddenWeek := false;
        If DateSelected Then
          PaintType := cdtSelected;
        Case PaintType Of
          cdtGray:
            Begin
              TextColor := FColors.Grayed;
              BkColor := BkGndColor
            End;
          cdtNormal:
            Begin
              If WD In FWorkDays Then
                TextColor := FontColor
              Else
                TextColor := FColors.FWeekEndText;
              BkColor := BkGndColor;
              Area := chtDay
            End;
          cdtSelected:
            Begin
              TextColor := FColors.SelectedText;
              BkColor := FColors.Selected;
              Area := chtDay
            End
        End;
        CustomFontStyle := FontStyle;
        DoGetPaintParams(PaintDate,TextColor,BkColor,
          CustomFontStyle,PaintType);
        DrawDay(PaintDate,BkColor,TextColor,CustomFontStyle,Area,
          PaintType,X,Y,R);
      End;
  end;

Begin
  With Canvas,R Do
  Begin
    FontStyle := Colors .DaysFont.Style;
    FontColor := Colors.DaysFont.Color;

    BkGndColor := Colors.Days;
    Try
      RTop := ABounds.Top + FDaysOfWeekHeight + 2 + FHeaderHeight;
      For Y := 0 To 5 Do
        Begin
          Left := ABounds.Left;
          Top := RTop;
          RTop := RTop + FRowHeight;
          Right := ABounds.Right;
          Bottom := RTop;
          If Not RectVisible(Handle,R) Then
            Continue;
          RLeft := ABounds.Left + FSideWidth;
          If FShowWeekNumbers Then
            RLeft := RLeft + 4;
          HiddenWeek := true;
          WD := Pred(PSCGetRealFirstDayOfWeek(FFirstDayOfWeek));

          MyDrawDaysRow;

          DrawWeekSides(ABegDate + Y * 7);
        End;
    Finally
      Brush.Color := BkGndColor;
      Font.Color := FontColor;
      Font.Style := FontStyle;
    End;
  End;
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.PaintMonth(MonthCol,MonthRow: Integer;
  Const Bounds: TRect);
Var
  MonthPos: TPSCMonthPos;
  BegDate,BegMonth,EndMonth: TDateTime;

Var
  R: TRect;
Begin
  If MonthCol = 0 Then
    MonthPos := [mpLeft]
  Else
    MonthPos := [];
  If MonthRow = 0 Then
    MonthPos := MonthPos + [mpTop];
  If MonthCol = MonthColCount - 1 Then
    MonthPos := MonthPos + [mpRight];
  If MonthRow = MonthRowCount - 1 Then
    MonthPos := MonthPos + [mpBottom];
  BegDate := GetMonthStartDate(MonthCol,MonthRow,true);
  BegMonth := GetMonthStartDate(MonthCol,MonthRow,false);
  EndMonth := PSCGetMonthEnd(BegMonth);
  With Bounds Do
    R := Rect(Left,Top,Right,Top + FHeaderHeight);

  Begin
    If DoDrawItem(BegMonth,chtMonthHead,cdtNormal,R) Then
      DrawHeader(BegMonth,MonthPos,R);
    If (mpTop In MonthPos) And FShowNavButtons Then
      Begin
        If (mpLeft In MonthPos) And (Not FMinDateLimit Or (FMinDate <= BegDate))
          Then
          Begin
            R := GetArrowRect(chtLeftArrow);
            OffsetRect(R,FMonthShiftX,FMonthShiftY);
            If DoDrawItem(BegMonth,chtLeftArrow,cdtNormal,R) Then
              DrawArrow(chtLeftArrow,R)
          End;
        If (mpRight In MonthPos) And (Not FMaxDateLimit Or
          (FMaxDate >= PSCIncMonth(FStartDate,MonthColCount * MonthRowCount))) Then
          Begin
            R := GetArrowRect(chtRightArrow);
            OffsetRect(R,FMonthShiftX,FMonthShiftY);
            If DoDrawItem(BegMonth,chtRightArrow,cdtNormal,R) Then
              DrawArrow(chtRightArrow,R)
          End
      End
  End;
  DrawDaysHeader(MonthPos,Rect(Bounds.Left,Bounds.Top + FHeaderHeight,
    Bounds.Right,Bounds.Top + FHeaderHeight + FDaysOfWeekHeight + 2));
  DrawDays(Bounds,MonthPos,BegDate,BegMonth,EndMonth);
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.DoGetPaintParams(Const Date: TDateTime;
  Var Color,BkColor: TPSCColor; Var FontStyle: TFontStyles;
  DateType: TPSCCalendarDateType);
var
  MyIsBold       : WordBool;
  MyIsItalic     : WordBool;
  MyIsUnderline  : WordBool;
  MyIsStrikeOut  : WordBool;
Begin
  If Assigned(FOnGetPaintParams) Then
  begin
    MyIsBold       := FontStyle_Bold in FontStyle;
    MyIsItalic     := FontStyle_Italic in FontStyle;
    MyIsUnderline  := FontStyle_Underline in FontStyle;
    MyIsStrikeOut  := FontStyle_StrikeOut in FontStyle;

    FOnGetPaintParams(Self,Date,Color,BkColor,MyIsBold, MyIsItalic,
      MyIsUnderline, MyIsStrikeOut,DateType);

    FontStyle      := [];

    If MyIsBold then
      FontStyle := FontStyle+[FontStyle_Bold];
    If MyIsItalic then
      FontStyle := FontStyle+[FontStyle_Italic];
    If MyIsUnderline then
      FontStyle := FontStyle+[FontStyle_Underline];
    If MyIsStrikeOut then
      FontStyle := FontStyle+[FontStyle_StrikeOut];
  end;
End;

{------------------------------------------------------------------------------}

Const
  EdgeKind: Array[Boolean] Of UINT = (BDR_SUNKENOUTER,EDGE_SUNKEN);

Procedure TPSCCustomCalendar.Paint;
Var
  Row,Col,ColCount,RowCount: Integer;
  MonthWidth,MonthHeight: Integer;
  Bounds: TRect;

  procedure NCPaint;
  var
    RC,RC1: TRect;
    delta: Integer;
    SaveBrushColor:TPSCColor;
  begin
    SaveBrushColor:=Canvas.Brush.Color;
    Canvas.Brush.Color:=FColors.Border;
    RC:=UpdateTopLeftCorner(ClientRect);
    RC.Right:=RC.Left+GetMonthWidth*(ColCount+1);
    RC.Bottom:=RC.Top+GetMonthHeight*(RowCount+1);

    try
      RC1:=RC;
      InflateRect(RC1, -1, -1);
      PSCFillRectExclude(Canvas,Rect(0,0,ClientWidth,ClientHeight),RC1);

      if BorderStyle<>bsNone then
      begin
        Delta:=2;
        InflateRect(RC, delta, delta);
        DrawEdge(Canvas.Handle, RC, EdgeKind[not Flat], BF_ADJUST or BF_RECT);

        If Flat then
        begin
          Canvas.Pen.Color:=clPSCBtnFace;
          Canvas.Brush.Style:=BrushStyle_Clear;
          Canvas.Rectangle(RC);
        end;

      end;
    finally
      Canvas.Brush.Color:=SaveBrushColor;
    end;
  end;

Begin
  ColCount := MonthColCount - 1;
  RowCount := MonthRowCount - 1;
  MonthWidth := GetMonthWidth;
  MonthHeight := GetMonthHeight;
  For Row := 0 To RowCount Do
    For Col := 0 To ColCount Do
    Begin
      With Bounds Do
        Begin
          Left := MonthWidth * Col;
          Top := MonthHeight * Row;
          Right := Left + MonthWidth;
          Bottom := Top + MonthHeight;
        End;
      Bounds := UpdateTopLeftCorner(Bounds);
      PaintMonth(Col,Row,Bounds);
    End;
  NCPaint;
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendar.GetMonthWidth: Integer;
Begin
  Result := FSideWidth * 2 + FColWidth * 7;
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendar.GetMonthHeight: Integer;
Begin
  Result := FHeaderHeight + FDaysOfWeekHeight + FRowHeight * 6 + 2
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendar.SelectionIsInOneWeek: boolean;
Var
  WeekStartDate,WeekEndDate: TDateTime;
Begin
  WeekStartDate := PSCDateOf(PSCGetWeekStartEx(SelStart,FirstDayOfWeek));
  WeekEndDate := PSCDateOf(PSCGetWeekEndEx(SelFinish,FirstDayOfWeek));

  Result := (PSCDateOf(SelStart) >= WeekStartDate) And (PSCDateOf(SelFinish) <=
    WeekEndDate)
    And (WeekEndDate - WeekStartDate = 6);
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendar.GetEndDate: TDateTime;
Begin
  Result := GetMonthStartDate(MonthColCount - 1,MonthRowCount - 1,true) + (7 *
    6);
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.SelectWorkWeek(Const ADate: TDateTime);
Var
  DateIterator: TDateTime;
Begin
  BeginUpdate;
  Try
    ClearSelection;
    ChangeSelection(GetBeginOfWeek(ADate),GetEndOfWeek(ADate));
    DateIterator := PSCDateOf(SelStart);
    While DateIterator <= PSCDateOf(SelFinish) Do
      Begin
        If Not PSCIsDateInDays(DateIterator,WorkDays) Then
          SelDate[DateIterator] := False;
        DateIterator := DateIterator + 1;
      End;
  Finally
    EndUpdate;
  End;
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.SelectionToView;
Begin
  BeginUpdate;
  Try
    If (SelStart > EndDate - 14) Or (SelStart < StartDate) Then
      StartDate := SelStart;
  Finally
    EndUpdate;
  End;
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendar.WorkDaysSelected: boolean;
Var
  DateIterator: TDateTime;
  DateIsSelected: boolean;
  DateInWorkDays: boolean;

Begin
  Result := SelectionIsInOneWeek And (SelCount = PSCGetWorkDaysCount(WorkDays));
  If Result Then
    Begin
      DateIterator := PSCDateOf(SelStart);
      While DateIterator <= PSCDateOf(SelFinish) Do
        Begin
          DateIsSelected := SelDate[DateIterator];
          DateInWorkDays := PSCIsDateInDays(DateIterator,WorkDays);

          Result := DateIsSelected = DateInWorkDays;

          If Not Result Then
            exit;

          DateIterator := DateIterator + 1;
        End;
    End;
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.RemeasureCalendar;
Begin
  FontChanged;
End;

{------------------------------------------------------------------------------}

procedure TPSCCustomCalendar.DoMeasureItem(AArea: TPSCCalendarHitTest;
  Var AWidth,AHeight: Integer);
begin
  If Assigned(FOnMeasureItem) then
    FOnMeasureItem(Self,AArea,AWidth,AHeight);
end;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.FontChanged;
Var
  Size: TSize;
  Delta: Integer;
begin
  With FColors Do
  If HandleAllocated Then
    Begin
      With Canvas Do
        Begin
          Font := Self.Font;
          Size := Canvas.TextExtent('00');
          FColWidth := Size.cx + 5;
          FRowHeight := Size.cy + 2;
          FSideWidth := Size.cx;
          FDaysOfWeekHeight := FRowHeight + 1;
          Font := FColors.HeaderFont;
          Size := Canvas.TextExtent('0');
          FHeaderHeight := Size.cy + 4;
          
          DoMeasureItem(chtDay,FColWidth,FRowHeight);
          DoMeasureItem(chtWeekLeft,FSideWidth,Delta);
          DoMeasureItem(chtMonthHead,Delta,FHeaderHeight);
          DoMeasureItem(chtWeekDays,Delta,FDaysOfWeekHeight);
          DoMeasureItem(chtFooter,Delta,FFooterHeight);
        End;
      BorderChanged;
    End;
end;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.CMFontChanged(Var Message: TMessage);
Begin
  Inherited;
  FontChanged;
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.SetStartDate(Value: TDateTime);
Var
  Date: TDateTime;
Begin
  Value := CorrectStartDate(Value);
  If Not HandleAllocated Then
    FStartDate := Value
  Else
    Begin
      If FMaxDateLimit Then
        Begin
          Date := PSCIncMonth(PSCGetMonthStart(FMaxDate),1 - MonthColCount *
            MonthRowCount);
          If Value > Date Then
            Value := Date
        End;
      If FMinDateLimit Then
        Begin
          Date := PSCGetMonthStart(FMinDate);
          If Value < Date Then
            Value := Date
        End;
      If FStartDate <> Value Then
        Begin
          FStartDate := Value;
          Invalidate;
        End;
    End;
  If Assigned(FOnStartDateChange) then
    FOnStartDateChange(Self);
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.SetShowNavButtons(Value: Boolean);
Var
  Rect: TRect;
Begin
  If FShowNavButtons <> Value Then
    Begin
      FShowNavButtons := Value;
      If HandleAllocated Then
        Begin
          Rect := GetArrowRect(chtLeftArrow);
          InvalidateRect(Rect);
          Rect := GetArrowRect(chtRightArrow);
          InvalidateRect(Rect);
        End
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.SetShowToday(Value: Boolean);
Var
  Rect: TRect;
Begin
  If FShowToday <> Value Then
    Begin
      FShowToday := Value;
      If HandleAllocated And DateToRect(PSCDateOf(PSCNow),Rect) Then
        InvalidateRect(Rect);
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.SetShowMonthDividers(Value: Boolean);
Begin
  If FShowMonthDividers <> Value Then
    Begin
      FShowMonthDividers := Value;
      Invalidate
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.SetShowWeekNumbers(Value: Boolean);
Begin
  If FShowWeekNumbers <> Value Then
    Begin
      FShowWeekNumbers := Value;
      Invalidate
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.SetShowHorzLines(Value: Boolean);
Begin
  If FShowHorzLines <> Value Then
    Begin
      FShowHorzLines := Value;
      Invalidate
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.SetShowVertLines(Value: Boolean);
Begin
  If FShowVertLines <> Value Then
    Begin
      FShowVertLines := Value;
      Invalidate
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.SetWeekDayNames(Value: TPSCWeekDaysLength);
Begin
  If FWeekDayNames <> Value Then
    Begin
      FWeekDayNames := Value;
      Invalidate
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.SetFirstWeekOfYear(Value: TPSCFirstWeekOfYear);
Begin
  If FFirstWeekOfYear <> Value Then
    Begin
      FFirstWeekOfYear := Value;
      Invalidate
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.SetColors(Value: TPSCCalendarColors);
Begin
  If FColors <> Value Then
    FColors.Assign(Value);
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.InvalidateSelection;
Var
  Rect: TRect;
  Date: TDateTime;
Begin
  If HandleAllocated Then
    Begin
      Date := FSelStart;
      While Date <= FSelFinish Do
        Begin
          If DateToRect(Date,Rect) Then
            InvalidateRect(Rect);
          Date := Date + 1;
          If (False {SelectKind = skWeekSel}) Then
            InvalidateWeek(FSelStart)
        End
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.InternalSetCursor(Value: TDateTime);
Begin
  If (MinDateLimit And (Value < MinDate)) Or
    (MaxDateLimit And (Value > MaxDate)) Then
    Exit;

  If Not CanSelectionChange(Value) Then
    Exit;
  InvalidateSelection;
  If False {SelectKind = skWeekSel} Then
    Begin
      Value := GetBeginOfWeek(Value);
      FSelStart := Value;
      FSelFinish := Value + 6;
      FCursorDate := FSelStart;
      FWeeksSelected := true;
      FSelCount := 7
    End
  Else
    Begin
      FSelStart := Value;
      FSelFinish := Value;
      FCursorDate := FSelStart;
      FSelCount := 1
    End;
  FModified := true;
  UpdateSelection;
  InvalidateSelection
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.SetCursorDate(Value: TDateTime);
Begin
  Value := Int(Value);
  If (FSelCount = 0) Or (FCursorDate <> Value) Then
    If FMultiSelect Then
      Begin
        ClearWeekSelection;
        BeginUpdate;
        Try
          SelDate[FCursorDate] := false;
          FCursorDate := Value;
          SelDate[FCursorDate] := true
        Finally
          EndUpdate
        End
      End
    Else
      Begin
        If FMinDateLimit And (Value < FMinDate) Or
          FMaxDateLimit And (Value > FMaxDate) Then
          Exit;
        If (Value < FStartDate) Or
          (Value >= PSCIncMonth(FStartDate,MonthColCount * MonthRowCount))
        Then
          StartDate := Value;
        InternalSetCursor(Value)
      End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.SetFirstDayOfWeek(Value: TPSCFirstDayOfWeek);
Begin
  If FFirstDayOfWeek <> Value Then
    Begin
      FFirstDayOfWeek := Value;
      Invalidate
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.SetWorkDays(Value: TPSCWeekDays);
Begin
  If FWorkDays <> Value Then
    Begin
      FWorkDays := Value;
      Invalidate
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.MoveSelection(Delta: Integer);
Var
  Current: TDateTime;
  Week: Integer;
Begin
  If Not FMultiSelect Then
    SelStart := FSelStart + Delta
  Else
    Begin
      BeginUpdate;
      Try
        If Delta > 0 Then
          Begin
            If (FSelCount = 0) Or FMaxDateLimit And (FSelFinish + Delta >
              FMaxDate) Then
              Exit;
            Current := FSelFinish;
            Week := 0;
            Repeat
              If Week < 6 Then
                Week := Week + 1
              Else
                Begin
                  Week := 0;
                  InvalidateWeek(Current);
                  InvalidateWeek(Current + Delta)
                End;
              If SelDate[Current] Then
                Begin
                  SelDate[Current] := false;
                  SelDate[Current + Delta] := true
                End;
              Current := Current - 1
            Until (Current < FSelStart) Or (FSelCount = 0);
            If (FSelCount > 0) And
              (PSCIncMonth(FStartDate,MonthColCount * MonthRowCount) <= FSelFinish)
                Then
              StartDate := FSelStart
          End
        Else
          Begin
            If (FSelCount = 0) Or FMinDateLimit And (FSelFinish + Delta <
              FMinDate) Then
              Exit;
            Current := FSelStart;
            Week := 0;
            Repeat
              If Week < 6 Then
                Week := Week + 1
              Else
                Begin
                  Week := 0;
                  InvalidateWeek(Current);
                  InvalidateWeek(Current + Delta)
                End;
              If SelDate[Current] Then
                Begin
                  SelDate[Current] := false;
                  SelDate[Current + Delta] := true
                End;
              Current := Current + 1
            Until (Current > FSelFinish) Or (FSelCount = 0);
            If (FSelCount > 0) And (FStartDate > FSelStart) Then
              StartDate := FSelStart
          End;
        FCursorDate := FCursorDate + Delta
      Finally
        EndUpdate
      End
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.Select(Delta: Integer; Kind: Boolean);
Var
  Date: TDateTime;
  Step: Integer;
  Value,Value2: Boolean;
Begin
  If (Delta = 0) Or Not FMultiSelect Or (FSelCount = 0) Or
    Not SelDate[FCursorDate] Then
    Exit;
  BeginUpdate;
  Try
    If Kind Then
      Begin
        Date := FCursorDate;
        While Delta <> 0 Do
          Begin
            If Delta > 0 Then
              Begin
                Delta := Delta - 1;
                Date := Date + 1
              End
            Else
              Begin
                Delta := Delta + 1;
                Date := Date - 1
              End;
            If SetSelDate(Date,true) Then
              FCursorDate := Date
          End
      End
    Else
      Begin
        Date := FCursorDate;
        Step := Ord(Delta > 0) * 2 - 1;
        Value := SelDate[Date - Step];
        Value2 := SelDate[Date + Step];
        If Value And Value2 Then
          While SelDate[Date + Step] Do
            Date := Date + Step;
        Value := Value Or Not Value2;
        If Delta < 0 Then
          Delta := -Delta;
        For Delta := Delta Downto 1 Do
          Begin
            If Value Then
              Begin
                If Not SetSelDate(Date + Step,true) Then
                  Break
              End
            Else
              If Not SelDate[Date + Step] Then
                Break
              Else
                SetSelDate(Date,false);
            Date := Date + Step
          End;
        FCursorDate := Date
      End
  Finally
    EndUpdate
  End;
  ClearWeekSelection;
  If (FCursorDate < FStartDate) Or
    (FCursorDate >= PSCIncMonth(FStartDate,MonthColCount * MonthRowCount))
  Then
    StartDate := FCursorDate;
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.SelectToPrevDay;
Begin
  MoveSelection(-1)
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.SelectToNextDay;
Begin
  MoveSelection(1)
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.SelectToPrevWeek;
Begin
  MoveSelection(-7)
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.SelectToNextWeek;
Begin
  MoveSelection(7)
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.SelectToWeekBeg;
Var
  Delta: Integer;
Begin
  Delta := Trunc(GetBeginOfWeek(FCursorDate) - FCursorDate);
  If Delta <> 0 Then
    MoveSelection(Delta);
  ClearWeekSelection
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.SelectToWeekEnd;
Var
  Delta: Integer;
Begin
  Delta := Trunc(GetEndOfWeek(FCursorDate) - FCursorDate);
  If Delta <> 0 Then
    MoveSelection(Delta);
  ClearWeekSelection
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.SelectToMonthBeg;
Var
  Delta: Integer;
Begin
  Delta := Trunc(PSCGetMonthStart(FCursorDate) - FCursorDate);
  If Delta <> 0 Then
    MoveSelection(Delta);
  ClearWeekSelection
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.SelectToMonthEnd;
Var
  Delta: Integer;
Begin
  Delta := Trunc(PSCGetMonthEnd(FCursorDate) - FCursorDate);
  If Delta <> 0 Then
    MoveSelection(Delta);
  ClearWeekSelection
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.SelectToCalendBeg;
Var
  Delta: Integer;
Begin
  Delta := Trunc(GetBeginOfWeek(PSCGetMonthStart(FCursorDate)) - FCursorDate);
  If Delta=0 then
    Delta:=-1;
  If Delta <> 0 Then
    MoveSelection(Delta);
  ClearWeekSelection;
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.SelectToCalendEnd;
Var
  Delta: Integer;
Begin
  Delta := Trunc(GetBeginOfWeek(PSCGetMonthStart(FCursorDate)) + (7 * 6 - 1)
    - FCursorDate);
  If Delta <> 0 Then
    MoveSelection(Delta);
  ClearWeekSelection
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendar.GetAutoSize: boolean;
Begin
  Result := [beLeft,beTop,beRight,beBottom] * BorderEdges = [];
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.SetAutoSize(V: boolean);
Begin
  If GetAutoSize <> V Then
    Begin
      If V Then
        BorderEdges := BorderEdges - [beLeft,beTop,beRight,beBottom]
      Else
        BorderEdges := BorderEdges + [beLeft,beTop,beRight,beBottom];
    End;
End;

{------------------------------------------------------------------------------}

procedure TPSCCustomCalendar.PrintWithDialog;
var
  MyDialog:TPrintDialog;
begin
  MyDialog:=TPrintDialog.Create(nil);
  With MyDialog do
  try
    If Execute then
      Print;
  finally
    MyDialog.Free;
  end;
end;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.Print;
Var
  LeftMargin,TopMargin: Integer;
  PageWidth,PageHeight: Integer;
  XPages,YPages,Pages: Integer;
  MetaFile: TPSCMetaFile;
  CalRect: TRect;
  PrnHeaderHeight: Integer;


  Procedure _InitPrintInfo;
  Var
    PX,PY,CX,CY: Integer;
    DevOffsetX,DevOffsetY: Integer;
    tm: TTextMetric;
  Begin
    PX := GetDeviceCaps(Printer.Handle,LOGPIXELSX);
    PY := GetDeviceCaps(Printer.Handle,LOGPIXELSY);
    CX := GetDeviceCaps(Canvas.Handle,LOGPIXELSX);
    CY := GetDeviceCaps(Canvas.Handle,LOGPIXELSY);

    Printer.Canvas.Font := Font;

    DevOffsetX := GetDeviceCaps(Printer.Handle,PHYSICALOFFSETX);
    DevOffsetY := GetDeviceCaps(Printer.Handle,PHYSICALOFFSETY);
    LeftMargin := (GetDeviceCaps(Printer.Handle,PHYSICALWIDTH) -
      Printer.PageWidth) Div 2 +
      DevOffsetX;
    TopMargin := (GetDeviceCaps(Printer.Handle,PHYSICALHEIGHT) -
      Printer.PageHeight) Div 2 +
      DevOffsetY;
    PageWidth := ((Printer.PageWidth - DevOffsetX * 2) * CX) Div PX;
    PageHeight := ((Printer.PageHeight - DevOffsetY * 2) * CY) Div PY;
    MetaFile.Width := PageWidth;
    MetaFile.Height := PageHeight;
    PrnHeaderHeight := 0;
    If poHeader In FPrintOptions.Options Then
      Begin
        Canvas.Font := FPrintOptions.FHeaderFont;
        GetTextMetrics(Canvas.Handle,tm);
        PrnHeaderHeight := tm.tmHeight;
        Dec(PageHeight,PrnHeaderHeight);
      End;
  End;

  Procedure _Step;
  Begin
    If Assigned(FOnPrintProgress) Then
      FOnPrintProgress(Self);
    Application.ProcessMessages;
  End;

  Procedure _PrintCalendar(AShiftX,AShiftY: Integer; Const AClipRect: TRect);
  Var
    Col,Row,ColCount,RowCount: Integer;
    MonthWidth,MonthHeight: Integer;
    Bounds,Temp: TRect;
  Begin
    ColCount := MonthColCount - 1;
    RowCount := MonthRowCount - 1;
    MonthWidth := GetMonthWidth;
    MonthHeight := GetMonthHeight;

    FMonthShiftX := AShiftX;
    FMonthShiftY := AShiftY;
    Try
      For Row := 0 To RowCount Do
        For Col := 0 To ColCount Do
          Begin
            With Bounds Do
              Begin
                Left := AShiftX + MonthWidth * Col;
                Top := AShiftY + MonthHeight * Row;
                Right := Left + MonthWidth;
                Bottom := Top + MonthHeight
              End;
            Bounds := UpdateTopLeftCorner(Bounds);
            If IntersectRect(Temp,AClipRect,Bounds) Then
              PaintMonth(Col,Row,Bounds);
          End;
    Finally
      FMonthShiftX := 0;
      FMonthShiftY := 0;
    End;
  End;

  Procedure _PrintPage(DC: HDC; X,Y: Integer);
  Var
    PageNo,PageNoLen: Integer;
    PageNoStr: String;
    ShiftX,ShiftY: Integer;
    Rgn: THandle;
    ClipRect: TRect;
  Begin
    PageNo := Y * (XPages + 1) + X + 1;

    FPrintedPageCurrent := PageNo;
    FPrintedPageTotal := Pages;

    ShiftX := -X * (PageWidth - FPrintOptions.Overlap);
    ShiftY := -Y * (PageHeight - PrnHeaderHeight - FPrintOptions.Overlap);
    FillChar(ClipRect,SizeOf(ClipRect),0);

    Canvas.Lock;
    Try
      Canvas.Handle := DC;
      If poHeader In FPrintOptions.Options Then
        Begin
          If FPrintOptions.ParentHeaderFont Then
            Canvas.Font := Font
          Else
            Canvas.Font := FPrintOptions.HeaderFont;

          Canvas.TextOut(0,0,FPrintOptions.FHeader);
          PageNoStr := PSCFormat(PSCConsts.PageNoStr, [PageNo,Pages]);
          PageNoLen := Canvas.TextWidth(PageNoStr);
          If poPrintPageNumber in FPrintOptions.Options then
            Canvas.TextOut(PageWidth - PageNoLen,0,PageNoStr);
          ShiftY := ShiftY + PrnHeaderHeight;
          ClipRect.Top := PrnHeaderHeight;
          If Not FPrintOptions.ParentHeaderFont Then
            Canvas.Font := Font;
        End;

      If BorderStyle <> bsNone Then
        Begin
          Inc(ShiftX);
          Inc(ShiftY);
        End;

      With ClipRect Do
        Begin
          Right := PageWidth;
          Bottom := PageHeight;
        End;
      Rgn := CreateRectRgnIndirect(ClipRect);
      SelectClipRgn(DC,Rgn);
      OffsetRect(CalRect,ShiftX,ShiftY);
      CalRect := UpdateTopLeftCorner(CalRect);
      Try
        If BorderStyle <> bsNone Then
          Begin
            DrawEdge(DC,CalRect,EdgeKind[not Flat],BF_ADJUST Or BF_RECT);
            InflateRect(CalRect,1,1);
          End;
        _PrintCalendar(ShiftX,ShiftY,ClipRect);
      Finally
        DeleteObject(Rgn);
        OffsetRect(CalRect, -ShiftX, -ShiftY);
      End;
    Finally
      Canvas.Handle := 0;
      Canvas.Unlock;
    End;
  End;

  Procedure _PrintPages;
  Var
    I,J,Shift: Integer;
    MetaFileCanvas: TMetaFileCanvas;
    ClipRect: TRect;
    Rgn: THandle;
  Begin
    Shift := 0;
    If BorderStyle <> bsNone Then
      Shift := 2;
    With CalRect Do
      Begin
        Left := -1;
        Top := -1;
        Right := GetMonthWidth * MonthColCount;
        Bottom := GetMonthHeight * MonthRowCount;
        XPages := (Right + Shift) Div (PageWidth - FPrintOptions.Overlap);
        YPages := (Bottom + Shift) Div (PageHeight - PrnHeaderHeight -
          FPrintOptions.Overlap);
        Inc(Right);
        Inc(Bottom);
      End;

    Pages := (XPages + 1) * (YPages + 1);

    FPrintedPageTotal := Pages;

    Printer.Title := FPrintOptions.Title;

    Printer.BeginDoc;
    Try
      For I := 0 To YPages Do
        For J := 0 To XPages Do
          Begin
            If FCancelPrinting Then
              Exit;
            If I + J <> 0 Then
              Printer.NewPage;
            MetaFileCanvas := TMetaFileCanvas.Create(MetaFile,0);
            Try
              _PrintPage(MetaFileCanvas.Handle,J,I);
            Finally
              MetaFileCanvas.Free;
              _Step;
            End;

            ClipRect :=
              Rect(LeftMargin,TopMargin,Printer.PageWidth,Printer.PageHeight);
            Rgn := CreateRectRgnIndirect(ClipRect);
            SelectClipRgn(Printer.Canvas.Handle,Rgn);
            Printer.Canvas.StretchDraw(ClipRect,MetaFile);
            DeleteObject(Rgn);
          End;
    Finally
      Printer.EndDoc;
    End;
  End;

Begin
  FCancelPrinting := False;
  MetaFile := TPSCMetaFile.Create;
  Try
    _InitPrintInfo;
    _PrintPages;
  Finally
    MetaFile.Free;
    MetaFile := Nil;
  End;
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendar.SetPrintOptions(
  Const Value: TPSCCalendarPrintOptions);
Begin
  FPrintOptions.Assign(Value);
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendar.DateInRange(Const ADate: TDateTime): Boolean;
Begin
  Result := Not ((MinDateLimit And (ADate < MinDate)) Or
    (MaxDateLimit And (ADate > MaxDate)));
End;

{------------------------------------------------------------------------------}

Procedure TPSCCalendarPrintOptions.Assign(Source: TPersistent);
Var
  ASource: TPSCCalendarPrintOptions;
Begin
  Inherited;
  If Source Is TPSCCalendarPrintOptions Then
    Begin
      ASource := TPSCCalendarPrintOptions(Source);
      FHeader := ASource.FHeader;
      FOptions := ASource.Options;
      FTitle := ASource.FTitle;
      FCalendar := ASource.FCalendar;
      FOverlap := ASource.FOverlap;
      FHeaderFont.Assign(ASource.FHeaderFont);
      FParentHeaderFont := ASource.FParentHeaderFont;
    End;
End;

{------------------------------------------------------------------------------}

Function TPSCCalendarPrintOptions.IsHeaderFontStored: boolean;
Begin
  Result := FParentHeaderFont <> True;
End;

{------------------------------------------------------------------------------}

Constructor TPSCCalendarPrintOptions.Create(AOwner: TPSCCustomCalendar);
Begin
  Inherited Create;
  FCalendar := AOwner;
  FOptions := [poHeader];
  FHeaderFont := TPSCFont.Create;
  FParentHeaderFont := True;
End;

{------------------------------------------------------------------------------}

Destructor TPSCCalendarPrintOptions.Destroy;
Begin
  FHeaderFont.Free;
  Inherited;
End;

{------------------------------------------------------------------------------}

Procedure TPSCCalendarPrintOptions.SetHeaderFont(Value: TPSCFont);
Begin
  FHeaderFont.Assign(Value);
  FParentHeaderFont := False;
End;

{------------------------------------------------------------------------------}

Procedure TPSCCalendarPrintOptions.SetParentHeaderFont(Value: Boolean);
Begin
  If FParentHeaderFont = Value Then
    Exit;
  FParentHeaderFont := Value;
  If Value Then
    FHeaderFont.Assign(FCalendar.Font);
End;

Const
  ArrowPointCount = 7;
  ArrowPoints: Array[0..ArrowPointCount - 1] Of TPoint = (
    (x: - 6; y: - 1),
    (x: 0; y: - 1),
    (x: 0; y: - 5),
    (x: 5; y: 0),
    (x: 0; y: 5),
    (x: 0; y: 1),
    (x: - 6; y: 1));

Procedure PSCDrawMSMoneyArrow(Canvas: TPSCCanvas; LeftArrow: Boolean;
  Const Rect: TRect);
Var
  I: Integer;
  Points: Array[0..ArrowPointCount - 1] Of TPoint;
  P: TPoint;
Begin
  With Canvas,Rect Do
    Begin
      For I := 0 To 6 Do
        With Points[I] Do
          Begin
            P := ArrowPoints[I];
            If LeftArrow Then
              P.x := -P.x;
            x := (Left + Right) Div 2 + P.x * (Right - Left) Div 32;
            y := (Top + Bottom) Div 2 + P.y * (Bottom - Top) Div 24
          End;
      Polygon(Points)
    End
End;

{------------------------------------------------------------------------------}

Constructor TPSCCalendarColorsPro.Create(AOwner: TPSCCustomCalendar);
Begin
  Inherited Create(AOwner);
  FHolidaysFont := TPSCParentedFont.Create(AOwner);
  With FHolidaysFont Do
    Begin
      Assign(TPSCCustomCalendarPro(Owner).Font);
      Color := clHolidayTextColor;
      Style := Style + [FontStyle_Bold];
      OnChange := FontChanged
    End;
  FArrowColor := clArrowColor;
  FWeekLineColor := clCalendarWeekLine;
  FHeaderBorderColor := clCalendarHeaderBorder;
  FGrayedBkColor := clCalendarGrayBk;
End;

{------------------------------------------------------------------------------}

Destructor TPSCCalendarColorsPro.Destroy;
Begin
  FHolidaysFont.Free;
  Inherited Destroy
End;

{------------------------------------------------------------------------------}

Procedure TPSCCalendarColorsPro.Assign(Source: TPersistent);
Begin
  If Source Is TPSCCalendarColorsPro Then
    With TPSCCalendarColorsPro(Source) Do
      Begin
        Self.FHolidaysFont.Assign(FHolidaysFont);
        Self.FArrowColor := FArrowColor
      End;
  Inherited Assign(Source)
End;

{------------------------------------------------------------------------------}

Procedure TPSCCalendarColorsPro.SetHolidaysFont(Value: TPSCFont);
Begin
  If FHolidaysFont <> Value Then
    FHolidaysFont.Assign(Value)
End;

{------------------------------------------------------------------------------}

Procedure TPSCCalendarColorsPro.SetArrowColor(Value: TPSCColor);
Begin
  If FArrowColor <> Value Then
    Begin
      FArrowColor := Value;
      With TPSCCustomCalendarPro(Owner) Do
        Begin
          InvalidateArrow(chtLeftArrow);
          InvalidateArrow(chtRightArrow)
        End
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendarPro.SetDefaultHolidays(V: boolean);
Begin
  If FDefaultHolidays <> V Then
    Begin
      FDefaultHolidays := V;
      Invalidate;
    End;
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendarPro.ItemsOnGetHolidayDate(Sender: TObject;
  Holiday: TPSCHolidayItem; Const BaseDate: TDateTime; Var ADate: TDateTime);
Begin
  DoOnGetHolidayDate(Holiday,BaseDate,ADate);
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendarPro.DoOnGetHolidayDate(Holiday: TPSCHolidayItem;
  Const BaseDate: TDateTime; Var ADate: TDateTime);
Begin
  If Assigned(FOnGetHolidayDate) Then
    FOnGetHolidayDate(Self,Holiday,BaseDate,ADate);
End;

{------------------------------------------------------------------------------}

Constructor TPSCCustomCalendarPro.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);
  FHolidayNames := PSCCreateStringList;
  FCalendarStyle := cstOutlook;
  FPopupType := ptStandard;
  FLeftArrow := -1;
  FLeftArrowHot := -1;
  FRightArrow := -1;
  FRightArrowHot := -1;
  FDefaultHolidays := True;
  FHolidays := TPSCHolidayItems.Create(Self,TPSCHolidayItem);
  FHolidays.OnChange := HolidaysChange;
  FHolidays.OngetHolidayDate := ItemsOnGetHolidayDate;
  FHeaderCase := tcDefault;
  ShowHint := True;
End;

{------------------------------------------------------------------------------}

Destructor TPSCCustomCalendarPro.Destroy;
Begin
  FMonths.Free;
  FMonths := Nil;
  FHolidays.Free;
  Inherited;
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendarPro.CreateCalendarColors: TPSCCalendarColors;
Begin
  Result := TPSCCalendarColorsPro.Create(Self)
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendarPro.Notification(AComponent: TComponent;
  Operation: TOperation);
Begin
  Inherited Notification(AComponent,Operation);
  If (AComponent = FImages) And (Operation = opRemove) Then
    FImages := Nil
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendarPro.DrawHeader(Const Date: TDateTime;
  MonthPos: TPSCMonthPos; Var Rect: TRect);
Const
  LeftEdge: Array[Boolean] Of Cardinal = (0,BF_LEFT);
  TopEdge: Array[Boolean] Of Cardinal = (0,BF_TOP);
  RightEdge: Array[Boolean] Of Cardinal = (0,BF_RIGHT);
Var
  ASize: TSize;
  S: String;
Begin
  If FCalendarStyle <> cstMSMoney Then
    Inherited DrawHeader(Date,MonthPos,Rect)
  Else
    Begin
      With Canvas Do
        Begin
          If not Flat Then
            DrawEdge(Canvas.Handle,Rect,BDR_RAISEDINNER,BF_ADJUST Or BF_TOP Or
              BF_BOTTOM Or LeftEdge[mpLeft In MonthPos] Or
              RightEdge[mpRight In MonthPos]);
          S := GetHeaderText(Date);
          If Brush.Color <> Colors.MonthHeader Then
            Brush.Color := Colors.MonthHeader;
          Font := Colors.HeaderFont;
          Try
            ASize:=Canvas.TextExtent(S);
            With Rect Do
              Canvas.TextRect(Rect,(Right + Left - ASize.cx) Div 2,
                (Top + Bottom - ASize.cy) Div 2,S);
          Finally
            Font := Self.Font;
          End;
          If not Flat Then
            Begin
              InflateRect(Rect,0, -1);
              DrawEdge(Canvas.Handle,Rect,BDR_RAISEDINNER,BF_ADJUST Or
                LeftEdge[Not (mpLeft In MonthPos)] Or
                RightEdge[Not (mpRight In MonthPos)])
            End
        End
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendarPro.DrawArrow(Arrow: TPSCCalendarHitTest;
  Const Rect: TRect);
Const
  SelectedTest: Array[TPSCSelectedArrow] Of TPSCCalendarHitTest = (
    chtNowhere,chtLeftArrow,chtRightArrow);
  Edges: Array[Boolean] Of Cardinal = (BDR_RAISEDINNER,BDR_SUNKENOUTER);
Var
  Hot,Down: Boolean;
  Delta: Integer;
  R: TRect;
  Rgn: TPSCRegion;

  Procedure DrawOutlookArrow;
  Begin
    With Rect Do
      Inherited DrawArrow(Arrow,
        Classes.Rect(Left + Delta,Top + Delta,Right + Delta,Bottom + Delta))
  End;

  Procedure DrawMSMoneyArrow;
  Begin
    With Canvas,Rect Do
      Begin
        If Pen.Color <> Colors.ArrowColor Then
          Pen.Color := Colors.ArrowColor;
        If Brush.Color <> Colors.ArrowColor Then
          Brush.Color := Colors.ArrowColor;
        With Rect Do
          PSCDrawMSMoneyArrow(Canvas,Arrow = chtLeftArrow,
            Classes.Rect(Left + Delta,Top + Delta,Right + Delta,Bottom + Delta))
      End
  End;

  Procedure DrawCustomArrow;
  Var
    Index: Integer;
  Begin
    If Arrow = chtLeftArrow Then
      If Hot And (FLeftArrowHot <> -1) Then
        Index := FLeftArrowHot
      Else
        Index := FLeftArrow
    Else
      If Hot And (FRightArrowHot <> -1) Then
        Index := FRightArrowHot
      Else
        Index := FRightArrow;
    If (FImages = Nil) Or (Index = -1) Then
      DrawOutlookArrow
    Else
      With FImages,Rect Do
        Draw(Canvas,(Left + Right - Width) Div 2 + Delta,
          (Top + Bottom - Height) Div 2 + Delta,Index,Enabled);
  End;

Begin
  With Canvas Do
    Begin
      Rgn := PSCGetClipRgn(Self,Canvas);
      With Rect Do
        IntersectClipRect(Canvas.Handle,Left + 2,Top + 2,Right - 2,Bottom - 2);
      Try
        Hot := Arrow = SelectedTest[FSelectedArrow];
        Down := Hot And MouseCapture;
        If Down And FShowArrowEdges Then
          Delta := 1
        Else
          Delta := 0;
        Case FArrowStyle Of
          astOutlook: DrawOutlookArrow;
          astMSMoney: DrawMSMoneyArrow;
          astCustom: DrawCustomArrow
        End;
      Finally
        PSCSelectClipRgn(Canvas,Rgn);
        PSCDeleteClipRgn(Rgn)
      End;
      If Hot And FShowArrowEdges Then
        Begin
          With Rect Do
            R := Classes.Rect(Left + 2,Top + 2,Right - 2,Bottom - 2);
          DrawEdge(Canvas.Handle,R,Edges[Down],BF_RECT Or BF_ADJUST)
        End
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendarPro.DrawWeekLine(Var Rect: TRect);
Begin
  If FCalendarStyle <> cstMSMoney Then
    Inherited DrawWeekLine(Rect)
End;

{------------------------------------------------------------------------------}

procedure TPSCCustomCalendarPro.SetHolidayCountries(
  const ACountries:Array of TPSCCountryID);
var
  i:Integer;
begin
  SetLength(FCountries,Length(ACountries));
  for i:=Low(ACountries) to High(ACountries) do
    FCountries[i]:=ACountries[i];
  Invalidate;
end;

{------------------------------------------------------------------------------}

procedure TPSCCustomCalendarPro.SetHolidayCountries(const ACountries:IPSCStrings);
var
  MyCount:Integer;
  MyID:TPSCCountryID;
  i:Integer;
  MyCountries:IPSCStrings;
begin
  If (ACountries=nil) or (ACountries.Count=0) then
    begin
      SetLength(FCountries,0);
      Invalidate;
    end
  else
    begin
      MyCountries:=PSCCreateSortedStringList;
      MyCountries.Duplicates:=DUP_IGNORE;
      MyCountries.Assign(ACountries);

      SetLength(FCountries,MyCountries.Count);
      MyCount:=0;
      for i:=0 to MyCountries.Count-1 do
      begin
        MyID:=PSCCountryNameToID(MyCountries[i]);
        If MyId>0 then
        begin
          FCountries[MyCount]:=MyID;
          Inc(MyCount);
        end;
      end;
      If MyCount<MyCountries.Count then
        SetLength(FCountries,MyCount);
      Invalidate;
    end;
end;

{------------------------------------------------------------------------------}

procedure TPSCCustomCalendarPro.EnumHolidayCountries(const S:IPSCStrings);
var
  i:Integer;
begin
  S.Clear;
  for i:=Low(FCountries) to High(FCountries) do
    S.Add(PSCCountryIDToName(FCountries[i]));
end;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendarPro.GetHolidayNames(Const ADate: TDateTime;
  const HolidayNames: IPSCStrings): boolean;
var
  MyCount:Integer;
Begin
  If HolidayNames <> Nil Then
    HolidayNames.Clear;

  Result := PSCAddAllHolidays(ADate,FCountries,HolidayNames,FDefaultHolidays);

  If HolidayNames<>nil then
    case CountryInHoliday of
      cihNever:
        PSCRemoveStringsPartEx(HolidayNames,':',True);
      cihSmart:
        begin
          MyCount:=Length(FCountries);
          If DefaultHolidays then
            inc(MyCount);
          If MyCount<=1 then
            PSCRemoveStringsPartEx(HolidayNames,':',True);
        end;
    end;

  If (Not Result) Or (HolidayNames <> Nil) Then
    Result := FHolidays.AddHolidayNames(ADate,HolidayNames) Or Result;
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendarPro.DrawDay(Const Date: TDateTime;
  BkgndColor,TextColor: TPSCColor; FontStyle: TFontStyles;
  Area: TPSCCalendarHitTest; DateType: TPSCCalendarDateType;
  X,Y: Integer; Const Rect: TRect);
Begin
  If FShowHolidays And Not (Area In [chtLeftGray,chtRightGray]) And
    GetHolidayNames(Date,HolidayNames) Then
    With Canvas Do
      Begin
        Font := Colors.HolidaysFont;
        Try
          FontStyle := Font.Style;
          TextColor := Font.Color;
          Inherited DrawDay(Date,BkgndColor,TextColor,FontStyle,
            Area,DateType,X,Y,Rect);
        Finally
          Font := Self.Font
        End
      End
  Else
    Inherited DrawDay(Date,BkgndColor,TextColor,FontStyle,Area,
      DateType,X,Y,Rect);
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendarPro.SetColors(Value: TPSCCalendarColorsPro);
Begin
  If Inherited Colors <> Value Then
    Inherited Colors.Assign(Value)
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendarPro.GetColors: TPSCCalendarColorsPro;
Begin
  Result := TPSCCalendarColorsPro(Inherited Colors)
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendarPro.SetCalendarStyle

(Value: TPSCCalendarStyle);
Begin
  If FCalendarStyle <> Value Then
    Begin
      FCalendarStyle := Value;
      Case Value Of
        cstOutlook:
          Begin
            Color := clPSCWindow;
            ShowMonthPopup := true;
            Flat := true;
            BorderStyle := bsSingle;
            ShowNavButtons := true;
            ShowToday := true;
            WeekDayNames := wdlOne;
            ShowMonthDividers := false;
            ShowHorzLines := false;
            ShowVertLines := false;
            WeekCursor := crDefault;
            SelectKind := skProgressive;
            ExtendedSelect := true;
            Font.Color := clPSCWindowText;
            FArrowStyle := astOutlook;
            HeaderCase := tcDefault;
            FShortMonthName := false;
            PopupType := ptStandard;
            With Colors Do
              Begin
                Days := Self.Color;
                Border := clBorderColor;
                MonthHeader := clMonthHeaderColor;
                DayLines := clDayOutlineColor;
                Grayed := clGrayedColor;
                WeekEndText := clWeekEndTextColor;
                NowRect := clNowRectColor;
                WeekNumbersFont.Color := clPSCWindowText;
                WeekDaysFont.Color := clPSCWindowText;
                DayLinesStyle := psDayOutlineStyle;
                ArrowColor := clArrowColor;
                Selected := clSelectedColor;
                SelectedText := clSelectedTextColor;
                WeekSide := Self.Color;
              End
          End;
        cstMSMoney:
          Begin
            Color := clMSMoneyBkColor;
            ShowMonthPopup := true;
            Flat := true;
            BorderStyle := bsNone;
            ShowNavButtons := true;
            ShowToday := true;
            WeekDayNames := wdlTwo;
            ShowMonthDividers := false;
            ShowHorzLines := true;
            ShowVertLines := false;
            WeekCursor := crDefault;
            SelectKind := skProgressive;
            ExtendedSelect := true;
            Font.Color := clPSCBtnHighlight;
            FArrowStyle := astMSMoney;
            HeaderCase := tcUpper;
            FShortMonthName := true;
            PopupType := ptMSMoney;
            With Colors Do
              Begin
                Days := Self.Color;
                Border := clMSMoneyBkColor;
                MonthHeader := clMSMoneyBkColor;
                DayLines := clDayOutlineColor;
                Grayed := clPSCBtnShadow;
                WeekEndText := clPSCBtnHighlight;
                NowRect := clPSCRed;
                DayLinesStyle := psDayOutlineStyle;
                WeekNumbersFont.Color := clPSCBtnHighlight;
                WeekDaysFont.Color := clPSCBtnHighlight;
                DayLinesStyle := psDayOutlineStyle;
                ArrowColor := clPSCWhite;
                WeekHeader := clMSMoneySelection;
                Selected := clMSMoneySelection;
                SelectedText := clPSCHighLightText;
                WeekSide := Self.Color;
              End
          End
      End;
      Invalidate;
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendarPro.SetArrowStyle(Value: TPSCArrowStyle);
Begin
  If FArrowStyle <> Value Then
    Begin
      FArrowStyle := Value;
      InvalidateArrow(chtLeftArrow);
      InvalidateArrow(chtRightArrow)
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendarPro.SetShowArrowEdges(Value: Boolean);
Begin
  If FShowArrowEdges <> Value Then
    Begin
      FShowArrowEdges := Value;
      InvalidateArrow(chtLeftArrow);
      InvalidateArrow(chtRightArrow)
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendarPro.SetImages(Value: TImageList);
Begin
  If FImages <> Value Then
    Begin
      FImages := Value;
      Invalidate
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendarPro.SetLeftArrowHot(Value: Integer);
Begin
  If FLeftArrowHot <> Value Then
    Begin
      FLeftArrowHot := Value;
      InvalidateArrow(chtLeftArrow)
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendarPro.SetRightArrowHot(Value: Integer);
Begin
  If FRightArrowHot <> Value Then
    Begin
      FRightArrowHot := Value;
      InvalidateArrow(chtRightArrow)
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendarPro.SetLeftArrow(Value: Integer);
Begin
  If FLeftArrow <> Value Then
    Begin
      FLeftArrow := Value;
      InvalidateArrow(chtLeftArrow)
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendarPro.SetRightArrow(Value: Integer);
Begin
  If FRightArrow <> Value Then
    Begin
      FRightArrow := Value;
      InvalidateArrow(chtRightArrow)
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendarPro.SetShowHolidays(Value: Boolean);
Begin
  If FShowHolidays <> Value Then
    Begin
      FShowHolidays := Value;
      Invalidate
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendarPro.SetHolidays(Value: TPSCHolidayItems);
Begin
  If FHolidays <> Value Then
    Begin
      FHolidays.Assign(Value);
      Invalidate
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendarPro.SetHeaderCase(Value: TPSCTextCase);
Begin
  If FHeaderCase <> Value Then
    Begin
      FHeaderCase := Value;
      Invalidate
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendarPro.SetShortMonthName(Value: Boolean);
Begin
  If FShortMonthName <> Value Then
    Begin
      FShortMonthName := Value;
      Invalidate
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendarPro.MouseHitTest(Var HitTest: TPSCCalendarHitTest);
Var
  OldSelectedArrow: TPSCSelectedArrow;
Begin
  Inherited MouseHitTest(HitTest);
  OldSelectedArrow := FSelectedArrow;
  Case HitTest Of
    chtLeftArrow: FSelectedArrow := saLeftArrow;
    chtRightArrow: FSelectedArrow := saRightArrow;
  Else
    FSelectedArrow := saNotSelected
  End;
  If OldSelectedArrow <> FSelectedArrow Then
    Begin
      Case OldSelectedArrow Of
        saLeftArrow: InvalidateArrow(chtLeftArrow);
        saRightArrow: InvalidateArrow(chtRightArrow)
      End;
      Case FSelectedArrow Of
        saLeftArrow: InvalidateArrow(chtLeftArrow);
        saRightArrow: InvalidateArrow(chtRightArrow)
      End
    End
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendarPro.GetHeaderText(Const Date: TDateTime): String;
Var
  Year,Month,Day: Word;
Begin
  PSCDecodeDate(Date,Year,Month,Day);
  If FShortMonthName Then
    Result := PSCShortMonthNames(Month)
  Else
    Result := PSCLOngMonthNames(Month);
  Result := PSCFormat('%s %d', [Result,Year]);//don't resource
  Case FHeaderCase Of
    tcUpper: Result := PSCUpperCase(Result);
    tcLower: Result := PSCLowerCase(Result)
  End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendarPro.CMMouseLeave(Var Message: TMessage);
Var
  HitTest: TPSCCalendarHitTest;
Begin
  Inherited;
  HitTest := chtNoWhere;
  MouseHitTest(HitTest)
End;
{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendarPro.WMCaptureChanged(Var Message: TMessage);
Begin
  Inherited;
  Case FSelectedArrow Of
    saLeftArrow: InvalidateArrow(chtLeftArrow);
    saRightArrow: InvalidateArrow(chtRightArrow)
  End
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendarPro.GetHintStr(
  Var HintStr: String; CursorPos: TPoint; Var CursorRect: TRect): Integer;
Var
  HitTest: TPSCCalendarHitTest;
  Date: TDateTime;
  i: Integer;
  S: String;
Begin
  Result := 1;
  If FShowHolidays Or ForceShowHint Then
    Begin
      HintStr := '';
      HitTest := GetHitTest(CursorPos,Date);
      If HitTest In [chtDay,chtLeftGray,chtRightGray] Then
        Begin
          GetHolidayNames(Date,HolidayNames);
          For i := 0 To HolidayNames.Count - 1 Do
            Begin
              S := PSCTrim(HolidayNames[i]);
              If i > 0 Then
                S := #13#10 + S;
              HintStr := HintStr + S;
            End;
          If HintStr <> '' Then
            Begin
              DateToRect(Date,CursorRect);
              Result := 0;
            End;
        End;
    End;
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendarPro.CMHintShow(Var Message: TCMHintShow);
Begin
  With Message,HintInfo^ Do
    If HintControl <> Self Then
      Inherited
    Else
      Result := GetHintStr(HintStr,CursorPos,CursorRect);
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendarPro.IsCustomHolidaysStored: boolean;
Begin
  Result := CustomHolidays.Count > 0;
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendarPro.HolidaysChange(Sender: TObject);
Begin
  Invalidate
End;

{------------------------------------------------------------------------------}

Function PSCIsDateInDays(Const Date: TDateTime; Days: TPSCWeekDays): boolean;
Begin
  Result := TPSCWeekDay(PSCDayOfWeek(Date)) In Days;
End;

{-------------------------------------------}

Function PSCGetWeekNumber(Const Date: TDateTime; FirstDay:
  TPSCFirstDayOfWeek;
  FirstWeek: TPSCFirstWeekOfYear): Integer;
Var
  YearFirstWeek,NextFirstWeek: TDateTime;
  Year: Word;

  Function GetFirstWeek(Year: Word): TDateTime;
  Var
    YearDate: TDateTime;
  Begin
    YearDate := PSCEncodeDate(Year,1,1);
    Result := PSCGetWeekStartEx(YearDate,FirstDay);
    If FirstWeek = fwLocaleDefault Then
      FirstWeek := PSCLocaleFirstWeekOfYear;
    Case FirstWeek Of
      fwFirstFourthDayOfWeek:
        If YearDate - Result > 3 Then
          Result := Result + 7;
      fwFirstFullWeek:
        If Result <> YearDate Then
          Result := Result + 7
    End
  End;

Begin
  Year := PSCGetDateYear(Date);
  YearFirstWeek := GetFirstWeek(Year);
  If Date < YearFirstWeek Then
    YearFirstWeek := GetFirstWeek(Year - 1)
  Else
    Begin
      NextFirstWeek := GetFirstWeek(Year + 1);
      If Date >= NextFirstWeek Then
        YearFirstWeek := NextFirstWeek
    End;
  Result := Trunc(Date - YearFirstWeek) Div 7 + 1
End;

{------------------------------}

Function PSCGetWorkDaysCount(WorkDays: TPSCWeekDays): Integer;

  Procedure IncWorkDaysCount(WeekDay: TPSCWeekDay);
  Begin
    If WeekDay In WorkDays Then
      inc(Result);
  End;

Begin
  Result := 0;
  IncWorkDaysCount(dwMonday);
  IncWorkDaysCount(dwTuesday);
  IncWorkDaysCount(dwWednesday);
  IncWorkDaysCount(dwThursday);
  IncWorkDaysCount(dwFriday);
  IncWorkDaysCount(dwSaturday);
  IncWorkDaysCount(dwSunday);
End;

{---------------------}

Procedure TPSCHolidayItem.Assign(Source: TPersistent);
Begin
  If Source Is TPSCHolidayItem Then
    Begin
      SetDayConst(TPSCHolidayItem(Source).DayConst);
      Name := TPSCHolidayItem(Source).Name;
      Changed(false);
    End
  Else
    Inherited;
End;

{-------------------------------------------}

Function PSCCalcWorkDays(Const StartDate,FinishDate: TDateTime;
  const Countries:Array of TPSCCountryID; CustomHolidays: TPSCHolidayItems;
  DefaultHolidays: boolean; WorkDays: TPSCWeekDays;
  ExcludeEvent: TPSCDateSelectedProc): Integer;
Var
  FStartDate,FFinishDate: TDateTime;

  Function IsHoliday(Const Date: TDateTime): boolean;
  Begin
    Result := Not PSCIsDateInDays(Date,WorkDays);
    If Not Result Then
      Begin
        Result := PSCAddAllHolidays(Date,Countries,Nil,DefaultHolidays);
        If Not Result And (CustomHolidays <> Nil) Then
          Result := CustomHolidays.AddHolidayNames(Date,Nil);
      End;
  End;
  
Begin
  Result := 0;
  FStartDate := PSCMin(StartDate,FinishDate);
  FFinishDate := PSCMax(StartDate,FinishDate);

  While FStartDate <= FFinishDate Do
    Begin
      If (Not Assigned(ExcludeEvent) Or Not ExcludeEvent(FStartDate))
        And Not IsHoliday(FStartDate) Then
        inc(Result);
      FStartDate := FStartDate + 1;
    End;
End;

{------------------------------------------------------------------------------}

Function TPSCHolidayItem.GetWeekDay: TPSCWeekDay;
Begin
  Result := FDayConst.WeekDay;
End;

{------------------------------------------------------------------------------}

Function TPSCHolidayItem.GetKind: TPSCDayConstKind;
Begin
  Result := FDayConst.Kind;
End;

{------------------------------------------------------------------------------}

Function TPSCHolidayItem.GetDay: TPSCDay;
Begin
  Result := FDayConst.Day;
End;

{------------------------------------------------------------------------------}

Function TPSCHolidayItem.GetMonth: TPSCMonth;
Begin
  Result := FDayConst.Month;
End;

{------------------------------------------------------------------------------}

Constructor TPSCHolidayItem.Create(Collection: TCollection);
Begin
  Inherited Create(Collection);
  FDayConst.Kind := dckDate;
  FDayConst.Month := mJanuary;
  FDayConst.Day := 1;
  FDayConst.WeekDay := dwSunday;
End;

{------------------------------------------------------------------------------}

Function TPSCHolidayItem.IsDayNumber: boolean;
Begin
  Result := (KindOfDay = dckDayNumber)
End;

{------------------------------------------------------------------------------}

Procedure TPSCHolidayItem.SetDayConst(Const ADateConst: TPSCDayConst);
Begin
  FDayConst := ADateConst;
  FDayConst.DateProc := Nil;
End;

{------------------------------------------------------------------------------}

Function TPSCHolidayItem.HolidayToDateTime(Const BaseDate: TDateTime):
  TDateTime;
Begin
  If FDayConst.Kind = dckProc Then
    Result := TPSCHolidayItems(Collection).GetItemDate(BaseDate,Self)
  Else
    Result := PSCDayConstToDateTime(BaseDate,FDayConst);
End;

{------------------------------------------------------------------------------}

Procedure TPSCHolidayItem.SetWeekDay(Value: TPSCWeekDay);
Begin
  If Value <> FDayConst.WeekDay Then
    Begin
      FDayConst.WeekDay := Value;
      Changed(false);
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCHolidayItem.SetKind(Value: TPSCDayConstKind);
Begin
  If Value <> FDayConst.Kind Then
    Begin
      FDayConst.Kind := Value;
      Changed(false)
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCHolidayItem.SetDay(Value: TPSCDay);
Begin
  If Value <> FDayConst.Day Then
    Begin
      FDayConst.Day := Value;
      Changed(false)
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCHolidayItem.SetMonth(Value: TPSCMonth);
Begin
  If Value <> FDayConst.Month Then
    Begin
      FDayConst.Month := Value;
      Changed(false)
    End
End;

{------------------------------------------------------------------------------}

Function TPSCHolidayItems.GetHolidayItem(Index: integer): TPSCHolidayItem;
Begin
  If (Index < 0) Or (Index >= Count) Then
    Result := Nil
  Else
    Result := TPSCHolidayItem(Items[Index])
End;

{------------------------------------------------------------------------------}

Function TPSCHolidayItems.AddHolidayNames(ADate: TDateTime;
  const HolidayNames:IPSCStrings): boolean;
Var
  i: integer;
Begin
  Result := false;
  For i := 0 To Count - 1 Do
    With TPSCHolidayItem(Items[i]) Do
      If PSCDatesCompare(ADate,HolidayToDateTime(ADate),cpkDate) = 0 Then
        Begin
          Result := true;
          If HolidayNames <> Nil Then
            HolidayNames.Add(Name)
          Else
            exit;
        End;
End;

{------------------------------------------------------------------------------}

Function PSCAddAllHolidays(Const ADate: TDateTime;
  const ACountries:Array of TPSCCountryID;
  const HolidayList:IPSCStrings;DefaultHolidays: boolean): boolean;
Var
  i: integer;
  ThisCountry,DefCountry: TPSCCountryID;
  CountryCount: Integer;
Begin
  Result := False;
  CountryCount := Length(ACountries);

  DefCountry := TPSCCountryID(GetUserDefaultLCID);

  For i := 0 To CountryCount - 1 Do
    Begin
      ThisCountry := ACountries[i];
      If DefCountry = ThisCountry Then
        DefaultHolidays := False;
      Result := PSCGetHolidays.EnumCountryHolidaysForDate(ThisCountry,ADate,
        HolidayList) Or Result;
      If Result And (HolidayList = Nil) Then
        exit;
    End;

  If DefaultHolidays Then
    Result := PSCGetHolidays.EnumCountryHolidaysForDate(DefCountry,ADate,
      HolidayList) Or Result;
End;

{------------------------------------------------------------------------------}

Function TPSCHolidayItems.GetItemDate(Const BaseDate: TDateTime; Item:
  TPSCHolidayItem): TDateTime;
Begin
  Result := 1;
  If Assigned(FOnGetHolidayDate) Then
    FOnGetHolidayDate(Self,Item,BaseDate,Result);
End;

{------------------------------------------------------------------}
type
  TPSCYearsListBox = Class(TListBox)
  private
    FSelectedColor,FSelectedTextColor: TPSCColor;
    FTimer: TObject;
    FTimerId: Integer;
    FSelYear: Integer;
    FOnSelect: TPSCNotifyEvent;

    Procedure SetSelectedColor(V: TPSCColor);
    Procedure SetSelectedTextColor(V: TPSCColor);
    Procedure TimerEvent(Timer: TObject; EventID: Integer);
    Procedure SetSelYear(Value: Integer);
    Function GetItemYear: Integer;
    Procedure DoSelect;
    Procedure KillTimer;
    Procedure WMEraseBkgnd(Var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
  protected
    Procedure CNDrawItem(Var Message: TWMDrawItem); message cn_drawitem;
    Procedure DrawItem(Index: Integer; Rect: TRect;
      State: TOwnerDrawState); override;
    Procedure KeyDown(Var Key: Word; Shift: TShiftState); override;
    Procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    Procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X,Y: Integer); override;
  public
    Constructor Create(AOwner: TComponent); override;

    Property SelYear: Integer read FSelYear write SetSelYear;
    Property ItemYear: Integer read GetItemYear;
    Property OnSelect: TPSCNotifyEvent read FOnSelect write FOnSelect;
    Property SelectedColor: TPSCColor read FSelectedColor write SetSelectedColor;
    Property SelectedTextColor: TPSCColor read FSelectedTextColor write
      SetSelectedTextColor;
  End;

  {------------------------------------------------------------------------------}

Procedure TPSCYearsListBox.SetSelectedColor(V: TPSCColor);
Begin
  If FSelectedColor <> V Then
    Begin
      FSelectedColor := V;
      Invalidate;
    End;
End;

{------------------------------------------------------------------------------}

Procedure TPSCYearsListBox.SetSelectedTextColor(V: TPSCColor);
Begin
  If FSelectedTextColor <> V Then
    Begin
      FSelectedTextColor := V;
      Invalidate;
    End;
End;

{------------------------------------------------------------------------------}

Constructor TPSCYearsListBox.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);
  ControlStyle := ControlStyle + [csCaptureMouse];
  Ctl3d := false;
  BorderStyle := bsNone;
  Style := lbOwnerDrawFixed;
  IntegralHeight := true;
  Height := ItemHeight * 7;
  FSelectedColor := Font.Color;
  FSelectedTextColor := Brush.Color;
End;

{------------------------------------------------------------------------------}

Procedure TPSCYearsListBox.DoSelect;
Begin
  If Assigned(FOnSelect) Then
    FOnSelect(Self)
End;

{------------------------------------------------------------------------------}

Procedure TPSCYearsListBox.KeyDown(Var Key: Word; Shift: TShiftState);
Var
  N,I: Integer;

  Procedure SelectYear(Year: Integer);
  Begin
    Items.BeginUpdate;
    Try
      SelYear := Year;
      ItemIndex := I
    Finally
      Items.EndUpdate;
    End
  End;

Begin
  If ssAlt In Shift Then
    Begin
      ItemIndex := -1;
      DoSelect;
      Key := 0
    End;
  N := ClientHeight Div ItemHeight;
  I := ItemIndex;
  Case Key Of
    VK_UP:
      Begin
        If I = 0 Then
          SelectYear(SelYear - 1)
        Else
          ItemIndex := ItemIndex - 1;
        Key := 0
      End;
    VK_DOWN:
      Begin
        If I = N - 1 Then
          SelectYear(SelYear + 1)
        Else
          ItemIndex := ItemIndex + 1;
        Key := 0
      End;
    VK_PRIOR:
      Begin
        If I = 0 Then
          SelectYear(SelYear - N)
        Else
          ItemIndex := 0;
        Key := 0
      End;
    VK_NEXT:
      Begin
        If I = N - 1 Then
          SelectYear(SelYear + N)
        Else
          ItemIndex := N - 1;
        Key := 0
      End;
    VK_ESCAPE:
      Begin
        ItemIndex := -1;
        DoSelect;
        Key := 0
      End;
    VK_RETURN,VK_SPACE: DoSelect
  End;
  Inherited KeyDown(Key,Shift)
End;

{------------------------------------------------------------------------------}

Procedure TPSCYearsListBox.MouseMove(Shift: TShiftState; X,Y: Integer);
Var
  NewTimerId: Integer;
Begin
  Inherited MouseMove(Shift,X,Y);
  If Not (ssLeft In Shift) Or ((Y >= 0) And (Y < ClientHeight)) Then
    Begin
      If FTimerID <> 0 Then
        KillTimer;
      ItemIndex := Y Div ItemHeight;
      Exit
    End;
  If Y < 0 Then
    NewTimerId := Y
  Else
    NewTimerId := Y - ClientHeight;
  NewTimerId := NewTimerId Div ItemHeight + 1 - Ord(NewTimerId < 0) * 2;
  If NewTimerId > 4 Then
    NewTimerId := 4
  Else
    If NewTimerId < -4 Then
      NewTimerId := -4;
  If (FTimer = nil) Or (NewTimerId <> FTimerId) Then
    Begin
      KillTimer;
      FTimerId := NewTimerId;
      FTimer := PSCSetTimer(FTimerId,cPSCScrollSpeed[Abs(FTimerId)],TimerEvent);
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCYearsListBox.SetSelYear(Value: Integer);
Var
  I,N: Integer;
Begin
  If Value < 1 Then
    Value := 1
  Else
    If Value > 9999 Then
      Value := 9999;
  If FSelYear <> Value Then
    Begin
      FSelYear := Value;
      With Items Do
        Begin
          BeginUpdate;
          Try
            Clear;
            N := ClientHeight Div ItemHeight;
            For I := 0 To N - 1 Do
              Begin
                Add(PSCIntToStr(Value + I - N Div 2));
              End;
          Finally
            EndUpdate;
          End;
        End
    End
End;

{------------------------------------------------------------------------------}

Function TPSCYearsListBox.GetItemYear: Integer;
Begin
  If ItemIndex >= 0 Then
    Result := FSelYear + ItemIndex - ClientHeight Div ItemHeight Div 2
  Else
    Result := SelYear
End;

{------------------------------------------------------------------------------}

Procedure TPSCYearsListBox.KillTimer;
Begin
  PSCKillTimer(FTimer);
  FTimerID := 0;
End;

{------------------------------------------------------------------------------}

Procedure TPSCYearsListBox.TimerEvent(Timer: TObject; EventID: Integer);
Begin
  With Items Do
    Begin
      BeginUpdate;
      Try
        If Integer(EventID) < 0 Then
          Begin
            SelYear := SelYear - 1;
            ItemIndex := 0;
          End
        Else
          Begin
            SelYear := SelYear + 1;
            ItemIndex := Count - 1
          End;
      Finally
        EndUpdate
      End
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCYearsListBox.WMEraseBkgnd(Var Message: TWmEraseBkgnd);
Begin
  Message.Result := 1
End;

{------------------------------------------------------------------------------}

Procedure TPSCYearsListBox.CNDrawItem(Var Message: TWMDrawItem);
Begin
  With Message.DrawItemStruct^ Do
    Begin
      itemState := itemState And (Not ods_Focus);
      Inherited;
    End;
End;

{------------------------------------------------------------------------------}

Procedure TPSCYearsListBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
Var
  S: String;
Begin
  With Canvas Do
    Begin
      Font := Self.Font;
      Brush := Self.Brush;

      If (odSelected In State)
        Then
        Begin
          Font.Color := SelectedTextColor;
          Brush.Color := SelectedColor;
          Brush.Style := BrushStyle_Solid;
        End;
      FillRect(Rect);
      S := Items[Index];
      PSCDrawText(Canvas,S,Length(S),Rect,DT_CENTER Or DT_VCENTER);
    End;
End;

{------------------------------------------------------------------------------}

Procedure TPSCYearsListBox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X,Y: Integer);
Begin
  If Button = mbLeft Then
    Begin
      KillTimer;
      If Not PtInRect(ClientRect,Point(X,Y)) Then
        ItemIndex := -1;
      DoSelect
    End
  Else
    Inherited MouseUp(Button,Shift,X,Y)
End;

{------------------------------------------------------------------------------}

Procedure TPSCYearsPopup.ResizeFormControls;
Const
  SPSCTestWidth = 'XX2000XX'; //don't resource
Begin
  Inherited;
  Canvas.Font := Font;
  With TPSCYearsListBox(ListBox) Do
    Begin
      ItemHeight := Self.Canvas.TextHeight('X') + 4;
      ClientHeight := ItemHeight * 7;
      ClientWidth := Self.Canvas.TextWidth(SPSCTestWidth);
    End;
  ClientHeight := ListBox.Height;
  ClientWidth := ListBox.Width;
End;

{------------------------------------------------------------------------------}

constructor TPSCYearsPopup.CreateNew(AOwner: TComponent; Dummy: Integer=0);
Begin
  Inherited;
  With TPSCYearsListBox(ListBox) Do
    Begin
      OnSelect := SelectYear;
      Align := AlNone;
      ParentColor := true;
      ParentFont := True;
      Parent := Self;
      ResizeFormControls;
    End;
  ActiveControl := ListBox;
End;

{------------------------------------------------------------------------------}

Function TPSCYearsPopup.GetListBoxClass: TListBoxClass;
Begin
  Result := TPSCYearsListBox;
End;

{------------------------------------------------------------------------------}

Procedure TPSCYearsPopup.CreateParams(Var Params: TCreateParams);
Begin
  Inherited CreateParams(Params);
  With Params Do
    Begin
      If Not Ctl3D Then
        Style := Style And Not (WS_DLGFRAME Or WS_THICKFRAME) Or WS_BORDER;
      ExStyle := ExStyle And Not WS_EX_CLIENTEDGE
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCYearsPopup.SelectYear(Sender: TObject);
Begin
  ClosePopup(false,True);
End;

{------------------------------------------------------------------------------}

Constructor TPSCMonthsBox.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);
  FAllowedHitTests := [Low(TPSCMonthHitTest)..High(TPSCMonthHitTest)];
  FHeaderFont := TPSCFont.Create;
  With FHeaderFont Do
    Begin
      Assign(Self.Font);
      OnChange := OnFontChanged;
    End;
  FInheritHeaderFont := true;
  FCaptionColor := clPSCWindow;
  FCaptionTextColor := clPSCWindowText;

  FSelectedColor := clHighlight;
  FSelectedTextColor := clHighlightText;
  FHeaderColor := clPSCBtnFace;
  FArrowColor := clArrowColor;
  FYearStep := 1;
  Width := 120;
  Height := 140;
  Self.Year := PSCGetYear;
  Caption := PSCConsts.SelectMonth;
  FArrowStyle := astOutlook;
  FLeftArrow := -1;
  FLeftArrowHot := -1;
  FRightArrow := -1;
  FRightArrowHot := -1;
  ParentColor := false;
  ParentCtl3D := false;
  Ctl3D := false;
  Color := clPSCWindow;
  TabStop := true;
  Flat:=true;
  ControlStyle :=
    [csCaptureMouse,csOpaque,csDoubleClicks,csFramed,csClickEvents];
End;

{------------------------------------------------------------------------------}

Destructor TPSCMonthsBox.Destroy;
Begin
  KillTimer;
  FYearsPopup.Free;
  FYearsPopup := Nil;
  FHeaderFont.Free;
  Inherited Destroy
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthsBox.CreateParams(Var Params: TCreateParams);
Begin
  Inherited CreateParams(Params);
  With Params Do
    WindowClass.style := WindowClass.style Or CS_HREDRAW Or CS_VREDRAW
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthsBox.Notification(AComponent: TComponent;
  Operation: TOperation);
Begin
  Inherited Notification(AComponent,Operation);
  If (AComponent = FImages) And (Operation = opRemove) Then
    FImages := Nil
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthsBox.GetRect(Area: TPSCMonthHitTest; Month: Integer;
  Var Rect: TRect);
Var
  ASize: TSize;
  Delta: Integer;
Begin
  ASize := Canvas.TextExtent('M');
  If FHeaderHeight <= 0 Then
    Delta := ASize.cy + 4
  Else
    Delta := FHeaderHeight;

  Rect := ClientRect;

  With Rect Do
    Case Area Of
      mhtHead: Bottom := Delta;
      mhtCaption:
        Begin
          Top := Delta;
          Bottom := Top + ASize.cy + 4
        End;
      mhtMonths: Top := Delta + ASize.cy + 4;
      mhtLeftArrow:
        Begin
          Left := 0;
          Top := 0;
          Right := Left + Delta * 3 Div 2;
          Bottom := Top + Delta;
        End;
      mhtRightArrow:
        Begin
          Left := Right - Delta * 3 Div 2;
          Bottom := Top + Delta;
        End;
      mhtYear:
        Begin
          Right := Right Div 2;
          Left := Right - ASize.cx * 3;
          Right := Right + ASize.cx * 3;
          Bottom := Delta;
        End;
      mhtMonth:
        If Month In [1..12] Then
          Begin

            Top := Delta + ASize.cy + 4;

            Rect := Classes.Rect(
              ((Month - 1) Mod 3) * Right Div 3,
              Top + ((Month - 1) Div 3) * (Bottom - Top) Div 4,
              ((Month - 1) Mod 3 + 1) * Right Div 3,
              Top + ((Month - 1) Div 3 + 1) * (Bottom - Top) Div 4);

            If FShowVertLines And Not (Month In [1,4,7,10]) Then
              Left := Left + 1;
            If FShowHorzLines And Not (Month In [1..3]) Then
              Top := Top + 1
          End
    End;
End;

{------------------------------------------------------------------------------}

Function TPSCMonthsBox.GetHitTest(P: TPoint; Var Month: Integer):
  TPSCMonthHitTest;
Var
  aSize: TSize;
  Delta: Integer;
  Rect: TRect;
Begin
  If Not PtInRect(ClientRect,P) Then
    Result := mhtNowhere
  Else
    Begin
      aSize := Canvas.TextExtent('M');
      If FHeaderHeight <= 0 Then
        Delta := aSize.cy + 4
      Else
        Delta := FHeaderHeight;

      If P.y < Delta Then
        Begin
          GetRect(mhtYear,Month,Rect);
          If PtInRect(Rect,P) Then
            Result := mhtYear
          Else
            Begin
              GetRect(mhtLeftArrow,Month,Rect);
              If PtInRect(Rect,P) Then
                Result := mhtLeftArrow
              Else
                Begin
                  GetRect(mhtRightArrow,Month,Rect);
                  If PtInRect(Rect,P) Then
                    Result := mhtRightArrow
                  Else
                    Result := mhtHead
                End
            End
        End
      Else
        If P.y < Delta + aSize.cy + 4 Then
          Result := mhtCaption
        Else
          Begin
            Result := mhtMonths;
            With CLientRect Do
              Begin
                aSize.cx := P.x Div (Right Div 3);
                aSize.cy := (P.y - (Delta + aSize.cy + 4)) Div ((Bottom - (Delta +
                  aSize.cy + 4)) Div 4)
              End;
            If (aSize.cx In [0..2]) And (aSize.cy In [0..3]) Then
              Begin
                Month := aSize.cy * 3 + aSize.cx + 1;
                Result := mhtMonth
              End
          End
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthsBox.InvalidateArea(Area: TPSCMonthHitTest; Month: Integer);
Var
  Rect: TRect;
Begin
  If HandleAllocated Then
    Begin
      GetRect(Area,Month,Rect);
      InvalidateRect(Rect);{was TRUE}
    End;
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthsBox.SetYear(Value: Integer);
Begin
  If Value < 1 Then
    Value := 1
  Else
    If Value > 9999 Then
      Value := 9999;
  If FYear <> Value Then
    Begin
      FYear := Value;
      InvalidateArea(mhtYear,FMonth)
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthsBox.SetMonth(Value: Integer);
Begin
  If (Value < 0) Or (Value > 12) Then
    Value := 0;
  If FMonth <> Value Then
    Begin
      If FSelected = mhtMonth Then
        Begin
          If FMonth <> 0 Then
            InvalidateArea(mhtMonth,FMonth);
          If Value <> 0 Then
            InvalidateArea(mhtMonth,Value)
        End;
      FMonth := Value
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthsBox.SetSelDate(Value: TDateTime);
Var
  AYear,AMonth,ADay: Word;
Begin
  PSCDecodeDate(Value,AYear,AMonth,ADay);
  Year := AYear;
  Month := AMonth
End;

{------------------------------------------------------------------------------}

Function TPSCMonthsBox.GetSelDate: TDateTime;
Var
  Month: Integer;
Begin
  Month := Self.Month;
  If Not (Month In [1..12]) Then
    Month := 1;
  Result := PSCEncodeDate(Year,Month,1)
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthsBox.SetSelected(Value: TPSCMonthHitTest);
Begin
  If FSelected <> Value Then
    Begin
      FDown := false;
      If FSelected In [mhtLeftArrow,mhtRightArrow,mhtYear,mhtMonth] Then
        InvalidateArea(FSelected,FMonth);
      If Value In [mhtLeftArrow,mhtRightArrow,mhtYear,mhtMonth] Then
        InvalidateArea(Value,FMonth);
      FSelected := Value
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthsBox.SetCaptionColor(Value: TPSCColor);
Begin
  If FCaptionColor <> Value Then
    Begin
      FCaptionColor := Value;
      InvalidateArea(mhtCaption,FMonth)
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthsBox.SetCaptionTextColor(Value: TPSCColor);
Begin
  If FCaptionTextColor <> Value Then
    Begin
      FCaptionTextColor := Value;
      InvalidateArea(mhtCaption,FMonth)
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthsBox.SetSelectedColor(Value: TPSCColor);
Begin
  If FSelectedColor <> Value Then
    Begin
      FSelectedColor := Value;
      Invalidate
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthsBox.SetSelectedTextColor(Value: TPSCColor);
Begin
  If FSelectedTextColor <> Value Then
    Begin
      FSelectedTextColor := Value;
      Invalidate
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthsBox.SetHeaderColor(Value: TPSCColor);
Begin
  If FHeaderColor <> Value Then
    Begin
      FHeaderColor := Value;
      Invalidate
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthsBox.SetArrowColor(Value: TPSCColor);
Begin
  If FArrowColor <> Value Then
    Begin
      FArrowColor := Value;
      Invalidate
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthsBox.SetShowEdges(Value: Boolean);
Begin
  If FShowEdges <> Value Then
    Begin
      FShowEdges := Value;
      Invalidate
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthsBox.SetArrowStyle(Value: TPSCArrowStyle);
Begin
  If FArrowStyle <> Value Then
    Begin
      FArrowStyle := Value;
      Invalidate
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthsBox.SetHeaderHeight(Value: Integer);
Begin
  If FHeaderHeight <> Value Then
    Begin
      FHeaderHeight := Value;
      Invalidate
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthsBox.SetImages(Value: TImageList);
Begin
  If FImages <> Value Then
    Begin
      FImages := Value;
      If FArrowStyle = astCustom Then
        Invalidate
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthsBox.SetLeftArrow(Value: Integer);
Begin
  If FLeftArrow <> Value Then
    Begin
      FLeftArrow := Value;
      If FArrowStyle = astCustom Then
        Invalidate
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthsBox.SetLeftArrowHot(Value: Integer);
Begin
  If FLeftArrowHot <> Value Then
    Begin
      FLeftArrowHot := Value;
      If FArrowStyle = astCustom Then
        Invalidate
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthsBox.SetRightArrow(Value: Integer);
Begin
  If FRightArrow <> Value Then
    Begin
      FRightArrow := Value;
      If FArrowStyle = astCustom Then
        Invalidate
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthsBox.SetRightArrowHot(Value: Integer);
Begin
  If FRightArrowHot <> Value Then
    Begin
      FRightArrowHot := Value;
      If FArrowStyle = astCustom Then
        Invalidate
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthsBox.SetShowVertLines(Value: Boolean);
Begin
  If FShowVertLines <> Value Then
    Begin
      FShowVertLines := Value;
      Invalidate
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthsBox.SetShowHorzLines(Value: Boolean);
Begin
  If FShowHorzLines <> Value Then
    Begin
      FShowHorzLines := Value;
      Invalidate
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthsBox.SetYearsPopupCtl3D(Value: Boolean);
Begin
  If FYearsPopupCtl3D <> Value Then
    Begin
      FYearsPopupCtl3D := Value;
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthsBox.SetHeaderFont(Value: TPSCFont);
Begin
  If FHeaderFont <> Value Then
    FHeaderFont.Assign(Value)
End;

{------------------------------------------------------------------------------}

Function TPSCMonthsBox.IsCaptionColorStored: Boolean;
Begin
  Result := FCaptionColor <> Color
End;

{------------------------------------------------------------------------------}

Function TPSCMonthsBox.IsCaptionTextColorStored: Boolean;
Begin
  Result := FCaptionTextColor <> Font.Color
End;

{------------------------------------------------------------------------------}

Function TPSCMonthsBox.IsHeaderFontStored: Boolean;
Begin
  Result := Not FInheritHeaderFont
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthsBox.OnFontChanged(Sender: TObject);
Begin
  FInheritHeaderFont := false;
  Invalidate
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthsBox.DoSelect;
Begin
  If Assigned(FOnChangeDate) Then
    FOnChangeDate(Self)
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthsBox.SelectYear;
Var
  ARect: TRect;
Begin
  If FYearsPopup <> Nil Then
    FYearsPopup.Free;
  FYearsPopup := TPSCYearsPopup.CreateNew(Nil,0);
  GetRect(mhtYear,FMonth,ARect);
  FDown := true;
  With TPSCYearsPopup(FYearsPopup) Do
    Begin
      Font := Self.Font;
      With TPSCYearsListBox(ListBox) Do
        Begin
          SelectedColor := Self.SelectedColor;
          SelectedTextColor := Self.SelectedTextColor;

          Font := Self.Font;
          SelYear := Year;
          ItemIndex := 3;
          BorderStyle := bsNone;
        End;
      Ctl3D := YearsPopupCtl3D;
      PopupEx(Self,ARect,YearsPopupClosed,pskCentered,
        [poParentColor,poParentFontColor]);
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthsBox.YearsPopupClosed(Sender: TObject; Canceled: Boolean);
Begin
  If Not Canceled Then
    Begin
      If FYearsPopup <> Nil Then
        With TPSCYearsListBox(TPSCYearsPopup(FYearsPopup).ListBox) Do
          If ItemIndex <> -1 Then
            Year := ItemYear;
      FDown := false;
      Selected := mhtNowhere;
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthsBox.CMFontChanged(Var Message: TMessage);
Begin
  Inherited;
  If FInheritHeaderFont Then
    Begin
      FHeaderFont.Assign(Font);
      FInheritHeaderFont := true
    End;
  If csReading In ComponentState Then
    FCaptionTextColor := Font.Color
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthsBox.CMColorChanged(Var Message: TMessage);
Begin
  Inherited;
  If csReading In ComponentState Then
    FCaptionColor := Color
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthsBox.CMCtl3DChanged(Var Message: TMessage);
Begin
  Inherited;
  Invalidate
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthsBox.CMTextChanged(Var Message: TMessage);
Begin
  Inherited;
  InvalidateArea(mhtCaption,FMonth)
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthsBox.CMMouseLeave(Var Message: TMessage);
Begin
  Inherited;
  If Focused Then
    Selected := mhtNowhere
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthsBox.WMEraseBkgnd(Var Message: TWmEraseBkgnd);
Begin
  Message.Result := 1
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthsBox.TimerEvent(Timer: TObject; EventID: Integer);
Var
  Delta: Integer;
Begin
  Inherited;
  If TPSCMonthHitTest(EventID) In [mhtLeftArrow,mhtRightArrow] Then
    If TPSCMonthHitTest(EventID) <> Selected Then
      Begin
        FStart := Year;
        FYearStep := 1
      End
    Else
      Begin
        Delta := FYear;
        If Delta Mod 10 = 0 Then
          Begin
            Delta := Abs(Delta - FStart);
            If Delta >= 100 Then
              FYearStep := 50
            Else
              If Delta >= 50 Then
                FYearStep := 10
              Else
                If Delta >= 10 Then
                  FYearStep := 5
          End;
        If TPSCMonthHitTest(EventID) = mhtLeftArrow Then
          Year := FYear - FYearStep
        Else
          Year := FYear + FYearStep
      End
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthsBox.WMGetDlgCode(Var Message: TMessage);
Begin
  Inherited;
  Message.Result := Message.Result Or DLGC_WANTARROWS
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthsBox.DrawHeader;
Var
  Rect: TRect;
Begin
  GetRect(mhtHead,FMonth,Rect);
  With Canvas Do
    Begin
      Brush.Style := BrushStyle_Solid;
      Brush.Color := FHeaderColor;
      FillRect(Rect);

      If Ctl3D Then
        DrawEdge(Canvas.Handle,Rect,BDR_SUNKENOUTER,BF_ADJUST Or BF_MONO Or
          BF_RECT);
      DrawEdge(Canvas.Handle,Rect,BDR_RAISEDINNER,BF_ADJUST Or BF_RECT);

    End;
  DrawYear;
  DrawArrow(mhtLeftArrow);
  DrawArrow(mhtRightArrow)
End;

Const
  Edges: Array[Boolean] Of Cardinal = (BDR_RAISEDINNER,BDR_SUNKENOUTER);

Procedure TPSCMonthsBox.DrawYear;
Var
  Rect: TRect;
  ASize: TSize;
  S: String;
  Rgn: TPSCRegion;
  Delta: Integer;
  Hot,Down: Boolean;
Begin
  GetRect(mhtYear,FMonth,Rect);
  With Canvas Do
    Begin
      Hot := FShowEdges And (FSelected = mhtYear);
      Down := Hot And (MouseCapture Or FDown);
      If Down Then
        Delta := 1
      Else
        Delta := 0;
      Rgn := PSCGetClipRgn(Self,Canvas);
      With Rect Do
        IntersectClipRect(Canvas.Handle,Left + 2,Top + 2,Right - 2,Bottom - 2);
      Try
        S := PSCIntToStr(FYear);
        If Not FInheritHeaderFont Then
          Font := FHeaderFont;
        Try
          ASize:=Canvas.TextExtent(S);

          With Rect Do
            Canvas.TextRect(Rect,(Right + Left - ASize.cx) Div 2 + Delta,
              (Top + Bottom - ASize.cy) Div 2 + Delta,S);

        Finally
          If Not FInheritHeaderFont Then
            Font := Self.Font
        End;
      Finally
        PSCSelectClipRgn(Canvas,Rgn);
        PSCDeleteClipRgn(Rgn)
      End;
      If Hot Then
        Begin
          InflateRect(Rect, -2, -2);
          DrawEdge(Canvas.Handle,Rect,Edges[Down],BF_RECT Or BF_ADJUST)
        End
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthsBox.DrawArrow(Arrow: TPSCMonthHitTest);
Const
  ArrowKind: Array[mhtLeftArrow..mhtRightArrow] Of Cardinal = (
    PSC_DFCS_SCROLLLEFT,PSC_DFCS_SCROLLRIGHT);
Var
  Rect: TRect;
  Rgn: TPSCRegion;
  Delta: Integer;
  Hot,Down: Boolean;

  Procedure DrawOutlookArrow;
  Begin
    With Rect Do
      PSCDrawOutlookArrow(Canvas,Arrow = mhtLeftArrow,
        Classes.Rect(Left + Delta,Top + Delta,Right + Delta,Bottom + Delta))
  End;

  Procedure DrawMSMoneyArrow;
  Var
    OldBrushColor: TPSCColor;
  Begin
    With Canvas,Rect Do
      Begin
        OldBrushColor := Brush.Color;
        Try
          Pen.Color := FArrowColor;
          Brush.Color := FArrowColor;
          With Rect Do
            PSCDrawMSMoneyArrow(Canvas,Arrow = mhtLeftArrow,
              Classes.Rect(Left + Delta,Top + Delta,Right + Delta,Bottom +
                Delta))
        Finally
          Brush.Color := OldBrushColor
        End
      End
  End;

  Procedure DrawCustomArrow;
  Var
    Index: Integer;
  Begin
    If Arrow = mhtLeftArrow Then
      If Hot And (FLeftArrowHot <> -1) Then
        Index := FLeftArrowHot
      Else
        Index := FLeftArrow
    Else
      If Hot And (FRightArrowHot <> -1) Then
        Index := FRightArrowHot
      Else
        Index := FRightArrow;
    If (FImages = Nil) Or (Index = -1) Then
      DrawOutlookArrow
    Else
      With FImages,Rect Do
        Draw(Canvas,(Left + Right - Width) Div 2 + Delta,
          (Top + Bottom - Height) Div 2 + Delta,Index,Enabled);
  End;

Begin
  GetRect(Arrow,FMonth,Rect);
  With Canvas Do
    Begin
      Hot := FShowEdges And (FSelected = Arrow);
      Down := Hot And (MouseCapture Or FDown);
      If Down Then
        Delta := 1
      Else
        Delta := 0;
      Rgn := PSCGetClipRgn(Self,Canvas);
      With Rect Do
        IntersectClipRect(Canvas.Handle,Left + 2,Top + 2,Right - 2,Bottom - 2);
      Try
        Case FArrowStyle Of
          astOutlook: DrawOutlookArrow;
          astMSMoney: DrawMSMoneyArrow;
          astCustom: DrawCustomArrow
        End
      Finally
        PSCSelectClipRgn(Canvas,Rgn);
        PSCDeleteClipRgn(Rgn)
      End;
      If Hot Then
        Begin
          InflateRect(Rect, -2, -2);
          DrawEdge(Canvas.Handle,Rect,Edges[Down],BF_RECT Or BF_ADJUST)
        End
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthsBox.DrawCaption;
Var
  Rect: TRect;
  ASize: TSize;
  Color,TextColor: TPSCColor;
Begin
  GetRect(mhtCaption,FMonth,Rect);
  With Canvas Do
    Begin
      Color := Brush.Color;
      TextColor := Font.Color;
      Try
        Brush.Color := FCaptionColor;
        Font.Color := FCaptionTextColor;
        ASize:=Canvas.TextExtent(Caption);
        With Rect Do
          Canvas.TextRect(Rect,(Right + Left - ASize.cx) Div 2,
            (Top + Bottom - ASize.cy) Div 2,Caption);
      Finally
        Font.Color := TextColor;
        Brush.Color := Color
      End
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthsBox.DrawMonths;
Var
  I: Integer;
  Rect,R: TRect;
Begin
  GetRect(mhtMonths,FMonth,Rect);
  With Canvas Do
    Begin
      Brush.Style := BrushStyle_Solid;
      Brush.Color := Self.Brush.Color;

      For I := 1 To 12 Do
        DrawMonth(I);

      With Rect Do
        Begin
          If FShowVertLines Then
            For I := 1 To 2 Do
              Begin
                R := Classes.Rect(Left + I * (Right - Left) Div
                  3,Top,Right,Bottom);
                DrawEdge(Canvas.Handle,R,BDR_SUNKENOUTER,BF_LEFT)
              End;
          If FShowHorzLines Then
            For I := 1 To 3 Do
              Begin
                R := Classes.Rect(Left,Top + I * (Bottom - Top) Div
                  4,Right,Bottom);
                DrawEdge(Canvas.Handle,R,BDR_SUNKENOUTER,BF_TOP)
              End
        End
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthsBox.DrawMonth(Month: Integer);
Var
  Rect: TRect;
  ASize: TSize;
  S: String;
  Rgn: TPSCRegion;
  Delta: Integer;
  Hot,Down: Boolean;
Begin
  GetRect(mhtMonth,Month,Rect);
  With Canvas Do
    Begin
      Hot := FShowEdges And (FSelected = mhtMonth) And (FMonth = Month);
      Down := Hot And (MouseCapture Or FDown);
      If Down Then
        Delta := 1
      Else
        Delta := 0;
      Rgn := PSCGetClipRgn(Self,Canvas);
      With Rect Do
        IntersectClipRect(Canvas.Handle,Left,Top,Right,Bottom);
      Try
        Brush.Style := BrushStyle_Solid;
        If (FSelected = mhtMonth) And (FMonth = Month) Then
          Begin
            Brush.Color := FSelectedColor;
            Font.Color := FSelectedTextColor;
          End
        Else
          Begin
            Font.Color := Self.Font.Color;
            Brush.Color := Self.Brush.Color;
          End;
        S := PSCLongMonthNames(Month);
        ASize:=Canvas.TextExtent(S);
        If ASize.cx > Rect.Right - Rect.Left - 4 Then
          Begin
            S := PSCShortMonthNames(Month);
            S := PSCUpperCase(S[1])+Copy(S,2,MaxInt);
            ASize:=Canvas.TextExtent(S);
          End;
          
        With Rect Do
          Canvas.TextRect(Rect,(Right + Left - ASize.cx) Div 2 + Delta,
            (Top + Bottom - ASize.cy) Div 2 + Delta,S);

      Finally
        PSCSelectClipRgn(Canvas,Rgn);
        PSCDeleteClipRgn(Rgn);
      End;

      If Hot Then
        Begin
          DrawEdge(Canvas.Handle,Rect,Edges[Down],BF_RECT)
        End;
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthsBox.Paint;
Begin
  If Not HandleAllocated Then
    exit;
  Try
    Canvas.Brush := Brush;
    Canvas.Font := Font;
    DrawHeader;
    DrawCaption;
    DrawMonths;
  Finally
  End;
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthsBox.MouseMove(Shift: TShiftState; X,Y: Integer);
Var
  HitTest: TPSCMonthHitTest;
  M: Integer;
Begin
  Inherited MouseMove(Shift,X,Y);
  If Focused Then
    Begin
      If ssLeft In Shift Then
        Begin
          HitTest := GetHitTest(Point(X,Y),M);
          If (HitTest <> FSelButton) Or ((HitTest = mhtMonth) And (FMonth <> M))
            Then
            HitTest := mhtNoWhere
        End
      Else
        Begin
          HitTest := GetHitTest(Point(X,Y),M);
          If HitTest = mhtMonth Then
            Month := M
        End;
      Selected := HitTest
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthsBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X,Y: Integer);
Var
  M: Integer;
Begin
  Inherited MouseDown(Button,Shift,X,Y);
  If Not (csDesigning In ComponentState) And CanFocus Then
    Begin
      SetFocus;
      Selected := GetHitTest(Point(X,Y),M);
      If Selected = mhtMonth Then
        Month := M;
    End;

  If Button = mbRight Then
    exit;

  If (Selected In [mhtLeftArrow,mhtRightArrow,mhtYear,mhtMonth]) And
    (Selected In AllowedHitTests) Then
    Begin
      FSelButton := Selected;
      InvalidateArea(Selected,FMonth);
      If FSelButton In [mhtLeftArrow,mhtRightArrow] Then
        Begin
          FStart := FYear;
          FYearStep := 1;
          If FSelButton = mhtLeftArrow Then
            Year := FStart - 1
          Else
            Year := FStart + 1;

          If FTimer = nil Then
            FTimer := PSCSetTimer(Integer(FSelButton),170,TimerEvent);

        End
      Else
        If FSelButton = mhtYear Then
          If (FYearsPopup <> Nil) And FYearsPopup.Visible Then
            With TPSCYearsPopup(FYearsPopup) Do
              Begin
                ListBox.ItemIndex := -1;
                ClosePopup(false,True);
              End
          Else
            Begin
              SelectYear;
              TPSCYearsListBox(TPSCYearsPopup(FYearsPopup).ListBox).MouseCapture
                := true;
            End
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthsBox.KillTimer;
Begin
  PSCKillTimer(FTimer);
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthsBox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X,Y: Integer);
Begin
  Inherited MouseUp(Button,Shift,X,Y);
  If Focused And
    (FSelected In [mhtLeftArrow,mhtRightArrow,mhtYear,mhtMonth]) Then
    Begin
      InvalidateArea(FSelButton,FMonth);
      If (FSelected In [mhtLeftArrow,mhtRightArrow]) Then
        KillTimer
      Else
        If FSelected = mhtMonth Then
          DoSelect
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthsBox.KeyDown(Var Key: Word; Shift: TShiftState);

  Procedure MoveMonthSel(Delta: Integer);
  Begin
    If FMonth = 0 Then
      Month := 1
    Else
      Begin
        Delta := Delta + FMonth;
        If Delta < 1 Then
          Delta := 1
        Else
          If Delta > 12 Then
            Delta := 12;
        Month := Delta
      End
  End;

Begin
  If ssAlt In Shift Then
    Begin
      Month := 0;
      DoSelect;
      Key := 0
    End;
  If (Key In [Ord('Y'),VK_SPACE]) And (ssCtrl In Shift) Then
    Begin
      SelectYear;
      Key := 0
    End;

  Case Key Of
    VK_LEFT:
      Case Selected Of
        mhtRightArrow: Selected := mhtYear;
        mhtYear: Selected := mhtLeftArrow;
        mhtMonth: MoveMonthSel(-1)
      End;
    VK_RIGHT:
      Case Selected Of
        mhtLeftArrow: Selected := mhtYear;
        mhtYear: Selected := mhtRightArrow;
        mhtMonth: MoveMonthSel(1)
      End;
    VK_UP:
      If Selected = mhtMonth Then
        If FMonth <= 3 Then
          Selected := mhtYear
        Else
          MoveMonthSel(-3);
    VK_DOWN:
      If Selected <> mhtMonth Then
        Selected := mhtMonth
      Else
        If FMonth <= 9 Then
          MoveMonthSel(3);
    VK_NEXT: Year := Year + 1;
    VK_PRIOR: Year := Year - 1;
    VK_SPACE,VK_RETURN:
      Begin
        If (Selected In [mhtLeftArrow,mhtRightArrow,mhtYear,mhtMonth]) Then
          Begin
            If Not FDown Then
              Begin
                FDown := true;
                InvalidateArea(Selected,FMonth)
              End;
            Case Selected Of
              mhtLeftArrow: Year := Year - 1;
              mhtRightArrow: Year := Year + 1;
              mhtYear: SelectYear;
              mhtMonth:
                Begin
                  Update;
                  DoSelect
                End
            End
          End
      End;
    VK_ESCAPE:
      Begin
        Month := 0;
        Update;
        DoSelect
      End
  End;
  Inherited KeyDown(Key,Shift)
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthsBox.KeyUp(Var Key: Word; Shift: TShiftState);
Begin
  Inherited KeyUp(Key,Shift);
  Case Key Of
    VK_SPACE,VK_RETURN:
      If FDown Then
        Begin
          FDown := false;
          InvalidateArea(Selected,FMonth)
        End
  End
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthsBox.SetFlat(Value: Boolean);
Begin
  Ctl3D := Not Value
End;

{------------------------------------------------------------------------------}

Function TPSCMonthsBox.GetFlat: Boolean;
Begin
  Result := Not Ctl3D
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthsPopup.ResizeFormControls;
Begin
  Inherited;
End;

{------------------------------------------------------------------------------}

constructor TPSCMonthsPopup.CreateNew(AOwner: TComponent; Dummy: Integer=0);
Begin
  Inherited;
  FMonths := TPSCMonthsBox.Create(Self);
  With FMonths Do
    Begin
      Parent := Self;
      OnChangeDate := SelectMonth;
      Self.ClientWidth := Width;
      Self.ClientHeight := Height;
      Align := alClient;
    End;
  ResizeFormControls;
  ActiveControl := FMonths;
  BorderWidth:=0;
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthsPopup.SelectMonth(Sender: TObject);
Begin
  ClosePopup(false,True);
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthsPopup.CreateParams(Var Params: TCreateParams);
Begin
  Inherited CreateParams(Params);
  With Params Do
    Begin
      ExStyle := ExStyle And Not WS_EX_CLIENTEDGE;
      WindowClass.Style := WindowClass.Style And Not CS_SAVEBITS
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCMonthsPopup.WMEraseBkgnd(Var Message: TWMEraseBkgnd);
Begin
  Message.Result := 1
End;

{-------------------------------------------}

Procedure TPSCPopupTListBox.ListBoxMouseUp(Sender: TObject;
  Button: TMouseButton;Shift: TShiftState; X,Y: Integer);
var
  MyHidePopup:Boolean;
Begin
  With ListBox Do
    If (Button = mbLeft) And (ItemAtPos(Point(X,Y),True) >= 0) then
    Begin
      MyHidePopup:=not (ssCtrl in Shift);
      If ([ssShift,ssAlt] * Shift = []) Then
        ClosePopup(Not PtInRect(ClientRect,Point(X,Y)),MyHidePopup);
    End;
End;

{-------------------------------------------}

Function TPSCPopupTListBox.GetListBoxClass: TListBoxClass;
Begin
  Result := TListBox;
End;

{-------------------------------------------}

constructor TPSCPopupTListBox.CreateNew(AOwner: TComponent; Dummy: Integer=0);
Begin
  Inherited;
  FListBox := GetListBoxClass.Create(Self);
  FListBox.Parent := Self;
  FListBox.BorderStyle := bsNone;
  FListBox.OnMouseUp := ListBoxMouseUp;
  ClientHeight := FListBox.Height;
  ClientWidth := FListBox.Width;
  FListBox.Align := AlClient;
  ActiveControl := FListBox;
  BorderWidth:=0;{don't remove this line}
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendarPro.OpenMonthPopup(Const Date: TDateTime);
Var
  R: TRect;
  AYear,AMonth,ADay: Word;
Begin
  If FPopupType = ptStandard Then
    Inherited OpenMonthPopup(Date)
  Else
    Begin
      FMonths.Free;
      FMonths := TPSCMonthsPopup.CreateNew(Nil,0);
      PSCDecodeDate(Date,AYear,AMonth,ADay);
      With TPSCMonthsPopup(FMonths).Months Do
        Begin
          FMonths.ClientWidth := GetMonthWidth;
          FMonths.ClientHeight := GetMonthHeight;
          FMonths.Color := Self.Colors.Days;
          FMonths.Font := Self.Colors.DaysFont;
            HeaderFont := Self.Colors.HeaderFont;
          HeaderHeight := Self.HeaderHeight;
          Ctl3D := Self.Ctl3D;
          Flat:=true;
          YearsPopupCtl3D := FCalendarStyle = cstMSMoney;
          If FCalendarStyle = cstMSMoney Then
            CaptionColor := Colors.WeekHeader
          Else
            CaptionColor := Self.Color;
          ParentColor := true;
          ParentFont := false;
          Year := AYear;
          Month := AMonth;
          HeaderColor := Self.Colors.MonthHeader;
          CaptionTextColor := Self.Colors.DaysFont.Color;
          SelectedColor := Self.Colors.Selected;
          SelectedTextColor := Self.Colors.SelectedText;
          ShowEdges := Self.ShowArrowEdges;
          ArrowColor := Self.Colors.ArrowColor;
          ArrowStyle := Self.ArrowStyle;
          Images := Self.Images;
          LeftArrow := Self.LeftArrow;
          LeftArrowHot := Self.LeftArrowHot;
          RightArrow := Self.RightArrow;
          RightArrowHot := Self.RightArrowHot;
          ShowVertLines := Self.ShowVertLines;
          ShowHorzLines := Self.ShowHorzLines;
          FPopupDate := Date;
          GetHeaderRect(Date,R);
          With TPSCMonthsPopup(FMonths) ,ClientRect Do
            PopupExact(Self,R.Left - (Width - Right) Div 2,
              R.Top - (Height - Bottom) Div 2,PopupClosed)
        End
    End
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendarPro.PopupClosed(Sender: TObject; Canceled: Boolean);
Var
  Col,Row: Integer;
  Date: TDateTime;
Begin
  If (FMonths <> Nil) And Not Canceled Then
    Begin
      With TPSCMonthsPopup(FMonths).Months Do
        If Month = 0 Then
          Exit
        Else
          Date := PSCEncodeDate(Year,Month,1);
      If DateToPos(FPopupDate,Col,Row) Then
        Date := PSCIncMonth(Date, -(Col + Row * MonthColCount));
      StartDate := Date;
      If CanFocus Then
        SetFocus;
    End;
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendarPro2.IsDayHeaderHorzAlignStored: boolean;
Begin
  result := FCalendarState.DayHeaderHorzAlign <> DayHeaderHorzAlign;
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendarPro2.SetDayHeaderHorzAlign(
  AValue: TPSCHorzAlign);
Begin
  If FDayHeaderHorzAlign <> AValue Then
  Begin
    FDayHeaderHorzAlign := AValue;
    Changed;
  End;
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendarPro2.SetMouseDate(Const Value: TDateTime);
var
  OldMouseDate : TDateTime;
Begin
  If FMouseDate <> Value Then
  Begin
    OldMouseDate := FMouseDate;
    FMouseDate := Value;
    If IsLeftMouseDown Then
    Begin
      InvalidateRect(GetCalDateRect(OldMouseDate));{WAS TRUE}
      InvalidateRect(GetCalDateRect(FMouseDate));{WAS TRUE}
    End;
  End;
End;

{------------------------------------------------------------------------------}

procedure TPSCCustomCalendarPro2.MouseMove(Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  MouseDate := GetMouseDate;
end;

{------------------------------------------------------------------------------}

function TPSCCustomCalendarPro2.GetCalDateRect(ADate : TDateTime): TRect;
var
  MyStartDate : TDateTime;
  MyDateIndent : integer;
  IndentX, IndentY : integer;
  MyMonthCol, MyMonthRow : integer;
  StartDaysX, StartDaysY : integer;
  MyStartDaysRect : TRect;
  MyCursorPos : TPoint;
  MyRect : TRect;
begin
  result := Rect(0, 0, 0, 0);
  GetCursorPos(MyCursorPos);
  MyCursorPos := ScreenToClient(MyCursorPos);
  MyStartDaysRect := UpdateTopLeftCorner(ClientRect);
  DateToPos(ADate, MyMonthCol, MyMonthRow);
   if (MyMonthCol > (MonthColCount - 1)) or (MyMonthRow > (MonthRowCount - 1)) then
    exit;
  MyStartDate := GetMonthStartDate(MyMonthCol, MyMonthRow, true);
  StartDaysX := MyStartDaysRect.Left + SideWidth + MyMonthCol * GetMonthWidth;
  if FShowWeekNumbers then
    StartDaysX := StartDaysX + 4;
  StartDaysY := MyStartDaysRect.Top + DaysOfWeekHeight + 2 + HeaderHeight +
   MyMonthRow * GetMonthHeight;
  MyDateIndent := Round(ADate - MyStartDate);
  IndentY := MyDateIndent div 7;
  IndentX := MyDateIndent mod 7;
  MyRect := Rect(ColWidth * IndentX + StartDaysX, RowHeight * IndentY + StartDaysY,
   ColWidth * (IndentX + 1) + StartDaysX, RowHeight * (IndentY + 1) + StartDaysY);
   result := MyRect;
end;

{------------------------------------------------------------------------------}

Function TPSCCalendarColorsPro.IsArrowColorStored: boolean;
Begin
  with TPSCCustomCalendarPro2(Owner) do
    result := FCalendarState.ArrowColor <> FArrowColor;
End;

{------------------------------------------------------------------------------}

Function TPSCCalendarColorsPro.IsWeekLineColorStored: boolean;
Begin
  with TPSCCustomCalendarPro2(Owner) do
    result := FCalendarState.WeekLineColor <> FWeekLineColor;
End;

{------------------------------------------------------------------------------}

Function TPSCCalendarColorsPro.IsGrayedBkColorStored: boolean;
Begin
  with TPSCCustomCalendarPro2(Owner) do
    result := (FCalendarState.GrayedBkColor <> GrayedBkColor);
End;

{------------------------------------------------------------------------------}

function TPSCCustomCalendarPro2.IsColorStored: boolean;
begin
  result := FCalendarState.Color <> Color;
end;

{------------------------------------------------------------------------------}

function TPSCCustomCalendarPro2.IsShortMonthNameStored: boolean;
begin
  result := FCalendarState.ShortMonthName <> ShortMonthName;
end;

{------------------------------------------------------------------------------}

function TPSCCustomCalendarPro2.IsShowNavButtonsStored: boolean;
begin
  result := FCalendarState.ShowNavButtons <> ShowNavButtons;
end;

{------------------------------------------------------------------------------}

function TPSCCustomCalendarPro2.IsShowMonthDividersStored: boolean;
begin
  result := FCalendarState.ShowMonthDividers <> ShowMonthDividers;
end;

{------------------------------------------------------------------------------}

function TPSCCustomCalendarPro2.IsHeaderCaseStored: boolean;
begin
  result := FCalendarState.HeaderCase <> HeaderCase;
end;

{------------------------------------------------------------------------------}

function TPSCCustomCalendarPro2.IsNavButtonsSpeedStored: boolean;
begin
  result := FCalendarState.NavButtonsSpeed <> NavButtonsSpeed;
end;

{------------------------------------------------------------------------------}

function TPSCCustomCalendarPro2.IsShowArrowEdgesStored: boolean;
begin
  result := FCalendarState.ShowArrowEdges <> ShowArrowEdges;
end;

{------------------------------------------------------------------------------}

function TPSCCustomCalendarPro2.IsWeekDayNamesStored: boolean;
begin
  result := FCalendarState.WeekDayNames <> WeekDayNames;
end;

{------------------------------------------------------------------------------}

function TPSCCustomCalendarPro2.IsFlatStored: boolean;
begin
  result := FCalendarState.Flat <> Flat;
end;

{------------------------------------------------------------------------------}

function TPSCCustomCalendarPro2.IsPopupTypeStored: boolean;
begin
  result := FCalendarState.PopupType <> PopupType;
end;

{------------------------------------------------------------------------------}

function TPSCCustomCalendarPro2.IsArrowStyleStored: boolean;
begin
  result := FCalendarState.ArrowStyle <> ArrowStyle;
end;

{------------------------------------------------------------------------------}

function TPSCCustomCalendarPro2.IsShowTodayStored: boolean;
begin
  result := FCalendarState.ShowToday <> ShowToday;
end;

{------------------------------------------------------------------------------}

function TPSCCustomCalendarPro2.IsBorderStyleStored: boolean;
begin
  result := FCalendarState.BorderStyle <> BorderStyle;
end;

{------------------------------------------------------------------------------}

function TPSCCalendarColorsPro.IsDayLinesStyleStored: boolean;
begin
  with TPSCCustomCalendarPro2(Owner) do
  result := FCalendarState.DayLinesStyle <> DayLinesStyle;
end;

{------------------------------------------------------------------------------}

procedure TPSCCustomCalendar.SetOptions(V:TPSCCalendarOptions);
begin
  If FOptions<>V then
  begin
    FOptions:=V;
    Invalidate;
  end;
end;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendarPro2.IsOptionsStored:Boolean;
begin
  Result:=Options<>FCalendarState.Options;
end;

{------------------------------------------------------------------------------}

function TPSCCustomCalendarPro2.IsExtendedSelectStored: boolean;
begin
  result := FCalendarState.ExtendedSelect <> ExtendedSelect;
end;

{------------------------------------------------------------------------------}

function TPSCCustomCalendarPro2.IsSelectKindStored: boolean;
begin
  result := FCalendarState.SelectKind <> SelectKind;
end;

{------------------------------------------------------------------------------}

function TPSCCustomCalendarPro2.IsShowHorzLinesStored: boolean;
begin
  result := FCalendarState.ShowHorzLines <> ShowHorzLines;
end;

{------------------------------------------------------------------------------}

function TPSCCustomCalendarPro2.IsShowMonthPopupStored: boolean;
begin
  result := FCalendarState.ShowMonthPopup <> ShowMonthPopup;
end;

{------------------------------------------------------------------------------}

function TPSCCustomCalendarPro2.IsShowVertLinesStored: boolean;
begin
  result := FCalendarState.ShowVertLines <> ShowVertLines;
end;

{------------------------------------------------------------------------------}

function TPSCCustomCalendarPro2.IsWeekCursorStored: boolean;
begin
  result := FCalendarState.WeekCursor <> WeekCursor;
end;

{------------------------------------------------------------------------------}

Function TPSCCalendarColorsPro.IsBorderStored(Index : integer): boolean;
Begin
  with TPSCCustomCalendarPro2(Owner) do
    result := FCalendarState.Border <> Border;
End;

{------------------------------------------------------------------------------}

Function TPSCCalendarColorsPro.IsDayLinesStored(Index : integer): boolean;
Begin
  with TPSCCustomCalendarPro2(Owner) do
    result := FCalendarState.DayLines <> DayLines;
End;

{------------------------------------------------------------------------------}

Function TPSCCalendarColorsPro.IsDaysStored(Index : integer): boolean;
Begin
  with TPSCCustomCalendarPro2(Owner) do
    result := FCalendarState.Days <> Days;
End;

{------------------------------------------------------------------------------}

Function TPSCCalendarColorsPro.IsGrayedStored(Index : integer): boolean;
Begin
  with TPSCCustomCalendarPro2(Owner) do
    result := FCalendarState.Grayed <> Grayed;
End;

{------------------------------------------------------------------------------}

Function TPSCCalendarColorsPro.IsMonthHeaderStored(Index : integer): boolean;
Begin
  with TPSCCustomCalendarPro2(Owner) do
    result := FCalendarState.MonthHeader <> MonthHeader;
End;

{------------------------------------------------------------------------------}

Function TPSCCalendarColorsPro.IsNowRectStored(Index : integer): boolean;
Begin
  with TPSCCustomCalendarPro2(Owner) do
    result := FCalendarState.NowRect <> NowRect;
End;

{------------------------------------------------------------------------------}

Function TPSCCalendarColorsPro.IsSelectedStored(Index : integer): boolean;
Begin
  with TPSCCustomCalendarPro2(Owner) do
    result := FCalendarState.Selected <> Selected;
End;

{------------------------------------------------------------------------------}

Function TPSCCalendarColorsPro.IsSelectedTextStored(Index : integer): boolean;
Begin
  with TPSCCustomCalendarPro2(Owner) do
    result := FCalendarState.SelectedText <> SelectedText;
End;

{------------------------------------------------------------------------------}

Function TPSCCalendarColorsPro.IsWeekEndTextStored(Index : integer): boolean;
Begin
  with TPSCCustomCalendarPro2(Owner) do
    result := FCalendarState.WeekEndText <> WeekEndText;
End;

{------------------------------------------------------------------------------}

Function TPSCCalendarColorsPro.IsWeekHeaderStored(Index : integer): boolean;
Begin
  with TPSCCustomCalendarPro2(Owner) do
    result := FCalendarState.WeekHeader <> WeekHeader;
End;

{------------------------------------------------------------------------------}

Function TPSCCalendarColorsPro.IsWeekSideStored(Index : integer): boolean;
Begin
  with TPSCCustomCalendarPro2(Owner) do
    result := FCalendarState.WeekSide <> WeekSide;
End;

{------------------------------------------------------------------------------}

procedure TPSCCustomCalendarPro2.SetOtherProperties;
Var
  MyDefFontAttr : TPSCFontAttr;
  MyFontAttr : TPSCFontAttr;
begin
  With MyDefFontAttr, Colors Do
  Begin
    Color := Font.Color;
    Size := Font.Size;
    Style := Font.Style;

    MyFontAttr := MyDefFontAttr;
    MyFontAttr.Color := FCalendarState.WeekNumbersFont.Color;
    TPSCParentedFont(WeekNumbersFont).FDefFontAttr := MyFontAttr;

    MyFontAttr := MyDefFontAttr;
    MyFontAttr.Color := FCalendarState.WeekDaysFont.Color;
    TPSCParentedFont(WeekDaysFont).FDefFontAttr := MyFontAttr;

    MyFontAttr := MyDefFontAttr;
    MyFontAttr.Color := FCalendarState.HeaderFont.Color;
    MyFontAttr.Size := FCalendarState.HeaderFont.Size;
    MyFontAttr.Style := FCalendarState.HeaderFont.Style;
    TPSCParentedFont(HeaderFont).FDefFontAttr := MyFontAttr;

    MyFontAttr := MyDefFontAttr;
    MyFontAttr.Color := FCalendarState.DaysFont.Color;
    MyFontAttr.Style := FCalendarState.DaysFont.Style;
    TPSCParentedFont(DaysFont).FDefFontAttr := MyFontAttr;

    MyFontAttr := MyDefFontAttr;
    MyFontAttr.Color := FCalendarState.HolidaysFont.Color;
    MyFontAttr.Style := FCalendarState.HolidaysFont.Style;
    TPSCParentedFont(HolidaysFont).FDefFontAttr := MyFontAttr;
  End;

  Color := FCalendarState.Color;
  CalendarsInHeight := FCalendarState.CalendarsInHeight;
  CalendarsInWidth := FCalendarState.CalendarsInWidth;
  WantEsc := FCalendarState.WantEsc;
  SelectionAndBorderDists := FCalendarState.SelectionAndBorderDists;
  ShortMonthName := FCalendarState.ShortMonthName;
  ShowNavButtons := FCalendarState.ShowNavButtons;
  ShowMonthDividers := FCalendarState.ShowMonthDividers;
  DayHeaderStyle := FCalendarState.DayHeaderStyle;
  AlterEvenOdd := FCalendarState.AlterEvenOdd;
  FirstDayFormat := FCalendarState.FirstDayFormat;
  ShowFocusRect := FCalendarState.ShowFocusRect;
  HeaderCase := FCalendarState.HeaderCase;
  PastDaysAsGrayed := FCalendarState.PastDaysAsGrayed;
  NavButtonsSpeed := FCalendarState.NavButtonsSpeed;
  TodayStyle := FCalendarState.TodayStyle;
  ShowArrowEdges := FCalendarState.ShowArrowEdges;
  HeaderFormat := FCalendarState.HeaderFormat;
  with Colors do
  begin
    DaysFont.Color := FCalendarState.DaysFont.Color;
    DaysFont.Style := FCalendarState.DaysFont.Style;

    ArrowColor := FCalendarState.ArrowColor;
    Border := FCalendarState.Border;
    MonthHeader := FCalendarState.MonthHeader;
    WeekHeader := FCalendarState.WeekHeader;
    Days := FCalendarState.Days;
    DayLines := FCalendarState.DayLines;
    Selected := FCalendarState.Selected;
    SelectedText := FCalendarState.SelectedText;
    WeekDaysFont.Color := FCalendarState.WeekDaysFont.Color;
    WeekNumbersFont.Color := FCalendarState.WeekNumbersFont.Color;
    HolidaysFont.Color := FCalendarState.HolidaysFont.Color;
    HolidaysFont.Style := FCalendarState.HolidaysFont.Style;
    HeaderFont.Color := FCalendarState.HeaderFont.Color;
    HeaderFont.Style := FCalendarState.HeaderFont.Style;
    HeaderFont.Size := FCalendarState.HeaderFont.Size;
    Grayed := FCalendarState.Grayed;
    WeekEndText := FCalendarState.WeekEndText;
    NowRect := FCalendarState.NowRect;
    WeekSide := FCalendarState.WeekSide;
    WeekDayNames := FCalendarState.WeekDayNames;
    WeekLineColor := FCalendarState.WeekLineColor;
    HeaderBorderColor := FCalendarState.HeaderBorderColor;
    GrayedBkColor := FCalendarState.GrayedBkColor;
    DayLinesStyle := FCalendarState.DayLinesStyle;
  end;
  Flat := FCalendarState.Flat;
  WeekLineStyle := FCalendarState.WeekLineStyle;
  SelectStyle := FCalendarState.SelectStyle;
  PopupType := FCalendarState.PopupType;
  ShowHeader := FCalendarState.ShowHeader;
  HeaderStyle := FCalendarState.HeaderStyle;
  ArrowStyle := FCalendarState.ArrowStyle;
  WeekDayCase := FCalendarState.WeekDayCase;
  SizeStyle := FCalendarState.SizeStyle;
  ShowFooter := FCalendarState.ShowFooter;
  ShowToday := FCalendarState.ShowToday;
  DayVertAlign := FCalendarState.DayVertAlign;
  DayHorzAlign := FCalendarState.DayHorzAlign;
  DayHeaderHorzAlign := FCalendarState.DayHeaderHorzAlign;
  BorderStyle := FCalendarState.BorderStyle;
  ShowDayDelimiters := FCalendarState.ShowDayDelimiters;
  SelectKind := FCalendarState.SelectKind;
  ShowVertLines := FCalendarState.ShowVertLines;
  ShowHorzLines := FCalendarState.ShowHorzLines;
  ShowMonthPopup := FCalendarState.ShowMonthPopup;
  WeekCursor := FCalendarState.WeekCursor;
  ExtendedSelect := FCalendarState.ExtendedSelect;
  Options := FCalendarState.Options;
end;

{------------------------------------------------------------------------------}

Function TPSCCalendarColorsPro.IsHeaderBorderColorStored: boolean;
Begin
  with TPSCCustomCalendarPro2(Owner) do
  result := FCalendarState.HeaderBorderColor <> HeaderBorderColor;
End;

{------------------------------------------------------------------------------}

function PSCGetMonthsBetween(const ANow, AThen: TDateTime) : integer;
var
  MyYearNow, MyMonthNow, MyDayNow : word;
  MyYearThen, MyMonthThen, MyDayThen : word;
begin
  PSCDecodeDate(ANow, MyYearNow, MyMonthNow, MyDayNow);
  PSCDecodeDate(AThen, MyYearThen, MyMonthThen, MyDayThen);
  result := (MyYearNow - MyYearThen) * 12 + (MyMonthNow - MyMonthThen);
  if result < 0 then
    result := - result;
end;

{------------------------------------------------------------------------------}

Function TPSCCalendarColorsPro.IsHolidaysFontStored: boolean;
begin
  result := TPSCParentedFont(HolidaysFont).IsFontStored;
end;

{------------------------------------------------------------------------------}

Function TPSCCalendarColorsPro.IsDaysFontStored: boolean;
Begin
  result := TPSCParentedFont(DaysFont).IsFontStored;
End;

{------------------------------------------------------------------------------}

Procedure TPSCCalendarColors.SetDaysFont(Value: TPSCFont);
Begin
  if FDaysFont <> Value then
    FDaysFont.Assign(Value);
End;

{------------------------------------------------------------------------------}

function TPSCParentedFont.IsFontStored:Boolean;
begin
  Result:=IsCharsetStored or IsColorStored or IsHeightStored or IsNameStored or
    IsPitchStored or IsSizeStored or IsStyleStored;
end;

{------------------------------------------------------------------------------}

constructor TPSCParentedFont.Create(AOwner : TControl);
begin
  Inherited Create;
  FOwner := AOwner;
end;

{------------------------------------------------------------------------------}

function TPSCParentedFont.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

{------------------------------------------------------------------------------}

function TPSCParentedFont.IsCharsetStored: boolean;
begin
  result := (Charset <> THackControl(FOwner).Font.Charset);
end;

{------------------------------------------------------------------------------}

function TPSCParentedFont.IsColorStored: boolean;
begin
  result := (Color <> THackControl(FOwner).Font.Color) and
   (Color <> FDefFontAttr.Color);
end;

{------------------------------------------------------------------------------}

function TPSCParentedFont.IsHeightStored: boolean;
begin
  result := False;{(Height <> THackControl(FOwner).Font.Height);}
end;

{------------------------------------------------------------------------------}

function TPSCParentedFont.IsNameStored: boolean;
begin
  result := (Name <> THackControl(FOwner).Font.Name);
end;

{------------------------------------------------------------------------------}

function TPSCParentedFont.IsPitchStored: boolean;
begin
  result := Pitch <> THackControl(FOwner).Font.Pitch;
end;

{------------------------------------------------------------------------------}

function TPSCParentedFont.IsSizeStored: boolean;
begin
  result := (Size <> THackControl(FOwner).Font.Size) and
   (Size <> FDefFontAttr.Size);
end;

{------------------------------------------------------------------------------}

function TPSCParentedFont.IsStyleStored: boolean;
begin
  result := (Style <> THackControl(FOwner).Font.Style) and
   (Style <> FDefFontAttr.Style);
end;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendar.CorrectStartDate(Value: TDateTime) : TDateTime;
Begin
  result := PSCGetMonthStart(Int(Value));
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendarPro2.DoMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint): Boolean;
Begin
  Result := true;
  If WheelDelta > 0 Then
  Begin
    If ssCtrl In Shift Then
      StartFromPrevYear
    Else
      StartFromPrevMonth;
  End
  Else
    If ssCtrl In Shift Then
      StartFromNextYear
    Else
      StartFromNextMonth;
End;

{------------------------------------------------------------------------------}

procedure TPSCCustomCalendarPro2.DrawWeekSide(const Date: TDateTime;
  Side: TPSCCalendarHitTest; DateType: TPSCCalendarDateType;
  const Rect: TRect);
Const
  FlatBounds: Array[Boolean] Of Integer = (1,0);
Var
  MyOldPenColor : TPSCColor;
Begin
  With Canvas Do
  Begin
    MyOldPenColor := Pen.Color;
    Pen.Color := Colors.WeekLineColor;
    Try
      Inherited;
    Finally
      Pen.Color := MyOldPenColor;
    End;
  End;
End;

End.

