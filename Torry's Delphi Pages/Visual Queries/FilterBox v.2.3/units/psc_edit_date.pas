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
unit psc_edit_date;

interface
{$I psc_defines.inc}

Uses
  winapi.Windows,
  winapi.messages,
  forms,
  controls,
  classes,
  db,
  System.Types,
  
  myla_system,
  myla_interfaces,

  psc_parser_date,
  psc_calendar,
  psc_edit,
  psc_edit_parts,
  psc_procs,
  psc_wrapper,
  psc_const;

{-------------------------------------}

Const
  cPSCDefDTBackColor = clCalendarColor;
  cPSCDefDTTextColor = clCalendarTextColor;
  cPSCDefDTTitleBackColor = clMonthHeaderColor;
  cPSCDefDTTitleTextColor = clCalendarTextColor;
  cPSCDefDTMonthBackColor = clCalendarColor;
  cPSCDefDTTrailingTextColor = clGrayedColor;

  cPSCDefIntegralSize = True;
  cPSCDefShowTodayButton = True;
  cPSCDefBevelOuter = bvNone;
  cPSCDefShowDateEdit = True;

Type
  TPSCGetDateTimeProc=function:TDateTime of object;
  TPSCSetDateTimeProc=procedure(const AValue:TDateTime) of object;

  IPSCTextPartDateTime=interface(IPSCTextPartInteger)
    ['{766CEEB2-92E2-4309-AE4D-D7656BD47D43}']
    function GetDateTimePartKind:TPSCDateTimePart;
    procedure SetDateTimePartKind(V:TPSCDateTimePart);

    property DateTimePartKind:TPSCDateTimePart
      Read GetDateTimePartKind Write SetDateTimePartKind;
  end;

  TPSCTextPartDateTimeElement = class(TPSCTextPartInteger,IPSCTextPartDateTime)
  private
    FGetValue:TPSCGetDateTimeProc;
    FSetValue:TPSCSetDateTimeProc;
    FFormat:IPSCDateTimeFormat;
    FKind:TPSCDateTimePart;
    function GetDateTimePartKind:TPSCDateTimePart;
    procedure SetDateTimePartKind(V:TPSCDateTimePart);
  protected
    procedure SetDateTime(const AValue:TDateTime);
    Procedure SetMinValue(AValue: Integer);override;
    Procedure SetMaxValue(AValue: Integer);override;

    Function GetElementIndex:Integer;virtual;
    Function GetDateTime:TDateTime;
    Function GetMinValue: Integer;override;

    property DateTimeFormat:IPSCDateTimeFormat Read FFormat;
  public
    constructor Create(AGetValue:TPSCGetDateTimeProc;
      ASetValue:TPSCSetDateTimeProc;const AFormat:IPSCDateTimeFormat);virtual;
  end;

  TPSCTextPartDateTimeElementClass=class of TPSCTextPartDateTimeElement;

  TPSCPopupCalendar=class;

  TPSCDateTimeUpDown=class(TPSCCustomPartsEdit)
  private
    FSelStartEdit: TPSCDateTimeUpDown;
    FSelEndEdit: TPSCDateTimeUpDown;
    FDateTime: TDateTime;
    FDateTimeFormat:TPSCDateTimeFormat;
    FKind:TPSCDateTimeKind;
    FPartsEnabled:TPSCDateTimeParts;
    FPartsChecked:TPSCDateTimeParts;
    FPartsCheckVisible:TPSCDateTimeParts;
    FShowDateEdit:Boolean;

    Procedure SetShowDateEdit(V: boolean);
    procedure SetPartsEnabled(V:TPSCDateTimeParts);
    procedure SetPartsChecked(V:TPSCDateTimeParts);
    procedure SetPartsCheckVisible(V:TPSCDateTimeParts);
    procedure SetDateTimeFormat(V:TPSCDateTimeFormat);
    procedure DateFormatChanged(Sender:TObject);
    procedure SetKind(V:TPSCDateTimeKind);
    procedure AddPart(const APart:TPSCDTFormatPart);overload;
    procedure AddPart_AMPM(const APart:TPSCDTFormatPart);
    procedure AddPart_Month(const APart:TPSCDTFormatPart);
    procedure AddPart_Day(const APart:TPSCDTFormatPart);
    procedure AddPart_Time(const APart:TPSCDTFormatPart;
      AClass:TPSCTextPartDateTimeElementClass;APartKind:TPSCDateTimePart);
    procedure AddPart_Delimeter(const S:String;APartKind:TPSCDateTimePart);
    procedure AddPart_Hour(const APart:TPSCDTFormatPart);
    procedure AddPart_Year(const APart:TPSCDTFormatPart);
    procedure AddPart(const APart:IPSCTextPart;APartKind:TPSCDateTimePart);overload;
    Procedure SetDateTime(Const V: TDateTime);

    function GetPopupCalendar: TPSCPopupCalendar;
    Function GetDateTime:TDateTime;

    function GetTime:TTime;
    function GetDate:TDate;
    procedure SetTime(const ATime:TTime);
    procedure SetDate(const ADate:TDate);

  protected
    Procedure TextPartsChanged(Sender: TObject; AItem: TPSCNamedItem);override;
    Procedure PopupCloseEvent(Sender: TObject; Canceled: Boolean); override;
    Procedure Loaded;override;
    Procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    Procedure UpdatePopup; override;

    Function PostValue: Boolean;override;
    Function CreatePopup: TPSCPopupForm; override;
  public
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
    Property CalendarPopup: TPSCPopupCalendar read GetPopupCalendar;
  published
    Property SelStartEdit: TPSCDateTimeUpDown read FSelStartEdit write
      FSelStartEdit;
    Property SelEndEdit: TPSCDateTimeUpDown read FSelEndEdit write FSelEndEdit;
    Property ShowDateEdit: boolean read FShowDateEdit write SetShowDateEdit
      default True;
    Property DateTime: TDateTime read GetDateTime write SetDateTime;

    Property Time: TTime read GetTime write SetTime Stored False;
    Property Date: TDate read GetDate write SetDate Stored False;

    property PartsEnabled:TPSCDateTimeParts Read FPartsEnabled Write SetPartsEnabled Default [];
    property PartsChecked:TPSCDateTimeParts Read FPartsChecked Write SetPartsChecked Default [];
    property PartsCheckVisible:TPSCDateTimeParts Read FPartsCheckVisible Write SetPartsCheckVisible Default [];

    property DateTimeFormat:TPSCDateTimeFormat Read FDateTimeFormat Write SetDateTimeFormat;
    property Kind:TPSCDateTimeKind Read FKind Write SetKind Default cpkDateTime;
    Property ButtonsVisible default True;
    Property FlatCheckBoxes;
    Property ShowCheckBoxes;

    Property BtnKind default bkUpDown;
    Property SelectedPart;
    Property OnDropDown;
    Property OnCloseUp;
    Property PopupColor;
    Property WantReturns;
    Property OnButtonClick;
    Property OnButtonDown;
    Property OnButtonUp;

    Property Constraints;
    Property Anchors;
    Property DragKind;
    Property AutoSize;
    Property DragMode;
    Property Enabled;
    Property Font;
    Property DragCursor;
    Property ParentFont;
    Property ParentShowHint;
    Property PopupMenu;
    Property ShowHint;
    Property TabOrder;
    Property TabStop;
    Property Visible;

    Property OnChange;
    Property OnClick;
    Property OnDblClick;
    Property OnEnter;
    Property OnExit;
    Property OnKeyDown;
    Property OnKeyPress;
    Property OnKeyUp;
    Property OnMouseDown;
    Property OnMouseMove;
    Property OnMouseUp;

    Property OnContextPopup;
    Property OnDragDrop;
    Property OnDragOver;
    Property OnEndDock;
    Property OnEndDrag;
    Property OnStartDock;
    Property OnStartDrag;
  end;

  TPSCDaySelectedEvent = Procedure(Sender: TObject; const ADate: TDateTime;
    ANoneBtn:WordBool) Of Object;

  TPSCCalendarPanelButton=(cpbTodayButton,cpbNoneButton);

  TPSCCustomCalendarPanel = Class(TPSCPanel)
  private
    FShowDateEdit: boolean;
    FDateEdit: TPSCDateTimeUpDown;
    FTimeEdit: TPSCDateTimeUpDown;
    FDateLabel: TPSCImage;
    FTimeLabel: TPSCImage;
    FPanelKind: TPSCDateTimeKind;
    FIntegralSize: Boolean;
    FCalendar: TPSCCustomCalendar;
    FOnDaySelected: TPSCDaySelectedEvent;

    procedure DateEditOnChange(Sender:TObject);
    Procedure UpdateWidthHeight(Var cx,cy: Integer);
    Procedure SetCalendarBorderStyle(V: TBorderStyle);
    Procedure SetShowDateEdit(V: boolean);
    Procedure CalendarToDateEdit;
    Procedure SetPanelKind(Value: TPSCDateTimeKind);
    Procedure SetDate(Value: TDateTime);
    Procedure SetTime(Value: TDateTime);
    Procedure SetDateTime(Value: TDateTime);
    Procedure DateChanged(Sender: TObject);
    Procedure SetIntegralSize(AValue: Boolean);
    Procedure CalendarMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
    Procedure SetMaxDate(Const AValue: TDateTime);
    Procedure SetMaxDateLimit(AValue: Boolean);
    Procedure SetMinDate(Const AValue: TDateTime);
    Procedure SetMinDateLimit(AValue: Boolean);
    Procedure SetVisibility(ABtn: TControl; AVisible: Boolean);

    Function GetCalendarBorderStyle: TBorderStyle;
    Function GetCalendar: TPSCCalendar;
    Function GetDate: TDateTime;
    Function GetTime: TDateTime;
    Function GetDateTime: TDateTime;
    Function GetMaxDate: TDateTime;
    Function GetMaxDateLimit: Boolean;
    Function GetMinDate: TDateTime;
    Function GetMinDateLimit: Boolean;
  protected
    Function CreateCalendar: TPSCCustomCalendar; virtual;
    Function CalculateIntegratedHeight: Integer;
    Function CalculateIntegratedWidth: Integer;
    Function DateInRange(ADate: TDateTime): Boolean;

    Procedure UpdatePanel; virtual;
    Procedure AlignContents; virtual;
    Procedure AlignControls(AControl: TControl; Var Rect: TRect); override;
    Procedure CMFontChanged(Var Message: TMessage); message CM_FontChanged;
    Procedure CreateWnd; override;
    Procedure DoDaySelected(Sender: TObject; const ADate: TDateTime;
      ANoneBtn:WordBool);
  public
    Function GetMinHeight: Integer;
    Function GetMinWidth: Integer;
    Function IsEmpty: Boolean;

    procedure ButtonClick(ABtnKind:TPSCCalendarPanelButton);
    Procedure WMWindowPosChanging(Var Message: TWMWindowPosChanging);
      message WM_WINDOWPOSCHANGING;

    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    Property DateLabel: TPSCImage Read FDateLabel;
    Property TimeLabel: TPSCImage Read FTimeLabel;
    Property Calendar: TPSCCalendar read GetCalendar;
    Property CalendarBorderStyle: TBorderStyle read GetCalendarBorderStyle write
      SetCalendarBorderStyle default cPSCDefCalendarBorderStyle;
    Property Date: TDateTime read GetDate write SetDate;
    Property Time: TDateTime read GetTime write SetTime;
    Property DateTime: TDateTime read GetDateTime write SetDateTime;
    Property MinDate: TDateTime read GetMinDate write SetMinDate;
    Property MaxDate: TDateTime read GetMaxDate write SetMaxDate;
    Property MinDateLimit: Boolean read GetMinDateLimit write SetMinDateLimit
      default False;
    Property MaxDateLimit: Boolean read GetMaxDateLimit write SetMaxDateLimit
      default False;
    Property DateEdit: TPSCDateTimeUpDown read FDateEdit;
    Property TimeEdit: TPSCDateTimeUpDown read FTimeEdit;
    Property PanelKind: TPSCDateTimeKind read FPanelKind
      write SetPanelKind default cpkDateTime;
    Property IntegralSize: Boolean read FIntegralSize write SetIntegralSize
      default cPSCDefIntegralSize;
    Property BevelOuter default cPSCDefBevelOuter;
    Property OnDaySelected: TPSCDaySelectedEvent read FOnDaySelected write
      FOnDaySelected;
    Property ShowDateEdit: boolean read FShowDateEdit write SetShowDateEdit
      default cPSCDefShowDateEdit;
  published
  End;

  TPSCCalendarPanel = Class(TPSCCustomCalendarPanel)
  published
    Property Constraints;
    Property DragCursor;
    Property DragKind;
    Property DragMode;
    Property ParentShowHint;
    Property PopupMenu;
    Property ShowHint;
    Property TabOrder;
    Property TabStop;
    Property Visible;
    Property OnClick;
    Property OnDblClick;
    Property OnEnter;
    Property OnExit;
    Property OnKeyDown;
    Property OnKeyPress;
    Property OnKeyUp;
    Property OnMouseDown;
    Property OnMouseMove;
    Property OnMouseUp;

    Property Font;
    Property ParentFont;
    Property Color;
    Property CalendarBorderStyle;
    Property ParentColor;
    Property ShowDateEdit;
    Property PanelKind;
    Property IntegralSize;
    Property Align;
    Property BevelOuter;
    Property BevelInner;

    Property MinDate;
    Property MaxDate;
    Property MinDateLimit;
    Property MaxDateLimit;

    Property OnDaySelected;
  End;

  TPSCPopupCalendar = Class(TPSCPopupForm)
  private
    FCalendarPanel: TPSCCustomCalendarPanel;
    FNonePressed: Boolean;
    FNowButton:TPSCSpeedButton;
    FNoneButton:TPSCSpeedButton;
    Procedure DaySelected(Sender: TObject; const ADate: TDateTime; ANoneBtn: WordBool);
  protected
    procedure OnTodayClick(Sender:TObject);virtual;
    procedure OnNoneClick(Sender:TObject);virtual;
  public
    Procedure PopupEx(Control: TControl; Const PopupRect: TRect;
      PopupEvent: TPSCOnPopupClosed; ShowKind: TPSCPopupShowKind;
      ShowOptions: TPSCPopupOptions);override;
    Procedure ResizeFormControls;override;
    constructor CreateNew(AOwner: TComponent; Dummy: Integer=0);override;
    Property CalendarPanel: TPSCCustomCalendarPanel read FCalendarPanel;
    Property NonePressed: Boolean Read FNonePressed Write FNonePressed;
    Property NowButton:TPSCSpeedButton Read FNowButton;
    Property NoneButton:TPSCSpeedButton Read FNoneButton;
  End;

  TPSCPopupCalendarClass=class of TPSCPopupCalendar;

  TPSCMonthCalColors = Packed Record
    BackColor: TPSCColor;
    TextColor: TPSCColor;
    TitleBackColor: TPSCColor;
    TitleTextColor: TPSCColor;
    MonthBackColor: TPSCColor;
    TrailingTextColor: TPSCColor;
  End;

  TPSCDateEditOption = (deoMultiSelect,deoTodayButton,deoNoneButton,deoIntegralSize);
  TPSCDateEditOptions = Set Of TPSCDateEditOption;

  TPSCCustomDateEdit = Class(TPSCCustomContainerEdit)
  private
    FDateTimeFormat:TPSCDateTimeFormat;
    FCalColors: TPSCMonthCalColors;
    FShowDateEdit: boolean;
    FSelStartEdit: TPSCCustomDateEdit;
    FSelEndEdit: TPSCCustomDateEdit;
    FDateTime: TDateTime;
    FKind: TPSCDateTimeKind;
    FOptions: TPSCDateEditOptions;
    FMinDateLimit: Boolean;
    FMaxDateLimit: Boolean;
    FMinDate: TDateTime;
    FMaxDate: TDateTime;

    Function IsOptionsStored:Boolean;
    Function IsTimeStored:Boolean;
    Function IsDateStored:Boolean;
    Function GetPopupCalendar: TPSCPopupCalendar;
    Function GetDate: TDate;
    Function GetTime: TTime;
    Function GetDateTime: TDateTime; virtual;
    function CorrectKind(V: TPSCDateTimeKind):TPSCDateTimeKind;

    Procedure SetTime(Const V: TTime);
    Procedure SetDate(Const V: TDate);
    Procedure SetOptions(V: TPSCDateEditOptions);
    Procedure SetMinDate(A: TDateTime);
    Procedure SetMaxDate(A: TDateTime);
    Procedure SetMinDateLimit(A: Boolean);
    Procedure SetMaxDateLimit(A: Boolean);
    Procedure SetDateTime(Const V: TDateTime); virtual;
    procedure DateFormatChanged(Sender:TObject);
    Procedure SetCalColors(ACalColors: TPSCMonthCalColors);
    Procedure CalColorsChanged;
    Procedure SetShowDateEdit(V: boolean);
    procedure SetDateTimeFormat(V:TPSCDateTimeFormat);
    Procedure SetKind(V: TPSCDateTimeKind);
  protected
    Function GetAsText: String;
    Function PostValue: Boolean; override;
    Function CreatePopup: TPSCPopupForm; override;
    Procedure UpdatePopup; override;

    Procedure PopupCloseEvent(Sender: TObject; Canceled: Boolean); override;
    Procedure Loaded;override;
    Procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;

    Property ShowDateEdit: boolean read FShowDateEdit write SetShowDateEdit
      default True;
    Property Time: TTime read GetTime write SetTime Stored IsTimeStored;
    Property Date: TDate read GetDate write SetDate Stored IsDateStored;
    Property Kind: TPSCDateTimeKind read FKind write SetKind default
      cpkDateTime;
    Property Options: TPSCDateEditOptions read FOptions write SetOptions Stored
      IsOptionsStored;
    Property MinDate: TDateTime read FMinDate write SetMinDate;
    Property MaxDate: TDateTime read FMaxDate write SetMaxDate;
    Property MinDateLimit: Boolean read FMinDateLimit write SetMinDateLimit
      default False;
    Property MaxDateLimit: Boolean read FMaxDateLimit write SetMaxDateLimit
      default False;
    Property SelStartEdit: TPSCCustomDateEdit read FSelStartEdit write
      FSelStartEdit;
    Property SelEndEdit: TPSCCustomDateEdit read FSelEndEdit write FSelEndEdit;
    Property CalendarPopup: TPSCPopupCalendar read GetPopupCalendar;
    Property DateTime: TDateTime read GetDateTime write SetDateTime;
    Property CalColors: TPSCMonthCalColors read FCalColors write SetCalColors;
    property DateTimeFormat:TPSCDateTimeFormat Read FDateTimeFormat Write SetDateTimeFormat;
  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
  published
  End;

  TPSCDateEdit = Class(TPSCCustomDateEdit)
  public
    Property Field;
    Property CalendarPopup;
    Property DateTime;
    Property CalColors;
  published
    Property DataSource;
    Property DataField;
    Property Time;
    Property Date;
    Property Kind;
    Property Options;
    Property MinDate;
    Property MaxDate;
    Property MinDateLimit;
    Property MaxDateLimit;
    Property SelStartEdit;
    Property SelEndEdit;
    Property EditMask;
    Property OnDropDown;
    Property ButtonsVisible default True;
    Property OnCloseUp;
    Property PopupColor;
    Property ShowDateEdit;

    Property Constraints;
    Property Anchors;
    Property BiDiMode;
    Property DragKind;
    Property ParentBiDiMode;
    Property AutoSelect;
    Property AutoSize;
    Property DragMode;
    Property Enabled;
    Property Font;
    Property HideSelection default True;
    Property DragCursor;
    Property ParentFont;
    Property ParentShowHint;
    Property PopupMenu;
    Property ReadOnly;
    Property ShowHint;
    Property TabOrder;
    Property TabStop;
    Property Visible;
    Property OnChange;
    Property OnClick;
    Property OnDblClick;
    Property OnEnter;
    Property OnExit;
    Property OnKeyDown;
    Property OnKeyPress;
    Property OnKeyUp;
    Property OnMouseDown;
    Property OnMouseMove;
    Property OnMouseUp;

    Property OnContextPopup;
    Property OnDragDrop;
    Property OnDragOver;
    Property OnEndDock;
    Property OnEndDrag;
    Property OnStartDock;
    Property OnStartDrag;
  End;

  TPSCTextPartTimeElement=class(TPSCTextPartDateTimeElement)
  protected
    Function GetValue: integer;override;
    Procedure SetValue(AValue: Integer);override;
  end;

  TPSCTextPartDateElement=class(TPSCTextPartDateTimeElement)
  protected
    Function GetValue: integer;override;
    Procedure SetValue(AValue: Integer);override;
  public
  end;

  TPSCTextPartMinute = Class(TPSCTextPartTimeElement)
  protected
    Function GetMaxValue: Integer;override;
    Function GetElementIndex:Integer;override;
  public
  End;

  TPSCTextPartSecond = Class(TPSCTextPartTimeElement)
  protected
    Function GetMaxValue: Integer;override;
    Function GetElementIndex:Integer;override;
  public
  End;

  TPSCTextPartMilliSecond = Class(TPSCTextPartTimeElement)
  protected
    Function GetMaxValue: Integer;override;
    Function GetElementIndex:Integer;override;
  public
  End;

  IPSCTextPartHour=interface(IPSCTextPartDateTime)
    ['{7444B78A-0661-4D5C-A537-558FE3055BCD}']
    procedure SetUseAMPM(V:Boolean);
  end;

  TPSCTextPartHour = Class(TPSCTextPartTimeElement,IPSCTextPartHour)
  private
    FUseAMPM:Boolean;
    procedure SetUseAMPM(V:Boolean);
  protected
    Function GetMaxValue: Integer;override;
    Function GetElementIndex:Integer;override;
    Function GetDisplayStr(AValue : integer): String;override;
    Function GetEditStr(AValue : integer): String;override;
  public
    property UseAMPM:Boolean Read FUseAMPM Write SetUseAMPM;
  End;

  IPSCTextPartAMPM=interface(IPSCTextPartDateTime)
    ['{CD64ADCF-7906-4749-89BC-E7650AC167AB}']
    procedure SetAMValue(const V:String);
    procedure SetPMValue(const V:String);

    function GetAMValue:String;
    function GetPMValue:String;

    property AMValue:String Read GetAMValue Write SetAMValue;
    property PMValue:String Read GetPMValue Write SetPMValue;
  end;

  TPSCTextPartAMPM = class(TPSCTextPartTimeElement,IPSCTextPartAMPM)
  private
    FAMValue:String;
    FPMValue:String;
    procedure SetAMValue(const V:String);
    procedure SetPMValue(const V:String);

    function GetRealAMValue:String;
    function GetRealPMValue:String;
    function GetAMValue:String;
    function GetPMValue:String;
  protected
    Function GetDisplayStr(AValue : integer): String;override;
    Function GetEditStr(AValue : integer): String;override;
    Function GetMaxWidth(const ACanvas:IPSCCanvas): Integer;override;
    Function GetMinValue: Integer;override;
    Function GetMaxValue: Integer;override;
    Function GetValue: integer;override;
    Function GetElementIndex:Integer;override;

    Procedure SetValue(AValue: Integer);override;
  public
  published
    property AMValue:String Read GetAMValue Write SetAMValue;
    property PMValue:String Read GetPMValue Write SetPMValue;
  end;

  TPSCMonthFormat=(mfLong,mfShort,mfTwoDigits,mfDigits);

  IPSCTextPartMonth=interface(IPSCTextPartDateTime)
    ['{6631B99C-CA6A-41A5-AC23-DA19BDEDFA68}']
    procedure SetMonthFormat(V:TPSCMonthFormat);
  end;

  TPSCTextPartMonth = Class(TPSCTextPartDateElement,IPSCTextPartMonth)
  private
    FMonthFormat:TPSCMonthFormat;
    procedure SetMonthFormat(V:TPSCMonthFormat);
  protected
    Function GetMinValue: Integer;override;
    Function GetMaxValue: Integer;override;
    Function GetElementIndex:Integer;override;
    Function GetDisplayStr(AValue : integer): String;override;
    Function GetEditStr(AValue : integer): String;override;
    Function GetMaxWidth(const ACanvas:IPSCCanvas): Integer;override;
    Function GetMonthName(AIndex:Integer):String;virtual;
  public
  published
    property MonthFormat:TPSCMonthFormat Read FMonthFormat Write SetMonthFormat;
  End;

  TPSCTextPartDay = Class(TPSCTextPartDateElement)
  private
  protected
    Function GetMinValue: Integer;override;
    Function GetMaxValue: Integer;override;
    Function GetElementIndex:Integer;override;
  public
  End;

  IPSCTextPartDayName=interface(IPSCTextPartDateTime)
    ['{6FF8B2A7-6815-48BC-BB34-803BEFCB4C80}']
    procedure SetUseLongNames(V:Boolean);
  end;

  TPSCTextPartDayName = class(TPSCTextPartDateElement,IPSCTextPartDayName)
  private
    FUseLongNames:Boolean;
    procedure SetUseLongNames(V:Boolean);
  protected
    Function GetMinValue: Integer;override;
    Function GetMaxValue: Integer;override;
    Function GetElementIndex:Integer;override;
    Function GetDisplayStr(AValue : integer): String;override;
    Function GetEditStr(AValue : integer): String;override;
    Function GetMaxWidth(const ACanvas:IPSCCanvas): Integer;override;
    Function GetValue: integer;override;
    Procedure SetValue(AValue: Integer);override;
  published
    property UseLongNames:Boolean Read FUseLongNames Write SetUseLongNames;
  End;

  IPSCTextPartYear=interface(IPSCTextPartDateTime)
    ['{8EC3AE8D-0328-4B62-AC67-D2F9589BD992}']
    procedure SetTwoDigits(V:Boolean);
  end;

  TPSCTextPartYear = Class(TPSCTextPartDateElement,IPSCTextPartYear)
  private
    FTwoDigits:Boolean;
    procedure SetTwoDigits(V:Boolean);
    Function CorrectYear(AValue: Integer):Integer;
  protected
    Function GetElementIndex:Integer;override;
    Function GetMinValue: Integer;override;
    Function GetMaxValue: Integer;override;
    Function GetDisplayStr(AValue : integer): String;override;
    Function GetEditStr(AValue : integer): String;override;
    Function GetMaxWidth(const ACanvas:IPSCCanvas): Integer;override;
  public
    property TwoDigits:Boolean Read FTwoDigits Write SetTwoDigits;
  End;

var
  CPSCUsedPopupCalendarClass:TPSCPopupCalendarClass=TPSCPopupCalendar;
  CPSCDefaultDateEditOptions:TPSCDateEditOptions=[deoTodayButton,deoNoneButton];
{-------------------------------------}

implementation

{-------------------------------------}

Procedure TPSCDateTimeUpDown.PopupCloseEvent(Sender: TObject; Canceled: Boolean);
Var
  ASelFinish,ASelStart,ADateTime,SelEndEditDateTime: TDateTime;
begin
  inherited;
  SelectedPart:=SelectedPart;

{
  If ReadOnly Then
    exit;
}

  If Not Canceled Then
    Begin
      If (CalendarPopup.CalendarPanel.Calendar.SelCount = 0) or (CalendarPopup.NonePressed) Then
        DateTime := 0
      Else
        Begin
          If (CalendarPopup.CalendarPanel.Calendar.SelCount > 1) And
            ((SelStartEdit <> Nil) Or (SelEndEdit <> Nil)) Then
            Begin
              ASelFinish := CalendarPopup.CalendarPanel.Calendar.SelFinish;
              ASelStart := CalendarPopup.CalendarPanel.Calendar.SelStart;
              ADateTime := CalendarPopup.CalendarPanel.DateTime;
              If SelStartEdit <> Nil Then
                Begin
                  SelStartEdit.DateTime := PSCCombineDateTime(ASelStart,SelStartEdit.DateTime);
                  DateTime := PSCCombineDateTime(ASelFinish,ADateTime);
                End
              Else
                If SelEndEdit <> Nil Then
                  Begin
                    SelEndEditDateTime := SelEndEdit.DateTime;
                    DateTime := PSCCombineDateTime(ASelStart,ADateTime);
                    SelEndEdit.DateTime := PSCCombineDateTime(ASelFinish,SelEndEditDateTime);
                  End;
            End
          Else
            DateTime := CalendarPopup.CalendarPanel.DateTime;
        End;
    End;
end;

{-------------------------------------}

Function TPSCDateTimeUpDown.CreatePopup: TPSCPopupForm;
begin
  Result := CPSCUsedPopupCalendarClass.CreateNew(Nil,0);
  With TPSCPopupCalendar(Result) Do
    Begin
      CalendarPanel.Calendar.ExtendedSelect := False;
    End;
end;

{-------------------------------------}

Procedure TPSCDateTimeUpDown.TextPartsChanged(Sender: TObject; AItem: TPSCNamedItem);

  procedure MyUpdateItem(APart:TPSCTextPart);
  var
    MyAccess:IPSCTextPartDateTime;
    MyKind:TPSCDateTimePart;
  begin
    If PSCSupports(APart.PartData,IPSCTextPartDateTime,MyAccess) then
    begin
      MyKind:=MyAccess.GetDateTimePartKind;
      If APart.Checked then
        FPartsChecked:=PartsChecked+[MyKind]
      else
        FPartsChecked:=PartsChecked-[MyKind];
    end;
  end;

begin
  If AItem<>nil then
    MyUpdateItem(TPSCTextPart(AItem));
  inherited;
end;

{-------------------------------------}

constructor TPSCDateTimeUpDown.Create(AOwner:TComponent);
begin
  inherited;
  FShowDateEdit:=True;
  FDateTime:=PSCNow;
  FDateTimeFormat:=TPSCDateTimeFormat.Create(Self,PSCGetDefaultDateTimeFormat);
  FDateTimeFormat.OnChange:=DateFormatChanged;

  DateFormatChanged(nil);
end;

{--------------------------------------}

procedure TPSCDateTimeUpDown.SetPartsEnabled(V:TPSCDateTimeParts);
begin
  IF FPartsEnabled<>V then
  begin
    FPartsEnabled:=V;
    DateFormatChanged(nil);
  end;
end;

{--------------------------------------}

procedure TPSCDateTimeUpDown.SetPartsCheckVisible(V:TPSCDateTimeParts);
begin
  IF FPartsCheckVisible<>V then
  begin
    FPartsCheckVisible:=V;
    DateFormatChanged(nil);
  end;
end;

{--------------------------------------}

procedure TPSCDateTimeUpDown.SetPartsChecked(V:TPSCDateTimeParts);
begin
  IF FPartsChecked<>V then
  begin
    FPartsChecked:=V;
    DateFormatChanged(nil);
  end;
end;

{--------------------------------------}

procedure TPSCDateTimeUpDown.SetKind(V:TPSCDateTimeKind);
begin
  IF FKind<>V then
  begin
    FKind:=V;
    DateFormatChanged(nil);
  end;
end;

{--------------------------------------}

procedure TPSCDateTimeUpDown.AddPart(const APart:IPSCTextPart;
  APartKind:TPSCDateTimePart);
var
  MyAccess:IPSCTextPartDateTime;
begin
  With TextParts.Add do
  begin
    PartData:=APart;
    Alignment:=haRight;
    ExpandWidth:=True;
    CheckEnabled:=APartKind in PartsEnabled;
    Checked:=APartKind in PartsChecked;
    CheckVisible:=APartKind in PartsCheckVisible;

    If PSCSupports(APart,IPSCTextPartDateTime,MyAccess) then
      MyAccess.SetDateTimePartKind(APartKInd);
  end;
end;

{--------------------------------------}

procedure TPSCDateTimeUpDown.AddPart_Month(const APart:TPSCDTFormatPart);
var
  MyPartMonth:IPSCTextPartMonth;
begin
  MyPartMonth:=TPSCTextPartMonth.Create(GetDateTime,SetDateTime,
    DateTimeFormat.ThisFormat);
  MyPartMonth.SetLeadingZero(APart.Style=DTFPS_ZERO);
  case APart.Style of
    DTFPS_NOZERO:
      MyPartMonth.SetMonthFormat(mfDigits);
    DTFPS_ZERO:
      MyPartMonth.SetMonthFormat(mfTwoDigits);
    DTFPS_SHORT:
      MyPartMonth.SetMonthFormat(mfShort);
    DTFPS_LONG:
      MyPartMonth.SetMonthFormat(mfLong);
  end;
  AddPart(MyPartMonth,dtpMonth);
end;

{--------------------------------------}

procedure TPSCDateTimeUpDown.AddPart_Day(const APart:TPSCDTFormatPart);
var
  MyPart:IPSCTextPartInteger;
  MyPartDayName:IPSCTextPartDayName;
begin
  If APart.Style in [DTFPS_SHORT,DTFPS_LONG] then
    begin
      MyPartDayName:=TPSCTextPartDayName.Create(GetDateTime,SetDateTime,
        DateTimeFormat.ThisFormat);
      MyPartDayName.SetUseLongNames(APart.Style=DTFPS_LONG);
      AddPart(MyPartDayName,dtpDay);
    end
  else
    begin
      MyPart:=TPSCTextPartDay.Create(GetDateTime,SetDateTime,
        DateTimeFormat.ThisFormat);
      MyPart.SetLeadingZero(APart.Style=DTFPS_ZERO);
      AddPart(MyPart,dtpDay);
    end;
end;

{--------------------------------------}

procedure TPSCDateTimeUpDown.AddPart_AMPM(const APart:TPSCDTFormatPart);
var
  MyPartAMPM:IPSCTextPartAMPM;
  MyAMPM: TPSCAMPM;
begin
  MyPartAMPM:=TPSCTextPartAMPM.Create(GetDateTime,SetDateTime,
    DateTimeFormat.ThisFormat);

  case APart.Style of
    DTFPS_AMPM:
      begin
        MyAMPM:=PSCDecodeAMPM(APart.DataAsStr,CPSCTIME_AMPM);
        MyPartAMPM.SetAMValue(MyAMPM[False]);
        MyPartAMPM.SetPMValue(MyAMPM[True]);
      end;
    DTFPS_AP:
      begin
        MyAMPM:=PSCDecodeAMPM(APart.DataAsStr,CPSCTIME_AP);
        MyPartAMPM.SetAMValue(MyAMPM[False]);
        MyPartAMPM.SetPMValue(MyAMPM[True]);
      end;
  end;
  AddPart(MyPartAMPM,dtpAMPM);
end;

{--------------------------------------}

procedure TPSCDateTimeUpDown.AddPart_Time(const APart:TPSCDTFormatPart;
  AClass:TPSCTextPartDateTimeElementClass;APartKind:TPSCDateTimePart);
var
  MyPart:IPSCTextPartDateTime;
begin
  MyPart:=AClass.Create(GetDateTime,SetDateTime,
    DateTimeFormat.ThisFormat);
  MyPart.SetLeadingZero(APart.Style=DTFPS_ZERO);
  AddPart(MyPart,APartKind);
end;

{--------------------------------------}

procedure TPSCDateTimeUpDown.AddPart_Delimeter(const S:String;
  APartKind:TPSCDateTimePart);
begin
  AddPart(TPSCTextPartDelimiter.Create(S),APartKind);
end;

{--------------------------------------}

procedure TPSCDateTimeUpDown.AddPart_Year(const APart:TPSCDTFormatPart);
var
  MyPartYear:IPSCTextPartYear;
begin
  MyPartYear:=TPSCTextPartYear.Create(GetDateTime,SetDateTime,
    DateTimeFormat.ThisFormat);
  MyPartYear.SetLeadingZero(True);
  MyPartYear.SetTwoDigits(APart.Style=DTFPS_TWODIGITS);
  AddPart(MyPartYear,dtpYear);
end;

{--------------------------------------}

procedure TPSCDateTimeUpDown.AddPart_Hour(const APart:TPSCDTFormatPart);
var
  MyPartHour:IPSCTextPartHour;
begin
  MyPartHour:=TPSCTextPartHour.Create(GetDateTime,SetDateTime,
    DateTimeFormat.ThisFormat);
  MyPartHour.SetLeadingZero(APart.Style=DTFPS_ZERO);
  MyPartHour.SetUseAMPM(APart.Kind=DTFPK_HOUR12);
  AddPart(MyPartHour,dtpHour);
end;

{--------------------------------------}

procedure TPSCDateTimeUpDown.AddPart(const APart:TPSCDTFormatPart);
var
  S:String;
begin
  With APart do
    case Kind of
      DTFPK_YEAR_IN_ERA:
        begin
          If Style=DTFPS_ZERO then
            S:=DateTimeFormat.ToStringEx(GetDateTime,'ee')
          else
            S:=DateTimeFormat.ToStringEx(GetDateTime,'e');
          AddPart_Delimeter(S,dtpSeparator);
        end;
      DTFPK_ERA:
        begin
          If Style=DTFPS_ZERO then
            S:=DateTimeFormat.ToStringEx(GetDateTime,'gg')
          else
            S:=DateTimeFormat.ToStringEx(GetDateTime,'g');
          AddPart_Delimeter(S,dtpSeparator);
        end;
      DTFPK_DAY:
        AddPart_Day(APart);
      DTFPK_MONTH:
        AddPart_Month(APart);
      DTFPK_YEAR:
        AddPart_Year(APart);
      DTFPK_HOUR12,DTFPK_HOUR24:
        AddPart_Hour(APart);
      DTFPK_MINUTE:
        AddPart_Time(APart,TPSCTextPartMinute,dtpMinute);
      DTFPK_SECOND:
        AddPart_Time(APart,TPSCTextPartSecond,dtpSecond);
      DTFPK_MILLISECOND:
        AddPart_Time(APart,TPSCTextPartMilliSecond,dtpMSec);
      DTFPK_AMPM:
        AddPart_AMPM(APart);
      DTFPK_ASIS:
        AddPart_Delimeter(DataAsStr,dtpSeparator);
      DTFPK_DATE_SEPARATOR:
        AddPart_Delimeter(DateTimeFormat.DateSeparator,dtpSeparator);
      DTFPK_TIME_SEPARATOR:
        AddPart_Delimeter(DateTimeFormat.TimeSeparator,dtpSeparator);
    end;
end;

{--------------------------------------}

procedure TPSCDateTimeUpDown.DateFormatChanged(Sender:TObject);
var
  i:Integer;
  MyFormatParts:TPSCDTFormatParts;
begin
  MyFormatParts:=PSCParseDateTimeFormatToParts(
    DateTimeFormat.GetFormat(Kind),DateTimeFormat.ThisFormat);

  With TextParts do
  begin
    BeginUpdate;
    try
      Clear;
      With MyFormatParts do
        for i:=Low(Parts) to PartCount-1 do
          AddPart(Parts[i]);
    finally
      EndUpdate;
    end;
  end;
end;

{--------------------------------------}

procedure TPSCDateTimeUpDown.SetDateTimeFormat(V:TPSCDateTimeFormat);
begin
  DateTimeFormat.Assign(V);
end;

{-------------------------------------}

destructor TPSCDateTimeUpDown.Destroy;
begin
  FDateTimeFormat.Free;
  inherited;
end;

{-------------------------------------}

Procedure TPSCDateTimeUpDown.SetDateTime(Const V: TDateTime);
Begin
  If FDateTime <> V Then
    Begin
      FDateTime := V;
      TextParts.Changed;
    End;
End;

{-------------------------------------}

function TPSCDateTimeUpDown.GetPopupCalendar: TPSCPopupCalendar;
begin
  Result:=TPSCPopupCalendar(Popup);
end;

{-------------------------------------}

function TPSCDateTimeUpDown.GetTime:TTime;
begin
  Result:=PSCTimeOf(DateTime);
end;

{-------------------------------------}

function TPSCDateTimeUpDown.GetDate:TDate;
begin
  Result:=PSCDateOf(DateTime);
end;

{-------------------------------------}

procedure TPSCDateTimeUpDown.SetTime(const ATime:TTime);
begin
  DateTime:=PSCCombineDateTime(DateTime,ATime);
end;

{-------------------------------------}

Function TPSCDateTimeUpDown.PostValue: Boolean;
begin
  Result:=inherited PostValue;
end;

{-------------------------------------}

procedure TPSCDateTimeUpDown.SetDate(const ADate:TDate);
begin
  DateTime:=PSCCombineDateTime(ADate,DateTime);
end;

{-------------------------------------}

Function TPSCDateTimeUpDown.GetDateTime:TDateTime;
begin
  Result:=FDateTime;
end;

{----------------------------------}

Const
  cPSCButtonWidth = 70;
  cPSCButtonHeight = 25;
  cPSCEditHeight = 22;
  cHorzShift = 5;
  cPSCButtonSpace = cHorzShift;
  cDateTimeHorzIndent = 15;
  cPSCTimeEditWidth = 150;

{------------------------------------------------------------------------------}

Type
  TPSCCustomCalendarPanelEx = Class(TPSCCustomCalendarPanel)
  protected
    Function CreateCalendar: TPSCCustomCalendar; override;
  public
    Constructor CreateWithCalendar(AOwner: TComponent; ACalInstance:
      TPSCCustomCalendar); virtual;
  End;

{------------------------------------------------------------------------------}

Constructor TPSCCustomCalendarPanelEx.CreateWithCalendar(AOwner: TComponent;
  ACalInstance: TPSCCustomCalendar);
Begin
  FCalendar := ACalInstance;
  Create(AOwner);
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendarPanelEx.CreateCalendar: TPSCCustomCalendar;
Begin
  Result := FCalendar;
End;

{------------------------------------------------------------------------------}

procedure TPSCCustomCalendarPanel.DateEditOnChange(Sender:TObject);
begin
  If PSCDatesCompare(Self.Date,DateEdit.DateTime,cpkDate)<>0 then
    Self.Date:=PSCDateOf(DateEdit.DateTime);
end;

{------------------------------------------------------------------------------}

Type
  THackCalendar = Class(TPSCCustomCalendar)
  End;

Constructor TPSCCustomCalendarPanel.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);

  ControlStyle := ControlStyle -
    [csCaptureMouse,csClickEvents,csDoubleClicks,csSetCaption];
  FullRepaint := False;
  FShowDateEdit := cPSCDefShowDateEdit;
  BevelOuter := cPSCDefBevelOuter;
  FIntegralSize := cPSCDefIntegralSize;
  FCalendar := CreateCalendar;
  FDateEdit := TPSCDateTimeUpDown.Create(Self);
  FDateEdit.Kind:=cpkDate;
  FTimeEdit := TPSCDateTimeUpDown.Create(Self);
  FTimeEdit.Kind:=cpkTime;
  FDateLabel := TPSCImage.Create(Self);
  FTimeLabel := TPSCImage.Create(Self);
  Color := clPSCBtnFace;

  With FDateEdit Do
    Begin
      Visible := False;
      OnChange:=DateEditOnChange;
      Height := cPSCEditHeight;
      If Not (csDesigning In ComponentState) Then
        Parent := Self;
    End;
  With FTimeEdit Do
    Begin
      Visible := False;
      Height := FDateEdit.Height + 2;
      Width := cPSCTimeEditWidth;
      If Not (csDesigning In ComponentState) Then
        Parent := Self;
    End;
  With FDateLabel Do
    Begin
      Width:=27;
      Height:=27;
      PSCLoadBitmapFromResource(Picture.Bitmap,SPSCResName_Img_Date);
      Visible := False;
      Transparent := True;
      Center:=True;
      If Not (csDesigning In ComponentState) Then
        Parent := Self;
    End;
  With FTimeLabel Do
    Begin
      Width:=FDateLabel.Width;
      Height:=FDateLabel.Height;
      PSCLoadBitmapFromResource(Picture.Bitmap,SPSCResName_Img_Clock);
      Visible := False;
      Transparent := True;
      Center:=True;
      If Not (csDesigning In ComponentState) Then
        Parent := Self;
    End;
  With Calendar Do
    Begin
      BorderStyle:=bsSingle;
      TabStop := True;
      MultiSelect := False;
      BorderEdges := [];
      OnMouseUp := Self.CalendarMouseUp;
      OnDateChanged := Self.DateChanged;
      If Not (csDesigning In ComponentState) Then
        Parent := Self;
    End;
  UpdatePanel;
  SetBounds(0,0,0,0);
  CalendarToDateEdit;
End;

{------------------------------------------------------------------------------}

Destructor TPSCCustomCalendarPanel.Destroy;
Begin
  Inherited Destroy;
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendarPanel.DateChanged(Sender: TObject);
Begin
  CalendarToDateEdit;
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendarPanel.CalendarToDateEdit;
Begin
  If FDateEdit.Focused then
    exit;
    
  If Calendar.SelCount = 0 Then
    FDateEdit.DateTime := PSCDateOf(PSCNow)
  Else
    FDateEdit.DateTime := Calendar.SelStart;
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendarPanel.GetCalendar: TPSCCalendar;
Begin
  Result := TPSCCalendar(FCalendar);
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendarPanel.SetPanelKind(Value: TPSCDateTimeKind);
Begin
  If FPanelKind <> Value Then
    Begin
      FPanelKind := Value;
      UpdatePanel;
      AdjustSize;
      Invalidate;
    End;
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendarPanel.DoDaySelected(Sender: TObject;
  const ADate:TDateTime; ANoneBtn:WordBool);
Begin
  If Assigned(OnDaySelected) Then
    OnDaySelected(Sender,ADate,ANoneBtn);
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendarPanel.CMFontChanged(Var Message: TMessage);
Begin
  Inherited;
  UpdatePanel;
  AdjustSize;
  AlignContents;
  Invalidate;
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendarPanel.SetVisibility(ABtn: TControl; AVisible:
  Boolean);
Begin
  ABtn.Visible := AVisible;
  If csDesigning In ComponentState Then
    If AVisible Then
      ABtn.Parent := Self
    Else
      ABtn.Parent := Nil
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendarPanel.UpdatePanel;
Begin
  DisableAlign;
  Try
    Case PanelKind Of
      cpkDate:
        Begin
          SetVisibility(FDateEdit,ShowDateEdit);
          SetVisibility(FTimeEdit,False);
          SetVisibility(FDateLabel,False);
          SetVisibility(FTimeLabel,False);
          SetVisibility(FCalendar,True);
        End;
      cpkTime:
        Begin
          SetVisibility(FDateEdit,False);
          SetVisibility(FTimeEdit,True);
          SetVisibility(FDateLabel,False);
          SetVisibility(FTimeLabel,False);
          SetVisibility(FCalendar,False);
        End;
      cpkDateTime:
        Begin
          SetVisibility(FDateEdit,ShowDateEdit);
          SetVisibility(FTimeEdit,True);
          SetVisibility(FDateLabel,True);
          SetVisibility(FTimeLabel,True);
          SetVisibility(FCalendar,True);
        End
    Else
      Exit
    End;
  Finally
    EnableAlign;
  End;
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendarPanel.SetDate(Value: TDateTime);
Begin
  Calendar.StartDate := Calendar.SelStart;
  Calendar.ChangeSelection(PSCDateOf(Value),PSCDateOf(Value));
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendarPanel.GetDate: TDateTime;
Begin
  Result := PSCDateOf(Calendar.SelStart)
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendarPanel.SetTime(Value: TDateTime);
Begin
  FTimeEdit.DateTime := PSCTimeOf(Value);
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendarPanel.GetTime: TDateTime;
Begin
  Try
    Result := PSCTimeOf(FTimeEdit.DateTime);
  Except;
    Result := PSCTimeOf(PSCNow);
  End;
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendarPanel.SetDateTime(Value: TDateTime);
Begin
  Date := Value;
  Time := Value;
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendarPanel.GetDateTime: TDateTime;
Begin
  Result := Date + Time;
End;

{------------------------------------------------------------------------------}

procedure TPSCCustomCalendarPanel.ButtonClick(ABtnKind:TPSCCalendarPanelButton);
begin
  case ABtnKind of
    cpbTodayButton:
      Begin
        DateTime:=PSCNow;
{        With Calendar Do
          Begin
            ClearSelection;
            SelStart := PSCDateOf(PSCNow);
            CursorDate := SelStart;
          End;}
        DoDaySelected(Self,DateTime,False);
      End;
    cpbNoneButton:
      Begin
        Calendar.ClearSelection;
        DoDaySelected(Self,Calendar.SelStart,True);
      End;
  end;
end;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendarPanel.CreateCalendar: TPSCCustomCalendar;
Begin
  Result := CPSCDefaultCalendarClass.Create(Self);
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendarPanel.AlignContents;
Var
  LW: Integer;

  Procedure AlignDate;
  Var
    L: Integer;
  Begin
    If PanelKind In [cpkDate,cpkDateTime] Then
      Begin
        If PanelKind = cpkDate Then
          L := cHorzShift
        Else
          Begin
            With FDateLabel Do
              SetBounds(cHorzShift,2 + (DateEdit.Height - FDateLabel.Height) Div
                2,
                Width,Height);
            L := LW + 10
          End;
        With FDateEdit Do
        begin
          SetBounds(L,2,PSCMax(Calendar.GetMonthWidth + 2,GetMaxTextWidth(True)),Height);
        end;
      End
  End;

  Procedure AlignCalendar;
  Var
    CalendarTop,L: Integer;
  Begin
    If PanelKind In [cpkDate,cpkDateTime] Then
      With ClientRect Do
        Begin
          Calendar.HandleNeeded;

          If PanelKind = cpkDate Then
            L := cHorzShift
          Else
            L := LW + 10;

          If ShowDateEdit Then
            CalendarTop := FDateEdit.Top + DateEdit.Height + 2
          Else
            CalendarTop := FDateEdit.Top;

          Calendar.SetBounds(L,CalendarTop,
            Right - L - cHorzShift,ClientHeight - CalculateIntegratedHeight)
        End
  End;

  Procedure AlignTime;
  Var
    T: Integer;
  Begin
    Case PanelKind Of
      cpkDateTime:
        Begin
          T:=Calendar.BoundsRect.Bottom+2;

          With FTimeLabel Do
            SetBounds(cHorzShift,T + (TimeEdit.Height - FTimeLabel.Height) Div 2,
              Width,Height);
          With FTimeEdit Do
            SetBounds(LW + 10,T,FDateEdit.Width,Height);
        End;
      cpkTime:
        With CLientRect Do
          FTimeEdit.SetBounds(2,2,cPSCTimeEditWidth,TimeEdit.Height);
    End;
  End;


Begin
  LW := PSCMax(FDateLabel.Width,FTimeLabel.Width);
  AlignCalendar;
  AlignDate;
  AlignTime;
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendarPanel.AlignControls(AControl: TControl; Var Rect:
  TRect);
Begin
  AlignContents;
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendarPanel.SetIntegralSize(AValue: Boolean);
Begin
  FIntegralSize := AValue;
  UpdatePanel;
  AdjustSize;
  AlignContents;
  Invalidate;
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendarPanel.UpdateWidthHeight(Var cx,cy: Integer);
Var
  CW,CH,DeltaX,DeltaY: Integer;
Begin
  DeltaX := CalculateIntegratedWidth;
  DeltaY := CalculateIntegratedHeight;
  If FPanelKind = cpkTime Then
    Begin
      cy := DeltaY;
      cx := DeltaX;
      Exit;
    End;

  With ClientRect Do
    Begin
      DeltaX := DeltaX + (Width - Right) + 4;
      DeltaY := DeltaY + (Height - Bottom) + 4;
    End;
  CW := FCalendar.GetMonthWidth;
  CH := FCalendar.GetMonthHeight;
  If CW > 0 Then
    Begin
      cx := (cx - DeltaX) Div CW;
      If cx < 1 Then
        cx := 1;
      cx := cx * CW + DeltaX;
    End;
  If CH > 0 Then
    Begin
      cy := (cy - DeltaY) Div CH;
      If cy < 1 Then
        cy := 1;
      cy := cy * CH + DeltaY;
    End;
  CX:=PSCMax(CX,GetMinWidth);
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendarPanel.WMWindowPosChanging(
  Var Message: TWMWindowPosChanging);
Begin
  Inherited;
  With Message.WindowPos^ Do
    If FIntegralSize And (flags And SWP_NOSIZE = 0) Then
      UpdateWidthHeight(cx,cy);
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendarPanel.CalendarMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
Var
  Date: TDateTime;
Begin
  If (Button = mbLeft) And (FCalendar.GetHitTest(Point(X,Y),Date) In
    [chtDay,chtLeftGray,chtRightGray]) Then
    If ((Shift = []) Or (Not Calendar.MultiSelect)) And
      DateInRange(Date) Then
      DoDaySelected(Self,Calendar.SelStart,False);
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendarPanel.GetMinHeight: Integer;
Begin
  If PanelKind = cpkTime Then
    Result := TimeEdit.Height + 4
  Else
    Result := CalculateIntegratedHeight + Calendar.GetMonthHeight + 4;
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendarPanel.GetMinWidth: Integer;
var
  A:Integer;
Begin
  If PanelKind = cpkTime Then
    Result := cPSCTimeEditWidth + 4
  Else
    begin
      If FDateEdit.Visible then
        A:=FDateEdit.GetMaxTextWidth(True)
      else
        A:=0;
      A:=PSCMax(Calendar.GetMonthWidth + 2,A);
      Result := CalculateIntegratedWidth + A;
    end;
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendarPanel.CreateWnd;
Begin
  Inherited;

  UpdatePanel;
  If (Width = 0) And (Height = 0) Then
    Begin
      SetBounds(Left,Top,GetMinWidth,GetMinHeight);
      With Calendar Do
        SetBounds(Left,Top,Calendar.GetMonthWidth + 2,Calendar.GetMonthHeight +
          2);
    End
  Else
    Begin
      AdjustSize;
      AlignContents;
    End;
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendarPanel.CalculateIntegratedHeight: Integer;
Begin
  Result := 0;

  Case FPanelKind Of
    cpkDate,cpkDateTime:
      Begin
          Result := Result + cPSCButtonSpace;

        If ShowDateEdit Then
          Result := Result + DateEdit.Height + DateEdit.Top;

        If PanelKind = cpkDateTime Then
          Result := Result + TimeEdit.Height + cPSCButtonSpace + 2;

      End;
    cpkTime:
      Result := TimeEdit.Height + 4;
  End;
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendarPanel.CalculateIntegratedWidth: Integer;
Begin
  Case FPanelKind Of
    cpkDate:
      Result := 10;
    cpkDateTime:
      Result := PSCMax(FDateLabel.Width,FTimeLabel.Width) + cDateTimeHorzIndent;
    cpkTime:
      Result := cPSCTimeEditWidth + 4;
  Else
    Result := 0;
  End;
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendarPanel.IsEmpty: Boolean;
Begin
  Case PanelKind Of
    cpkDate,
      cpkDateTime: Result := Calendar.SelCount = 0;
  Else
    Result := False;
  End;
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendarPanel.SetMaxDate(Const AValue: TDateTime);
Begin
  Calendar.MaxDate := AValue;
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendarPanel.SetMaxDateLimit(AValue: Boolean);
Begin
  Calendar.MaxDateLimit := AValue;
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendarPanel.SetMinDate(Const AValue: TDateTime);
Begin
  Calendar.MinDate := AValue;
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendarPanel.SetMinDateLimit(AValue: Boolean);
Begin
  Calendar.MinDateLimit := AValue;
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendarPanel.DateInRange(ADate: TDateTime): Boolean;
Begin
  Result := Not ((MinDateLimit And (ADate < MinDate)) Or
    (MaxDateLimit And (ADate > MaxDate)));
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendarPanel.GetMaxDate: TDateTime;
Begin
  Result := Calendar.MaxDate;
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendarPanel.GetMaxDateLimit: Boolean;
Begin
  Result := Calendar.MaxDateLimit;
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendarPanel.GetMinDate: TDateTime;
Begin
  Result := Calendar.MinDate;
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendarPanel.GetMinDateLimit: Boolean;
Begin
  Result := Calendar.MinDateLimit;
End;

{------------------------------------------------------------------------------}

Procedure TPSCPopupCalendar.PopupEx(Control: TControl; Const PopupRect: TRect;
  PopupEvent: TPSCOnPopupClosed; ShowKind: TPSCPopupShowKind;
  ShowOptions: TPSCPopupOptions);
begin
  FNonePressed:=False;
  inherited;
end;

{------------------------------------------------------------------------------}

Procedure TPSCPopupCalendar.ResizeFormControls;
begin
  inherited;
  Constraints.MaxHeight:=0;
  Constraints.MaxWidth:=0;
  Constraints.MinHeight:=0;
  Constraints.MinWidth:=0;
  CalendarPanel.Calendar.HandleNeeded;
  CalendarPanel.DateEdit.HandleNeeded;
  CalendarPanel.TimeEdit.HandleNeeded;
  ClientWidth:=CalendarPanel.GetMinWidth+
    (CalendarPanel.Width-CalendarPanel.ClientWidth);
  ClientHeight:=CalendarPanel.GetMinHeight+
    (CalendarPanel.Height-CalendarPanel.ClientHeight)+GetFooterPanel.Height;
  Constraints.MinHeight:=Height;
  Constraints.MinWidth:=Width;
  Constraints.MaxHeight:=Constraints.MinHeight;
  Constraints.MaxWidth:=Constraints.MinWidth;
end;

{------------------------------------------------------------------------------}

procedure TPSCPopupCalendar.OnTodayClick(Sender:TObject);
begin
  CalendarPanel.ButtonClick(cpbTodayButton);
end;

{------------------------------------------------------------------------------}

procedure TPSCPopupCalendar.OnNoneClick(Sender:TObject);
begin
  CalendarPanel.ButtonClick(cpbNoneButton);
end;

{------------------------------------------------------------------------------}

constructor TPSCPopupCalendar.CreateNew(AOwner: TComponent; Dummy: Integer=0);
Begin
  Inherited;
  BorderStyle := bsSizeable;
  GetFooterPanel;
  FCalendarPanel := TPSCCustomCalendarPanelEx.CreateWithCalendar(Self,
    CPSCDefaultCalendarClass.Create(Self));
  With CalendarPanel Do
  Begin
    ParentColor := True;
    Parent := Self;
    OnDaySelected := DaySelected;
    IntegralSize:=True;
    Align:=alClient;
  End;

  FNowButton:=AddButton;

  With FNowButton do
  begin
    OnClick:=OnTodayClick;
    Hint := PSCRemoveCharSet(['&'],PSCConsts.NowButton);
    PSCLoadBitmapFromResource(Glyph,SPSCResName_Btn_SelectToday);
  end;

  FNoneButton:=AddButton;
  With FNoneButton do
  begin
    OnClick:=OnNoneClick;
    Hint := PSCRemoveCharSet(['&'],PSCConsts.NoneButton);
    PSCLoadBitmapFromResource(Glyph,SPSCResName_Btn_SelectNULL);
  end;

  ActiveControl := CalendarPanel;
End;

{------------------------------------------------------------------------------}

Procedure TPSCPopupCalendar.DaySelected(Sender: TObject; const ADate: TDateTime;
  ANoneBtn: WordBool);
Begin
  If ssCtrl in FCalendarPanel.Calendar.ShiftStateValue then
    exit;

  FNonePressed:=ANoneBtn;
  If (ADate <> 0) Or ANoneBtn Then
    ClosePopup(False,True);
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendarPanel.SetShowDateEdit(V: boolean);
Begin
  If FShowDateEdit <> V Then
    Begin
      FShowDateEdit := V;
      UpdatePanel;
      AdjustSize;
      AlignContents;
      Invalidate;
    End;
End;

{------------------------------------------------------------------------------}

Function TPSCCustomCalendarPanel.GetCalendarBorderStyle: TBorderStyle;
Begin
  Result := Calendar.BorderStyle;
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomCalendarPanel.SetCalendarBorderStyle(V: TBorderStyle);
Begin
  Calendar.BorderStyle := V;
End;

{--------------------------------------}

Function TPSCCustomDateEdit.IsTimeStored:Boolean;
begin
  Result:=(not IsDataAware) and (Kind<>cpkDate);
end;

{--------------------------------------}

Function TPSCCustomDateEdit.IsDateStored:Boolean;
begin
  Result:=(not IsDataAware) and (Kind<>cpkTime);
end;

{--------------------------------------}

Procedure TPSCCustomDateEdit.SetCalColors(ACalColors: TPSCMonthCalColors);
Begin
  FCalColors := ACalColors;
  CalColorsChanged;
End;

{--------------------------------------}

Procedure TPSCCustomDateEdit.CalColorsChanged;
Begin
  If PopupCreated Then
    With CalendarPopup,CalendarPanel.Calendar Do
      Begin
        Font.Color := FCalColors.TextColor;
        Colors.WeekEndText := Font.Color;
        Colors.MonthHeader := FCalColors.TitleBackColor;
        Colors.HeaderFont.Color := FCalColors.TitleTextColor;
        Color := FCalColors.MonthBackColor;
        Colors.WeekHeader := Color;
        Colors.Grayed := FCalColors.TrailingTextColor;
      End;
End;

{--------------------------------------}

Procedure TPSCCustomDateEdit.Notification(AComponent: TComponent; Operation:
  TOperation);
Begin
  Inherited;
  If Operation = OpRemove Then
    If SelStartEdit = AComponent Then
      SelStartEdit := Nil
    Else
      If SelEndEdit = AComponent Then
        SelEndEdit := Nil;
End;

{--------------------------------------}

Procedure TPSCCustomDateEdit.PopupCloseEvent(Sender: TObject; Canceled:
  Boolean);
Var
  ASelFinish,ASelStart,ADateTime,SelEndEditDateTime: TDateTime;
Begin
  If ReadOnly Then
    exit;
  If Not Canceled Then
    Begin
      If (CalendarPopup.CalendarPanel.Calendar.SelCount = 0) or (CalendarPopup.NonePressed) Then
        DateTime := 0
      Else
        Begin
          If (CalendarPopup.CalendarPanel.Calendar.SelCount > 1) And
            ((SelStartEdit <> Nil) Or (SelEndEdit <> Nil)) Then
            Begin
              ASelFinish := CalendarPopup.CalendarPanel.Calendar.SelFinish;
              ASelStart := CalendarPopup.CalendarPanel.Calendar.SelStart;
              ADateTime := CalendarPopup.CalendarPanel.DateTime;
              If SelStartEdit <> Nil Then
                Begin
                  SelStartEdit.DateTime := PSCCombineDateTime(ASelStart,SelStartEdit.DateTime);
                  DateTime := PSCCombineDateTime(ASelFinish,ADateTime);
                End
              Else
                If SelEndEdit <> Nil Then
                  Begin
                    SelEndEditDateTime := SelEndEdit.DateTime;
                    DateTime := PSCCombineDateTime(ASelStart,ADateTime);
                    SelEndEdit.DateTime := PSCCombineDateTime(ASelFinish,SelEndEditDateTime);
                  End;
            End
          Else
            DateTime := CalendarPopup.CalendarPanel.DateTime;
        End;
    End;
End;

{--------------------------------------}

Procedure TPSCCustomDateEdit.SetOptions(V: TPSCDateEditOptions);
Begin
  If FOptions <> V Then
    Begin
      FOptions := V;
      UpdatePopup;
    End;
End;

{--------------------------------------}

Destructor TPSCCustomDateEdit.Destroy;
Begin
  FDateTimeFormat.Free;
  Inherited;
End;

{--------------------------------------}

procedure TPSCCustomDateEdit.DateFormatChanged(Sender:TObject);
begin
  DateTime:=DateTime;
end;

{--------------------------------------}

Constructor TPSCCustomDateEdit.Create(AOwner: TComponent);
Begin
  Inherited;
  FDateTimeFormat:=TPSCDateTimeFormat.Create(Self,PSCGetDefaultDateTimeFormat);
  FDateTimeFormat.OnChange:=DateFormatChanged;

  FCalColors.BackColor := cPSCDefDTBackColor;
  FCalColors.TextColor := cPSCDefDTTextColor;
  FCalColors.TitleBackColor := cPSCDefDTTitleBackColor;
  FCalColors.TitleTextColor := cPSCDefDTTitleTextColor;
  FCalColors.MonthBackColor := cPSCDefDTMonthBackColor;
  FCalColors.TrailingTextColor := cPSCDefDTTrailingTextColor;

  FShowDateEdit := True;
  FOptions := CPSCDefaultDateEditOptions;
  FDateTime := PSCNow;
  UpdatePopup;

  WantReturns := False;
  ButtonsVisible:=True;
End;

{--------------------------------------}

Function TPSCCustomDateEdit.IsOptionsStored:Boolean;
begin
  Result:=Options<>CPSCDefaultDateEditOptions;
end;

{--------------------------------------}

Procedure TPSCCustomDateEdit.SetTime(Const V: TTime);
Begin
  DateTime := PSCCombineDateTime(DateTime,V);
End;

{--------------------------------------}

Procedure TPSCCustomDateEdit.SetDateTime(Const V: TDateTime);
Var
  NewValue,DatePart,TimePart: TDateTime;
Begin
  If Field <> Nil Then
    Begin
      If Field Is TTimeField Then
        DatePart := 0
      Else
        DatePart := PSCDateOf(Field.AsDateTime);

      If Field Is TDateField Then
        TimePart := 0
      Else
        TimePart := PSCTimeOf(Field.AsDateTime);

      Case Kind Of
        cpkDate:
          NewValue := PSCCombineDateTime(V,TimePart);
        cpkTime:
          NewValue := PSCCombineDateTime(DatePart,V);
      Else
        NewValue := V;
      End;

      If Field.AsDateTime <> V Then
        Begin

          if not Field.ReadOnly and (Field.DataSet <> nil) then
            Field.DataSet.Edit;

          If NewValue = 0 Then
            Field.Clear
          Else
            Field.AsDateTime := NewValue;
        End;

    End;

  FDateTime := V;
  UpdatePopup;
End;

{--------------------------------------}

Procedure TPSCCustomDateEdit.SetDate(Const V: TDate);
Begin
  DateTime := PSCCombineDateTime(V,DateTime);
End;

{--------------------------------------}

function TPSCCustomDateEdit.CorrectKind(V: TPSCDateTimeKind):TPSCDateTimeKind;
begin
  Result:=V;
  If Field <> Nil Then
    Begin
      If Field Is TTimeField Then
        Result := cpkTime;
      If Field Is TDateField Then
        Result := cpkDate;
    End;
end;

{--------------------------------------}

Procedure TPSCCustomDateEdit.SetKind(V: TPSCDateTimeKind);
Begin
  If FKind <> V Then
    Begin
      FKind := CorrectKind(V);
      UpdatePopup;
    End;
End;

{--------------------------------------}

Procedure TPSCDateTimeUpDown.Loaded;
begin
  inherited;
end;

{--------------------------------------}

Procedure TPSCDateTimeUpDown.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  If Operation = OpRemove Then
    If SelStartEdit = AComponent Then
      SelStartEdit := Nil
    Else
      If SelEndEdit = AComponent Then
        SelEndEdit := Nil;
end;

{--------------------------------------}

Procedure TPSCDateTimeUpDown.UpdatePopup;
var
  MyKind:TPSCDateTimeKind;
begin
  inherited;

  If PopupCreated Then
    With CalendarPopup,CalendarPanel,Calendar Do
      Begin
        DateEdit.DateTimeFormat:=Self.DateTimeFormat;
        TimeEdit.DateTimeFormat:=Self.DateTimeFormat;

        DateEdit.ThemeName:=Self.ThemeName;
        TimeEdit.ThemeName:=Self.ThemeName;

        CalendarPopup.Font:=Self.Font;

        MyKind:=Self.Kind;

        If MyKind=cpkDate then
          begin
            PanelKind:=cpkDateTime;
            TimeEdit.Enabled:=False;
          end
        else
          PanelKind := MyKind;
{
        OnDateChanged := nil;
        MultiSelect := deoMultiSelect In Self.FOptions;
        OnDateChanged := CalendarPanel.DateChanged;
}
        If Kind <> cpkTime Then
          CalendarPopup.ActiveControl := Calendar
        Else
          CalendarPopup.ActiveControl := TimeEdit;
{
        Calendar.MinDate := Self.MinDate;
        Calendar.MaxDate := Self.MaxDate;
        Calendar.MinDateLimit := Self.MinDateLimit;
        Calendar.MaxDateLimit := Self.MaxDateLimit;
}
        If Self.DateTime = 0 Then
          DateTime := PSCNow
        Else
          DateTime := Self.DateTime;

        CalendarPanel.ShowDateEdit := Self.ShowDateEdit;
{
        CalColorsChanged;
}        
      End;
end;

{--------------------------------------}

Function TPSCCustomDateEdit.CreatePopup: TPSCPopupForm;
Begin
  Result := CPSCUsedPopupCalendarClass.CreateNew(Nil,0);
  With TPSCPopupCalendar(Result) Do
    Begin
      CalendarPanel.Calendar.ExtendedSelect := False;
    End;
End;

{--------------------------------------}

Procedure TPSCCustomDateEdit.Loaded;
begin
  inherited;
  Kind := Kind;
end;

{--------------------------------------}

function TPSCCustomDateEdit.PostValue:boolean;
begin
  If PSCTrim(Text)='' then
    DateTime:=0
  else
    DateTime:=DateTimeFormat.FromString(Text,Kind,Self.DateTime);
  Result:=True;
end;

{--------------------------------------}

Function TPSCCustomDateEdit.GetDateTime: TDateTime;
Begin
  If Field <> Nil Then
    Begin
      If Field.IsNull Then
        Result := 0
      Else
        Result := Field.AsDateTime;
    End
  Else
    Result := FDateTime;
End;

{--------------------------------------}

procedure TPSCCustomDateEdit.SetDateTimeFormat(V:TPSCDateTimeFormat);
begin
  DateTimeFormat.Assign(V);
end;

{--------------------------------------}

function TPSCCustomDateEdit.GetAsText:String;
begin
  Result:=DateTimeFormat.ToStringEx(DateTime,Kind);
end;

{--------------------------------------}

Procedure TPSCCustomDateEdit.UpdatePopup;
var
  MyKind:TPSCDateTimeKind;
Begin
  Inherited;

  If Text <> GetAsText Then
    Text := GetAsText;

  If PopupCreated Then
    With CalendarPopup,CalendarPanel,Calendar Do
      Begin
        DateEdit.DateTimeFormat:=Self.DateTimeFormat;
        TimeEdit.DateTimeFormat:=Self.DateTimeFormat;
        
        DateEdit.ThemeName:=Self.ThemeName;
        TimeEdit.ThemeName:=Self.ThemeName;

        CalendarPopup.Font:=Self.Font;

        MyKind:=CorrectKind(Self.Kind);

        If MyKind=cpkDate then
          begin
            PanelKind:=cpkDateTime;
            TimeEdit.Enabled:=False;
          end
        else
          PanelKind := MyKind;

        OnDateChanged := nil;                           // --
        MultiSelect := deoMultiSelect In Self.FOptions;
        OnDateChanged := CalendarPanel.DateChanged;     // fixes exception when Multiselect is changed

        If Kind <> cpkTime Then
          CalendarPopup.ActiveControl := Calendar
        Else
          CalendarPopup.ActiveControl := TimeEdit;

        Calendar.MinDate := Self.MinDate;
        Calendar.MaxDate := Self.MaxDate;
        Calendar.MinDateLimit := Self.MinDateLimit;
        Calendar.MaxDateLimit := Self.MaxDateLimit;

        If Self.DateTime = 0 Then
          DateTime := PSCNow
        Else
          DateTime := Self.DateTime;

        CalendarPanel.ShowDateEdit := Self.ShowDateEdit;
        CalColorsChanged;
      End;
End;

{--------------------------------------}

Function TPSCCustomDateEdit.GetPopupCalendar: TPSCPopupCalendar;
Begin
  Result := TPSCPopupCalendar(Popup);
End;

{--------------------------------------}

Function TPSCCustomDateEdit.GetDate: TDate;
Begin
  Result := PSCDateOf(DateTime);
End;

{--------------------------------------}

Function TPSCCustomDateEdit.GetTime: TTime;
Begin
  Result := PSCTimeOf(DateTime);
End;

{--------------------------------------}

Procedure TPSCCustomDateEdit.SetMinDate(A: TDateTime);
Begin
  If FMinDate = A Then
    Exit;
  FMinDate := A;
  UpdatePopup;
End;

{--------------------------------------}

Procedure TPSCCustomDateEdit.SetMaxDate(A: TDateTime);
Begin
  If FMaxDate = A Then
    Exit;
  FMaxDate := A;
  UpdatePopup;
End;

{--------------------------------------}

Procedure TPSCCustomDateEdit.SetMinDateLimit(A: Boolean);
Begin
  If FMinDateLimit = A Then
    Exit;
  FMinDateLimit := A;
  UpdatePopup;
End;

{--------------------------------------}

Procedure TPSCCustomDateEdit.SetShowDateEdit(V: boolean);
Begin
  If FShowDateEdit <> V Then
    Begin
      FShowDateEdit := V;
      If PopupCreated Then
        CalendarPopup.CalendarPanel.ShowDateEdit := V;
    End;
End;

{--------------------------------------}

Procedure TPSCDateTimeUpDown.SetShowDateEdit(V: boolean);
Begin
  If FShowDateEdit <> V Then
    Begin
      FShowDateEdit := V;
      If PopupCreated Then
        CalendarPopup.CalendarPanel.ShowDateEdit := V;
    End;
End;

{--------------------------------------}

Procedure TPSCCustomDateEdit.SetMaxDateLimit(A: Boolean);
Begin
  If FMaxDateLimit = A Then
    Exit;
  FMaxDateLimit := A;
  UpdatePopup;
End;

{-------------------------------------}

Function TPSCTextPartAMPM.GetDisplayStr(AValue : integer): String;
begin
  Result:=GetEditStr(AValue);
end;

{-------------------------------------}

Function TPSCTextPartAMPM.GetEditStr(AValue : integer): String;
begin
  If AValue>0 then
    Result := GetRealPMValue
  else
    Result := GetRealAMValue;
end;

{-------------------------------------}

Function TPSCTextPartAMPM.GetMinValue: Integer;
begin
  Result:=0;
end;

{-------------------------------------}

Function TPSCTextPartAMPM.GetMaxValue: Integer;
begin
  Result:=1;
end;

{-------------------------------------}

Function TPSCTextPartAMPM.GetValue: integer;
var
  MyHour:Integer;
begin
  MyHour:=PSCGetTimeElement(GetDateTime,CPSCElementHour);
  If MyHour>=12 then
    Result:=1
  else
    Result:=0;
end;

{-------------------------------------}

Function TPSCTextPartAMPM.GetElementIndex:Integer;
begin
  Result:=CPSCElementHour;
end;

{-------------------------------------}

Procedure TPSCTextPartAMPM.SetValue(AValue: Integer);
var
  MyHour:Integer;
begin
  If GetValue<>AValue then
  begin
    MyHour:=PSCGetTimeElement(GetDateTime,CPSCElementHour);
    If MyHour>=12 then
      Dec(MyHour,12)
    else
      Inc(MyHour,12);
    inherited SetValue(MyHour);
  end;
end;

{-------------------------------------}

function TPSCTextPartAMPM.GetAMValue:String;
begin
  Result:=FAMValue;
end;

{-------------------------------------}

function TPSCTextPartAMPM.GetPMValue:String;
begin
  Result:=FPMValue;
end;

{-------------------------------------}

function TPSCTextPartAMPM.GetRealAMValue:String;
begin
  If AMValue<>'' then
    Result:=AMValue
  else
    Result:=DateTimeFormat.GetTimeAMString;
end;

{-------------------------------------}

function TPSCTextPartAMPM.GetRealPMValue:String;
begin
  If PMValue<>'' then
    Result:=PMValue
  else
    Result:=DateTimeFormat.GetTimePMString;
end;

{-------------------------------------}

Function TPSCTextPartAMPM.GetMaxWidth(const ACanvas:IPSCCanvas): Integer;
begin
  Result:=PSCMax(ACanvas.TextWidth(GetRealAMValue),
    ACanvas.TextWidth(GetRealPMValue));
end;

{-------------------------------------}

procedure TPSCTextPartAMPM.SetAMValue(const V:String);
begin
  If FAMValue<>V then
  begin
    FAMValue:=V;
    Changed;
  end;
end;

{-------------------------------------}

procedure TPSCTextPartAMPM.SetPMValue(const V:String);
begin
  If FPMValue<>V then
  begin
    FPMValue:=V;
    Changed;
  end;
end;

{-------------------------------------}

Function TPSCTextPartYear.GetMinValue: Integer;
begin
  Result:=PSCGetDateYear(0);
end;

{-------------------------------------}

Function TPSCTextPartYear.GetDisplayStr(AValue : integer): String;
begin
  AValue:=CorrectYear(AValue);
  Result:=inherited GetDisplayStr(AValue);
end;

{-------------------------------------}

Function TPSCTextPartYear.CorrectYear(AValue: Integer):Integer;
begin
  If TwoDigits and (Abs(AValue)>99) then
    Result:=AValue mod 100
  else
    Result:=AValue;
end;

{-------------------------------------}

Function TPSCTextPartYear.GetEditStr(AValue : integer): String;
begin
  AValue:=CorrectYear(AValue);
  Result:=inherited GetDisplayStr(AValue);
end;

{-------------------------------------}

Function TPSCTextPartYear.GetMaxWidth(const ACanvas:IPSCCanvas): Integer;
begin
  If TwoDigits and not LeadingZero then
    Result:=ACanvas.TextWidth('00')
  else
    Result:=inherited GetMaxWidth(ACanvas);
end;

{-------------------------------------}

Function TPSCTextPartYear.GetMaxValue: Integer;
begin
  Result:=PSCGetDateYear(PSCMaxDateTime);
end;

{-------------------------------------}

Function TPSCTextPartMinute.GetElementIndex:Integer;
begin
  Result:=CPSCElementMinute;
end;

{-------------------------------------}

Function TPSCTextPartMilliSecond.GetElementIndex:Integer;
begin
  Result:=CPSCElementMSec;
end;

{-------------------------------------}

Function TPSCTextPartSecond.GetElementIndex:Integer;
begin
  Result:=CPSCElementSecond;
end;

{-------------------------------------}

Function TPSCTextPartHour.GetElementIndex:Integer;
begin
  Result:=CPSCElementHour;
end;

{-------------------------------------}

Function TPSCTextPartMonth.GetElementIndex:Integer;
begin
  Result:=CPSCElementMonth;
end;

{-------------------------------------}

Function TPSCTextPartDay.GetElementIndex:Integer;
begin
  Result:=CPSCElementDay;
end;

{-------------------------------------}

procedure TPSCTextPartYear.SetTwoDigits(V:Boolean);
begin
  If FTwoDigits<>V then
  begin
    FTwoDigits:=V;
    Changed;
  end;
end;

{-------------------------------------}

Function TPSCTextPartYear.GetElementIndex:Integer;
begin
  Result:=CPSCElementYear;
end;

{-------------------------------------}

Function TPSCTextPartTimeElement.GetValue: integer;
begin
  Result:=PSCGetTimeElement(GetDateTime,GetElementIndex);
end;

{-------------------------------------}

Procedure TPSCTextPartTimeElement.SetValue(AValue: Integer);
begin
  SetDateTime(PSCChangeTimeElement(GetDateTime,GetElementIndex,AValue));
end;

{-------------------------------------}

Function TPSCTextPartDateElement.GetValue: integer;
begin
  Result:=PSCGetDateElement(GetDateTime,GetElementIndex);
end;

{-------------------------------------}

Procedure TPSCTextPartDateElement.SetValue(AValue: Integer);
var
  MyYear,MyMonth,MyDay:Word;
begin
  PSCDecodeDate(GetDateTime,MyYear,MyMonth,MyDay);
  case GetElementIndex of
    CPSCElementYear:
      MyYear:=AValue;
    CPSCElementMonth:
      MyMonth:=AValue;
    CPSCElementDay:
      MyDay:=AValue;
  end;
  MyDay:=PSCMin(MyDay,PSCDaysPerMonth(MyYear,MyMonth));
  SetDateTime(PSCTimeOf(GetDateTime)+PSCEncodeDate(MyYear,MyMonth,MyDay));
end;

{-------------------------------------}

Function TPSCTextPartMinute.GetMaxValue: Integer;
begin
  Result:=59;
end;

{-------------------------------------}

Function TPSCTextPartMilliSecond.GetMaxValue: Integer;
begin
  Result:=999;
end;

{-------------------------------------}

Function TPSCTextPartSecond.GetMaxValue: Integer;
begin
  Result:=59;
end;

{-------------------------------------}

procedure TPSCTextPartHour.SetUseAMPM(V:Boolean);
begin
  If FUseAMPM<>V then
  begin
    FUseAMPM:=V;
    Changed;
  end;
end;

{-------------------------------------}

Function TPSCTextPartHour.GetDisplayStr(AValue : integer): String;
begin
  If UseAMPM and (AValue>12) then
    dec(AValue,12);
  Result:=inherited GetDisplayStr(AValue);
end;

{-------------------------------------}

Function TPSCTextPartHour.GetEditStr(AValue : integer): String;
begin
  If UseAMPM and (AValue>12) then
    dec(AValue,12);
  Result:=inherited GetEditStr(AValue);
end;

{-------------------------------------}

Function TPSCTextPartHour.GetMaxValue: Integer;
begin
  Result:=23;
end;

{-------------------------------------}

Function TPSCTextPartMonth.GetDisplayStr(AValue : integer): String;
begin
  Result:=GetEditStr(AValue);
end;

{-------------------------------------}

procedure TPSCTextPartMonth.SetMonthFormat(V:TPSCMonthFormat);
begin
  If FMonthFormat<>V then
  begin
    FMonthFormat:=V;
    Changed;
  end;
end;

{-------------------------------------}

Function TPSCTextPartMonth.GetMonthName(AIndex:Integer):String;
begin
  case FMonthFormat of
    mfShort:
      Result:=DateTimeFormat.GetShortMonthName(AIndex);
    mfTwoDigits:
      begin
        Result:=PSCIntToStr(AIndex);
        If AIndex<10 then
          Result:=' '+Result;
      end;
    mfDigits:
      Result:=PSCIntToStr(AIndex);
  else
    Result:=DateTimeFormat.GetLongMonthName(AIndex);
  end;
end;

{-------------------------------------}

Function TPSCTextPartMonth.GetEditStr(AValue : integer): String;
begin
  Result:=GetMonthName(AValue);
end;

{-------------------------------------}

Function TPSCTextPartMonth.GetMaxWidth(const ACanvas:IPSCCanvas): Integer;
var
  i:Integer;
begin
  Result:=0;
  for i:=1 to 12 do
    Result:=PSCMax(Result,ACanvas.TextWidth(GetMonthName(i)));
end;

{-------------------------------------}

Function TPSCTextPartMonth.GetMinValue: Integer;
begin
  Result:=1;
end;

{-------------------------------------}

Function TPSCTextPartMonth.GetMaxValue: Integer;
begin
  Result:=12;
end;

{-------------------------------------}

Function TPSCTextPartDay.GetMinValue: Integer;
begin
  Result:=1;
end;

{-------------------------------------}

Function TPSCTextPartDay.GetMaxValue: Integer;
begin
  Result:=PSCDaysPerMonth(PSCGetDateYear(GetDateTime),
    PSCGetDateMonth(GetDateTime));
end;

{-------------------------------------}

Function TPSCTextPartDateTimeElement.GetElementIndex:Integer;
begin
  Result:=0;
end;

{-------------------------------------}

Function TPSCTextPartDateTimeElement.GetDateTime:TDateTime;
begin
  Result:=FGetValue;
end;

{-------------------------------------}

function TPSCTextPartDateTimeElement.GetDateTimePartKind:TPSCDateTimePart;
begin
  Result:=FKind;
end;

{-------------------------------------}

procedure TPSCTextPartDateTimeElement.SetDateTimePartKind(V:TPSCDateTimePart);
begin
  FKind:=V;
end;

{-------------------------------------}

procedure TPSCTextPartDateTimeElement.SetDateTime(const AValue:TDateTime);
begin
  If GetDateTime<>AValue then
  begin
    FSetValue(AValue);
    {Changed; is not needed as OnChange is called from FSetValue}
  end;
end;

{-------------------------------------}

Function TPSCTextPartDateTimeElement.GetMinValue: Integer;
begin
  Result:=0;
end;

{-------------------------------------}

constructor TPSCTextPartDateTimeElement.Create(
  AGetValue:TPSCGetDateTimeProc;ASetValue:TPSCSetDateTimeProc;
  const AFormat:IPSCDateTimeFormat);
begin
  inherited Create;
  FGetValue:=AGetValue;
  FSetValue:=ASetValue;
  LeadingZero:=True;
  FFormat:=AFormat;
end;

{-------------------------------------}

Procedure TPSCTextPartDateTimeElement.SetMinValue(AValue: Integer);
begin
end;

{-------------------------------------}

Procedure TPSCTextPartDateTimeElement.SetMaxValue(AValue: Integer);
begin
end;

{-------------------------------------}

Function TPSCTextPartDayName.GetDisplayStr(AValue : integer): String;
begin
  Result:=GetEditStr(AValue);
end;

{-------------------------------------}

procedure TPSCTextPartDayName.SetUseLongNames(V:Boolean);
begin
  If UseLongNames<>V then
  begin
    FUseLongNames:=V;
    Changed;
  end;
end;

{-------------------------------------}

Function TPSCTextPartDayName.GetMinValue: Integer;
begin
  Result:=1;
end;

{-------------------------------------}

Function TPSCTextPartDayName.GetMaxValue: Integer;
begin
  Result:=7;
end;

{-------------------------------------}

Function TPSCTextPartDayName.GetElementIndex:Integer;
begin
  Result:=CPSCElementDay;
end;

{-------------------------------------}

Function TPSCTextPartDayName.GetMaxWidth(const ACanvas:IPSCCanvas): Integer;
var
  i:Integer;
begin
  Result:=0;
  for i:=GetMinValue to GetMaxValue do
    Result:=PSCMax(Result,ACanvas.TextWidth(GetEditStr(i)));
end;

{-------------------------------------}

Function TPSCTextPartDayName.GetEditStr(AValue : integer): String;
begin
  If UseLongNames then
    Result:=DateTimeFormat.GetLongDayName(AValue)
  else
    Result:=DateTimeFormat.GetShortDayName(AValue);
end;

{-------------------------------------}

Function TPSCTextPartDayName.GetValue: integer;
begin
  Result:=PSCDayOfWeek(GetDateTime);
end;

{-------------------------------------}

Procedure TPSCTextPartDayName.SetValue(AValue: Integer);
begin
  If GetValue<>AValue then
    SetDateTime(PSCChangeWeekDay(GetDateTime,TPSCWeekDay(AValue)));
end;

{-------------------------------------}

end.
