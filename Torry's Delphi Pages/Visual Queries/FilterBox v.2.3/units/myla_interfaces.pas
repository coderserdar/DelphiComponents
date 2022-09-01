{***********************************************************************}
{                                                                       }
{  Copyright (c) 1999-2015 Sergiy Kurinny                               }
{                                                                       }
{  This library is free software; you can redistribute it and/or        }
{  modify it under the terms of the GNU Lesser General Public           }
{  License version 2.1 as published by the Free Software Foundation     }
{  and appearing in the file license.txt which is included in the root  }
{  folder of the package.                                               }
{                                                                       }
{  This library is distributed in the hope that it will be useful,      }
{  but WITHOUT ANY WARRANTY; without even the implied warranty of       }
{  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU    }
{  Lesser General Public License for more details.                      }
{                                                                       }
{***********************************************************************}
unit myla_interfaces;

interface
{$I psc_defines.inc}

uses
  System.UITypes;

type
  TPSCFieldType = (
    FT_UNK,        // =0
    FT_INT,        // =1
    FT_DATETIME,   // =2
    FT_DATE,       // =3
    FT_TIME,       // =4
    FT_CURRENCY,   // =5
    FT_FLOAT,      // =6
    FT_STRING,     // =7
    FT_BOOL,       // =8
    FT_OBJECT,     // =9
    FT_INTERFACE,  // =10
    FT_MEMO,       // =11
    FT_WIDESTR     // =12
  );

  TPSCFieldTypes = set of TPSCFieldType;

  TPSCEventType=(
    EVENT_AFTER_CHANGE,         // =0
    EVENT_AFTER_CANCEL,         // =1
    EVENT_AFTER_EDIT,           // =2
    EVENT_AFTER_POST,           // =3
    EVENT_AFTER_REFRESH,        // =4
    EVENT_BEFORE_CHANGE,        // =5
    EVENT_BEFORE_EDIT,          // =6
    EVENT_BEFORE_POST,          // =7
    EVENT_BEFORE_CANCEL,        // =8
    EVENT_BEFORE_REFRESH,       // =9
    EVENT_AFTER_DELETE,         // =10
    EVENT_AFTER_INSERT,         // =11
    EVENT_AFTER_SCROLL,         // =12
    EVENT_BEFORE_INSERT,        // =13
    EVENT_BEFORE_DELETE,        // =14
    EVENT_BEFORE_SCROLL         // =15
  );

  {$IFNDEF D6}
  IInterface=IUnknown;
  {$ENDIF}

  IPSCInterface=interface
    ['{5E22111A-64A3-4EFC-B910-860F3028E300}']
  end;

  IPSCInstanceID=interface(IPSCInterface)
  ['{B8CC16CC-BA76-4F5E-AD8A-7907D180EF2D}']
    function GetInstanceID:Integer;
  end;

  TPSCEventParams=record
    ASender:IPSCInterface;
    AEventType:TPSCEventType;
    AEventData:Cardinal;
    AEventDataEx:IPSCInterface;
  end;

  IPSCEventHandler=Interface(IPSCInstanceID)
  ['{F6C619D5-AD83-11D3-BCE0-00E07D743236}']
    Procedure HandleEvent(const AParams:TPSCEventParams);
  End;

  IPSCEvents=Interface(IPSCEventHandler)
  ['{F6C619D4-AD83-11D3-BCE0-00E07D743236}']
    Procedure HandleSimpleEvent(AEventType:TPSCEventType);
    procedure RegisterHandler(const AEventHandler:IPSCEventHandler);
    procedure UnregisterHandler(const AEventHandler:IPSCEventHandler);
  End;

  IPSCDataField = interface(IPSCEvents)
    ['{F158FF76-336F-40A8-91E7-77C238239B60}']
    function GetAsObject: TObject;
    function GetAsInterface: IPSCInterface;
    function GetAsBoolean : boolean;
    function GetAsCurrency : Currency;
    function GetAsDateTime : TDateTime;
    function GetAsDouble : Double;
    function GetAsInteger : Integer;
    function GetAsString : String;  //AnsiString;
    function GetFieldType : TPSCFieldType;
    function GetAsWideString: WideString;

    procedure SetAsObject(Value:TObject);
    procedure SetAsInterface(const Value:IPSCInterface);
    procedure SetAsBoolean(Value : boolean);
    procedure SetAsDateTime(const Value : TDateTime);
    procedure SetAsDouble(const Value : Double);
    procedure SetAsCurrency(const Value : Currency);
    procedure SetAsInteger (Value : Integer);
    procedure SetAsString(const Value : String {AnsiString});
    procedure SetAsWideString(const Value : WideString);

    function IsNull:Boolean;
    function CanModify:Boolean;
    function GetDisplayLabel:String;
    function GetRequired:Boolean;
    function GetVisible:Boolean;
    function GetName:String;

    procedure Clear;

    procedure Edit;
    procedure Post;
    procedure Cancel;


    property AsObject: TObject          Read GetAsObject Write SetAsObject;
    property AsInterface: IPSCInterface Read GetAsInterface Write SetAsInterface;
    property AsBoolean : boolean        Read GetAsBoolean Write SetAsBoolean;
    property AsCurrency : Currency      Read GetAsCurrency Write SetAsCurrency;
    property AsDateTime : TDateTime     Read GetAsDateTime Write SetAsDateTime;
    property AsDouble : Double          Read GetAsDouble Write SetAsDouble;
    property AsInteger : Integer        Read GetAsInteger Write SetAsInteger;
    property AsString : String {AnsiString} Read GetAsString Write SetAsString;
    property AsWideString: WideString   Read GetAsWideString Write SetAsWideString;
    property FieldType : TPSCFieldType  Read GetFieldType;
  end;

  IPSCDataFieldDef = interface(IPSCInterface)
    ['{93834E21-0CA3-47E2-A332-85AB3EC6FA90}']
    function GetName:String;
    function GetType:TPSCFieldType;
  end;

  IPSCDataFieldDefs = interface(IPSCInterface)
    ['{24C72B94-CA52-4CDD-B3A8-888A2FCBC712}']
    function GetFieldDefCount:Integer;
    function GetFieldDef(const AIndex: Integer): IPSCDataFieldDef;
  end;

  IPSCDataFieldDefsEx = interface(IPSCDataFieldDefs)
    ['{29DCA665-82DF-4058-8533-A854D6251057}']
    function AddFieldDef(const AName:String; AType:TPSCFieldType):IPSCDataFieldDef;
  end;

  IPSCDataFields = interface(IPSCEvents)
    ['{DB3DBA67-79D9-4ACC-8F46-4732D6C06F1F}']
    function GetField(const AIndex: integer): IPSCDataField;
    function FindField(const AFieldName:String): IPSCDataField;
    function GetFieldCount:Integer;
  end;

  IPSCDataSet = interface(IPSCDataFields)
    ['{E428CE2D-48F5-481D-82C0-15FA297F4A03}']
    function IsEmpty: Boolean;
    function Bof:Boolean;
    function Eof:Boolean;

    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Cancel;
    procedure Post;
    procedure Append;
    procedure Delete;
    procedure Edit;
    procedure First;
    procedure Last;
    procedure Next;
    procedure Prior;
  end;

  TPSCSeekOrigin=(
    SeekFromBOF,
    SeekFromCurrent,
    SeekFromEOF
  );

  IPSCStream=interface(IPSCInterface)
    ['{DF5BB9B9-BD12-4510-ABB7-58782D672611}']
    function GetPosition:Int64;
    function GetSize:Int64;
    function Seek(const AOffset: Int64; AOrigin: TPSCSeekOrigin): Int64;
    function CopyTo(const AStream: IPSCStream; const ACount: Int64): Int64;
    function Write(const ABuffer:Array of Byte; const ACount: Int64): Int64;
    function ReadByte(out V:Byte):Boolean;

    procedure SetPosition(const V:Int64);
    procedure SetSize(const V:Int64);

    property Position: Int64 read GetPosition write SetPosition;
    property Size: Int64 read GetSize write SetSize;
  end;

  TPSCItemOwnership=(
    ioReferenced,
    ioOwned
  );

  IPSCCompareObjects=interface(IPSCInterface)
    ['{BEDE08A7-6E8C-432B-851A-8D8ABD995624}']
    function CompareObjects(AObject1,AObject2:TObject):Integer;
  end;

  TPSCDuplicates = (
    DUP_Ignore,
    DUP_Accept
  );

  TPSCCharSet = Set Of Ansichar;
  
  TPSCDateTimeKind = (
    cpkDateTime,
    cpkDate,
    cpkTime
  );

  TPSCCheckBoxState=(
    CheckBox_Unchecked,
    CheckBox_Checked,
    CheckBox_Grayed
  );

  IPSCCustomList=interface(IPSCInterface)
    ['{4EF6FF93-C82E-4B60-92F9-C80A1FD74934}']
    function GetCount:Integer;
    function GetCapacity:Integer;

    procedure SetCapacity(V:Integer);
    procedure Delete(AIndex:Integer);
    procedure SetCount(V:Integer);
    procedure Clear;

    property Count:Integer Read GetCount Write SetCount;
    property Capacity:Integer Read GetCapacity Write SetCapacity;
  end;

  IPSCCustomSortedList=interface(IPSCCustomList)
    ['{DAEE61C7-6CC4-48BC-AB06-72082298C289}']
    function GetDuplicates:TPSCDuplicates;
    function GetSorted:Boolean;

    procedure SetDuplicates(V:TPSCDuplicates);
    
    property Duplicates:TPSCDuplicates Read GetDuplicates Write SetDuplicates;
  end;

  IPSCObjectList=interface(IPSCCustomSortedList)
    ['{3D706157-64C7-4A4E-B7C5-9C2A61F530FF}']
    function Add(AObject:TObject):Integer;
    function GetItem(AIndex:Integer):TObject;
    function IndexOf(AObject:TObject):Integer;
    function Find(AObject:TObject; var Index: Integer): Boolean;

    procedure SetItem(AIndex:Integer;AObject:TObject);
    procedure Remove(AObject:TObject);
    procedure Sort(const ACriteria:IPSCCompareObjects);
    procedure SetSortCriteria(const V:IPSCCompareObjects);
    procedure Insert(AIndex: Integer; AItem: TObject);

    property Items[AIndex:Integer]:TObject Read GetItem Write SetItem;default;
    property Sorted:Boolean Read GetSorted;
  end;

  IPSCInterfaceList=interface(IPSCCustomList)
    ['{B13749A6-6C1A-42BF-BD78-2BECB194D887}']
    function Add(const AItem:IPSCInterface):Integer;
    function GetItem(AIndex:Integer):IPSCInterface;
  end;

  IPSCStrings=interface(IPSCCustomSortedList)
    ['{7E045AEF-8EF2-4EE1-B19D-6FB7705EBBCA}']
    function IndexOfName(const Name: string): Integer;
    function Add(const S: string): Integer;
    function AddObject(const S: string; AObject: TObject): Integer;
    function IndexOf(const S: string): Integer;
    function IndexOfObject(AObject: TObject): Integer;

    procedure Assign(const AStrings:IPSCStrings);
    procedure AssignTo(const AStrings:IPSCStrings);
    procedure AddStrings(const AStrings:IPSCStrings);
    procedure BeginUpdate;
    procedure EndUpdate;

    function GetCommaText:String;
    function GetName(AIndex: Integer):String;
    function GetObject(AIndex: Integer):TObject;
    function GetValue(const AName: string): string;
    function GetString(AIndex: Integer): string;
    function GetTextStr:String;
    function Find(const S: string; var Index: Integer): Boolean;

    procedure SetTextStr(const AValue:String);
    procedure PutString(AIndex: Integer;const AValue: string);
    procedure SetValue(const AName,AValue: string);
    procedure PutObject(AIndex: Integer;AObject:TObject);
    procedure SetSorted(AValue:Boolean);
    procedure Sort;

    property Sorted: Boolean read GetSorted write SetSorted;
    property Names[Index: Integer]: string read GetName;
    property Objects[Index: Integer]: TObject read GetObject write PutObject;
    property Values[const Name: string]: string read GetValue write SetValue;
    property Strings[Index: Integer]: string read GetString write PutString;default;
    property Text: String read GetTextStr write SetTextStr;
  end;

  IPSCSaveToFile=interface(IPSCInterface)
    ['{70416D72-4751-4C13-A812-A1A88116C4A3}']
    procedure LoadFromFile(const AFileName:String);
    procedure SaveToFile(const AFileName:String);
  end;

  IPSCParserStream=interface(IPSCInterface)
    ['{9D2F29C3-CA5D-440C-8CB7-F34D6864E68A}']
    function GetChar:Char;
    procedure Next;
    procedure Prior;
    function Eof:Boolean;
  end;
  
  TPSCDateTimeFormatRec = Record
    ShortTimeFormat: String;
    ShortDateFormat: String;
    LongDateFormat: String;
    LongTimeFormat: String;
    DateSeparator: String;
    TimeSeparator: String;
    TimeAMString: String;
    TimePMString: String;
  End;

  TPSCFieldPrepareType = (fptNoPrepare,fptEncloseInBrackets);
  TPSCGetStringsProc = Procedure(const AStrings: IPSCStrings) Of Object;
  
  PCardinal=^Cardinal;
  PDouble=^Double;
  PWord=^Word;
  PInteger=^Integer;

  TPSCCountryID = Type Integer;
  TPSCStringsOperateProc=function(const S:IPSCStrings;AUserData:Cardinal):boolean;
  
  TPSCLongRec = packed record
    Lo, Hi: Word;
  end;

  TPSCMenuSepAction = (msaRemove,msaHide,msaShowHide);

  TPSCGetStrProc = procedure(const S: string) of object;

  TPSCNotifyEvent=procedure(Sender:TObject) of Object;

  TPSCArrowKind = (ArrowUp, ArrowDown, ArrowLeft, ArrowRight);
  TPSCHorzAlign=(haLeft,haCenter,haRight);
  TPSCVertAlign=(vaTop,vaCenter,vaBottom);

  TPSCFirstDayOfWeek = (dwLocaleDefault,dwSunday,dwMonday,dwTuesday,
    dwWednesday,dwThursday,dwFriday,dwSaturday);

  TPSCWeekDay = TPSCFirstDayOfWeek;

  TPSCFirstWeekOfYear = (fwLocaleDefault,
    fwJanFirst,fwFirstFourthDayOfWeek,fwFirstFullWeek);

  TPSCStrToDateTimeProc = Function(Const S: String; Const AOldValue:
    TDateTime): TDateTime;

  TPSCDateTimePart=(
    dtpSeparator,
    dtpYear,
    dtpMonth,
    dtpDay,
    dtpHour,
    dtpMinute,
    dtpSecond,
    dtpMSec,
    dtpAMPM
  );

  TPSCDateTimePartsArray=Array[TPSCDateTimePart] of Word;

  TPSCDateTimeParts=set of TPSCDateTimePart;
  
  TPSCStrConvertProc=function(const S:String):String;

  TPSCWeekDays = Set Of TPSCWeekDay;
  TPSCDateSelectedProc = Function(Const ADate: TDateTime): boolean Of Object;

  TPSCMonth =
    (mNotAMonth,mJanuary,mFebruary,mMarch,mApril,mMay,mJune,mJuly,mAugust,
    mSeptember,mOctober,mNovember,mDecember);
  TPSCMonths=set of TPSCMonth;
  TPSCDayConstKind = (dckDate,dckDayNumber,dckProc);
  TPSCDay = -1..31;
  TPSCGetHolidayDateProc = Function(Const ABaseDate: TDateTime): TDateTime;

  {
    Encoding examples:

    Last Sun in May
    ---------------
      Kind=dckDayNumber
      Day=-1
      Month=mMay
      WeekDay=wdSunday

    3rd Wed in November
    -------------------
      Kind=dckDayNumber
      Day=3
      Month=mNovember
      WeekDay=wdWednesday

    31 July
    -------------------
      Kind=dckDate
      Day=31
      Month=mJuly
  }

  TPSCDayConst = Packed Record
    Kind: TPSCDayConstKind;
    Day: TPSCDay;
    Month: TPSCMonth;
    WeekDay: TPSCWeekDay;
    Name: String;
    DateProc: TPSCGetHolidayDateProc;
  End;

  TPSCShiftStateElem=(
    ssShiftPressed,
    ssAltPressed,
    ssCtrlPressed,
    ssLeftPressed,
    ssRightPressed,
    ssMiddlePressed,
    ssDoubleClick
  );

  TPSCShiftState = set of TPSCShiftStateElem;

  TPSCMouseButton = (
    mbButtonLeft,
    mbButtonRight,
    mbButtonMiddle
  );

  TPSCTextCase = (tcDefault,tcUpper,tcLower);

  TPSCColor = -$7FFFFFFF-1..$7FFFFFFF;

  TPSCPenMode = (
    pm_Black,
    pm_White,
    pm_Nop,
    pm_Not,
    pm_Copy,
    pm_NotCopy,
    pm_MergePenNot,
    pm_MaskPenNot,
    pm_MergeNotPen,
    pm_MaskNotPen,
    pm_Merge,
    pm_NotMerge,
    pm_Mask,
    pm_NotMask,
    pm_Xor,
    pm_NotXor
  );

  TPSCBrushStyle = (
    BS_Solid,
    BS_Clear,
    BS_Horizontal,
    BS_Vertical,
    BS_FDiagonal,
    BS_BDiagonal,
    BS_Cross,
    BS_DiagCross
  );

  TPSCFillStyle = (
    FS_Surface,
    FS_Border
  );

  TPSCPenStyle = (
    ps_Solid,
    ps_Dash,
    ps_Dot,
    ps_DashDot,
    ps_DashDotDot,
    ps_Clear,
    ps_InsideFrame
  );

  TPSCFontStyle = (
    FS_Bold,
    FS_Italic,
    FS_Underline,
    FS_StrikeOut
  );

  TPSCFontStyles = set of TPSCFontStyle;

  TPSCPoint = packed record
    X: Longint;
    Y: Longint;
  end;

  TPSCRect = packed record
    Left, Top, Right, Bottom: Longint;
  end;

  TPSCSize = packed record
    cx: Longint;
    cy: Longint;
  end;

  IPSCBrush=interface(IPSCInterface)
    ['{478DE782-6B59-44BB-A91C-FF6B302D8990}']
    function GetColor:TPSCColor;
    function GetStyle:TPSCBrushStyle;

    procedure Assign(const ABrush:IPSCBrush);
    procedure SetColor(AValue:TPSCColor);
    procedure SetStyle(AValue:TPSCBrushStyle);

    property Color: TPSCColor read GetColor write SetColor;
    property Style: TPSCBrushStyle read GetStyle write SetStyle;
  end;

  IPSCPen=interface(IPSCInterface)
    ['{FD715A88-8EF4-4232-923E-988428383CD4}']
    function GetColor: TPSCColor;
    function GetStyle: TPSCPenStyle;
    function GetMode: TPSCPenMode;
    function GetWidth: Integer;

    procedure SetStyle(AValue: TPSCPenStyle);
    procedure SetWidth(AValue: Integer);
    procedure SetColor(AValue: TPSCColor);
    procedure SetMode(AValue: TPSCPenMode);
    procedure Assign(const APen:IPSCPen);

    property Color: TPSCColor read GetColor write SetColor;
    property Mode: TPSCPenMode read GetMode write SetMode;
    property Style: TPSCPenStyle read GetStyle write SetStyle;
    property Width: Integer read GetWidth write SetWidth;
  end;

  IPSCFont=interface(IPSCInterface)
    ['{35729C7E-DDDC-4C73-A2FE-7F2B1F176C95}']
    function GetColor: TPSCColor;
    function GetName: string;
    function GetSize: Integer;
    function GetStyle: TPSCFontStyles;

    procedure SetColor(AValue: TPSCColor);
    procedure SetName(const AValue: string);
    procedure SetSize(AValue: Integer);
    procedure SetStyle(AValue: TPSCFontStyles);
    procedure Assign(const AFont:IPSCFont);

    property Color: TPSCColor read GetColor write SetColor;
    property Name: string read GetName write SetName;
    property Size: Integer read GetSize write SetSize;
    property Style: TPSCFontStyles read GetStyle write SetStyle;
  end;

  IPSCCanvas=interface(IPSCInterface)
  ['{2B7D4193-7888-4B13-8C57-56D81C35B025}']
    Procedure ExcludeClipRect(X1,Y1,X2,Y2: Integer);

    function TextExtent(const AText: WideString): TPSCSize;
    function TextHeight(const AText: WideString): Integer;
    function TextWidth(const AText: WideString): Integer;
    function GetBrush: IPSCBrush;
    function GetFont: IPSCFont;
    function GetPen: IPSCPen;
    function GetTextFlags : integer;

    procedure Arc(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);
    procedure Chord(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);
    procedure DrawFocusRect(const ARect: TPSCRect);
    procedure Ellipse(const ARect: TPSCRect);
    procedure FillRect(const ARect: TPSCRect);
    procedure FrameRect(const Rect: TPSCRect);
    procedure LineTo(X, Y: Integer);
    procedure MoveTo(X, Y: Integer);
    procedure Line(X1, Y1, X2, Y2: Integer);
    procedure Pie(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);
    procedure Polygon(const APoints: array of TPSCPoint);
    procedure Polyline(const APoints: array of TPSCPoint);
    procedure PolyBezier(const APoints: array of TPSCPoint);
    procedure Rectangle(X1, Y1, X2, Y2: Integer); overload;
    procedure Rectangle(const ARect: TPSCRect); overload;
    procedure RoundRect(X1, Y1, X2, Y2, X3, Y3: Integer);
    procedure TextOut(X, Y: Integer; const AText: WideString);
    procedure TextRect(const ARect: TPSCRect; X, Y: Integer; const AText: WideString);
    procedure SetTextFlags(AValue : integer);
    procedure SetBrush(const AValue:IPSCBrush);
    procedure SetPen(const AValue:IPSCPen);
    procedure SetFont(const AValue:IPSCFont);

    property Brush: IPSCBrush read GetBrush write SetBrush;
    property Font: IPSCFont read GetFont write SetFont;
    property Pen: IPSCPen read GetPen write SetPen;
    property TextFlags : integer read GetTextFlags write SetTextFlags;
  end;

  TPSCThemeClass = (
    tcWindow,
    tcButton,
    tcReBar,
    tcToolBar,
    tcStatus,
    tcListView,
    tcHeader,
    tcProgress,
    tcTab,
    tcTrackBar,
    tcToolTip,
    tcTreeView,
    tcSpin,
    tcScrollBar,
    tcEdit,
    tcComboBox,
    tcTaskBar,
    tcTaskBand,
    tcStartPanel,
    tcExplorerBar
    );

  IPSCThemeLib = interface(IPSCInterface)
  ['{E92AA365-ABD5-491C-9BFD-2550AF10CD64}']
    procedure DrawThemeBackground(
      AClass: TPSCThemeClass;
      const ACanvas: IPSCCanvas;
      APart: Integer;
      AState: Integer;
      const ARect: TPSCRect
      );
    procedure DrawThemeText(
      AClass: TPSCThemeClass;
      const ACanvas: IPSCCanvas;
      APart: Integer;
      AState: Integer;
      const ARect: TPSCRect;
      const ACaption: WideString;
      AFlags: Integer
      );
    procedure GetThemeIntData(
      AClass: TPSCThemeClass;
      APart, AState: Integer;
      APropID: Integer;
      var AValue: Integer
      );
    function GetThemeBoolData(
      AClass: TPSCThemeClass;
      APart, AState: Integer;
      APropID: Integer
    ): LongBool;

    function GetThemeColorData(
      AClass: TPSCThemeClass;
      APart, AState: Integer;
      APropID: Integer
    ): TPSCColor;

    function SupportsThemeClass(AClass: TPSCThemeClass): Boolean;
  end;

{------------------------------------------------------------------}

const

  TMT_TEXTCOLOR                               = 3803;
  TMT_FILLCOLOR                               = 3802;
  TMT_BORDERCOLOR                             = 3801;
  TMT_BORDERSIZE                              = 2403;

  TMT_PSCINTERNAL                             = 100;
  TMT_SEPARATEBUTTONSFROMEDIT                 = TMT_PSCINTERNAL+1;
  TMT_SEPARATESPINBUTTONS                     = TMT_PSCINTERNAL+2;
  TMT_FOCUSEDEDITWHENDROPPED                  = TMT_PSCINTERNAL+3;
  TMT_NOHOTWHENSIBLINGFOCUSED                 = TMT_PSCINTERNAL+4;
  TMT_PRESSEDBTNWHENDROPPED                   = TMT_PSCINTERNAL+5;
  TMT_NARROWCOMBOBTN                          = TMT_PSCINTERNAL+6;

  TMT_PSCINTERNAL_MAX                         = 107; {please update me with max internal+1 value}

//----------------------------------------------------------------------------------------------------------------------
//   "Window"
//----------------------------------------------------------------------------------------------------------------------

  WINDOW_PART_CAPTION                         = 1;
  WINDOW_PART_SMALLCAPTION                    = 2;
  WINDOW_PART_MINCAPTION                      = 3;
  WINDOW_PART_SMALLMINCAPTION                 = 4;
  WINDOW_PART_MAXCAPTION                      = 5;
  WINDOW_PART_SMALLMAXCAPTION                 = 6;
  WINDOW_PART_FRAMELEFT                       = 7;
  WINDOW_PART_FRAMERIGHT                      = 8;
  WINDOW_PART_FRAMEBOTTOM                     = 9;
  WINDOW_PART_SMALLFRAMELEFT                  = 10;
  WINDOW_PART_SMALLFRAMERIGHT                 = 11;
  WINDOW_PART_SMALLFRAMEBOTTOM                = 12;
  WINDOW_PART_SYSBUTTON                       = 13;
  WINDOW_PART_MDISYSBUTTON                    = 14;
  WINDOW_PART_MINBUTTON                       = 15;
  WINDOW_PART_MDIMINBUTTON                    = 16;
  WINDOW_PART_MAXBUTTON                       = 17;
  WINDOW_PART_CLOSEBUTTON                     = 18;
  WINDOW_PART_SMALLCLOSEBUTTON                = 19;
  WINDOW_PART_MDICLOSEBUTTON                  = 20;
  WINDOW_PART_RESTOREBUTTON                   = 21;
  WINDOW_PART_MDIRESTOREBUTTON                = 22;
  WINDOW_PART_HELPBUTTON                      = 23;
  WINDOW_PART_MDIHELPBUTTON                   = 24;
  WINDOW_PART_HORZSCROLL                      = 25;
  WINDOW_PART_HORZTHUMB                       = 26;
  WINDOW_PART_VERTSCROLL                      = 27;
  WINDOW_PART_VERTTHUMB                       = 28;
  WINDOW_PART_DIALOG                          = 29;
  WINDOW_PART_CAPTIONSIZINGTEMPLATE           = 30;
  WINDOW_PART_SMALLCAPTIONSIZINGTEMPLATE      = 31;
  WINDOW_PART_FRAMELEFTSIZINGTEMPLATE         = 32;
  WINDOW_PART_SMALLFRAMELEFTSIZINGTEMPLATE    = 33;
  WINDOW_PART_FRAMERIGHTSIZINGTEMPLATE        = 34;
  WINDOW_PART_SMALLFRAMERIGHTSIZINGTEMPLATE   = 35;
  WINDOW_PART_FRAMEBOTTOMSIZINGTEMPLATE       = 36;
  WINDOW_PART_SMALLFRAMEBOTTOMSIZINGTEMPLATE  = 37;

  WINDOW_FRAME_STATE_ACTIVE                   = 1;
  WINDOW_FRAME_STATE_INACTIVE                 = 2;

  WINDOW_CAPTION_STATE_ACTIVE                 = 1;
  WINDOW_CAPTION_STATE_INACTIVE               = 2;
  WINDOW_CAPTION_STATE_DISABLED               = 3;

//----------------------------------------------------------------------------------------------------------------------
//   "Button"
//----------------------------------------------------------------------------------------------------------------------

  BUTTON_PART_PUSHBUTTON                      = 1;
  BUTTON_PART_RADIOBUTTON                     = 2;
  BUTTON_PART_CHECKBOX                        = 3;
  BUTTON_PART_GROUPBOX                        = 4;

  PUSHBUTTON_STATE_NORMAL                     = 1;
  PUSHBUTTON_STATE_HOT                        = 2;
  PUSHBUTTON_STATE_PRESSED                    = 3;
  PUSHBUTTON_STATE_DISABLED                   = 4;

  BUTTON_RADIO_STATE_UNCHECKEDNORMAL    = 1;
  BUTTON_RADIO_STATE_UNCHECKEDHOT       = 2;
  BUTTON_RADIO_STATE_UNCHECKEDPRESSED   = 3;
  BUTTON_RADIO_STATE_UNCHECKEDDISABLED  = 4;
  BUTTON_RADIO_STATE_CHECKEDNORMAL      = 5;
  BUTTON_RADIO_STATE_CHECKEDHOT         = 6;
  BUTTON_RADIO_STATE_CHECKEDPRESSED     = 7;
  BUTTON_RADIO_STATE_CHECKEDDISABLED    = 8;

  BUTTON_CHECK_STATE_UNCHECKEDNORMAL       = 1;
  BUTTON_CHECK_STATE_UNCHECKEDHOT          = 2;
  BUTTON_CHECK_STATE_UNCHECKEDPRESSED      = 3;
  BUTTON_CHECK_STATE_UNCHECKEDDISABLED     = 4;
  BUTTON_CHECK_STATE_CHECKEDNORMAL         = 5;
  BUTTON_CHECK_STATE_CHECKEDHOT            = 6;
  BUTTON_CHECK_STATE_CHECKEDPRESSED        = 7;
  BUTTON_CHECK_STATE_CHECKEDDISABLED       = 8;
  BUTTON_CHECK_STATE_MIXEDNORMAL           = 9;
  BUTTON_CHECK_STATE_MIXEDHOT              = 10;
  BUTTON_CHECK_STATE_MIXEDPRESSED          = 11;
  BUTTON_CHECK_STATE_MIXEDDISABLED         = 12;

  BUTTON_GROUPBOX_STATE_NORMAL                = 1;
  BUTTON_GROUPBOX_STATE_DISABLED              = 2;

//----------------------------------------------------------------------------------------------------------------------
//   "Rebar"
//----------------------------------------------------------------------------------------------------------------------

  REBAR_PART_GRIPPER                          = 1;
  REBAR_PART_GRIPPERVERT                      = 2;
  REBAR_PART_BAND                             = 3;
  REBAR_PART_CHEVRON                          = 4;
  REBAR_PART_CHEVRONVERT                      = 5;

  REBAR_CHEVRON_STATE_NORMAL                  = 1;
  REBAR_CHEVRON_STATE_HOT                     = 2;
  REBAR_CHEVRON_STATE_PRESSED                 = 3;

//----------------------------------------------------------------------------------------------------------------------
//   "Toolbar"
//----------------------------------------------------------------------------------------------------------------------

  TOOLBAR_PART_BUTTON                         = 1;
  TOOLBAR_PART_DROPDOWNBUTTON                 = 2;
  TOOLBAR_PART_SPLITBUTTON                    = 3;
  TOOLBAR_PART_SPLITBUTTONDROPDOWN            = 4;
  TOOLBAR_PART_SEPARATOR                      = 5;
  TOOLBAR_PART_SEPARATORVERT                  = 6;

  TOOLBAR_STATE_NORMAL                        = 1;
  TOOLBAR_STATE_HOT                           = 2;
  TOOLBAR_STATE_PRESSED                       = 3;
  TOOLBAR_STATE_DISABLED                      = 4;
  TOOLBAR_STATE_CHECKED                       = 5;
  TOOLBAR_STATE_HOTCHECKED                    = 6;

//----------------------------------------------------------------------------------------------------------------------
//   "Status"
//----------------------------------------------------------------------------------------------------------------------

  STATUS_PART_PANE                            = 1;
  STATUS_PART_GRIPPERPANE                     = 2;
  STATUS_PART_GRIPPER                         = 3;

//----------------------------------------------------------------------------------------------------------------------
//   "Menu"
//----------------------------------------------------------------------------------------------------------------------

  MENU_PART_MENUITEM                          = 1;
  MENU_PART_MENUDROPDOWN                      = 2;
  MENU_PART_MENUBARITEM                       = 3;
  MENU_PART_MENUBARDROPDOWN                   = 4;
  MENU_PART_CHEVRON                           = 5;
  MENU_PART_SEPARATOR                         = 6;

  MENU_STATE_NORMAL                           = 1;
  MENU_STATE_SELECTED                         = 2;
  MENU_STATE_DEMOTED                          = 3;

//----------------------------------------------------------------------------------------------------------------------
//   "ListView"
//----------------------------------------------------------------------------------------------------------------------

  LISTVIEW_PART_LISTITEM                      = 1;
  LISTVIEW_PART_LISTGROUP                     = 2;
  LISTVIEW_PART_LISTDETAIL                    = 3;
  LISTVIEW_PART_LISTSORTEDDETAIL              = 4;
  LISTVIEW_PART_EMPTYTEXT                     = 5;

  LISTVIEW_ITEM_STATE_NORMAL                   = 1;
  LISTVIEW_ITEM_STATE_HOT                      = 2;
  LISTVIEW_ITEM_STATE_SELECTED                 = 3;
  LISTVIEW_ITEM_STATE_DISABLED                 = 4;
  LISTVIEW_ITEM_STATE_SELECTEDNOTFOCUS         = 5;

//----------------------------------------------------------------------------------------------------------------------
//   "Header"
//----------------------------------------------------------------------------------------------------------------------

  HEADER_PART_HEADERITEM                      = 1;
  HEADER_PART_HEADERITEMLEFT                  = 2;
  HEADER_PART_HEADERITEMRIGHT                 = 3;
  HEADER_PART_HEADERSORTARROW                 = 4;

  HEADER_ITEM_STATE_NORMAL                    = 1;
  HEADER_ITEM_STATE_HOT                       = 2;
  HEADER_ITEM_STATE_PRESSED                   = 3;

  HEADER_SORTARROW_STATE_SORTEDUP             = 1;
  HEADER_SORTARROW_STATE_SORTEDDOWN           = 2;

//----------------------------------------------------------------------------------------------------------------------
//   "Progress"
//----------------------------------------------------------------------------------------------------------------------

  PROGRESS_PART_BAR                           = 1;
  PROGRESS_PART_BARVERT                       = 2;
  PROGRESS_PART_CHUNK                         = 3;
  PROGRESS_PART_CHUNKVERT                     = 4;

//----------------------------------------------------------------------------------------------------------------------
//   "Tab"
//----------------------------------------------------------------------------------------------------------------------

  TAB_PART_TABITEM                            = 1;
  TAB_PART_TABITEMLEFTEDGE                    = 2;
  TAB_PART_TABITEMRIGHTEDGE                   = 3;
  TAB_PART_TABITEMBOTHEDGE                    = 4;
  TAB_PART_TOPTABITEM                         = 5;
  TAB_PART_TOPTABITEMLEFTEDGE                 = 6;
  TAB_PART_TOPTABITEMRIGHTEDGE                = 7;
  TAB_PART_TOPTABITEMBOTHEDGE                 = 8;
  TAB_PART_PANE                               = 9;
  TAB_PART_BODY                               = 10;

  TAB_STATE_NORMAL                            = 1;
  TAB_STATE_HOT                               = 2;
  TAB_STATE_SELECTED                          = 3;
  TAB_STATE_DISABLED                          = 4;
  TAB_STATE_FOCUSED                           = 5;

//----------------------------------------------------------------------------------------------------------------------
//   "Trackbar"
//----------------------------------------------------------------------------------------------------------------------

  TRACKBAR_PART_TRACK                         = 1;
  TRACKBAR_PART_TRACKVERT                     = 2;
  TRACKBAR_PART_THUMB                         = 3;
  TRACKBAR_PART_THUMBBOTTOM                   = 4;
  TRACKBAR_PART_THUMBTOP                      = 5;
  TRACKBAR_PART_THUMBVERT                     = 6;
  TRACKBAR_PART_THUMBLEFT                     = 7;
  TRACKBAR_PART_THUMBRIGHT                    = 8;
  TRACKBAR_PART_TICS                          = 9;
  TRACKBAR_PART_TICSVERT                      = 10;

  TRACKBAR_TICK_STATE_NORMAL                  = 1;

  TRACKBAR_TRACK_STATE_NORMAL                 = 1;

  TRACKBAR_THUMB_STATE_NORMAL                 = 1;
  TRACKBAR_THUMB_STATE_HOT                    = 2;
  TRACKBAR_THUMB_STATE_PRESSED                = 3;
  TRACKBAR_THUMB_STATE_FOCUSED                = 4;
  TRACKBAR_THUMB_STATE_DISABLED               = 5;

//----------------------------------------------------------------------------------------------------------------------
//   "Tooltips"
//----------------------------------------------------------------------------------------------------------------------

  TOOLTIP_PART_STANDARD                       = 1;
  TOOLTIP_PART_STANDARDTITLE                  = 2;
  TOOLTIP_PART_BALLOON                        = 3;
  TOOLTIP_PART_BALLOONTITLE                   = 4;
  TOOLTIP_PART_CLOSE                          = 5;

  TOOLTIP_CLOSE_STATE_NORMAL                  = 1;
  TOOLTIP_CLOSE_STATE_HOT                     = 2;
  TOOLTIP_CLOSE_STATE_PRESSED                 = 3;

  TOOLTIP_BALLOON_STATE_NORMAL                = 1;
  TOOLTIP_BALLOON_STATE_LINK                  = 2;

  TOOLTIP_STANDARD_STATE_NORMAL               = 1;
  TOOLTIP_STANDARD_STATE_LINK                 = 2;

//----------------------------------------------------------------------------------------------------------------------
//   "TreeView"
//----------------------------------------------------------------------------------------------------------------------

  TREEVIEW_PART_TREEITEM                       = 1;
  TREEVIEW_PART_GLYPH                          = 2;
  TREEVIEW_PART_BRANCH                         = 3;

  TREEVIEW_TREEITEM_STATE_NORMAL               = 1;
  TREEVIEW_TREEITEM_STATE_HOT                  = 2;
  TREEVIEW_TREEITEM_STATE_SELECTED             = 3;
  TREEVIEW_TREEITEM_STATE_DISABLED             = 4;
  TREEVIEW_TREEITEM_STATE_SELECTEDNOTFOCUS     = 5;

  TREEVIEW_GLYPH_STATE_CLOSED                  = 1;
  TREEVIEW_GLYPH_STATE_OPENED                  = 2;

//----------------------------------------------------------------------------------------------------------------------
//   "Spin"
//----------------------------------------------------------------------------------------------------------------------

  SPIN_PART_UP                                 = 1;
  SPIN_PART_DOWN                               = 2;
  SPIN_PART_UPHORZ                             = 3;
  SPIN_PART_DOWNHORZ                           = 4;

  SPIN_STATE_NORMAL                            = 1;
  SPIN_STATE_HOT                               = 2;
  SPIN_STATE_PRESSED                           = 3;
  SPIN_STATE_DISABLED                          = 4;

//----------------------------------------------------------------------------------------------------------------------
//   "Page"
//----------------------------------------------------------------------------------------------------------------------

  PAGE_PART_UP                                 = 1;
  PAGE_PART_DOWN                               = 2;
  PAGE_PART_UPHORZ                             = 3;
  PAGE_PART_DOWNHORZ                           = 4;

//----------------------------------------------------------------------------------------------------------------------
//   "Scrollbar"
//----------------------------------------------------------------------------------------------------------------------

  SCROLLBAR_PART_ARROWBTN                      = 1;
  SCROLLBAR_PART_THUMBBTNHORZ                  = 2;
  SCROLLBAR_PART_THUMBBTNVERT                  = 3;
  SCROLLBAR_PART_LOWERTRACKHORZ                = 4;
  SCROLLBAR_PART_UPPERTRACKHORZ                = 5;
  SCROLLBAR_PART_LOWERTRACKVERT                = 6;
  SCROLLBAR_PART_UPPERTRACKVERT                = 7;
  SCROLLBAR_PART_GRIPPERHORZ                   = 8;
  SCROLLBAR_PART_GRIPPERVERT                   = 9;
  SCROLLBAR_PART_SIZEBOX                       = 10;

  SCROLLBAR_ARROWBTN_STATE_UPNORMAL            = 1;
  SCROLLBAR_ARROWBTN_STATE_UPHOT               = 2;
  SCROLLBAR_ARROWBTN_STATE_UPPRESSED           = 3;
  SCROLLBAR_ARROWBTN_STATE_UPDISABLED          = 4;
  SCROLLBAR_ARROWBTN_STATE_DOWNNORMAL          = 5;
  SCROLLBAR_ARROWBTN_STATE_DOWNHOT             = 6;
  SCROLLBAR_ARROWBTN_STATE_DOWNPRESSED         = 7;
  SCROLLBAR_ARROWBTN_STATE_DOWNDISABLED        = 8;
  SCROLLBAR_ARROWBTN_STATE_LEFTNORMAL          = 9;
  SCROLLBAR_ARROWBTN_STATE_LEFTHOT             = 10;
  SCROLLBAR_ARROWBTN_STATE_LEFTPRESSED         = 11;
  SCROLLBAR_ARROWBTN_STATE_LEFTDISABLED        = 12;
  SCROLLBAR_ARROWBTN_STATE_RIGHTNORMAL         = 13;
  SCROLLBAR_ARROWBTN_STATE_RIGHTHOT            = 14;
  SCROLLBAR_ARROWBTN_STATE_RIGHTPRESSED        = 15;
  SCROLLBAR_ARROWBTN_STATE_RIGHTDISABLED       = 16;

  SCROLLBAR_BUTTON_STATE_NORMAL                = 1;
  SCROLLBAR_BUTTON_STATE_HOT                   = 2;
  SCROLLBAR_BUTTON_STATE_PRESSED               = 3;
  SCROLLBAR_BUTTON_STATE_DISABLED              = 4;

  SCROLLBAR_SIZEBOX_STATE_RIGHTALIGN           = 1;
  SCROLLBAR_SIZEBOX_STATE_LEFTALIGN            = 2;

//----------------------------------------------------------------------------------------------------------------------
//   "Edit"
//----------------------------------------------------------------------------------------------------------------------

  EDIT_PART_EDITTEXT                           = 1;
  EDIT_PART_CARET                              = 2;

  EDIT_STATE_NORMAL                            = 1;
  EDIT_STATE_HOT                               = 2;
  EDIT_STATE_SELECTED                          = 3;
  EDIT_STATE_DISABLED                          = 4;
  EDIT_STATE_FOCUSED                           = 5;
  EDIT_STATE_READONLY                          = 6;
  EDIT_STATE_ASSIST                            = 7;

//----------------------------------------------------------------------------------------------------------------------
//   "ComboBox"
//----------------------------------------------------------------------------------------------------------------------

  COMBOBOX_PART_DROPDOWNBUTTON                 = 1;

  COMBOBOX_STATE_NORMAL                        = 1;
  COMBOBOX_STATE_HOT                           = 2;
  COMBOBOX_STATE_PRESSED                       = 3;
  COMBOBOX_STATE_DISABLED                      = 4;

//----------------------------------------------------------------------------------------------------------------------
//   "Taskbar Clock"
//----------------------------------------------------------------------------------------------------------------------

  CLOCK_PART_TIME                              = 1;

  CLOCK_STATE_NORMAL                           = 1;

//----------------------------------------------------------------------------------------------------------------------
//   "Tray Notify"
//----------------------------------------------------------------------------------------------------------------------

  TRAY_PART_BACKGROUND                         = 1;
  TRAY_PART_ANIMBACKGROUND                     = 2;

//----------------------------------------------------------------------------------------------------------------------
//   "TaskBar"
//----------------------------------------------------------------------------------------------------------------------

  TASKBAR_PART_BACKGROUNDBOTTOM                = 1;
  TASKBAR_PART_BACKGROUNDRIGHT                = 2;
  TASKBAR_PART_BACKGROUNDTOP                  = 3;
  TASKBAR_PART_BACKGROUNDLEFT                 = 4;
  TASKBAR_PART_SIZINGBARBOTTOM                = 5;
  TASKBAR_PART_SIZINGBARRIGHT                 = 6;
  TASKBAR_PART_SIZINGBARTOP                   = 7;
  TASKBAR_PART_SIZINGBARLEFT                  = 8;

//----------------------------------------------------------------------------------------------------------------------
//   "TaskBand"
//----------------------------------------------------------------------------------------------------------------------

  TASKBAND_PART_GROUPCOUNT                    = 1;
  TASKBAND_PART_FLASHBUTTON                   = 2;
  TASKBAND_PART_FLASHBUTTONGROUPMENU          = 3;

//----------------------------------------------------------------------------------------------------------------------
//   "StartPanel"
//----------------------------------------------------------------------------------------------------------------------

  STARTPANEL_PART_USERPANE                    = 1;
  STARTPANEL_PART_MOREPROGRAMS                = 2;
  STARTPANEL_PART_MOREPROGRAMSARROW           = 3;
  STARTPANEL_PART_PROGLIST                    = 4;
  STARTPANEL_PART_PROGLISTSEPARATOR           = 5;
  STARTPANEL_PART_PLACESLIST                  = 6;
  STARTPANEL_PART_PLACESLISTSEPARATOR         = 7;
  STARTPANEL_PART_LOGOFF                      = 8;
  STARTPANEL_PART_LOGOFFBUTTONS               = 9;
  STARTPANEL_PART_USERPICTURE                 = 10;
  STARTPANEL_PART_PREVIEW                     = 11;

  STARTPANEL_STATE_NORMAL                     = 1;
  STARTPANEL_STATE_HOT                        = 2;
  STARTPANEL_STATE_PRESSED                    = 3;

//----------------------------------------------------------------------------------------------------------------------
//   "ExplorerBar"
//----------------------------------------------------------------------------------------------------------------------

  EXPLORERBAR_PART_HEADERBACKGROUND           = 1;
  EXPLORERBAR_PART_HEADERCLOSE                = 2;
  EXPLORERBAR_PART_HEADERPIN                  = 3;
  EXPLORERBAR_PART_IEBARMENU                  = 4;
  EXPLORERBAR_PART_NORMALGROUPBACKGROUND      = 5;
  EXPLORERBAR_PART_NORMALGROUPCOLLAPSE        = 6;
  EXPLORERBAR_PART_NORMALGROUPEXPAND          = 7;
  EXPLORERBAR_PART_NORMALGROUPHEAD            = 8;
  EXPLORERBAR_PART_SPECIALGROUPBACKGROUND     = 9;
  EXPLORERBAR_PART_SPECIALGROUPCOLLAPSE       = 10;
  EXPLORERBAR_PART_SPECIALGROUPEXPAND         = 11;
  EXPLORERBAR_PART_SPECIALGROUPHEAD           = 12;

  EXPLORERBAR_GROUP_STATE_NORMAL              = 1;
  EXPLORERBAR_GROUP_STATE_HOT                 = 2;
  EXPLORERBAR_GROUP_STATE_PRESSED             = 3;

  EXPLORERBAR_HEADERCLOSE_STATE_NORMAL        = 1;
  EXPLORERBAR_HEADERCLOSE_STATE_HOT           = 2;
  EXPLORERBAR_HEADERCLOSE_STATE_PRESSED       = 3;

  EXPLORERBAR_IEBARMENU_STATE_NORMAL        = 1;
  EXPLORERBAR_IEBARMENU_STATE_HOT           = 2;
  EXPLORERBAR_IEBARMENU_STATE_PRESSED       = 3;

  EXPLORERBAR_HEADERPIN_STATE_NORMAL          = 1;
  EXPLORERBAR_HEADERPIN_STATE_HOT             = 2;
  EXPLORERBAR_HEADERPIN_STATE_PRESSED         = 3;
  EXPLORERBAR_HEADERPIN_STATE_SELECTEDNORMAL  = 4;
  EXPLORERBAR_HEADERPIN_STATE_SELECTEDHOT     = 5;
  EXPLORERBAR_HEADERPIN_STATE_SELECTEDPRESSED = 6;

{------------------------------------------------------------------}

type
  IPSCTypeConvert=interface(IPSCInterface)
    ['{AC9103CD-3CC7-4EFB-B922-CBF53F2FEFA2}']
    function StrToInt(const V:String):Integer;
    function StrToDateTime(const V:String):TDateTime;
    function StrToDate(const V:String):TDateTime;
    function StrToTime(const V:String):TDateTime;
    function StrToCurrency(const V:String):Currency;
    function StrToDouble(const V:String):Double;
    function StrToBoolean(const V:String):Boolean;

    function IntToStr(V:Integer):String;
    function DateTimeToStr(const V:TDateTime):String;
    function DateToStr(const V:TDateTime):String;
    function TimeToStr(const V:TDateTime):String;
    function CurrencyToStr(const V:Currency):String;
    function DoubleToStr(const V:Double):String;
    function BooleanToStr(V:Boolean):String;
  end;

  TPSCVariantRec=record
    AsObject:TObject;
    AsInterface:IPSCInterface;
    AsBoolean:Boolean;
    AsCurrency:Currency;
    AsDateTime:TDateTime;
    AsDouble:Double;
    AsInteger:Integer;
    //AsString:AnsiString;
    AsString:String;
    AsWideStr:WideString;
    HasValue:Boolean;
    DataType:TPSCFieldType;
  end;

{------------------------------------------------------------------}

implementation

{------------------------------------------------------------------}

end.
