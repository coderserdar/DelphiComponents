unit __SRCal;

{ TSRCalendar (C)opyright 2002 Version 2.00
  Autor : Simon Reinhardt
  eMail : reinhardt@picsoft.de
  Internet : http://www.picsoft.de

  Die Komponente TSRCalendar ist eine Weiterentwicklung der
  TCalendar-Komponente aus den Beispielkomponenten der Delphi-VCL.
  Sie enthält viele Zusatzinformationen, wie Feiertage,
  Sternzeichen und verschiedene astronomische Daten.

  Die Routinen aus der Unit TimeFunc stammen aus der TMoon-Komponente
  von Andreas Hörstemeier : http://www.hoerstemeier.com
  Andreas hat die Routinen aus dem Buch "Astronomical Algorithms" von Jean Meeus.

  Die GetWeekOfYear-Funktion, die die Wochennummer nach DIN 1355 ermittelt,
  stammt von Christoph Kremer, Aachen.

  Vielen Dank auch an:
  - Edmund Matzke <edmund_matzke@gmx.de> für die Korrektur der
    Schleswig-Holsteinischen Feiertage,
  - Ralph Kerzig <ralph.kerzig@t-online.de> für die Korrektur der
    sächsischen Feiertage,
  - Matthias Frey <info@Matthias-Frey.de> für die Korrektur der
    Advents-Berechnung.

  Diese Komponente ist Public Domain, das Urheberrecht liegt aber beim Autor. }


interface


uses Windows, Classes, Controls, Messages, Forms, Graphics, StdCtrls, Grids, SysUtils;

const
  { deutsche Bezeichnungen: }
  Bundesland : array [0..16] of string[25] =
   ('Baden-Württemberg', 'Bayern', 'Berlin', 'Brandenburg', 'Bremen', 'Hamburg',
    'Hessen', 'Mecklenburg-Vorpommern', 'Niedersachsen', 'Nordrhein-Westfalen',
    'Rheinland-Pfalz', 'Saarland', 'Sachsen', 'Sachsen-Anhalt', 'Schleswig-Holstein',
    'Thüringen', '');
  Feiertag : array [1..19] of string[25] =
   ('Neujahr', 'Maifeiertag', 'Tag der deutschen Einheit', 'Allerheiligen',
    'Totensonntag', 'Volkstrauertag', '1. Weihnachtstag', '2. Weihnachtstag',
    'Karfreitag', 'Ostersonntag', 'Ostermontag', 'Christi Himmelfahrt',
    'Pfingstsonntag', 'Pfingstmontag', 'Fronleichnam', 'Heilige 3 Könige',
    'Mariä Himmelfahrt', 'Reformationstag', 'Buß- und Bettag');
  Jahreszeit : array [0..4] of string[25] =
    ('Winter','Frühling', 'Sommer', 'Herbst', '');
  Mondphase : array [0..4] of string[25] =
    ('Neumond','Zunehmender Mond', 'Vollmond', 'Abnehmender Mond', '');
  Sondertag : array [1..24] of string[25] =
   ('Mariä Lichtmeß', 'Valentinstag', 'Weiberfastnacht', 'Rosenmontag', 'Fastnacht',
    'Aschermittwoch', 'Mariä Verkündigung', 'Palmsonntag', 'Gründonnerstag', 'Muttertag',
    'Peter und Paul', 'Mariä Geburt', 'Erntedankfest', 'Mariä Empfängnis', 'Silvester',
    '1. Advent', '2. Advent', '3. Advent', '4. Advent', 'Heiligabend', 'Frühlingsanfang',
    'Sommmeranfang', 'Herbstanfang', 'Winteranfang');
  Sternzeichen : array [0..12] of string[10] =
   ('Wassermann', 'Fische', 'Widder', 'Stier', 'Zwilling', 'Krebs', 'Löwe', 'Jungfrau',
    'Waage', 'Skorpion', 'Schütze', 'Steinbock', '');

  { geometrische Längen- und Breitengrade der Bundesländer: }
  GermanStateLong : array [0..15] of extended =
   (-9, -11.5, -13.4, -13.4, -8.8, -10, -8.7, -12.2, -8.8, -7.5, -7.3, -7, -14, -11.7, -10.2, -11);
  GermanStateLat : array [0..15] of extended =
   (48.6, 48.8, 52.5, 52.5, 53.1, 53.5, 50.5, 53.7, 53.1, 51.6, 50.2, 49.2, 51, 52, 54.3, 51);

type
  TGermanState =
   (gsBaden_Wuerttemberg, gsBayern, gsBerlin, gsBrandenburg, gsBremen, gsHamburg,
    gsHessen, gsMecklenburg_Vorpommern, gsNiedersachsen, gsNordrhein_Westfalen,
    gsRheinland_Pfalz, gsSaarland, gsSachsen, gsSachsen_Anhalt, gsSchleswig_Holstein,
    gsThueringen, gsNone);
  TCalendarDrawStyle = (cdsColorGrid, cdsMonoGrid, cdsButtons);
  TCalendarOption = (coAutoDeleteMarks, coCalcAstroData, coCalcHolidays,
                     coGridLines, coReadOnly, coFrameSelection, coShowMarks,
                     coUseCurrentDate);
  TCalendarOptions = set of TCalendarOption;
  TDayOfWeek = (dowSunday, dowMonday, dowTuesday, dowWednesday,
                dowThursday, dowFriday, dowSaturday);
  TEclipse=(ecNone, ecPartial, ecNoncentral,
            ecCircular, ecCirculartotal, ecTotal, ecHalfshadow);
  THolidays = array [1..31] of integer;
  TMarked = array [1..31] of boolean;
  TMoonPhase = (mpNewmoon, mpFirstQuarter, mpFullmoon, mpLastQuarter);
  TSeason = (seWinter, seSpring, seSummer, seAutumn, seNone);
  TZodiacSign = (zsAquarius, zsPisces, zsAries, zsTaurus, zsGemini, zsCancer, zsLeo,
                 zsVirgo, zsLibra, zsScorpio, zsSagittarius, zsCapricorn, zsNone);

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

  TSRCalendar = class(TCustomGrid)
  private
    FAutumnDate          : longint;
    FBackgroundColors    : TCalendarColors;
    FGermanState         : TGermanState;
    FCalendarOptions     : TCalendarOptions;
    FDate                : TDateTime;
    FDayOfYear,
    FDaysThisMonth       : word;
    FDrawStyle           : TCalendarDrawStyle;
    FHoliday             : string;
    FHolidayNr           : integer;
    FHolidays            : THolidays;
    FMarked              : TMarked;
    FMonthOffset         : integer;
    FMoonDistance        : extended;
    FMoonPhase           : TMoonPhase;
    FMoonRise,
    FMoonSet,
    FMoonTransit         : TDateTime;
    FSaturdayAsSunday    : boolean;
    FSeason              : TSeason;
    FSpringDate          : longint;
    FStartOfWeek         : TDayOfWeek;
    FSummerDate          : longint;
    FSunDistance         : extended;
    FSunRise,
    FSunSet,
    FSunTransit          : TDateTime;
    FTextColors          : TCalendarColors;
    FUpdating            : Boolean;
    FWeekOfYear          : word;
    FWinterDate          : longint;
    FZodiacSign          : TZodiacSign;

    FOnBeforeChange,
    FOnChange,
    FOnMonthChange,
    FOnYearChange        : TNotifyEvent;
    FLongitude: single;
    FLatitude: single;


    function GetCellText(ACol,ARow: Integer): string;
    function GetDateElement(const Index: Integer): Integer;
    function GetHolidays(Index: integer): integer;
    function GetMarked(Index: integer): boolean;
    procedure GetMoonData(const Dat:TDateTime);
    function GetSeason(Dat:TDateTime):TSeason;
    procedure GetSunData(const Dat:TDateTime);
    function GetZodiacSign(const Dat:TDateTime):TZodiacSign;
    procedure SetBackgroundColors(const newValue: TCalendarColors);
    procedure SetCalendarOptions(const newValue: TCalendarOptions);
    procedure SetDate(const newValue: TDateTime);
    procedure SetDateElement(const Index: Integer; const newValue: Integer);
    procedure SetDrawStyle(const newValue: TCalendarDrawStyle);
    procedure SetGermanState(const NewValue: TGermanState);
    procedure SetHolidays(Index: integer; const newValue: integer);
    procedure SetLatitude(const newValue: single);
    procedure SetLongitude(const newValue: single);
    procedure SetMarked(Index: integer; const newValue: boolean);
    procedure SetSaturdayAsSunday(const newValue: boolean);
    procedure SetStartOfWeek(const newValue: TDayOfWeek);
    procedure SetTextColors(const newValue: TCalendarColors);
    function StoreDate: Boolean;
    function GetDaysPerMonth(AYear,AMonth:integer):Integer;
    function GetDayOfYear(ADate:TDateTime):Word;
    function IsSummertime(ADate:TDateTime):Boolean;
    function GetWeekOfYear(ADate:TDateTime):Byte;
  protected
    procedure BeforeChange; dynamic;
    procedure Change; dynamic;
    procedure ChangeMonth(Delta: Integer);
    procedure Click; override;
    procedure DrawButton(ACanvas:TCanvas;ARect:TRect;Pushed:boolean);
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    function GetDaysThisMonth: Integer; virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MonthChange; dynamic;
    function SelectCell(ACol, ARow: Longint): Boolean; override;
    procedure YearChange; dynamic;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property CellText[ACol, ARow: Integer]: string read GetCellText;
    property Date: TDateTime read FDate write SetDate stored StoreDate;
    property DayOfYear: word read FDayOfYear;
    property DaysThisMonth: word read FDaysThisMonth;
    property Holiday: string read FHoliday;
    property HolidayNr: integer read FHolidayNr;
    property Holidays[Index: integer]: integer read GetHolidays write SetHolidays;
    property Marked[Index: integer]: boolean read GetMarked write SetMarked;
    property MoonDistance: extended read FMoonDistance;
    property MoonPhase: TMoonPhase read FMoonPhase;
    property MoonRise: TDateTime read FMoonRise;
    property MoonSet: TDateTime read FMoonSet;
    property MoonTransit: TDateTime read FMoonTransit;
    property Season: TSeason read FSeason;
    property SunDistance: extended read FSunDistance;
    property SunRise: TDateTime read FSunRise;
    property SunSet: TDateTime read FSunSet;
    property SunTransit: TDateTime read FSunTransit;
    property WeekOfYear: word read FWeekOfYear;
    property ZodiacSign: TZodiacSign read FZodiacSign;

    function GetHoliday(WhatDate:TDateTime;Land:integer):integer;
    procedure MouseToCell(X, Y: Integer; var ACol, ARow: Longint);
    function MouseToDate(X, Y: Integer):TDateTime;
    procedure NextMonth;
    procedure NextYear;
    procedure PrevMonth;
    procedure PrevYear;
    procedure UpdateCalendar; virtual;

  published
    property Align;
    property Anchors;
    property BackgroundColors: TCalendarColors read FBackgroundColors write SetBackgroundColors;
    property BorderStyle;
    property GermanState: TGermanState read FGermanState write SetGermanState;
    property CalendarOptions: TCalendarOptions read FCalendarOptions write SetCalendarOptions;
    property Ctl3D;
    property Day: Integer index 3 read GetDateElement write SetDateElement stored False;
    property DrawStyle: TCalendarDrawStyle read FDrawStyle write SetDrawStyle;
    property Enabled;
    property Font;
    property Latitude: single read FLatitude write SetLatitude;
    property Longitude: single read FLongitude write SetLongitude;
    property Month: Integer index 2 read GetDateElement write SetDateElement stored False;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property SaturdayAsSunday: boolean read FSaturdayAsSunday write SetSaturdayAsSunday;
    property StartOfWeek: TDayOfWeek read FStartOfWeek write SetStartOfWeek;
    property TabOrder;
    property TabStop;
    property TextColors: TCalendarColors read FTextColors write SetTextColors;
    property Visible;
    property Year: Integer index 1  read GetDateElement write SetDateElement stored False;

    property OnBeforeChange: TNotifyEvent read FOnBeforeChange write FOnBeforeChange;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEndDock;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMonthChange: TNotifyEvent read FOnMonthChange write FOnMonthChange;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property OnYearChange: TNotifyEvent read FOnYearChange write FOnYearChange;
  end;


implementation


uses __TimeFunc;

const
  DefaultWidth  = 192;
  DefaultHeight = 115;
  AU            = 149597869;

var
  FirstWeekDay  : Integer = 2;  { Wochentag, mit dem die Woche beginnt
                                (siehe Delphi-Wochentage)
                                2 : Montag (nach DIN 1355) }
  FirstWeekDate : Integer = 4;  { 1 : Beginnt am ersten Januar
                                4 : Erste-4 Tage-Woche (nach DIN 1355)
                                7 : Erste volle Woche }

{ Komponente TSRCalendar }
constructor TSRCalendar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBackgroundColors := TCalendarColors.Create;
  FTextColors := TCalendarColors.Create;

  { defaults }
  ColCount := 7;
  GridLineWidth := 1;
{  Font.Style:=[];
  Font.Color:=clWindowText;
  Canvas.Font.Assign(Font);}
  with FBackgroundColors do begin
    Headers := clBtnFace;
    Holiday := clWindow;
    Marked := clAqua;
    Selected := clHighlight;
    Standard := clWindow;
    Today := clWindow;
    Weekend := clWindow;
  end;
  FGermanState:=gsNordrhein_Westfalen;
  FLatitude:=GermanStateLat[ord(gsNordrhein_Westfalen)];
  FLongitude:=GermanStateLong[ord(gsNordrhein_Westfalen)];
  FDrawStyle := cdsColorGrid;
  FixedCols := 0;
  FixedRows := 1;
  FCalendarOptions := [coAutoDeleteMarks, coCalcAstroData, coCalcHolidays,
                       coGridLines, coFrameSelection, coShowMarks, coUseCurrentDate];
  with FTextColors do begin
    Headers := clBtnText;
    Holiday := clRed;
    Marked := clWindowText;
    Selected := clHighlightText;
    Standard := clWindowText;
    Today := clBlue;
    Weekend := clMaroon;
  end;
  DefaultDrawing := true;
  Height := DefaultHeight;
  Options := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goDrawFocusSelected];
  RowCount := 7;
  ScrollBars := ssNone;
  Width := DefaultWidth;
  FDate := Now;
  YearChange;
  UpdateCalendar;
end;

destructor TSRCalendar.Destroy;
begin
  FBackgroundColors:=TCalendarColors.Create;
  FTextColors:=TCalendarColors.Create;
  inherited Destroy;
end;

procedure TSRCalendar.BeforeChange;
begin
  if Assigned(FOnBeforeChange) then
    FOnBeforeChange(Self);
end;

procedure TSRCalendar.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TSRCalendar.ChangeMonth(Delta: Integer);
var
  AYear,
  AMonth,
  ADay    : Word;
  CurDay  : Integer;
  NewDate : TDateTime;
begin
  BeforeChange;
  try
    DecodeDate(FDate, AYear, AMonth, ADay);
    CurDay := ADay;
    if Delta > 0 then
      ADay := GetDaysPerMonth(AYear, AMonth)
    else
      ADay := 1;
    NewDate := EncodeDate(AYear, AMonth, ADay);
    NewDate := NewDate + Delta;
    DecodeDate(NewDate, AYear, AMonth, ADay);
    if GetDaysPerMonth(AYear, AMonth) > CurDay then
      ADay := CurDay
    else
      ADay := GetDaysPerMonth(AYear, AMonth);
    Date := EncodeDate(AYear, AMonth, ADay)+Time;
    MonthChange;
  except
  end;
end;

procedure TSRCalendar.Click;
var
  TheCellText: string;
begin
  inherited Click;
  TheCellText := CellText[Col, Row];
  if TheCellText <> '' then begin
    try
      Day := StrToInt(TheCellText);
    except
    end;
  end;
end;

procedure TSRCalendar.DrawButton(ACanvas:TCanvas;ARect:TRect;Pushed:boolean);
begin

  if Pushed then
    DrawFrameControl(ACanvas.Handle,
                     ARect,
                     DFC_Button,
                     DFCS_ButtonPush or DFCS_Pushed)
  else
    DrawFrameControl(ACanvas.Handle,
                     ARect,
                     DFC_Button,
                     DFCS_ButtonPush);

end;

procedure TSRCalendar.DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
var
  TheText    : string;
  IsWE,
  DoDrawRect : boolean;
  DoW,
  CellDay    : integer;
  CellDate   : TDateTime;
begin
  TheText:=CellText[ACol, ARow];
  with Canvas do begin
    Font.Style:=[];
    if DrawStyle<>cdsMonoGrid then begin
      CellDay:=0;
      CellDate:=0;
      DoW:=ACol+1+ord(FStartOfWeek);
      if DoW>7 then
        DoW:=DoW-7;
      if (TheText<>'') and (ARow>0) then begin
        try
          CellDay:=StrToInt(TheText);
          if CellDay>0 then
            CellDate:=EncodeDate(Year, Month, CellDay);
        except
        end;
      end;
      Brush.Color:=FBackgroundColors.Standard;
      Font.Color:=FTextColors.Standard;
      Font.Style:=[];
      if (DrawStyle=cdsButtons) or (ARow=0) then begin
        {Kalender im Button-Stil zeichnen}
        if (DoW=1) or ((DoW=7) and FSaturdayAsSunday) then
          Font.Color:=FTextColors.Weekend
        else
          Font.Color:=FTextColors.Headers;
        Brush.Color:=FBackgroundColors.Headers;
        if (ARow=0) or (trunc(CellDate)=trunc(Now)) then
          Font.Style:=[fsBold]
        else
          Font.Style:=[];
        FillRect(ARect);

        if (ARow>0) and (ACol=Col) and (ARow=Row) then
          DrawButton(Canvas, ARect, true)
        else
          DrawButton(Canvas, ARect, false);
        if (ARow>0) and (ACol=Col) and (ARow=Row) then begin
          Pen.Color:=FBackgroundColors.Selected;
          InflateRect(ARect, -1, -1);
          Rectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
        end;
        DoDrawRect:=false;
      end
      else
        DoDrawRect:=true;
      if DoDrawRect and (ACol=Col) and (ARow=Row) and (DrawStyle=cdsColorGrid) then begin
        {Farben für gewähltes Datum}
        if coFrameSelection in FCalendarOptions then begin
          InflateRect(ARect, -1, -1);
          Font.Color:=FTextColors.Standard;
          Brush.Color:=FBackgroundColors.Standard;
          Pen.Width:=2;
          Pen.Color:=FBackgroundColors.Selected;
          Rectangle(ARect.Left, ARect.Top, ARect.Right+1, ARect.Bottom+1);
          InflateRect(ARect, -1, -1);
        end
        else begin
          Font.Color:=FTextColors.Selected;
          Brush.Color:=FBackgroundColors.Selected;
          DoDrawRect:=false;
        end;
      end;
      IsWE:=(DoW=1) or ((DoW=7) and FSaturdayAsSunday);
      if DoDrawRect and IsWE and (CellDay>0) then begin
        {Farben für Wochenende}
        Font.Color:=FTextColors.Weekend;
        Brush.Color:=FBackgroundColors.Weekend;
      end;
      if trunc(CellDate)=trunc(Now) then begin
        {Farben für aktuelles Systemdatum}
        Font.Style:=[fsBold];
        if DoDrawRect then begin
          Font.Color:=FTextColors.Today;
          Brush.Color:=FBackgroundColors.Today;
        end;
      end;
      if CellDay>0 then begin
        if coCalcHolidays in FCalendarOptions then
          FHolidays[CellDay]:=GetHoliday(CellDate, ord(FGermanState))
        else
          FHolidays[CellDay]:=0;
        if FHolidays[CellDay]>0 then begin
          {Farben für Feiertage}
          Font.Style:=[fsBold];
          if DoDrawRect then begin
            Font.Color:=FTextColors.Holiday;
            Brush.Color:=FBackgroundColors.Holiday;
          end;
        end;
        if DoDrawRect and (coShowMarks in FCalendarOptions) and FMarked[Cellday] then begin
          {Farben für markierte Tage}
          Font.Color:=FTextColors.Marked;
          Brush.Color:=FBackgroundColors.Marked;
        end;
      end;
      if (DrawStyle<>cdsButtons) and (ARow>0) then
        FillRect(ARect);
    end;
    Brush.Style:=bsClear;

    DrawText(Handle,
             PChar(TheText),
             length(TheText),
             ARect,
             DT_SingleLine or DT_NoPrefix or DT_Center or DT_VCenter);
    Brush.Style:=bsSolid;

  end;
end;

function TSRCalendar.GetDaysThisMonth: Integer;
begin
  Result := GetDaysPerMonth(Year, Month);
end;

function TSRCalendar.GetCellText(ACol, ARow: Integer): string;
var DayNum: Integer;
begin
  if ARow = 0 then  { day names at tops of columns }
    Result := ShortDayNames[(ord(StartOfWeek) + ACol) mod 7 + 1]
  else begin
    DayNum := FMonthOffset + ACol + (ARow - 1) * 7;
    if (DayNum < 1) or (DayNum > GetDaysThisMonth) then
      Result := ''
    else begin
      try
        Result := IntToStr(DayNum);
      except
        Result:='';
      end;
    end;
  end;
end;

function TSRCalendar.GetDateElement(const Index: Integer): Integer;
var
  AYear, AMonth, ADay: Word;
begin
  DecodeDate(FDate, AYear, AMonth, ADay);
  case Index of
    1: Result := AYear;
    2: Result := AMonth;
    3: Result := ADay;
    else Result := -1;
  end;
end;

function TSRCalendar.GetHoliday(WhatDate:TDateTime;Land:integer):integer;
var DoY,Y,M    : word;
    D,dw,OM,aw : word;
    Dat        : TDateTime;
    Ostern     : TDateTime;
    Weihnacht  : TDateTime;

  function EasterSunday(Y:word):TDateTime;
  var a, b, c, d, e,
      Tag, Monat : integer;
  begin
    a:=y mod 19 ;
    b:=y mod 4;
    c:=y mod 7;
    d:=(19*a+24) mod 30;
    e:=(2*b+4*c+6*d+5) mod 7;
    Tag:=22+d+e;
    Monat:=3;
    if Tag>31 then begin
      Tag:=d+e-9;
      Monat:=4;
    end;
    if (Tag=26) and (Monat=4) then
      Tag:=19;
    if (Tag=25) and (Monat=4) and (d=28) and (e=6) and (a>10) then
      Tag:=18;
    try
      Result:= EncodeDate(y, Monat, Tag);
    except
      Result:=0;
    end;
  end; { EasterSunday }

begin
  Result:=0;
  try
    DecodeDate(WhatDate, Y, M, D);
  except
    Y:=0;
  end;
  if (D>=1) and (M>=1) and (M<=12) and (Y>=1900) then begin
    DoY:=GetDayOfYear(WhatDate);
    Ostern:=EasterSunday(Y);
    try
      DecodeDate(Ostern, Y, OM, D);
    except
      OM:=4;
    end;
    try
      Weihnacht:=EncodeDate(Y, 12, 25);
      if (DayOfWeek(Weihnacht)-1)=0 then
        dw:=7
      else
        dw:=DayOfWeek(Weihnacht)-1;
    except
      Weihnacht:=-1;
      dw:=0;
    end;
    { Mariä Lichtmeß }                     { Sondertage }
    Dat:=EncodeDate(Y, 2, 2);
    if DoY=GetDayOfYear(Dat) then
      Result:=-1;
    { Valentinstag }
    Dat:=Encodedate(Y, 2, 14);
    if DoY=GetDayOfYear(Dat) then
      Result:=-2;
    { Weiberfastnacht }
    Dat:=Ostern-45;
    while DayOfWeek(Dat)<>2 do
      Dat:=Dat-1;
    if DoY=GetDayOfYear(Dat-4) then
      Result:=-3;
    { Rosenmontag }
    if DoY=GetDayOfYear(Dat) then
      Result:=-4;
    { Fastnacht }
    if DoY=GetDayOfYear(Dat+1) then
      Result:=-5;
    { Aschermittwoch }
    if DoY=GetDayOfYear(Dat+2) then
      Result:=-6;
    { Mariä Verkündigung }
    Dat:=Encodedate(Y, 3, 25);
    if DoY=GetDayOfYear(Dat) then
      Result:=-7;
    { Palmsonntag }
    if DoY=GetDayOfYear(Ostern-7) then
      Result:=-8;
    { Gründonnerstag }
    if DoY=GetDayOfYear(Ostern-3) then
      Result:=-9;
    { Muttertag }
    Dat:=EncodeDate(y, 4, 30);
    aw:=DayOfWeek(Dat)-1;
    Dat:=Dat-aw+14;
    if Dat=(Ostern+49) then
      Dat:=Dat-7;
    if DoY=GetDayOfYear(Dat) then
      Result:=-10;
    { Peter und Paul }
    Dat:=Encodedate(Y, 6, 29);
    if DoY=GetDayOfYear(Dat) then
      Result:=-11;
    { Mariä Geburt }
    Dat:=Encodedate(Y, 9, 8);
    if DoY=GetDayOfYear(Dat) then
      Result:=-12;
    { Erntedankfest }
    Dat:=Encodedate(Y, 9, 29);
    while DayOfWeek(Dat)<>1 do
      Dat:=Dat+1;
    if DoY=GetDayOfYear(Dat) then
      Result:=-13;
    { Mariä Empfängnis }
    Dat:=Encodedate(Y, 12, 8);
    if DoY=GetDayOfYear(Dat) then
      Result:=-14;
    { Silvester }
    Dat:=Encodedate(Y, 12, 31);
    if DoY=GetDayOfYear(Dat) then
      Result:=-15;
    { 1. Advent }
    Dat:=Weihnacht-1;
    while DayOfWeek(Dat)<>1 do
      Dat:=Dat-1;
    if DoY=GetDayOfYear(Dat-21) then
      Result:=-16;
    { 2. Advent }
    if DoY=GetDayOfYear(Dat-14) then
      Result:=-17;
    { 3. Advent }
    if DoY=GetDayOfYear(Dat-7) then
      Result:=-18;
    { 4. Advent }
    if DoY=GetDayOfYear(Dat) then
      Result:=-19;
    { Heiligabend }
    if DoY=GetDayOfYear(Weihnacht-1) then
      Result:=-20;
    { Frühlingsanfang }
    if DoY=GetDayOfYear(FSpringDate) then
      Result:=-21;
    { Sommmeranfang }
    if DoY=GetDayOfYear(FSummerDate) then
      Result:=-22;
    { Herbstanfang }
    if DoY=GetDayOfYear(FAutumnDate) then
      Result:=-23;
    { Winteranfang }
    if DoY=GetDayOfYear(FWinterDate) then
      Result:=-24;
    { Neujahr }                     { Feiertage }
    if DoY=1 then
      Result:=1;
    { Maifeiertag }
    Dat:=EncodeDate(Y, 5, 1);
    if DoY=GetDayOfYear(Dat) then
      Result:=2;
    { Tag der deutschen Einheit }
    Dat:=EncodeDate(Y, 10, 3);
    if DoY=GetDayOfYear(Dat) then
      Result:=3;
    { Allerheiligen }
    if Land<>14 then begin
      Dat:=EncodeDate(Y, 11, 1);
      if DoY=GetDayOfYear(Dat) then
        Result:=4;
    end;
    { Totensonntag }
    if (Weihnacht>=0) and (DoY=GetDayOfYear(Weihnacht-dw-28)) then
      Result:=5;
    { Volkstrauertag }
    if (Weihnacht>=0) and (DoY=GetDayOfYear(Weihnacht-dw-35)) then
      Result:=6;
    { 1. Weihnachtstag }
    if (Weihnacht>=0) and (DoY=GetDayOfYear(Weihnacht)) then
      Result:=7;
    { 2. Weihnachtstag }
    if (Weihnacht>=0) and (DoY=GetDayOfYear(Weihnacht+1)) then
      Result:=8;
    { Karfreitag }
    if DoY=GetDayOfYear(Ostern-2) then
      Result:=9;
    { Ostersonntag }
    if DoY=GetDayOfYear(Ostern) then
      Result:=10;
    { Ostermontag }
    if DoY=GetDayOfYear(Ostern+1) then
      Result:=11;
    { Christi Himmelfahrt }
    if DoY=GetDayOfYear(Ostern+39) then
      Result:=12;
    { Pfingstsonntag }
    if DoY=GetDayOfYear(Ostern+49) then
      Result:=13;
    { Pfingstmontag }
    if DoY=GetDayOfYear(Ostern+50) then
      Result:=14;
    { Fronleichnam }
    if (Land<2) or (Land=6) or ((Land>=9) and (Land<=12)) or (Land=15) then
      if DoY=GetDayOfYear(Ostern+60) then
        Result:=15;
    { Heilige 3 Könige }
    if (Land=0) or (Land=1) or (Land=13) then
      if DoY=6 then
        Result:=16;
    { Mariä Himmelfahrt }
    if (Land=1) or (Land=11) then begin
      Dat:=EncodeDate(Y, 8, 15);
      if DoY=GetDayOfYear(Dat) then
        Result:=17;
    end;
    { Reformationstag }
    if (Land=3) or (Land=7) or (Land=12) or (Land=13) or (Land=15) then begin
      Dat:=Encodedate(Y, 10, 31);
      if DoY=GetDayOfYear(Dat) then
        Result:=18;
    end;
    { Buß- und Bettag }
    if (Weihnacht>=0) and (Land=12) and (DoY=GetDayOfYear(Weihnacht-dw-32)) then
      Result:=19;
  end;
end;

function TSRCalendar.GetHolidays(Index: integer):integer;
begin
  Result:=FHolidays[Index];
end;

function TSRCalendar.GetMarked(Index: integer):boolean;
begin
  Result:=FMarked[Index];
end;

procedure TSRCalendar.GetMoonData(const Dat:TDateTime);
var TimeDiff         : extended;

  function LowestPhase(Dat:TDateTime):extended;
  var Phase   : extended;
      Std     : byte;
  begin
    Result:=Current_Phase(trunc(Dat));
    for Std:=1 to 23 do begin
      Phase:=Current_Phase(trunc(Dat)+Std/24);
      if Phase<Result then
        Result:=Phase;
    end;
  end; { LowestPhase }

begin
  FMoonDistance:=Moon_Distance(Dat);
  if LowestPhase(Dat-1)>LowestPhase(Dat) then begin
    if LowestPhase(Dat+1)>LowestPhase(Dat) then
      FMoonPhase:=mpNewMoon
    else
      FMoonPhase:=mpLastQuarter;
  end
  else begin
    if LowestPhase(Dat+1)<LowestPhase(Dat) then
      FMoonPhase:=mpFullMoon
    else
      FMoonPhase:=mpFirstQuarter;
  end;
  TimeDiff:=1/24;
  if IsSummertime(Dat) then
    TimeDiff:=TimeDiff+1/24;
  FMoonRise:=Moon_Rise(Dat, FLatitude, FLongitude)+TimeDiff;
  FMoonSet:=Moon_Set(Dat, FLatitude, FLongitude)+TimeDiff;
  FMoonTransit:=Moon_Transit(Dat, FLatitude, FLongitude)+TimeDiff;
end;

function TSRCalendar.GetSeason(Dat:TDateTime):TSeason;
begin
  Dat:=trunc(Dat);
  Result:=seNone;
  if (Dat>=FWinterDate) or (Dat<FSpringDate) then
    Result:=seWinter;
  if (Dat>=FSpringDate) and (Dat<FSummerDate) then
    Result:=seSpring;
  if (Dat>=FSummerDate) and (Dat<FAutumnDate) then
    Result:=seSummer;
  if (Dat>=FAutumnDate) and (Dat<FWinterDate) then
    Result:=seAutumn;
end;

procedure TSRCalendar.GetSunData(const Dat:TDateTime);
var TimeDiff : extended;
begin
  FSunDistance:=Sun_Distance(Dat)*au;
  TimeDiff:=1/24;
  if IsSummertime(Dat) then
    TimeDiff:=TimeDiff+1/24;
  FSunRise:=Sun_Rise(Dat, FLatitude, FLongitude)+TimeDiff;
  FSunSet:=Sun_Set(Dat, FLatitude, FLongitude)+TimeDiff;
  FSunTransit:=Sun_Transit(Dat, FLatitude, FLongitude)+TimeDiff;
end;

function TSRCalendar.GetZodiacSign(const Dat:TDateTime):TZodiacSign;
var TiJ : word;
begin
  Result:=zsNone;
  TiJ:=GetDayOfYear(Dat);
  if (TiJ>=21) and (TiJ<=49) then
    Result:=zsAquarius;
  if (TiJ>=50) and (TiJ<=79) then
    Result:=zsPisces;
  if (TiJ>=80) and (TiJ<=111) then
    Result:=zsAries;
  if (TiJ>=112) and (TiJ<=141) then
    Result:=zsTaurus;
  if (TiJ>=142) and (TiJ<=172) then
    Result:=zsGemini;
  if (TiJ>=173) and (TiJ<=203) then
    Result:=zsCancer;
  if (TiJ>=204) and (TiJ<=235) then
    Result:=zsLeo;
  if (TiJ>=236) and (TiJ<=266) then
    Result:=zsVirgo;
  if (TiJ>=267) and (TiJ<=296) then
    Result:=zsLibra;
  if (TiJ>=297) and (TiJ<=326) then
    Result:=zsScorpio;
  if (TiJ>=327) and (TiJ<=355) then
    Result:=zsSagittarius;
  if (TiJ>=355) or (TiJ<=20) then
    Result:=zsCapricorn;
end;

procedure TSRCalendar.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key=VK_Left then begin
    if Day=1 then begin
      ChangeMonth(-1);
      Day:=GetDaysThisMonth;
      Key:=0;
    end
    else
      if (Col=0) and (Row>1) then begin
        Day:=Day-1;
        Key:=0;
      end;
  end;
  if Key=VK_Right then begin
    if Day=GetDaysThisMonth then begin
      Day:=1;
      ChangeMonth(1);
      Key:=0;
    end
    else
      if (Col=6) and (Row<6) then begin
        Day:=Day+1;
        Key:=0;
      end;
  end;
  if (Key=VK_Up) and (Row=1) then begin
    ChangeMonth(-1);
    Day:=GetDaysThisMonth;
    Key:=0;
  end;
  if (Key=VK_Down) and (Row=6) then begin
    ChangeMonth(1);
    Day:=1;
    Key:=0;
  end;
  inherited KeyDown(Key, Shift);
end;

procedure TSRCalendar.MonthChange;
var i : integer;
begin
  if coCalcHolidays in FCalendarOptions then
    for i:=1 to 31 do
      FHolidays[i]:=0;
  if coAutoDeleteMarks in FCalendarOptions then
    for i:=1 to 31 do
      FMarked[i]:=false;
  FDaysThisMonth:=GetDaysThisMonth;
  if Assigned(FOnMonthChange) then
    FOnMonthChange(Self);
end;

procedure TSRCalendar.MouseToCell(X, Y: Integer; var ACol, ARow: Longint);
var Coord : TGridCoord;
begin
  Coord := MouseCoord(X, Y);
  ACol := Coord.X;
  ARow := Coord.Y;
end;

function TSRCalendar.MouseToDate(X, Y: Integer):TDateTime;
var ACol, ARow : longint;
    ADay       : word;
begin
  MouseToCell(X, Y, ACol, ARow);
  try
    ADay := StrToInt(CellText[ACol, ARow]);
    if (ADay>=1) and (Month>=1) and (Month<=12) and (Year>=1900) then
      Result:=EncodeDate(Year, Month, ADay)
    else
      Result:=-1;
  except
    Result:=-1;
  end;
end;

procedure TSRCalendar.NextMonth;
begin
  ChangeMonth(1);
end;

procedure TSRCalendar.NextYear;
begin
  if IsLeapYear(Year) and (Month = 2) and (Day = 29) then
    Day := 28;
  Year := Year + 1;
end;

procedure TSRCalendar.PrevMonth;
begin
  ChangeMonth(-1);
end;

procedure TSRCalendar.PrevYear;
begin
  if IsLeapYear(Year) and (Month = 2) and (Day = 29) then
    Day := 28;
  Year := Year - 1;
end;

function TSRCalendar.SelectCell(ACol, ARow: Longint): Boolean;
begin
  if ((not FUpdating) and (coReadOnly in FCalendarOptions)) or (CellText[ACol, ARow] = '') then
    Result := False
  else
    Result := inherited SelectCell(ACol, ARow);
end;

procedure TSRCalendar.SetBackgroundColors(const newValue: TCalendarColors);
begin
  with FBackgroundColors do begin
    FHeaders:=NewValue.Headers;
    FHoliday:=NewValue.Holiday;
    FMarked:=NewValue.Marked;
    FSelected:=NewValue.Selected;
    FStandard:=NewValue.Standard;
    FToday:=NewValue.Today;
    FWeekend:=NewValue.Weekend;
  end;
  Invalidate;
end;

procedure TSRCalendar.SetCalendarOptions(const newValue: TCalendarOptions);
begin
  if FCalendarOptions<>newValue then begin
    if (coUseCurrentDate in newValue) and not (coUseCurrentDate in FCalendarOptions) then
      FDate:=Now;
    if (coGridLines in newValue) and not (coGridLines in FCalendarOptions) then
      Options:=Options+[goVertLine, goHorzLine];
    if not (coGridLines in newValue) and (coGridLines in FCalendarOptions) then
      Options:=Options-[goVertLine, goHorzLine];
    FCalendarOptions:=newValue;
    UpdateCalendar;
    Repaint;
  end;
end;

procedure TSRCalendar.SetDate(const newValue: TDateTime);
var AYear,
    AMonth,
    ADay    : Word;
    MChange,
    YChange : boolean;
begin
  if trunc(newValue)<>trunc(FDate) then begin
    BeforeChange;
    try
      DecodeDate(newValue, AYear, AMonth, ADay);
      MChange:=AMonth<>Month;
      YChange:=AYear<>Year;
    except
      MChange:=false;
      YChange:=false;
    end;
    FDate:=newValue;
    UpdateCalendar;
    Change;
    if MChange then
      MonthChange;
    if YChange then
      YearChange;
  end;
end;

procedure TSRCalendar.SetDateElement(const Index: Integer; const newValue: Integer);
var
  AYear,
  AMonth,
  ADay    : Word;
  MChange,
  YChange : boolean;
begin
  if newValue>0 then begin
    BeforeChange;
    DecodeDate(FDate, AYear, AMonth, ADay);
    MChange := false;
    YChange := false;
    case Index of
      1: if AYear <> newValue then begin
           AYear := newValue;
           MChange := true;
           YChange := true;
         end
         else
           Exit;
      2: if (newValue <= 12) and (newValue <> AMonth) then begin
           AMonth := newValue;
           MChange := true;
         end
         else
           Exit;
      3: if (newValue <= GetDaysThisMonth) and (newValue <> ADay) then
           ADay := newValue
         else
           Exit;
      else Exit;
    end;
    try
      FDate := EncodeDate(AYear, AMonth, ADay)+Time;
    except
    end;
    FCalendarOptions := FCalendarOptions - [coUseCurrentDate];
    UpdateCalendar;
    Change;
    if MChange then
      MonthChange;
    if YChange then
      YearChange;
  end;
end;

procedure TSRCalendar.SetDrawStyle(const newValue: TCalendarDrawStyle);
begin
  if newValue<>FDrawStyle then begin
    FDrawStyle:=newValue;
    DefaultDrawing:=FDrawStyle<>cdsButtons;
    Invalidate;
  end;
end;

procedure TSRCalendar.SetGermanState(const newValue: TGermanState);
begin
  if FGermanState<>newValue then begin
    BeforeChange;
    FGermanState:=newValue;
    if newValue<>gsNone then begin
      SetLatitude(GermanStateLat[ord(newValue)]);
      SetLongitude(GermanStateLong[ord(newValue)]);
    end
    else begin
      SetLatitude(-1);
      SetLongitude(-1);
    end;
    UpdateCalendar;
    Change;
  end;
end;

procedure TSRCalendar.SetHolidays(Index: integer; const newValue: integer);
begin
  FHolidays[Index]:=newValue;
  Invalidate;
end;

procedure TSRCalendar.SetLatitude(const newValue: single);
begin
  if FLatitude<>newValue then begin
    FLatitude:=newValue;
    UpdateCalendar;
  end;
end;

procedure TSRCalendar.SetLongitude(const newValue: single);
begin
  if FLongitude<>newValue then begin
    FLongitude:=newValue;
    UpdateCalendar;
  end;
end;

procedure TSRCalendar.SetMarked(Index: integer; const newValue: boolean);
begin
  FMarked[Index]:=newValue;
  Invalidate;
end;

procedure TSRCalendar.SetSaturdayAsSunday(const newValue: boolean);
begin
  if newValue <> FSaturdayAsSunday then begin
    FSaturdayAsSunday := newValue;
    Invalidate;
  end;
end;

procedure TSRCalendar.SetStartOfWeek(const newValue: TDayOfWeek);
begin
  if newValue <> FStartOfWeek then begin
    FStartOfWeek := newValue;
    UpdateCalendar;
  end;
end;

procedure TSRCalendar.SetTextColors(const newValue: TCalendarColors);
begin
  with FTextColors do begin
    FHeaders:=NewValue.Headers;
    FHoliday:=NewValue.Holiday;
    FMarked:=NewValue.Marked;
    FSelected:=NewValue.Selected;
    FStandard:=NewValue.Standard;
    FToday:=NewValue.Today;
    FWeekend:=NewValue.Weekend;
  end;
  Invalidate;
end;

function TSRCalendar.StoreDate: Boolean;
begin
  Result := not (coUseCurrentDate in FCalendarOptions);
end;

procedure TSRCalendar.UpdateCalendar;
var
  AYear,
  AMonth,
  ADay      : Word;
  FirstDate : TDateTime;
begin
  FUpdating := True;
  try
    DecodeDate(Date, AYear, AMonth, ADay);
    FDayOfYear := GetDayOfYear(Date);
    FWeekOfYear := GetWeekOfYear(Date);
    FDaysThisMonth := GetDaysThisMonth;
    FSeason := GetSeason(Date);
    if coCalcHolidays in FCalendarOptions then begin
      FHolidayNr := GetHoliday(FDate, ord(FGermanState));
      if FHolidayNr=0 then
        FHoliday := '';
      if FHolidayNr>0 then
        FHoliday := Feiertag[FHolidayNr];
      if FHolidayNr<0 then
        FHoliday := Sondertag[abs(FHolidayNr)];
    end
    else
      FHoliday := '';
    FZodiacSign := GetZodiacSign(Date);
    if coCalcAstroData in FCalendarOptions then begin
      GetMoonData(Date);
      GetSunData(Date);
    end;
    FirstDate := EncodeDate(AYear, AMonth, 1);
    FMonthOffset := 2 - ((DayOfWeek(FirstDate) - ord(StartOfWeek) + 7) mod 7);
    if FMonthOffset = 2 then
      FMonthOffset := -5;
    MoveColRow((ADay - FMonthOffset) mod 7, (ADay - FMonthOffset) div 7 + 1,
      False, False);
    Invalidate;
  finally
    FUpdating := False;
  end;
end;

procedure TSRCalendar.YearChange;
begin
  FWinterDate:=trunc(StartSeason(Year, seWinter));
  FSpringDate:=trunc(StartSeason(Year, seSpring));
  FSummerDate:=trunc(StartSeason(Year, seSummer));
  FAutumnDate:=trunc(StartSeason(Year, seAutumn));
  if Assigned(FOnYearChange) then
    FOnYearChange(Self);
end;

procedure TSRCalendar.WMSize(var Message: TWMSize);
var
  GridLines: Integer;
begin
  GridLines := 6 * GridLineWidth;
  DefaultColWidth := (Message.Width - GridLines) div 7;
  DefaultRowHeight := (Message.Height - GridLines) div 7;
end;


function TSRCalendar.GetDaysPerMonth(AYear, AMonth: integer): Integer;
const
  DaysInMonth: array [1..12] of Integer =
   (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
begin
  Result:=DaysInMonth[AMonth];
  if (AMonth=2) and IsLeapYear(AYear) then
    Inc(Result);

end;

function TSRCalendar.GetDayOfYear(ADate: TDateTime): Word;
var T,M,J  : word;
    Erster : TDateTime;
begin
  try
    DecodeDate(ADate,J,M,T);
    Erster:=EncodeDate(J,1,1);
    Result:=trunc(ADate-Erster+1);
  except
    Result:=0;
  end;
end;

function TSRCalendar.IsSummertime(ADate: TDateTime): Boolean;
var AYear,
    AMonth,
    ADay   : word;
    Beginn,
    Ende   : TDateTime;
begin
  try
    ADate:=trunc(ADate);
    DecodeDate(ADate, AYear, AMonth, ADay);
    if AYear<1980 then
      { Keine Sommerzeit vor 1980 }
      Result:=false
    else begin
      { Beginn der Sommerzeit: }
      Beginn:=EncodeDate(AYear, 3, 31);
      while DayOfWeek(Beginn)<>1 do
        Beginn:=Beginn-1;
      { Ende der Sommerzeit: }
      if AYear<=1995 then
        { bis 1995: letzter So im September }
        Ende:=EncodeDate(AYear, 9, 30)
      else
        { ab 1996: letzter So im Oktober }
        Ende:=EncodeDate(AYear, 10, 31);
      while DayOfWeek(Ende)<>1 do
        Ende:=Ende-1;
      Result:=(ADate>=Beginn) and (ADate<Ende);
    end;
  except
    Result:=false;
  end;

end;

function TSRCalendar.GetWeekOfYear(ADate: TDateTime): Byte;
var
  Year,Month,Day : Word;
begin
  ADate:=ADate-((DayOfWeek(ADate)-FirstWeekDay+7) mod 7)+ 7-FirstWeekDate;
  DecodeDate(ADate, Year, Month, Day);
  Result:=(Trunc(ADate-EncodeDate(Year, 1, 1)) div 7)+1;

end;

end.
