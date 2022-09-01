unit cal_demo_const;

interface
{$I psc_defines.inc}

const

  SPSCDateTime = 'Date and Time';
  SPSCOnlyDate = 'Only Date';
  SPSCOnlyTime = 'Only Time';

  CPSCArrowStyleCount = 3;

  SPSCColor             = 'Background';
  SPSCCaptionColor      = 'Caption';
  SPSCCaptionTextColor  = 'Caption Text';
  SPSCSelectedColor     = 'Selected';
  SPSCSelectedTextColor = 'Selected Text';
  SPSCHeaderColor       = 'Header';
  SPSCArrowColor        = 'Arrow';
  SPSCFont              = 'Work Area';
  SPSCHeaderFont        = 'Header';


  CPSCMonthsBoxColorsCount = 7;
  CPSCMonthsBoxFontsCount = 2;

  SPSCCalendarWidth       = 'PSCCalendar Width :';
  SPSCCalendarHeight      = 'PSCCalendar Height :';
  SPSCMonthCalendarWidth  = 'MonthCalendar Width :';
  SPSCMonthCalendarHeight = 'MonthCalendar Height :';

  CPSCCalendarsIndent = 30;

  SPSCBorder       = 'Border';
  SPSCMonthHeader  = 'Month Header';
  SPSCArrow        = 'Arrow';
  SPSCWeekHeader   = 'Week Header';
  SPSCDays         = 'DaysBk';
  SPSCSelected     = 'Selected';
  SPSCSelectedText = 'Selected Text';
  SPSCGrayed       = 'Grayed';
  SPSCWeekEndText  = 'Week End Text';
  SPSCNowRect      = 'Now Rect';
  SPSCWeekSide     = 'Week Side';
  SPSCDayLines     = 'Day Lines';
  SPSCHolidays     = 'Holidays';
  SPSCWeekNumbers  = 'Week Numbers';
  SPSCWeekDays     = 'Week Days';
  SPSCHeader       = 'Header';
  SPSCDaysFont     = 'Days';
  SPSCWeekLine     = 'Week Line';
  SPSCHeaderBorder = 'Header Border';
  SPSCGrayedBk     = 'GrayedBk';

  CPSCCalColorsCount       = 15;
  CPSCUsedFontSizesCount   = 16;
  CPSCDefaultThemeNumber   = 2;


  CPSCUsedFontSizes : array[1..CPSCUsedFontSizesCount] of integer =
   (8,9,10,11,12,14,16,18,20,22,24,26,28,36,48,72);

  CPSCCalColorsNames : array[1..CPSCCalColorsCount] of string =
  (
    SPSCBorder,
    SPSCMonthHeader,
    SPSCArrow,
    SPSCWeekHeader,
    SPSCDays,
    SPSCSelected,
    SPSCSelectedText,
    SPSCGrayed,
    SPSCWeekEndText,
    SPSCNowRect,
    SPSCWeekSide,
    SPSCDayLines,
    SPSCWeekLine,
    SPSCHeaderBorder,
    SPSCGrayedBk
  );

  CPSCCalFontNames : array[1..5] of string =
  (
    SPSCHolidays,
    SPSCWeekNumbers,
    SPSCWeekDays,
    SPSCHeader,
    SPSCDaysFont
  );

  CPSCMonthsBoxColorsArray : array[1..CPSCMonthsBoxColorsCount] of string =
  (
    SPSCColor,
    SPSCCaptionColor,
    SPSCCaptionTextColor,
    SPSCSelectedColor,
    SPSCSelectedTextColor,
    SPSCHeaderColor,
    SPSCArrowColor
  );

  CPSCMonthsBoxFontsArray : array[1..CPSCMonthsBoxFontsCount] of string =
  (
    SPSCFont,
    SPSCHeaderFont
  );

  CPSCPanelKindArray : array[1..3] of string =
  (
    SPSCDateTime,
    SPSCOnlyDate,
    SPSCOnlyTime
  );

type
  TPSCMonthColors =
  (
    Background,
    Caption,
    CaptionText,
    Selected,
    SelectedText,
    Header,
    Arrow
  );
  TPSCMonthFonts =
  (
    WorkArea,
    HeaderArea
  );

implementation

end.
