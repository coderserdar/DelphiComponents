{: Contains the Global Constants, Types & Variables
 that are used by the ESBPCS for CLX.

 This is designed to work in Borland Delphi 6 CLX and above, Borland
 C++ Builder 6 CLX and above, and Borland Kylix 2 and above.
 Most if not all features will work in Kylix 1 but it is not currently supported.<p>

 This supplies the various Constants, Data Types and Global
 Variables to be used throughout ESBPCS. Constants and Variables
 that are dependent on types from Graphics and other units that
 have "overhead" have been placed in <See Unit=ESBPCSGlobals2>.<p>

 Copyright © 1999-2002 ESB Consultancy<p>

 v2.3 - 14 September 2002
}

unit QESBPCSGlobals;

{$I esbpcs.inc}
{$DEFINE EnglishInternational}

interface

uses
     QESBPCS_RS_Globals;

const
     //: Current Verion of ESBPCS
     ESBPCSVersion = '2.3';
     ESBPCSVersionNo = $02030000;

type
     TESBPCSLanguages = (elEnglishInt, elEnglishUS, elCzech, elDanish, elGerman,
          elRussian, elFrench, elPolish, elBrazPort, elUSER);

     {$IFDEF EnglishInternational}
const
     ESBPCSLanguage = elEnglishInt;
     {$ENDIF}
     {$IFDEF EnglishUS}
const
     ESBPCSLanguage = elEnglishUS;
     {$ENDIF}
     {$IFDEF German}
const
     ESBPCSLanguage = elGerman;
     {$ENDIF}
     {$IFDEF French}
const
     ESBPCSLanguage = elFrench;
     {$ENDIF}
     {$IFDEF Czech}
const
     ESBPCSLanguage = elCzech;
     {$ENDIF}
     {$IFDEF Polish}
const
     ESBPCSLanguage = elPolish;
     {$ENDIF}
     {$IFDEF Danish}
const
     ESBPCSLanguage = elDanish;
     {$ENDIF}
     {$IFDEF Russian}
const
     ESBPCSLanguage = elRussian;
     {$ENDIF}
     {$IFDEF BrazPort}
const
     ESBPCSLanguage = elBrazPort;
     {$ENDIF}
     {$IFDEF USER}
const
     ESBPCSLanguage = elUSER;
     {$ENDIF}

var
     ESBCalHelpContext: Integer = 30100;

var
     ESBCalcHelpContext: Integer = 40100;
     ESBSmallCalcHelpContext: Integer = 40200;
     ESBSciCalcHelpContext: Integer = 40300;

type
     {: Identifies the type of "storage" to use for the various
      Global Settings of ESBPCS.
      @enum eriNeither Nothing is stored. left up to the programmer.
      @enum eriRegistry Registry Settings are used.
      @enum eriIniFile IniFile is Used.
     }
     TESBRegIniType = (eriNeither, eriRegistry, eriIniFile);

var
     {: Identifies the type of "storage" to use for the various
      Global Settings of ESBPCS.

      eriNeither - Nothing is stored. left up to the programmer.<p>
      eriRegistry - Registry Settings are used.<p>
      eriIniFile - IniFile is Used
     }
     ESBRegIniType: TESBRegIniType = eriNeither;
     {: Registry Path to use when <See Var=ESBRegIniType> = eriRegistry. }
     ESBRegPath: string = '\Software\ESB Consultancy';
     {: Registry Section to use when <See Var=ESBRegIniType> = eriRegistry. }
     ESBRegSection: string = 'Defaults';
     {: IniFile to use when <See Var=ESBRegIniType> = eriIniFile. Should be
      a fully qualified path, like c:\app\esbpcs.ini - left unqualified
      it will normally be stored in your "windows" directory. }
     ESBIniFile: string = 'esbpcs.ini';
     {: IniFile Section to use when <See Var=ESBRegIniType> = eriIniFile }
     ESBIniSection: string = 'Defaults';

const
     //: Smallest Magnitude Single Available.
     MinSingle = 1.5E-45;
     //: Largest Magnitude Single Available.
     MaxSingle = 3.4E+38;
     //: Smallest Magnitude Double Available.
     MinDouble = 5.0E-324;
     //: Largest Magnitude Double Available.
     MaxDouble = 1.7E+308;
     //: Smallest Magnitude Extended Available.
     MinExtended = 3.6E-4951;
     //: Largest Magnitude Extended Available.
     MaxExtended = 1.1E+4932;
     //: Smallest Delphi Currency Value.
     MinCurrency = -922337203685477.5807;
     //: Largest Delphi Currency Value.
     MaxCurrency = 922337203685477.5807;

var
     {: Default Tolerance when doing Floating Point Operations.
      For Maximum Tolerance use MinExtended (3.6e-4951).
     }
     ESBTolerance: Extended = 5.0E-324;

var
     {: Default Precision when doing Floating Point Comparisons.
      For Maximum Precision use 1.0e-19.
     }
     ESBPrecision: Extended = 1.0E-17;
     {: Default Precision when doing Double Comparisons.
      For Maximum Precision use 1.0e-15.
     }
     ESBDoublePrecision: Extended = 1.0E-13;
     {: Default Precision when doing Single Comparisons.
      For Maximum Precision use 1.0e-7.
     }
     ESBSinglePrecision: Extended = 1.0E-5;

var
     {: When set to true then Enter is treated as though it were Tab
      when pressed in an ESBEdit Field. Set to False if you don't
      want this behaviour. }
     ESBEnterAsTab: Boolean = True;
     {: When set to true then ESC is treated as though it were Ctrl-Z (Undo)
      when pressed in an ESBEdit Field. Set to False if you don't
      want this behaviour. }
     ESBESCAsUndo: Boolean = True;
     {: When set to true then Down Arrow is treated as though it were Tab
      and Up Arrow as Shift-Tab when pressed in an ESBEdit Field.
      Set to False if you don't want this behaviour. }
     ESBArrowsAsTab: Boolean = True;
     {: When set to true, when a control is made ReadOnly then the TabStop
      is Set to False. Toggling this does not affect existing ReadOnly
      controls. }
     ESBNoTabStopOnReadOnly: Boolean = True;

var
     {: ESBNullStr is the default value for the NullStr property of
      the various Edit Components. }
     ESBNullStr: string = rsempty;

type
     {: A "short" currency type with an implied 2 decimal places and
      reduced rounding errors. –$21,474,836.48 to $21,474,836.47 using US
      Currency Format. See <See Unit=ESBPCSFinancials> for usage. }
     TESBCurrency = type Longint;
     {: A more robust Currency type with an implied 6 decimal places and
      reduced rounding errors. –$9,223,372,036,854.775808 to
      $9,223,372,036,854.775807 using US Currency Format.
      See <See Unit=ESBPCSFinancials> for usage. }
     TESBLongCurrency = type Int64;

const
     // Smallest ESBCurrency Value.
     MinESBCurrency: TESBCurrency = Low (TESBCurrency);
     // Largest ESBCurrency Value.
     MaxESBCurrency: TESBCurrency = High (TESBCurrency);
     // Smallest ESBCurrency Value.
     MinESBLongCurrency: TESBLongCurrency = Low (TESBLongCurrency);
     // Largest ESBCurrency Value.
     MaxESBLongCurrency: TESBLongCurrency = High (TESBLongCurrency);

type
     {: Complex Number type - see <See Unit=ESBPCSComplex> for usage. }
     TESBComplex = packed record
          Real: Extended;
          Imaginary: Extended;
     end;

const
     //: Zero as a Complex Number.
     ComplexZero: TESBComplex = (Real: 0; Imaginary: 0);
     //: 1 as a Complex Number.
     ComplexUnity: TESBComplex = (Real: 1; Imaginary: 0);
     //: i (ie sqrt (-1)) as a Complex Number.
     ComplexI: TESBComplex = (Real: 0; Imaginary: 1);

type
     {: Different types of display for Complex Numbers.
     @enum ectXY the Complex Number is displayed as the Real and
      Imaginary Parts, as in X + iY.
     @enum ectPolar the Complex number is displayed as the Amplitude
      and AbsoluteValue, as in AbsVal(cos (amp) + i sin (amp)).
     }
     TESBComplexType = (ectXY, ectPolar);

type
     {: Different types of display for Mixed Fraction Edits.
     @enum eftExpr the Fraction is displayed as an Expression
     @enum eftFloat the Fraction is displayed as a Float.
     }
     TESBFractionDisplay = (eftExpr, eftFloat);

type
     {: Fraction type - see <See Unit=ESBPCSFraction> for usage. }
     TESBFraction = packed record
          Numerator: LongInt;
          Denominator: LongWord;
     end;

type
     {: Mixed Fraction type - see <See Unit=ESBPCSFraction> for usage. }
     TESBMixedFraction = packed record
          WholePart: Longword;
          Numerator: LongWord;
          Denominator: LongWord;
          Negative: Boolean;
     end;

const
     //: Zero as a Reduced Fraction.
     FractionZero: TESBFraction = (Numerator: 0; Denominator: 1);
     //: 1 as a Reduced Fraction.
     FractionUnity: TESBFraction = (Numerator: 1; Denominator: 1);

type
     //: Used for 2 Dimensional Vectors.
     TESBVector2D = array [1..2] of Extended;
     //: Used for 3 Dimensional Vectors.
     TESBVector3D = array [1..3] of Extended;

type
     //: Set of Bytes.
     TESBByteSet = set of Byte;

type
     //: Set of Characters.
     TESBCharSet = set of Char;

var
     //: Set of Characters that separate words.
     WordSepSet: TESBCharSet = [#0..#64, #91..#95, #123..#137, #139,
     #145..#153, #155, #171, #187];

var
     //: Set of Characters taken as White Space
     WhiteSpaceSet: TESBCharSet = [#0..#32];

type
     //: Used for a Bit List of 16 bits from 15 -> 0.
     TESBBitList = type Word;

type
     //: Used for a Bit List of 32 bits from 31 -> 0.
     TESBLongBitList = type LongWord;

type
     //: A Short String with 16 characters.
     String16 = string [16];
     //: A Short String with 32 characters.
     String32 = string [32];

type
     //: Contains the 9 Deciles - uses in <See Unit=ESBPCSStatistics>.
     TESBDeciles = array [1..9] of Extended;

type
     //: Vector made up of Extended Floats.
     TESBFloatVector = array of Extended;
     //: Vector made up of Double Floats.
     TESBDoubleVector = array of Double;
     //: Vector made up of Single Floats.
     TESBSingleVector = array of Single;
     //: Vector made up of Int64.
     TESBInt64Vector = array of Int64;
     //: Vector made up of LongWords.
     TESBLWordVector = array of LongWord;
     //: Vector made up of LongInts.
     TESBLIntVector = array of LongInt;
     //: Vector made up of Words.
     TESBWordVector = array of Word;
     //: Vector made up of SmallInts.
     TESBSmallIntVector = array of SmallInt;
     //: Vector made up of Bytes.
     TESBByteVector = array of Byte;
     //: Vector made up of ShortInts.
     TESBSIntVector = array of ShortInt;

type
     //: Vector made up of ESB Currency.
     TESBCurrVector = array of TESBCurrency;
     //: Vector made up of ESB Long Currency.
     TESBLongCurrVector = array of TESBLongCurrency;
     //: Vector made up of Delphi Currency.
     TESBCurrencyVector = array of Currency;

type
     //: Vector made up of Complex Numbers.
     TESBComplexVector = array of TESBComplex;

type
     //: Vector made up of Chars.
     TESBCharVector = array of Char;

type
     //: Matrix made up of Extended Floats.
     TESBFloatMatrix = array of TESBFloatVector;
     //: Matrix made up of Double Floats.
     TESBDoubleMatrix = array of TESBDoubleVector;
     //: Matrix made up of Single Floats.
     TESBSingleMatrix = array of TESBSingleVector;
     //: Matrix made up of Int64s.
     TESBInt64Matrix = array of TESBInt64Vector;
     //: Matrix made up of LongWords.
     TESBLWordMatrix = array of TESBLWordVector;
     //: Matrix made up of LongInts.
     TESBLIntMatrix = array of TESBLIntVector;
     //: Matrix made up of Words.
     TESBWordMatrix = array of TESBWordVector;
     //: Matrix made up of SmallInts.
     TESBSmallIntMatrix = array of TESBSmallIntVector;
     //: Matrix made up of Bytes.
     TESBByteMatrix = array of TESBByteVector;
     //: Matrix made up of ShortInts.
     TESBSIntMatrix = array of TESBSIntVector;

type
     //: Matrix made up of ESB Currency.
     TESBCurrMatrix = array of TESBCurrVector;
     //: Matrix made up of ESB Long Currency.
     TESBLongCurrMatrix = array of TESBLongCurrVector;
     //: Matrix made up of Delphi Currency.
     TESBCurrencyMatrix = array of TESBCurrencyVector;

type
     //: Matrix made up of Complex Numbers.
     TESBComplexMatrix = array of TESBComplexVector;

var
     {: Character to use for Left Hand Padding of Numerics. }
     ESBNumPadCh: Char = ' ';

     {: Signals whether a '+' sign should be shown with positives. }
     ESBNumPosSign: Boolean = False;

var
     {: Signals whether a Zero should be displayed by a "blank" string. }
     ESBBlankWhenZero: Boolean = False;

var
     {: Signals whether an exception is raised when doing Date/Time
      Conversions. }
     ESBRaiseDateError: Boolean = True;

var
     {: Signals whether an exception is raised when doing Float
      Conversions. }
     ESBRaiseFloatError: Boolean = True;

var
     {: Signals whether an exception is raised when doing IP Address
      Conversions. }
     ESBRaiseIPError: Boolean = True;

type
     {: Identifies which of the three common formats for Date Order
      that a given Date Format is in.

      @enum edoDMY Day/Month/Year
      @enum edoMDY Month/Day/Year
      @enum edoYMD Year/Month/Day
     }
     TESBDateOrder = (edoDMY, edoMDY, edoYMD, edoUnknown);

type
     {: Different ways in which 1 & 2 Digit Years are handled in Str2Date
      and in the Date Edit Components.

      @enum edyNone Nothing is done, left to Delphi to handle.
      @enum edyCutOff the <See Var=ESB2DigitCutOff> is used to decide
       which century the date lies in. If 1900 + Yr is less than
       ESB2DigitCutOff then it is assumed that 2000 + Yr is wanted,
       otherwise 1900 + Yr is used.
      @enum edyHistoric assumes that the yr is this year or earlier. }
     TESB2DigitYr = (edyNone, edyCutOff, edyHistoric);

var
     {: Different ways in which 1 & 2 Digit Years are handled in Str2Date
      and in the Date Edit Components.
      edyNone - Nothing is done, left to Delphi to handle. <p>
      edyCutOff - the <See Var=ESB2DigitCutOff> is used to decide
       which century the date lies in. If 1900 + Yr is less than
       ESB2DigitCutOff then it is assumed that 2000 + Yr is wanted,
       otherwise 1900 + Yr is used.<p>
      edyHistoric  - assumes that the yr is this year or earlier. }
     ESB2DigitYr: TESB2DigitYr = edyCutOff;

     {: If <See Var=ESB2DigitYr> = edyCutOff - then  ESB2DigitCutOff is used
      to decide	which century the date lies in. If 1900 + Yr less than
      ESB2DigitCutOff then it is assumed that 2000 + Yr is wanted,
      otherwise 1900 + Yr is used. }
     ESB2DigitCutOff: Word = 1920;

     // Virtual Keys that Delphi omitted
const
     VK_0 = 48;
     VK_1 = 49;
     VK_2 = 50;
     VK_3 = 51;
     VK_4 = 52;
     VK_5 = 53;
     VK_6 = 54;
     VK_7 = 55;
     VK_8 = 56;
     VK_9 = 57;

const
     VK_A = 65;
     VK_B = 66;
     VK_C = 67;
     VK_D = 68;
     VK_E = 69;
     VK_F = 70;
     VK_G = 71;
     VK_H = 72;
     VK_I = 73;
     VK_J = 74;
     VK_K = 75;
     VK_L = 76;
     VK_M = 77;
     VK_N = 78;
     VK_O = 79;
     VK_P = 80;
     VK_Q = 81;
     VK_R = 82;
     VK_S = 83;
     VK_T = 84;
     VK_U = 85;
     VK_V = 86;
     VK_W = 87;
     VK_X = 88;
     VK_Y = 89;
     VK_Z = 90;

const
     //: ` ~ Key in top left hand corner.
     VK_Tilda = 192;
     //: Minus/Underscore Key.
     VK_Minus = 189;
     //: Equals/Plus Key.
     VK_Equals = 187;
     //: Left Bracket/Brace Key.
     VK_LeftBracket = 219;
     //: Right Bracket/Brace Key.
     VK_RightBracket = 221;
     //: BackSlash/Vertical Key.
     VK_BackSlash = 220;
     //: SemiColon/Colon Key.
     VK_SemiColon = 186;
     //: Quote/Double Quote Key.
     VK_Quote = 222;
     //: Comma/Less Than Key.
     VK_Comma = 188;
     //: Full Stop/Greater Than Key.
     VK_Stop = 190;
     //: Forward Slash/Question Mark Key.
     VK_Slash = 191;

type
     {: Different types of Validation Error.
      @enum evtNone No Validation Error exists.
      @enum evtBelowLower the entered value is Below the BoundLower Value.
      @enum evtAboveUpper the entered value is Above the BoundUpper Value.
     }
     TESBBoundsValidationType = (evtNone, evtBelowLower, evtAboveUpper);

     {: The event for the User Handling of Validation Errors. }
     TESBBoundsValidationEvent = procedure (Sender: TObject;
          const Value: string; const ErrType: TESBBoundsValidationType) of object;

type
     {: Change Event used in Compound Components where the ElementNo identifies
      which element was changed. }
     TESBChangeEvent = procedure (Sender: TObject; const ElementNo: LongWord) of object;

type
     {: The event is used to notify a change in Value presenting both
      the Current/Old Value and the new Value. Normally used for
      Navigation. }
     TESBChangePosEvent = procedure (Sender: TObject;
          const OldValue, NewValue: LongWord) of object;

type
     {: The event is triggered when Virtual Mode is on and an
      Element is needed. }
     TESBGetElementEvent = procedure (Sender: TObject;
          const Index: LongWord; var Value: Extended) of object;
     {: The event is triggered when Virtual Mode is on and an
      Element is changed. }
     TESBSetElementEvent = procedure (Sender: TObject;
          const Index: LongWord; const Value: Extended) of object;
     {: The event is triggered when Virtual Mode is on and a
      Label is needed. }
     TESBGetLabelEvent = procedure (Sender: TObject;
          const Index: LongWord; var Value: string) of object;
     {: The event is triggered when Virtual Mode is on and a
      Label is changed. }
     TESBSetLabelEvent = procedure (Sender: TObject;
          const Index: LongWord; const Value: string) of object;

type
     {: The event is triggered when Virtual Mode is on and an
      Element is needed. }
     TESBGetMatrixElementEvent = procedure (Sender: TObject;
          const RowIndex, ColIndex: LongWord; var Value: Extended) of object;
     {: The event is triggered when Virtual Mode is on and an
      Element is changed. }
     TESBSetMatrixElementEvent = procedure (Sender: TObject;
          const RowIndex, ColIndex: LongWord; const Value: Extended) of object;
     {: Change Event that identifies which element was changed. }
     TESBMatrixChangeEvent = procedure (Sender: TObject; const Row, Col: LongWord) of object;

type
     {: Exit Start Event is called at the start of the DoExit but before the
      final conversion of text to value and before any Bounds Checking
      or Conversion Checking is done. Set FurtherChecking to False to
      turn off Conversion & Bounds Checking. The value of Text can be
      altered. }
     TESBExitStartEvent = procedure (Sender: TObject;
          var Text: string; var FurtherChecking: Boolean) of object;

type
     {: Convert Event used in components when conversion from text has resulted
      in an error - includes Text Responsible. Currently used in Date & Time
      Edits & IP Edit. }
     TESBConvertErrorEvent = procedure (Sender: TObject; const Text: string) of object;

type
     {: Convert Event used in components when conversion from text is being Enhanced. }
     TESBConvertEvent = procedure (Sender: TObject; var Text: string) of object;

type
     {: Calc Event used in CalcEdit which allow you to replace the Popup
      Calculator with your own. Value is the value currently in the
      CalcEdit and should be updated accordingly. }
     TESBCalcEvent = procedure (Sender: TObject; var Value: Extended) of object;

type
     {: Calendar Event used in CalEdit which allow you to replace the Popup
      Calendar with your own. Value is the value currently in the
      CalEdit and should be updated accordingly. }
     TESBCalendarEvent = procedure (Sender: TObject; var Value: TDateTime) of object;

type
     {: Event used to Retrieve Calendar Menu Description. ItemNo is the Order of
      the Entry, Value is what will appear in the Menu. Value = '-' means separator. }
     TESBGetCalMenuNameEvent = procedure (Sender: TObject; const ItemNo: Integer;
          var Value: string) of object;

type
     {: Event used to Identify how many extra Menu Items you are adding to Fast Button. }
     TESBGetCalMenuCountEvent = procedure (Sender: TObject; var NoOfItems: Integer) of object;

type
     {: Calendar Event called when User Menu Event is called. ItemNo identifies
      which Menu Item was selected. Value contains the current Date displayed,
      and if Aborted is False (default) then Value will be used to update the Calendar. }
     TESBCalMenuClickEvent = procedure (Sender: TObject; const ItemNo: Integer;
          var Value: TDateTime; var Aborted: Boolean) of object;

type
     {: Type of Date/Time Label Format to use.
      @enum edtfCustom use Format value.
      @enum edtfCustomRFC822 use Format value + (+HHMM).
      @enum edtfShortDateShortTime use System ShortDate & System ShortTime.
      @enum edtfLongDateShortTime use System LongDate & System ShortTime.
      @enum edtfShortDateLongTime use System ShortDate & System LongTime.
      @enum edtfLongDateLongTime use System LongDate & System LongTime.
      @enum edtfRFC822 dd mmm yy hh:mm:ss (+HHMM) with English Month.
      @enum edtfLongRFC822 ddd, dd mmm yy hh:mm:ss (+HHMM) with English DOW & Month.
      @enum edtfLocalRFC822 dd mmm yy hh:mm:ss (+HHMM) with Local Month.
      @enum edtfLocalLongRFC822 - ddd, dd mmm yy hh:mm:ss (+HHMM) with Local DOW & Month.
     }
     TESBDateTimeFormatType = (edtfCustom, edtfCustomRFC822,
          edtfShortDateShortTime, edtfLongDateShortTime,
          edtfShortDateLongTime, edtfLongDateLongTime,
          edtfRFC822, edtfLongRFC822,
          edtfLocalRFC822, edtfLocalLongRFC822);

type
     {: Type of Date Format to use.
      LongDate Format is really only for display, it cannot be used for input.<p>
      Not all Date Formats assigned to Custom can be used for Input.
      @enum edfShortDate use System ShortDate.
      @enum edfLongDate use System LongDate.
      @enum edfCustom use Format value.
     }
     TESBDateFormatType = (edfShortDate, edfLongDate, edfCustom);

type
     {: Type of Time Format to use.
      @enum etfShortTime use System ShortTime.
      @enum etfLongTime use System LongTime.
      @enum etfCustom use Format value.
     }
     TESBTimeFormatType = (etfShortTime, etfLongTime, etfCustom);

type
     {: Type of Month Format to display.
      @enum emfNumeric Display Month as a Number between 1 and 12.
      @enum emfShortMonth use System ShortMonthNames.
      @enum emfLongMonth use System LongMonthNames.
     }
     TESBMonthFormatType = (emfNumeric, emfShortMonth, emfLongMonth);

type
     {: Type of Day of Week Format to display.
      @enum edfNumeric  Display Day of Week as a Number between 1 and 7.
      @enum edfShortDOW use System ShortDayNames.
      @enum edfLongDOW use System LongDayNames.
     }
     TESBDOWFormatType = (edfNumeric, edfShortDOW, edfLongDOW);

type
     {: Type of Currency Format to use.
      @enum ecfFloat - as Float.
      @enum ecfSystem - use System Currency Settings.
      @enum ecfCustom - use component Currency Settings.
     }
     TESBCurrencyFormatType = (ecfFloat, ecfSystem, ecfCustom);

     {--- Mathematical Constants ---}

const
     //: Square Root of 2.
     Sqrt2: Extended = 1.4142135623730950488;

     //: Square Root of 3.
     Sqrt3: Extended = 1.7320508075688772935;

     //: Square Root of 5.
     Sqrt5: Extended = 2.2360679774997896964;

     //: Square Root of 10.
     Sqrt10: Extended = 3.1622776601683793320;

     //: Square Root of Pi.
     SqrtPi: Extended = 1.77245385090551602729;

     //: Cube Root of 2.
     Cbrt2: Extended = 1.2599210498948731648;

     //: Cube Root of 3.
     Cbrt3: Extended = 1.4422495703074083823;

     //: Cube Root of 10.
     Cbrt10: Extended = 2.1544346900318837219;

     //: Cube Root of 100.
     Cbrt100: Extended = 4.6415888336127788924;

     //: Cube Root of Pi.
     CbrtPi: Extended = 1.4645918875615232630;

     //: Inverse of Square Root of 2.
     InvSqrt2: Extended = 0.70710678118654752440;

     //: Inverse of Square Root of 3.
     InvSqrt3: Extended = 0.57735026918962576451;

     //: Inverse of Square Root of 5.
     InvSqrt5: Extended = 0.44721359549995793928;

     //: Inverse of Square Root of Pi.
     InvSqrtPi: Extended = 0.56418958354775628695;

     //: Inverse of Cube Root of Pi.
     InvCbrtPi: Extended = 0.68278406325529568147;

     //: Natural Constant.
     ESBe: Extended = 2.7182818284590452354;

     //: Square of Natural Constant.
     ESBe2: Extended = 7.3890560989306502272;

     //: Natural Constant raised to Pi.
     ESBePi: Extended = 23.140692632779269006;

     //: Natural Constant raised to Pi/2.
     ESBePiOn2: Extended = 4.8104773809653516555;

     //: Natural Constant raised to Pi/4.
     ESBePiOn4: Extended = 2.1932800507380154566;

     //: Natural Log of 2.
     Ln2: Extended = 0.69314718055994530942;

     //: Natural Log of 10.
     Ln10: Extended = 2.30258509299404568402;

     //: Natural Log of Pi.
     LnPi: Extended = 1.14472988584940017414;

     //: Log to Base 2 of 10.
     Log10Base2 = 3.3219280948873623478;

     //: Log to Base 10 of 2.
     Log2Base10: Extended = 0.30102999566398119521;

     //: Log to Base 10 of 3.
     Log3Base10: Extended = 0.47712125471966243730;

     //: Log to Base 10 of Pi.
     LogPiBase10: Extended = 0.4971498726941339;

     //: Log to Base 10 of Natural Constant.
     LogEBase10: Extended = 0.43429448190325182765;

     //: Accurate Pi Constant.
     ESBPi: Extended = 3.1415926535897932385;

     //: Inverse of Pi.
     InvPi: Extended = 3.1830988618379067154E-1;

     //: Two * Pi.
     TwoPi: Extended = 6.2831853071795864769;

     //: Three * Pi.
     ThreePi: Extended = 9.4247779607693797153;

     //: Square of Pi.
     Pi2: Extended = 9.8696044010893586188;

     //: Pi raised to the Natural Constant.
     PiToE: Extended = 22.459157718361045473;

     //: Half of Pi.
     PiOn2: Extended = 1.5707963267948966192;

     //: Third of Pi.
     PiOn3: Extended = 1.0471975511965977462;

     //: Quarter of Pi.
     PiOn4: Extended = 0.7853981633974483096;

     //: Three Halves of Pi.
     ThreePiOn2: Extended = 4.7123889803846898577;

     //: Four Thirds of Pi.
     FourPiOn3: Extended = 4.1887902047863909846;

     //: 2^63.
     TwoToPower63: Extended = 9223372036854775808.0;

     //: One Radian in Degrees.
     OneRadian: Extended = 57.295779513082320877;

     //: One Degree in Radians.
     OneDegree: Extended = 1.7453292519943295769E-2;

     //: One Minute in Radians.
     OneMinute: Extended = 2.9088820866572159615E-4;

     //: One Second in Radians.
     OneSecond: Extended = 4.8481368110953599359E-6;

     //: Gamma Constant.
     ESBGamma: Extended = 0.57721566490153286061;

     //: Natural Log of the Square Root of (2 * Pi)
     LnRt2Pi: Extended = 9.189385332046727E-1;

     {--- Physical Constants ---}

const
     //: Speed of Light in a Vacuum in Metres per second.  From NIST as of 1998.
     SpeedOfLight: Extended = 299792458;

     //: One Light Year in Metres.
     LightYear: Extended = 299792458.0 * 60.0 * 60.0 * 24.0 * 365.2425;

     //: One Light Days in Metres.
     LightDay: Extended = 299792458.0 * 60.0 * 60.0 * 24.0;

     //: One Light Hour in Metres.
     LightHour: Extended = 299792458.0 * 60.0 * 60.0;

     //: One Light Minute in Metres.
     LightMinute: Extended = 299792458.0 * 60.0;

     //: One Light Second in Metres.
     LightSecond: Extended = 299792458.0;

     //: Speed of Sound in Dry Air in Metres per second.
     MachOne: Extended = 1229 * 1000 / (60 * 60);

     //: Elementary Charge in Coloumbs.  From NIST as of 1998.
     ElemCharge: Extended = 1.602176462E-19;

     //: Avogadro's Constant mol^-1.  From NIST as of 1998.
     Avogadro: Extended = 6.02214199E+23;

     //: Proton Mass Unit in Kilograms.  From NIST as of 1998.
     ProtonMassUnit: Extended = 1.67262158E-27;

     //: Electron Mass Unit in Kilograms.  From NIST as of 1998.
     ElectronMassUnit: Extended = 9.10938188E-31;

     //: Atomic Mass Unit in Kilograms. From NIST as of 1998.
     AtomicMassUnit: Extended = 1.66053873E-27;

     //: Neutron Mass Unit in Kilograms. From NIST as of 1998.
     NeutronMassUnit: Extended = 1.67492716E-27;

     //: Faraday's Constant in Columbs/mol.  From NIST as of 1998.
     Faraday: Extended = 96485.3415;

     //: Planck's Constant in Joule seconds.  From NIST as of 1998.
     Planck: Extended = 6.62606876E-34;

     //: Gravitational Constant in m^3/kg/s^2.  From NIST as of 1998.
     GravConst: Extended = 6.673E-11;

     //: Standard Gravitional Acceleration in metres/second^2.  From NIST as of 1998.
     Gravity: Extended = 9.80665;

     //: Boltzmann's constant on joules per kelvin. From NIST as of 1998.
     Boltzmann: Extended = 1.3806503E-23;

     //: Josephson's Constant in Hz/V. From NIST as of 1998.
     Josephson: Extended = 483597.898E9;

     //: Rydberg's Constant in 1/m. From NIST as of 1998.
     Rydberg: Extended = 10973731.568549;

     //: Standard Atmospheric Pressure measured in Pascals. From NIST as of 1998.
     Atmosphere: Extended = 101325;

     //: Absolute Zero in Celsius.
     AbsoluteZeroC: Extended = -273.15;

     //: Absolute Zero in Fahrenheit.
     AbsoluteZeroF: Extended = -459.67;

     {--- Conversions ---}
const
     //: One Astronomical Unit in Metres.
     AstroUnit: Extended = 149598550000.0;

     //: One Parsec in Metres.
     Parsec: Extended = 3.085677E+16;

     //: One Calorie in Joules.
     OneCalorie: Extended = 4.1840;

     //: One Yard in Metres.
     OneYard: Extended = 0.9144;

     //: One Foot in Metres.
     OneFoot: Extended = 0.3048;

     //: One Inch in Metres.
     OneInch: Extended = 0.0254;

     //: One Mile in Metres.
     OneMile: Extended = 1609.344;

     //: One Nautical Mile in Metres.
     OneNautMile: Extended = 1852;

     //: One Pound in Kilograms.
     OnePound: Extended = 0.45359237;

     //: One Ounce in Kilograms.
     OneOunce: Extended = 0.02834952;

     //: One Pound Force in Newtons.
     OnePoundForce: Extended = 4.44822;

     //: One Foot Pound in Joules.
     OneFootPound: Extended = 1.35582;

     //: One Becquerel in Curies
     OneBecquerel: Extended = 2.7E-11;

const
     RadiusOfEarth = 6288150; // In Metres

type //: Used for QuickSort.
     TQStackRec = record
          Left, Right: Integer;
     end;

type
     {: Identifies the type of glyph to use for the Calculator Button.
     }
     TESBCalcGlyphType = (ecgCalc1, ecgCalc2);

var
     {: Current Value of the Calculator Memory - all Calculators share
      this value. }
     ESBCalcMemory: Extended = 0;
     {: Current Value of the Calculator Radians Setting - all Calculators share
      this value. }
     ESBCalcRadians: Boolean = True;
     {: Font used for Displaying the Calculator. }
     ESBCalcDispFont: string = 'Arial';

var
     {: Font used for Displaying the Calendar. }
     ESBCalDispFont: string = 'Arial';

type
     {: Identifies the type of Day of Month to use for the DOM processing
      routines of ESBPCS.

      @enum domFirst First occurrence in a Month.
      @enum domSecond Second occurrence in a Month.
      @enum domThird Third occurrence in a Month.
      @enum domFourth Fourth occurrence in a Month.
      @enum domLast Last occurrence in a Month.
     }
     TESBDOMType = (domFirst, domSecond, domThird, domFourth, domLast);

     // Holiday Variables & Types
var
     {: When True Good Friday, Easter Sunday
      and Christmas Day are included as Non-Working Holidays. }
     ESBUseChristianHolidays: Boolean = True;

     {: When True and <See Var=ESBUseChristianHolidays> is True,
      then Easter Saturday is included as a Non-Working Holiday }
     ESBUseEasterSaturday: Boolean = True;

     {: When True and <See Var=ESBUseChristianHolidays> is True,
      then Easter Monday is included as a Non-Working Holiday }
     ESBUseEasterMonday: Boolean = True;

     {: Controls whether the Holidays from the Holiday File are loaded. }
     ESBLoadHolidays: Boolean = True;

var
     {: Days of the week to consider as Non-Working - by default set
      to Saturday & Sunday. }
     NonWorkingDays: set of 1..7 = [1, 7];

const
     //: Fraction of a TDateTime that represents One Hour.
     OneDTHour = 1 / 24;

     //: Fraction of a TDateTime that represents One Minute.
     OneDTMinute = 1 / (24 * 60);

     //: Fraction of a TDateTime that represents One Second.
     OneDTSecond = 1 / (24 * 60 * 60);

     //: Fraction of a TDateTime that represents One Millisecond.
     OneDTMillisecond = 1 / (24 * 60 * 60 * 1000);

const
     //: Seconds Per Minute
     SecsPerMin = 60;

     //: Minutes Per Second
     MinsPerSec = 1 / 60;

     //: Minutes Per Hour
     MinsPerHr = 60;

     //: Hours Per Minute
     HrsPerMin = 1 / 60;

     //: Seconds Per Hour
     SecsPerHr = 3600;

     //: Hours Per Per Second
     HrsPerSec = 1 / 3600;

     //: Hours Per Day
     HrsPerDay = 24;

     //: Days Per Hour
     DaysPerHr = 1 / 24;

     //: Minutes Per Day
     MinsPerDay = 24 * 60;

     //: Days Per Minute
     DaysPerMin = 1 / (24 * 60);

     //: Seconds Per Day
     SecsPerDay = 24 * 3600;

     //: Days Per Second
     DaysPerSec = 1 / (24 * 3600);

     //: Days Per Week
     DaysPerWeek = 7;

     //: Weeks Per Day
     WeeksPerDay = 1 / 7;

     //: Days Per Fortnight
     DaysPerFortnight = 14;

     //: Fortnights Per Day
     FortnightsPerDay = 1 / 14;

const
     //: Days per Tropical Year
     DaysPerTropicalYear = 365.242191;

     // Days per Gregorian Year
     DaysPerGregorianYear = 365.2425;

     // Days per Julian Year
     DaysPerJulianYear = 365.25;

     //: Days per Synodic Month
     DaysPerSynodicMonth = 29.53059;

     //: Hours in a Sidereal Day (Mean Solar Time) - 23h56m04.091s
     HrsPerSiderealDay = 23 + 56 / 60 + 4.091 / 3600;

type
     {: When displaying an ESB Label the following apply.
      @enum elsNormal displays like a "normal" label.
      @enum elsRaised displays a "raised up" label.
      @enum elsSunken displays a "sunken" label.
     }
     TESBLabelStyle = (elsNormal, elsRaised, elsSunken);

type
     {: When an ESB Label is attached to a control it can be positioned
      in one of the following 4 positions.
      @enum elaLeft Left of the Control
      @enum elaTop Above the Control
      @enum elaRight Right of the Control
      @enum elaBottom Below the Control
     }
     TESBLabelAttachType = (elaLeft, elaTop, elaRight, elaBottom);

type
     {: When an ESB Label is attached to a control on either
      left (elaLeft) or right (elaRight) then then controls
      how the Vertical Alignment is handled.
      @enum elvTop Top of the side of the control
      @enum elvCenter Center of the side of the control
      @enum elvBottom Bottom of the side of the control
     }
     TESBLabelAttachAlignV = (elvTop, elvCenter, elvBottom);

type
     {: When an ESB Label is attached to a control on either
      left (elaLeft) or right (elaRight) then then controls
      how the Vertical Alignment is handled.
      @enum elhLeft Left of the Top/Bottom of the control
      @enum elhCenter Center of the Top/Bottom of the control
      @enum elhRight Right of the Top/Bottom of the control
     }
     TESBLabelAttachAlignH = (elhLeft, elhCenter, elhRight);

type
     {: Data Type for storing information about mixed Imperial units
      for example Feet & inches. Also allows  for Powers, eg Areas,
      Volumes etc; unit descriptions & Abbreviations }
     TESBImperial = record
          MajorUnit: LongInt; // Integral Feet
          MinorUnit: Double; // Inches which allow fractionals
          Power: LongInt; // By default 1, 0 => Null
          Ratio: LongWord; // Number or Minor Unit
          MajorAbbrev: string; // Abbreviation for Major unit
          MinorAbbrev: string; // Abbreviation for Minor unit
          MajorDesc: string; // Major unit Desc
          MinorDesc: string; // Minor unit Desc
     end;

type
     {: Identifies the type of URL.
      @enum urlHTTP HTTP Address - normal Web Address.
      @enum urlMailTo MailTo Address - normal Email Address.
      @enum urlFTP FTP Address - normal File Transfer Address.
      @enum urlHTTPS HTTPS Address - secure Web Address.
      @enum urlNews News Address - normal News Address.
      @enum urlNNTP News Address - News Address using NTTP.
      @enum urlTelenet Telnet Address - Interactive Session.
      @enum urlGopher Gopher Address - Gopher Protocol.
      @enum urlWAIS WAIS Address - Wide Area Information Server Address.
      @enum urlFile File Address - a file or registered file type.
      @enum urlProspero File Address - Prospero Directory Services Protocal.
     }
     TESBUrlType = (urlHTTP, urlMailTo, urlFTP, urlHTTPS,
          urlNews, urlNNTP, urlTelnet, urlGopher, urlWAIS,
          urlFile, urlProspero);

type
     //: Function used for transformation of Extended floats
     TFloatFunction = function (const X: Extended): Extended;
     //: Function used for transformation of Double floats
     TDoubleFunction = function (const X: Double): Double;
     //: Function used for transformation of Single floats
     TSingleFunction = function (const X: Single): Single;

type
     {: Identifies types of Labels.
      @enum eltCustom User can edit the labels when not in ReadOnly mode.
      @enum eltNumeric Labels are numerically generated - no storage,
       cannout be edited.
      @enum eltVirtual Labels are not stored but retrieved virtually,
       editing is permitted but the user must look after the storage.
     }
     TESBLabelType = (eltCustom, eltNumeric, eltVirtual);

var
     {: String to display for representing Tab, ie #9 }
     ESBTabStr: string = rsTAB;

type
     {: Used sligning a Caption for display.
      @enum eacCaptionLeft Caption is displayed to the Left of the CheckBox.
      @enum eacCaptionRight Caption is displayed to the Right of the CheckBox.
     }
     TESBAlignCaption = (eacCaptionLeft, eacCaptionRight);

type
     {: When drawing a Frame, these are the styles available.
      @enum efsNone No Frame Drawn.
      @enum efsSimple A Simple Rectangle is Drawn.
      @enum efsRaised A "raised up" Rectangle is Drawn.
      @enum efsSunken A "sunken" Rectangle is Drawn.
      @enum efsRound A Round Rectangle is Drawn.
     }
     TESBFrameStyle = (efsNone, efsSimple, efsRaised, efsSunken, efsRound);

type
     {: Controls the symbol that is displayed in the Box of an ESBCheckBox when
       it is checked.
      @enum ecsTick A Tick is displayed.
      @enum ecsCross A Cross is displayed.
     }
     TESBCheckStyle = (ecsTick, ecsCross);

type
     {: When drawing a GroupBox Frame, these are the styles available.
      @enum egfsNone No Frame Drawn.
      @enum egfsNormal Normal Groupbox Frame Drawn.
      @enum egfsSimple A Simple Rectangle is Drawn.
      @enum egfsRound A Round Rectangle is Drawn.
     }
     TESBGroupFrameStyle = (egfsNone, egfsNormal, egfsSimple, egfsRound);

type
     {: When drawing a Calendar Frame, these are the styles available.
      @enum ecfsNone No Frame Drawn.
      @enum ecfsNormal Normal Groupbox Frame Drawn.
      @enum ecfsSimple A Simple Rectangle is Drawn.
     }
     TESBCalFrameStyle = (ecfsNone, ecfsNormal, ecfsSimple);

type
     {: Event called to identify whether a Component within a Group Component
      should be Enabled or Disabled. }
     TESBItemEnabledEvent = procedure (Sender: TObject;
          const ElementNo: Longword; var Enabled: Boolean) of object;

type
     {: Platform type for Registry and Installation Routines.
      @enum epD4 Delphi 4
      @enum epD5 Delphi 5
      @enum epD6 Delphi 6
      @enum epD7 Delphi 7
      @enum epBCB4 C++ Builder 4
      @enum epBCB5 C++ Builder 5
      @enum epBCB6 C++ Builder 6
      @enum epUnknown Unknown Compiler Platform
     }
     TESBPlatform = (epD4, epD5, epd6, epd7, epBCB4, epBCB5, epBCB6, epUnknown);

type
     {: Hypothesis Comparison Types.
      @enum ahtLessThan Less Than Comparison
      @enum ahtGreaterThan Greater Than Comparison
      @enum ahtInequality Not Equal to Comparison
     }
     TESBAltHypType = (ahtLessThan, ahtGreaterThan, ahtInequality);

const
     //: String displayed to indicate the Default Directory & File
     DefaultStr = '%Default%';
     //: String displayed to indicate the Application Directory
     AppDirStr = '%AppDir%\';

type
     {: Separator Style - currently used for Thousand Separators.
      @enum essNone Don't use the Separator
      @enum essExit Fix Separators on Exit from control or when value assigned.
      @enum essAuto Automatically fix the separators during typing
     }
     TESBSeparatorStyle = (essNone, essExit, essAuto);

type
     {: Days of the week Type used for Calendars & Date/Time Calculations }
     TESBDayOfWeek = (dowSunday, dowMonday, dowTuesday, dowWednesday,
          dowThursday, dowFriday, dowSaturday);

     {: Set of Days of the week Type used for Calendars & Date/Time Calculations }
     TESBDOWSet = set of TESBDayOfWeek;

type
     {: Months used for Calendars & Date/Time Calculations }
     TESBMonth = (mthJanuary, mthFebruary, mthMarch, mthApril, mthMay, mthJune,
          mthJuly, mthAugust, mthSeptember, mthOctober, mthNovember, mthDecember);

     {: Set of Months Type used for Calendars & Date/Time Calculations }
     TESBMonthSet = set of TESBMonth;

const
     {: Star Sign Descriptions }
     ESBStarSigns: array [1..12] of string = (rsAquarius, rsPisces, rsAries,
          rsTaurus, rsGemini, rsCancer, rsLeo, rsVirgo, rsLibra, rsScorpio,
          rsSagittarius, rsCapricorn);

type
     {: Represents a Geographical Postion in Latitude and Longitude.
      Both are measured in degrees.<p>
      -90 <= Latitude <= 90, with Positive implying North, Negative implying South.<p>
      -180 <= Longitude <= 180, with Positive implying East, Negative implying West.<p>
      Latitude of 0 is the Equator, 90 is the North Pole, -90 is the South Pole.<p>
      Longitude of 0 is the Greenwich meridian, 180 = -180 and is known as the DateLine.
     }
     TESBPosition = record
          Latitude: Extended;
          Longitude: Extended;
     end;

type
     {: Stores the Astronomical Julian Day Number and represents the number of
      days since Midday, 1 January 4713 BC - measured at Greenwich Meridian.<p>
      Note: Julian Days start at Midday rather than Midnight.<p>
     }
     TESBJulian = Double;

const
     {: Stores the Date the Gregorian Calendar started on: 15 October 1582 }
     GregorianStart: TDateTime = -115858.0;

var
     ESBDegreeStr: string = '°';
     ESBMinuteStr: string = '’';
     ESBSecondStr: string = '’’';

type
     {: Available Time Types
      @enum ettUT Universal Time - Similar to GMT or UTC - related to the
       motion of the Sun as observed at 0° Longitude
      @enum ettGST Greenwich Mean Sidereal Time - basically Time recorded by
       the Stars rather than the sun at 0° Longitude
      @enum ettLST Local Sidereal Time - basically Time recorded by
       the Stars rather than the sun at a given Longitude
     }
     TESBTimeType = (ettUT, ettGST, ettLST);

     {: Stores Time Information }
     TESBTime = record
          Date: Integer;
          Hours: Integer;
          Minutes: Integer;
          Seconds: Double;
          Longitude: Double;
          TimeType: TESBTimeType
     end;

const
     //: North Galactic Pole Co-ordinates - Ascension - in Degrees
     GalacticAscension = 192.25;

     //: North Galactic Pole Co-ordinates - Declination - in Degrees
     GalacticDeclination = 27.4;

     //: Ascending Node of Galatic Plane on Equator - in Degrees
     GalacticAscNode = 33;

const
     //: Semi Major Axis of the Sun in Kilometres
     Sun_SemiMajorAxis = 1.495985E8;

     //: Angular Diameter of the Sun when Distance from the Sun = Semi Major Axis - in Degrees
     Sun_Theta0 = 0.533128;

var
     //: Geocentric Parallax in relation to the Sun
     Sun_Parallax: Extended = 8.79 / 3600; // 8.79''
     //: Atmospheric Refraction in relation to the Sun
     Sun_Refraction: Extended = 34 / 60; // 34'

implementation

end.
