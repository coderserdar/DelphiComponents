{: Contains extra Global Constants, Types & Variables
 that are used by the ESBPCS for CLX.

 This is designed to work in Borland Delphi 6 CLX and above, Borland
 C++ Builder 6 CLX and above, and Borland Kylix 2 and above.
 Most if not all features will work in Kylix 1 but it is not currently supported.<p>

 This supplies the various Constants, Data Types and Global
 Variables that are dependent on types from Graphics and other
 units that have "overhead". See <See Unit=ESBPCSGlobals> for the rest
 of them.<p>

 Copyright © 1999-2002 ESB Consultancy<p>

 v2.3 - 14 September 2002
}

unit QESBPCSGlobals2;

{$I esbpcs.inc}

interface

uses
     QGraphics, SysUtils;

const
     clSteel = $CCBDB5;
     clPaleBlue = $FFFFF0;

const
     //: Enhanced Default Read Only Color for Controls.
     DefROColor = clSteel;

     //: Enhanced Default Read Write Color for Controls.
     DefRWColor = clBase;

     //: Enhanced Default Read Only Color for Controls.
     DefDisabledColor = clBtnFace;

     //: Enhanced Default Read Only Color for Controls.
     DefDisabledFontColor = clSilver;

     //: Enhanced Default Color for Headings.
     DefHeadingColor = clHighlight;

     //: Enhanced Default Color for Captions.
     DefCaptionColor = clText;

     //: Enhanced Default On Focus Color for Flat Controls.
     DefFocusColor = clYellow;

     //: Enhanced Default On Border Focus Color for Flat Controls.
     DefBorderFocusColor = clActiveDark;

     //: Enhanced Default On Border Non-Focus Color for Flat Controls.
     DefBorderNonFocusColor = clBase;

     //: Enhanced Default On List Color for Owner Drawn Comboboxes.
     DefCBListColor = clAqua;

     //: Enhanced Default On List Font Color for Owner Drawn Comboboxes.
     DefCBListFontColor = clNavy;

     //: Enhanced Default On List Highlight Color for Owner Drawn Comboboxes.
     DefCBHighColor = clRed;

     //: Enhanced Default On List Highlight Font Color for Owner Drawn Comboboxes.
     DefCBHighFontColor = clWhite;

     //: Enhanced Default to use for Font Color of Positive Values.
     DefPosFontColor = clNavy;

     //: Enhanced Default to use for Font Color of Negative Values.
     DefNegFontColor = clRed;

     //: Enhanced Default to use for Line Drawing.
     DefLineColor = clText;

     //: Enhanced Default to use for Frame Drawing.
     DefFrameColor = clActiveDark;

     //: Enhanced Default to use for Checks & Radio Button Captions when Read only
     DefItemROColor = clText;

     //: Enhanced Default to use for Checks & Radio Button Captions when Focused or Hot
     DefItemFocusColor = clText;

     //: Enhanced Default to use for Checks & Radio Button Captions when Read Write
     DefItemRWColor = clText;

     //: Enhanced Default to use for Checks & Radio Buttons
     DefMarkColor = clText;

     //: Enhanced Default to use for Read Only Checks & Radio Buttons
     DefROMarkColor = clBackground;

     //: Enhanced Default to use for Disabled Checks & Radio Buttons
     DefDisMarkColor = clBase;

     //: Enhanced Default to use for Null CheckBoxes
     DefNullColor = clBtnFace;

     //: Enhanced Default to use for Day Color in Calendars
     DefDayColor = clBase;

     //: Enhanced Default to use for WeekEnd Color in Calendars
     DefWeekEndColor = clInfoBk;

     //: Enhanced Default to use for Week No Color in Calendars
     DefWeekNoColor = DefROColor;

     //: Enhanced Default to use for Day Font Color in Calendars
     DefDayFontColor = clText;

     //: Enhanced Default to use for Week No Font Color in Calendars
     DefWeekNoFontColor = clText;

     //: Enhanced Default to use for Column Heading Font Color in Calendars
     DefCalHeadingFontColor = clText;

     //: Enhanced Default to use for Font Color of days in Other Months in Calendars
     DefOtherMonthColor = clInactiveCaptionText;

     //: Enhanced Default to use for Font Color of Selected Day in Calendars
     DefDateSelFontColor = clHighlightText;

     //: Enhanced Default to use for Color of Selected Day in Calendars
     DefDateSelColor = clHighlight;

     //: Enhanced Default to use for Line Drawing in Calendars.
     DefCalLineColor = clText;

     //: Enhanced Default to use for Special Day Color in Calendars
     DefSpecialDayColor = clTeal;

     //: Enhanced Default to use for Special Day Font Color in Calendars
     DefSpecialDayFontColor = clBlack;

     //: Enhanced Default to use for Border Size in Pixels.
     DefBorderSize = 2;

const
     //: Conservative Default Read Only Color for Controls.
     DefROColor2 = clBtnFace;

     //: Conservative Default Read Write Color for Controls.
     DefRWColor2 = clBase;

     //: Conservative Default Color for Headings.
     DefHeadingColor2 = clText;

     //: Conservative Default Color for Captions.
     DefCaptionColors = clText;

     //: Conservative Default Read Only Color for Controls.
     DefDisabledColor2 = clBtnFace;

     //: Conservative Default Read Only Color for Controls.
     DefDisabledFontColor2 = clText;

     //: Conservative Default On Focus Color for Flat Controls.
     DefFocusColor2 = clBase;

     //: Conservative Default On Border Focus Color for Flat Controls.
     DefBorderFocusColor2 = clText;

     //: Conservative Default On Border Non-Focus Color for Flat Controls.
     DefBorderNonFocusColor2 = clBase;

     //: Conservative Default On List Color for Owner Drawn Comboboxes.
     DefCBListColor2 = clBase;

     //: Conservative Default On List Font Color for Owner Drawn Comboboxes.
     DefCBListFontColor2 = clText;

     //: Conservative Default On List Highlight Color for Owner Drawn Comboboxes.
     DefCBHighColor2 = clHighlight;

     //: Conservative Default On List Highlight Font Color for Owner Drawn Comboboxes.
     DefCBHighFontColor2 = clHighLightText;

     //: Conservative Default to use for Font Color of Positive Values.
     DefPosFontColor2 = clText;

     //: Conservative Default to use for Font Color of Negative Values.
     DefNegFontColor2 = clText;

     //: Conservative Default to use for Line Drawing.
     DefLineColor2 = clText;

     //: Conservative Default to use for Frame Drawing.
     DefFrameColor2 = clActiveDark;

     //: Conservative Default to use for Checks & Radio Button Captions when Read only
     DefItemROColor2 = clText;

     //: Conservative Default to use for Checks & Radio Button Captions when Focused or Hot
     DefItemFocusColor2 = clText;

     //: Conservative Default to use for Checks & Radio Button Captions when Read Write
     DefItemRWColor2 = clText;

     //: Conservative Default to use for Checks & Radio Buttons
     DefMarkColor2 = clText;

     //: Conservative Default to use for Read Only Checks & Radio Buttons
     DefROMarkColor2 = clText;

     //: Conservative Default to use for Disabled Checks & Radio Buttons
     DefDisMarkColor2 = clBase;

     //: Conservative Default to use for Null CheckBoxes
     DefNullColor2 = clBtnFace;

     //: Conservative Default to use for Day Color in Calendars
     DefDayColor2 = clBase;

     //: Conservative Default to use for WeekEnd Color in Calendars
     DefWeekEndColor2 = clBase;

     //: Conservative Default to use for Week No Color in Calendars
     DefWeekNoColor2 = clBase;

     //: Conservative Default to use for Day Font Color in Calendars
     DefDayFontColor2 = clText;

     //: Conservative Default to use for Week No Font Color in Calendars
     DefWeekNoFontColor2 = clText;

     //: Conservative Default to use for Column Heading Font Color in Calendars
     DefCalHeadingFontColor2 = clText;

     //: Conservative Default to use for Font Color of days in Other Months in Calendars
     DefOtherMonthColor2 = clInactiveCaptionText;

     //: Conservative Default to use for Font Color of Selected Day in Calendars
     DefDateSelFontColor2 = clHighlightText;

     //: Conservative Default to use for Color of Selected Day in Calendars
     DefDateSelColor2 = clHighlight;

     //: Conservative Default to use for Line Drawing in Calendars.
     DefCalLineColor2 = clText;

     //: Conservative Default to use for Special Day Color in Calendars
     DefSpecialDayColor2 = clBase;

     //: Conservative Default to use for Special Day Font Color in Calendars
     DefSpecialDayFontColor2 = clHighlight;

     //: Conservative Default to use for Border Size in Pixels.
     DefBorderSize2 = 2;

var
     //: Current Read Only Color for Controls.
     ESBROColor: TColor = DefROColor;

     //: Current Read Write Color for Controls.
     ESBRWColor: TColor = DefRWColor;

     //: Current Disabled Color for Controls.
     ESBDisabledColor: TColor = DefDisabledColor;

     //: Current Disabled Font Color for Controls.
     ESBDisabledFontColor: TColor = DefDisabledFontColor;

     //: Current Color for Headings.
     ESBHeadingColor: TColor = DefHeadingColor;

     //: Current Color for Captions.
     ESBCaptionColor: TColor = DefCaptionColor;

     //: Current Color for Focused Flat Controls.
     ESBFocusColor: TColor = DefFocusColor2;

     //: Current Color for Border of Focused Flat Controls.
     ESBBorderFocusColor: TColor = DefBorderFocusColor;

     //: Current Color for Border of NonFocused Flat Controls.
     ESBBorderNonFocusColor: TColor = DefBorderNonFocusColor;

     //: Current Color for List Color for Owner Drawn Comboboxes.
     ESBCBListColor: TColor = DefCBListColor2;

     //: Current Color for List Font Color for Owner Drawn Comboboxes.
     ESBCBListFontColor: TColor = DefCBListFontColor2;

     //: Current Color for List Highlight Color for Owner Drawn Comboboxes.
     ESBCBHighColor: TColor = DefCBHighColor2;

     //: Current Color for List Highlight Font Color for Owner Drawn Comboboxes.
     ESBCBHighFontColor: TColor = DefCBHighFontColor2;

     //: Current Font Color of Positive Values.
     ESBPosFontColor: TColor = DefPosFontColor2;

     //: Current Font Color of Negative Values.
     ESBNegFontColor: TColor = DefNegFontColor2;

     //: Current Color to use for Line Drawing
     ESBLineColor: TColor = DefLineColor;

     //: Current Color to use for Frame Drawing
     ESBFrameColor: TColor = DefFrameColor;

     //: Current Color to use for Checks & Radio Button Captions when Read only
     ESBItemROColor: TColor = DefItemROColor;

     //: Current Color to use for Checks & Radio Button Captions when Focused or Hot
     ESBItemFocusColor: TColor = DefItemFocusColor;

     //: Current Color to use for Checks & Radio Button Captions when Read Write
     ESBItemRWColor: TColor = DefItemRWColor;

     //: Current Color to use for Checks & Radio Buttons
     ESBMarkColor: TColor = DefMarkColor;

     //: Current Color to use for ReadOnly Checks & Radio Buttons
     ESBROMarkColor: TColor = DefROMarkColor;

     //: Current Color to use for Disabled Checks & Radio Buttons
     ESBDisMarkColor: TColor = DefDisMarkColor;

     //: Current Color to use for Null CheckBoxes
     ESBNullColor: TColor = DefNullColor;

     //: Current Color to use for Days in Calendars
     ESBDayColor: TColor = DefDayColor;

     //: Current Color to use for WeekEnds in Calendars
     ESBWeekEndColor: TColor = DefWeekEndColor;

     //: Current Color to use for Week Nos in Calendars
     ESBWeekNoColor: TColor = DefWeekNoColor;

     //: Current Color to use for Day Font Color in Calendars
     ESBDayFontColor: TColor = DefDayFontColor;

     //: Current Color to use for Week No Font Color in Calendars
     ESBWeekNoFontColor: TColor = DefWeekNoFontColor;

     //: Current Color to use for Column Heading Font Color in Calendars
     ESBCalHeadingFontColor: TColor = DefCalHeadingFontColor;

     //: Current Color to use for Font Color of days in Other Months in Calendars
     ESBOtherMonthColor: TColor = DefOtherMonthColor;

     //: Current Color to use for Font Color of Selected Day in Calendars
     ESBDateSelFontColor: TColor = DefDateSelFontColor;

     //: Current Color to use for Color of Selected Day in Calendars
     ESBDateSelColor: TColor = DefDateSelColor;

     //: Current Color to use for Line Drawing in Calendars.
     ESBCalLineColor: TColor = DefCalLineColor;

     //: Current Color to use for Special Days in Calendars
     ESBSpecialDayColor: TColor = DefSpecialDayColor;

     //: Current Color to use for Special Day Font Color in Calendars
     ESBSpecialDayFontColor: TColor = DefSpecialDayFontColor;

     //: Current Number of Pixels to use for Border Size.
     ESBBorderSize: Integer = DefBorderSize;

type
     {: Identifies how the Currency Format is constucted. Used together
      with <See Var=ESBNegativeFormatType>.

      @enum cftSymVal Currency Symbol then Currency Value.
      @enum cftValSym Currency Value then Currency Symbol.
      @enum cftSymSpaceVal Currency Symbol then Currency Value,
       separated by a space.
      @enum cftValSpaceSym Currency Value then Currency Symbol,
       separated by a space.
     }
     TCurrencyFormatType = (cftSymVal, cftValSym, cftSymSpaceVal, cftValSpaceSym);

     {: Identifies how the Currency Format is constucted, when Negatives
      are involved. Used together with <See Var=ESBCurrencyFormatType>.

      @enum nftBrackets Negative values are represented by
       the currency (value & symbol if present) being
       enclosed in parentheses '(' ')'.
      @enum nftFront 'Minus Sign' appears at the front, as first character.
      @enum nftBack 'Minus Sign' appears at the back, as last character.
      @enum nftFrontAfter 'Minus Sign' appears at the front but after the
       Currency Symbol.
      @enum nftBackBefore 'Minus Sign' appears at the back, but before the
       Currency Symbol.
     }
     TNegativeFormatType = (nftBrackets, nftFront, nftBack, nftFrontAfter,
          nftBackBefore);
var
     { Current value for Format of Currency. Must be used with
      <See Var=ESBNegativeFormatType>.
      cftSymVal - Currency Symbol then Currency Value.<p>
      cftValSym - Currency Value then Currency Symbol.<p>
      cftSymSpaceVal - Currency Symbol then Currency Value,
       separated by a space.<p>
      cftValSpaceSym - Currency Value then Currency Symbol,
       separated by a space.
     }
     ESBCurrencyFormatType: TCurrencyFormatType;
     { Current value for Negative Format of Currency. Must be used with
      <See Var=ESBCurrencyFormatType>.
      nftBrackets - Negative values are represented by
       the currency (value & symbol if present) being
       enclosed in parentheses '(' ')'.<p>
      nftFront - 'Minus Sign' appears at the front, as first character.<p>
      nftBack - 'Minus Sign' appears at the back, as last character.<p>
      nftFrontAfter - 'Minus Sign' appears at the front but after the
       Currency Symbol.<p>
      nftBackBefore - 'Minus Sign' appears at the back, but before the
       Currency Symbol.<p>
      }
     ESBNegativeFormatType: TNegativeFormatType;
     { Controls whether the <B>CurrencyString</B> is displayed in Currency Conversions. }
     ESBUseCurrencySymbol: Boolean = False;

type
     //: Exception used when a Field of inappropriate type is assigned.
     EInvalidFieldType = class (Exception);

type
     {: Type of Time Zone Format to display.
     @enum etfKeyNames Display the Key Name for each Time Zone from the Registry.
     @enum etfDisplayNames Display the Display Name for each Time Zone from the Registry.
     }
     TESBTZFormatType = (etfKeyNames, etfDisplayNames);

const
     {: English Days of the Week - used in RFC822 conversions. }
     DOWStrs: array [1..7] of string = ('Sunday', 'Monday', 'Tuesday',
          'Wednesday', 'Thursday', 'Friday', 'Saturday');

     {: English Month Names - used in RFC822 conversions. }
     MonthStrs: array [1..12] of string = ('January', 'February', 'March',
          'April', 'May', 'June', 'July', 'August', 'September',
          'October', 'November', 'December');

type
     {: Event called to identify what Colors a Component within a Group Component
      should be. Allows you to define the Read/Write Color, the Read Only Color
       and the On Focus Color. }
     TESBItemColorEvent = procedure (Sender: TObject;
          const ElementNo: Longword; var ColorRW, ColorRO, ColorFocus: TColor) of object;

implementation

initialization
     //: Get the Currency Format from the Regional Settings
     ESBCurrencyFormatType := TCurrencyFormatType (CurrencyFormat);

     //: Get the Negative Format from the Regional Settings
     case NegCurrFormat of
          0: ESBNegativeFormatType := nftBrackets;
          1: ESBNegativeFormatType := nftFront;
          2: ESBNegativeFormatType := nftFrontAfter;
          3: ESBNegativeFormatType := nftBack;
          4: ESBNegativeFormatType := nftBrackets;
          5: ESBNegativeFormatType := nftFront;
          6: ESBNegativeFormatType := nftBackBefore;
          7: ESBNegativeFormatType := nftBack;
          8: ESBNegativeFormatType := nftFront;
          9: ESBNegativeFormatType := nftFront;
          10: ESBNegativeFormatType := nftBack;
          11: ESBNegativeFormatType := nftBack;
          12: ESBNegativeFormatType := nftFrontAfter;
          13: ESBNegativeFormatType := nftBackBefore;
          14: ESBNegativeFormatType := nftBrackets;
          15: ESBNegativeFormatType := nftBrackets;
     else
          ESBNegativeFormatType := nftFront;
     end;

end.
