{: Contains the Data Type Conversions and String Routines used by ESBPCS for CLX.

 This is designed to work in Borland Delphi 6 CLX and above, Borland
 C++ Builder 6 CLX and above, and Borland Kylix 2 and above.
 Most if not all features will work in Kylix 1 but it is not currently supported.<p>

 This unit contains many useful String and conversion routines that
 are used throughout ESBPCS. Routines use const parameters were possible.<p>

 Copyright © 1999-2002 ESB Consultancy<p>

 v2.3 - 14 September 2002
}
unit QESBPCSConvert;

{$I esbpcs.inc}

interface

uses
     QESBPCSGlobals;

{--- Extra String Operations ---}

{: Ansi version of the standard UpCase.
 @param Ch Character to be converted to UpCase.
 @cat ExtraStrings
}
function AnsiUpCase (const Ch: Char): Char;

{: Returns the first Position of given Character. Can optionally have a starting
 point.
 @param Ch Character to be searched for
 @param S String to Search within
 @param Start Character Position at which to start. If Start < 1 then 1 is used.
  If Start > Length (S) then 0 is returned.
 @Result The Position of the Character, otherwise 0 is returned.
 @cat ExtraStrings
}
function ESBPosCh (const Ch: Char; const S: string; Start: Integer = 1): Integer;

{: Returns the Last Position of given Character, processing from the end of the string.
 Can optionally have a starting point and the search proceeds to the beginning
 of the string from that Start position.
 @param Ch Character to be searched for
 @param S String to Search within
 @param Start Character Position at which to start. If Start < 1 then Length (S) is used.
  If Start > Length (S) then 0 is returned.
 @Result The Position of the Character, otherwise 0 is returned.
 @cat ExtraStrings
}
function ESBLastPosCh (const Ch: Char; const S: string; Start: Integer = 0): Integer;

{: Returns the Position of the n'th occurrence  of given Character. Can
 optionally have a starting point.
 @param Ch Character to be searched for
 @param S String to Search within
 @param N is the Occurrence that is being looked for. If N < 1 then 0 is returned.
 @param Start Character Position at which to start. If Start < 1 then 1 is used.
  If Start > Length (S) then 0 is returned.
 @Result The Position of the Character, otherwise 0 is returned.
 @cat ExtraStrings
}
function ESBPosNCh (const Ch: Char; const S: string; const N: Integer;
     Start: Integer = 1): Integer;

{: Returns a string with specified characters added to the beginning and
 end of the string to in effect centre the string within the
 given length. If even amounts of Ch cannot be put on both side, the
 extra Ch will be on the right side.
 Also See: <See Routine=CentreStr>
 @param S the string to be centred. If Length (S) >= Len then NO padding
  occurs, and S is returned.
 @param Ch the character to Pad with.
 @param Len the Length of returned string.
 @cat ExtraStrings
}
function CentreChStr (const S: string; const Ch: Char;
     const Len: LongWord): string;

{: Returns a string with blank spaces added to the beginning and
 end of the string to in effect centre the string within the
 given length. If even amounts of blanks cannot be put on both side, the
 extra blank will be on the right side.
 Also See: <See Routine=CentreChStr>
 @param S the String to be centred. If Length (S) >= Len then NO padding
  occurs, and S is returned.
 @param Len the Length of returned string.
 @cat ExtraStrings
}
function CentreStr (const S: string; const Len: LongWord): string;

{: Returns Proper String - each word Capitalized - uses Ansi Capitalisation.
     Words are seen as being delimited by <See Var=WordSepSet> which you can alter.
 @param S the String to have its case adjusted.
 @cat ExtraStrings
}
function ESBProperStr (const S: string): string;

{: Returns a string filled with the given character.
 Also See: <See Routine=BlankStr>
 @param Ch the Character to use for filling.
 @param N the length of the resultant string.
 @cat ExtraStrings
}
function FillStr (const Ch: Char; const N: LongWord): string;

{: Returns a string composed of blank spaces (ie #32).
 Also See: <See Routine=FillStr>
 @param N the length of the resultant string, ie the number of blanks.
 @cat ExtraStrings
}
function BlankStr (const N: LongWord): string;

{: Returns a string composed of dashes (minus signs).
 Also See: <See Routine=FillStr>
 @param N the length of the resultant string, ie the number of dashes.
 @cat ExtraStrings
}
function DashStr (const N: LongWord): string;

{: Returns a string composed of stars (asterisks).
 Also See: <See Routine=FillStr>
 @param N the length of the resultant string, ie the number of stars.
 @cat ExtraStrings
}
function StarStr (const N: LongWord): string;

{: Returns a string composed of Underscores (Shift-Minus).
 Also See: <See Routine=FillStr>
 @param N the length of the resultant string, ie the number of underscores.
 @cat ExtraStrings
}
function UnderscoreStr (const N: LongWord): string;

{: Returns a string of Length N with blank spaces added to the <B>end</B>
 of the string if S is too short, or returning the N Left-most
 characters of S if S is too long.
 Also See: <See Routine=PadLeftStr>, <See Routine=RightAlignStr>
 @param S the String to be Left Aligned.
 @param N the length of the resultant string.
 @cat ExtraStrings
 }
function LeftAlignStr (const S: string; const N: LongWord): string;

{: Returns the substring consisting of the first N characters of S.
 Also See: <See Routine=LeftAlignStr>, <See Routine=RightStr>
 @param S the String from which the left substring is to be taken.
 @param N the length of the resultant substring. If Length (S) < N then
  S is returned.
 @cat ExtraStrings
}
function LeftStr (const S: string; const N: LongWord): string;

{: Returns the substring consisting of the characters from S
 up to but not including the specified one.  If the specified
 character is not found then a null string is returned.
 Also See: <See Routine=LeftStr>, <See Routine=RightAfterChStr>, <See Routine=RightTillChStr>
 @param S the String from which the left substring is to be taken.
 @param Ch the character that is searched for.
 @cat ExtraStrings
}
function LeftTillChStr (const S: string; const Ch: Char): string;

{: Returns the substring consisting of the right most characters from S
 up to but not including the specified one.  If the specified
 character is not found then a null string is returned.
 Also See: <See Routine=LeftStr>, <See Routine=RightAfterChStr>
 @param S the String from which the right substring is to be taken.
 @param Ch the character that is searched for.
 @cat ExtraStrings
}
function RightTillChStr (const S: string; const Ch: Char): string;

{: Returns a string with blank spaces added to the beginning of the
 string until the string is of the given length.
 Also See: <See Routine=PadLeftChStr>, <See Routine=PadRightStr>, <See Routine=LeftAlignStr>
 @param S the String to pad.
 @param Len the length of the resultant substring. If Length (S) >= Len then
  NO padding occurs, and S is returned.
 @cat ExtraStrings
}
function PadLeftStr (const S: string; const Len: LongWord): string;

{: Returns a string with the specified character added to the beginning of the
 string until the string is of the given length.
 Also See: <See Routine=PadLeftStr>, <See Routine=PadChRightStr>
 @param S the String to pad.
 @param Ch the character to use for padding
 @param Len the length of the resultant substring. If Length (S) >= Len then
  NO padding occurs, and S is returned.
 @cat ExtraStrings
}
function PadChLeftStr (const S: string; const Ch: Char;
     const Len: LongWord): string;

{: Returns a string with the specified character added to the beginning of the
 string until the string is of the given length.
 Also See: <See Routine=PadRightStr>, <See Routine=PadChLeftStr>
 @param S the String to pad.
 @param Ch the character to use for padding
 @param Len the length of the resultant substring. If Length (S) >= Len then
  NO padding occurs, and S is returned.
 @cat ExtraStrings
}
function PadChRightStr (const S: string; const Ch: Char;
     const Len: LongWord): string;

{: Returns a string with blank spaces added to the beginning of the
 string until the string is of the given length.
 Also See: <See Routine=PadLeftStr>, <See Routine=PadChRightStr>, <See Routine=RightAlignStr>
 @param S the String to pad.
 @param Len the length of the resultant substring. If Length (S) >= Len then
  NO padding occurs, and S is returned.
 @cat ExtraStrings
}
function PadRightStr (const S: string; const Len: LongWord): string;

{: Returns a string of Length N with blank spaces added to the <B>Beginning</B>
 of the string if S is too short, or returning the N left-most
 characters of S if S is too long.
 Also See: <See Routine=PadRightStr>, <See Routine=LeftAlignStr>
 @param S the String to be Right Aligned.
 @param N the length of the resultant string.
 @cat ExtraStrings
 }
function RightAlignStr (const S: string; const N: LongWord): string;

{: Returns the substring consisting of the last N characters of S.
 Also See: <See Routine=LeftStr>, <See Routine=RightAfterStr>
 @param S the String from which the right substring is to be taken.
 @param N the length of the resultant substring. If Length (S) < N then
  S is returned.
 @cat ExtraStrings
}
function RightStr (const S: string; const N: LongWord): string;

{: Returns the substring consisting of the remaining characters
 <b>after</b> the first N Characters of S.
 Also See: <See Routine=RightStr>, <See Routine=RightAfterChStr>, <See Routine=LeftStr>
 @param S the String from which the right substring is to be taken.
 @param N the number of leading characters to skip. If Length (S) < N then
  a null string is returned.
 @cat ExtraStrings
}
function RightAfterStr (const S: string; const N: LongWord): string;

{: Returns the substring consisting of the characters from S
 after but not including the specified one.  If the specified
 character is not found then a null string is returned.
 Also See: <See Routine=RightAfterStr>, <See Routine=LeftTillChStr>, <See Routine=RightTillChStr>
 @param S the String from which the right substring is to be taken.
 @param Ch the character that is searched for.
 @cat ExtraStrings
}
function RightAfterChStr (const S: string; const Ch: Char): string;

{: Returns the String with all specified trailing characters removed.
 Also See: <See Routine=StripLChStr>, <See Routine=StripChStr>
 @param S the String from which the characters are to be removed.
 @param Ch the character that is to be stripped off.
 @param Chars alternatively can pass a set of Characters to remove.
 @cat ExtraStrings
}
function StripTChStr (const S: string; const Ch: Char): string; overload;
function StripTChStr (const S: string; const Chars: TESBCharSet): string; overload;

{: Returns the String with all specified leading characters removed.
 Also See: <See Routine=StripTChStr>, <See Routine=StripChStr>, <See Routine=StripChFromStr>
 @param S the String from which the characters are to be removed.
 @param Ch the character that is to be stripped off.
 @param Chars alternatively can pass a set of Characters to remove.
 @cat ExtraStrings
}
function StripLChStr (const S: string; const Ch: Char): string; overload;
function StripLChStr (const S: string; const Chars: TESBCharSet): string; overload;

{: Returns the String with all specified leading & trailing characters removed.
 Also See: <See Routine=StripLChStr>, <See Routine=StripTChStr>, <See Routine=StripChFromStr>
 @param S the String from which the characters are to be removed.
 @param Ch the character that is to be stripped off.
 @param Chars alternatively can pass a set of Characters to remove.
 @cat ExtraStrings
}
function StripChStr (const S: string; const Ch: Char): string; overload;
function StripChStr (const S: string; const Chars: TESBCharSet): string; overload;

{: Returns the String with all occurrences of OldCh character
 replaced with NewCh character.
 @param S the String to process.
 @param OldCh the character to look for.
 @param NewCh the character to replace with.
 @cat ExtraStrings
}
function ReplaceChStr (const S: string; const OldCh, NewCh: Char): string;

{: Returns a String with all occurrences of TAB (ie #9) replaced with
 the contents of <See Var=ESBTabStr>.
 @param S the String to process.
 @cat ExtraStrings
}
function DisplayTabsInString (const S: string): string;

{: Returns a String with all occurrences of the given character removed.
 Also see: <See Routine=StripChStr> , <See Routine=StripTChStr>, <See Routine=StripLChStr>
 @param S the String to process.
 @param Ch the character to remove.
 @param Chars alternatively can pass a set of Characters to remove.
 @cat ExtraStrings
}
function StripChFromStr (const S: string; const Ch: Char): string; overload;
function StripChFromStr (const S: string; const Chars: TESBCharSet): string; overload;

{---  Convert string to Integers ---}

{: Returns a string with all occurrences of the Thousands Separator
 as defined in the regional settings removed. This routine is used
 with string to Integer/Float operations to handle "formatted" strings.
 @param S the String to process
 @cat StringIntConv
 @cat StringFloatConv
}
function StripThousandSeparators (const S: string): string;

{: Converts a string into a Int64. Removes Thousand Separators if they
 are present as well as any leading or trailing white spaces (ie <= #32).
 Non-numeric will return 0.
 @param S the String to process
 @cat StringIntConv
}
function Str2Int64 (const S: string): Int64;

{: Converts a string into a LongInt. Removes Thousand Separators if they
 are present as well as any leading or trailing  white spaces (ie <= #32).
 If Number is Valid but out of Range then High (LongInt) will be
 returned for a greater value and Low (LongInt) for a lesser value.
 Non-numeric will return 0.
 @param S the String to process
 @cat StringIntConv
}
function Str2LInt (const S: string): LongInt;

{: Converts a string into an Integer. Removes Thousand Separators if they
 are present as well as any leading or trailing  white spaces (ie <= #32).
 If Number is Valid but out of Range then High (Integer) will be
 returned for a greater value and Low (Integer) for a lesser value.
 Non-numeric will return 0.
 @param S the String to process
 @cat StringIntConv
}
function Str2Int (const S: string): Integer;

{: Converts a string into a SmallInt. Removes Thousand Separators if they
 are present as well as any leading or trailing  white spaces (ie <= #32).
 If Number is Valid but out of Range then High (SmallInt) will be
 returned for a greater value and Low (SmallInt) for a lesser value.
 Non-numeric will return 0.
 @param S the String to process
 @cat StringIntConv
}
function Str2SmallInt (const S: string): SmallInt;

{: Converts a string into a ShortInt. Removes Thousand Separators if they
 are present as well as any leading or trailing  white spaces (ie <= #32).
 If Number is Valid but out of Range then High (ShortInt) will be
 returned for a greater value and Low (ShortInt) for a lesser value.
 Non-numeric will return 0.
 @param S the String to process
 @cat StringIntConv
}
function Str2SInt (const S: string): ShortInt;

{: Converts a string into a LongWord. Removes Thousand Separators if they
 are present as well as any leading or trailing  white spaces (ie <= #32).
 If Number is Valid but out of Range then High (LongWord) will be
 returned for a greater value and 0 for a lesser value.
 Non-numeric will return 0.
 @param S the String to process
 @cat StringIntConv
}
function Str2LWord (const S: string): LongWord;

{: Converts a string into a Cardinal. Removes Thousand Separators if they
 are present as well as any leading or trailing  white spaces (ie <= #32).
 If Number is Valid but out of Range then High (Cardinal) will be
 returned for a greater value and 0 for a lesser value.
 Non-numeric will return 0.
 @param S the String to process
 @cat StringIntConv
}
function Str2Cardinal (const S: string): Cardinal;

{: Converts a string into a Word. Removes Thousand Separators if they
 are present as well as any leading or trailing  white spaces (ie <= #32).
 If Number is Valid but out of Range then High (Word) will be
 returned for a greater value and 0 for a lesser value.
 Non-numeric will return 0.
 @param S the String to process
 @cat StringIntConv
}
function Str2Word (const S: string): Word;

{: Converts a string into a Byte. Removes Thousand Separators if they
 are present as well as any leading or trailing  white spaces (ie <= #32).
 If Number is Valid but out of Range then High (Byte) will be
 returned for a greater value and 0 for a lesser value.
 Non-numeric will return 0.
 @param S the String to process
 @cat StringIntConv
}
function Str2Byte (const S: string): Byte;

{: Converts a Hexadecimal (string) into a LongWord. Removes any leading or
 trailing  white spaces (ie <= #32). Initial '$' not required but acceptable.
 If Number is Valid but out of Range then High (LongWord) will be
 returned for a greater value and 0 for a lesser value.
 Non-hexadecimal will return 0.
 @param S the String to process
 @cat StringIntConv
}
function Hex2LWord (const S: string): LongWord;

{: Converts a Hexadecimal (string) into an Integer. Removes any leading or
 trailing  white spaces (ie <= #32). Initial '$' not required but acceptable.
 Non-hexadecimal will return 0. Values > '7FFF FFFF' will return as negatives.
 @param S the String to process
 @cat StringIntConv
}
function Hex2Int (const S: string): Integer;

{: Converts a Hexadecimal (string) into a LongWord. Removes any leading or
 trailing  white spaces (ie <= #32). Initial '$' not required but acceptable.
 Non-hexadecimal will return 0.
 @param S the String to process
 @cat StringIntConv
}
function Hex2Int64 (const S: string): Int64;

{---  Convert Integers to strings ---}

{: Converts an Integer into a string without Padding. <See Var=ESBNumPosSign>
 controls whether a '+' Sign appears at the beginning for positive Integers.
 <See Var=ESBBlankWhenZero> can be set to True to have Zero returned as an
 Empty string.
 @param L Value to Convert to String.
 @cat StringIntConv
 }
function Int2EStr (const L: LongInt): string; overload;
function Int2EStr (const L: Int64): string; overload;

{: Converts an Integer into a Hexadecimal (string) without Padding.
 <See Var=ESBBlankWhenZero> can be set to True to have Zero returned as an
 Empty string.
 @param L Value to Convert to String.
 @cat StringIntConv
 }
function Int2EHex (const L: LongInt): string; overload;
function Int2EHex (const L: Int64): string; overload;

{: Converts an Integer into a string of length Len with <See Var=ESBNumPadCh>
 Padding to the Left. <See Var=ESBNumPosSign> controls whether a '+' Sign
 appears at the beginning for positive Integers. <See Var=ESBBlankWhenZero>
 can be set to True to have Zero returned as a string of blanks.
 @param L Value to Convert to String.
 @param Len is the length of the resultant string. If it is too small then
  valid digits will be truncated from the right.
 @cat StringIntConv
 }
function Int2Str (const L: LongInt; const Len: Byte): string; overload;
function Int2Str (const L: Int64; const Len: Byte): string; overload;

{: Converts an Integer into a Hexademical (string) of length Len with <See Var=ESBNumPadCh>
 Padding to the Left. <See Var=ESBBlankWhenZero> can be set to True to have
 Zero returned as a string of blanks.
 @param L Value to Convert to String.
 @param Len is the length of the resultant string. If it is too small then
  valid digits will be truncated from the right.
 @cat StringIntConv
 }
function Int2Hex (const L: LongInt; const Len: Byte): string; overload;
function Int2Hex (const L: Int64; const Len: Byte): string; overload;

{: Converts an Integer into a string of length Len with Zero Padding to
 the Left. <See Var=ESBNumPosSign> controls whether a '+' Sign
 appears at the beginning for positive Integers. <See Var=ESBBlankWhenZero>
 can be set to True to have Zero returned as a string of blanks.
 @param L Value to Convert to String.
 @param Len is the length of the resultant string. If it is too small then
  valid digits will be truncated from the right.
 @cat StringIntConv
 }
function Int2ZStr (const L: LongInt; const Len: Byte): string; overload;
function Int2ZStr (const L: Int64; const Len: Byte): string; overload;

{: Converts an Integer into a string of length Len with Zero Padding to
 the Left. <See Var=ESBBlankWhenZero> can be set to True to have Zero
 returned as a string of blanks.
 @param L Value to Convert to String.
 @param Len is the length of the resultant string. If it is too small then
  valid digits will be truncated from the right.
 @cat StringIntConv
 }
function Int2ZHex (const L: LongInt; const Len: Byte): string; overload;
function Int2ZHex (const L: Int64; const Len: Byte): string; overload;

{: Converts an Integer into a string without Padding and with <b>ThousandSeparators</b>
 as defined in the Regional Settings. <See Var=ESBNumPosSign>
 controls whether a '+' Sign appears at the beginning for positive Integers.
 <See Var=ESBBlankWhenZero> can be set to True to have Zero returned as an
 Empty string.
 @param L Value to Convert to String.
 @cat StringIntConv
 }
function Int2CEStr (const L: LongInt): string; overload;
function Int2CEStr (const L: Int64): string; overload;

{: Converts an Integer into a string of length Len with <See Var=ESBNumPadCh>
 Padding and with <b>ThousandSeparators</b> as defined in the Regional Settings.
  <See Var=ESBNumPosSign> controls whether a '+' Sign
 appears at the beginning for positive Integers. <See Var=ESBBlankWhenZero>
 can be set to True to have Zero returned as a string of blanks.
 @param L Value to Convert to String.
 @param Len is the length of the resultant string. If it is too small then
  valid digits will be truncated from the right.
 @cat StringIntConv
 }
function Int2CStr (const L: LongInt; const Len: Byte): string; overload;
function Int2CStr (const L: Int64; const Len: Byte): string; overload;

{: Returns the "Placing" suffix for an integer, ie 1 gives 'st' as in 1st,
 12 gives 'th' as in 12th, 22 gives 'nd' as in 22nd, etc.
 @param L Value to process
 @cat StringIntConv
}
function Int2Placing (L: LongInt): string; overload
function Int2Placing (L: Int64): string; overload;

{---  Convert string to Float ---}

{: Converts a string into an Extended. Removes Thousand Separators if they
 are present as well as any leading or trailing  white spaces (ie <= #32).
 Non-numeric will return 0 unless you set <See Var=ESBRaiseFloatError> to true.<p>
 Also ignores Percentage Signs (%).
 @param S the String to process
 @cat StringFloatConv
}
function Str2Float (const S: string): Extended;

{: Converts a string into a Double. Removes Thousand Separators if they
 are present as well as any leading or trailing  white spaces (ie <= #32).
 If Number is Valid but out of Range then MaxDouble will be
 returned for a greater value and -MaxDouble for a lesser value.
 Non-numeric will return 0 unless you set <See Var=ESBRaiseFloatError> to true.<p>
 Also ignores Percentage Signs (%).
 @param S the String to process
 @cat StringFloatConv
}
function Str2Double (const S: string): Double;

{: Converts a string into a Single. Removes Thousand Separators if they
 are present as well as any leading or trailing  white spaces (ie <= #32).
 If Number is Valid but out of Range then MaxSingle will be
 returned for a greater value and -MaxSingle for a lesser value.
 Non-numeric will return 0 unless you set <See Var=ESBRaiseFloatError> to true.<p>
 Also ignores Percentage Signs (%).
 @param S the String to process
 @cat StringFloatConv
}
function Str2Single (const S: string): Single;

{---  Convert Float to string ---}

{: Converts a Float into a string without Padding. <See Var=ESBNumPosSign>
 controls whether a '+' Sign appears at the beginning for positive Integers.
 <See Var=ESBBlankWhenZero> can be set to True to have Zero returned as an
 Empty string, where Zero is dependent upon <See Var=ESBTolerance>.
 Also see <See Routine=Float2EStr2>, <See Routine=Float2Str> &
 <See Routine=Float2CEStr>
 @param X Value to Convert to String.
 @param Decimals is the desired number of Decimal places, defaults to 4
 @cat StringFloatConv
 }
function Float2EStr (const X: Extended; const Decimals: Byte = 4): string; overload;
function Float2EStr (const X: Double; const Decimals: Byte = 4): string; overload;
function Float2EStr (const X: Single; const Decimals: Byte = 4): string; overload;

{: Like <See Routine=Float2EStr> this ccnverts a Float into a string without
 Padding, except this removes all trailing 0's and the decimal separator
 if not needed. <See Var=ESBNumPosSign> controls whether a '+' Sign
 appears at the beginning for positive Integers.
 <See Var=ESBBlankWhenZero> can be set to True to have Zero returned as an
 Empty string, where Zero is dependent upon <See Var=ESBTolerance>.
 Also see <See Routine=Float2EStr>, <See Routine=Float2Str> &
 <See Routine=Float2CEStr>
 @param X Value to Convert to String.
 @param Decimals is the desired number of Decimal places, defaults to 4
 @cat StringFloatConv
 }
function Float2EStr2 (const X: Extended; const Decimals: Byte = 4): string; overload;
function Float2EStr2 (const X: Double; const Decimals: Byte = 4): string; overload;
function Float2EStr2 (const X: Single; const Decimals: Byte = 4): string; overload;

{: Converts a Float into a string without Padding and with Thousands Separators.
  <See Var=ESBNumPosSign> controls whether a '+' Sign appears at the beginning
  for positive Integers. <See Var=ESBBlankWhenZero> can be set to True to
  have Zero returned as an  Empty string, where Zero is dependent upon
  <See Var=ESBTolerance>.
 Also see <See Routine=Float2CEStr2>, <See Routine=Float2Str> &
 <See Routine=Float2EStr>
 @param X Value to Convert to String.
 @param Decimals is the desired number of Decimal places, defaults to 4
 @cat StringFloatConv
 }
function Float2CEStr (const X: Extended; const Decimals: Byte = 4): string; overload;
function Float2CEStr (const X: Double; const Decimals: Byte = 4): string; overload;
function Float2CEStr (const X: Single; const Decimals: Byte = 4): string; overload;

{: Like <See Routine=Float2CEStr> this ccnverts a Float into a string without
 Padding & with Thousands Separators, except this removes all trailing 0's
 and the decimal separator if not needed.
 <See Var=ESBNumPosSign> controls whether a '+' Sign appears at the beginning
 for positive Integers. <See Var=ESBBlankWhenZero> can be set to True to
 have Zero returned as an  Empty string, where Zero is dependent upon
 <See Var=ESBTolerance>.
 Also see <See Routine=Float2CEStr>, <See Routine=Float2Str> &
 <See Routine=Float2EStr>
 @param X Value to Convert to String.
 @param Decimals is the desired number of Decimal places, defaults to 4
 @cat StringFloatConv
 }
function Float2CEStr2 (const X: Extended; const Decimals: Byte = 4): string; overload;
function Float2CEStr2 (const X: Double; const Decimals: Byte = 4): string; overload;
function Float2CEStr2 (const X: Single; const Decimals: Byte = 4): string; overload;

{: Converts a Float into a string of length Len with <See Var=ESBNumPadCh>
 Padding to the Left. <See Var=ESBNumPosSign>  controls whether a '+' Sign
 appears at the beginning for positive Integers. <See Var=ESBBlankWhenZero>
 can be set to True to have Zero returned as an Empty string, where Zero is
 dependent upon <See Var=ESBTolerance>.
 Also see <See Routine=Float2EStr>, <See Routine=Float2CStr> &
 <See Routine=Float2ZStr>
 @param X Value to Convert to String.
 @param Len is the length of the resultant string. If it is too small then
  valid digits will be truncated from the right.
 @param Decimals is the desired number of Decimal places, defaults to 4
 @cat StringFloatConv
 }
function Float2Str (const X: Extended; const Len: Byte;
     const Decimals: Byte = 4): string; overload;
function Float2Str (const X: Double; const Len: Byte;
     const Decimals: Byte = 4): string; overload;
function Float2Str (const X: Single; const Len: Byte;
     const Decimals: Byte = 4): string; overload;

{: Converts a Float into a string of length Len with <See Var=ESBNumPadCh>
 Padding to the Left and with Thousands Separators. <See Var=ESBNumPosSign>
  controls whether a '+' Sign appears at the beginning for positive Integers.
  <See Var=ESBBlankWhenZero> can be set to True to have Zero returned as an
  Empty string, where Zero is dependent upon <See Var=ESBTolerance>.
 Also see <See Routine=Float2EStr>, <See Routine=Float2Str> &
 <See Routine=Float2ZStr>
 @param X Value to Convert to String.
 @param Len is the length of the resultant string. If it is too small then
  valid digits will be truncated from the right.
 @param Decimals is the desired number of Decimal places, defaults to 4
 @cat StringFloatConv
 }
function Float2CStr (const X: Extended; const Len: Byte;
     const Decimals: Byte = 4): string; overload;
function Float2CStr (const X: Double; const Len: Byte;
     const Decimals: Byte = 4): string; overload;
function Float2CStr (const X: Single; const Len: Byte;
     const Decimals: Byte = 4): string; overload;

{: Converts a Float into a string of length Len with Zero Padding to the Left.
 <See Var=ESBNumPosSign>  controls whether a '+' Sign appears at the
 beginning for positive Integers. <See Var=ESBBlankWhenZero>	can be set to
 True to have Zero returned as an Empty string, where Zero is
 dependent upon <See Var=ESBTolerance>.
 Also see <See Routine=Float2EStr>, <See Routine=Float2CStr> &
 <See Routine=Float2Str>
 @param X Value to Convert to String.
 @param Len is the length of the resultant string. If it is too small then
  valid digits will be truncated from the right.
 @param Decimals is the desired number of Decimal places, defaults to 4
 @cat StringFloatConv
 }
function Float2ZStr (const X: Extended; const Len: Byte;
     const Decimals: Byte = 4): string; overload;
function Float2ZStr (const X: Double; const Len: Byte;
     const Decimals: Byte = 4): string; overload;
function Float2ZStr (const X: Single; const Len: Byte;
     const Decimals: Byte = 4): string; overload;

{: Converts a Float into a string in Scientific Notation without Padding.
 This is of the form d.dddEnn. <See Var=ESBBlankWhenZero> can be set to True
 to have Zero returned as an Empty string, where Zero is dependent upon
 <See Var=ESBTolerance>.
 @param X Value to Convert to String.
 @param Decimals is the desired number of Decimal places in the Mantissa, defaults to 4
 @cat StringFloatConv
 }
function SciFloat2EStr (const X: Extended; const Decimals: Byte = 4): string; overload;
function SciFloat2EStr (const X: Double; const Decimals: Byte = 4): string; overload;
function SciFloat2EStr (const X: Single; const Decimals: Byte = 4): string; overload;

{: Converts a Float into a string in Scientific Notation without Padding, except
 this removes all trailing 0's and the decimal separator if not needed. This
 is of the form d.dddEnn. <See Var=ESBBlankWhenZero> can be set to True to
 have Zero returned as an Empty string, where Zero is
 dependent upon <See Var=ESBTolerance>.
 @param X Value to Convert to String.
 @param Decimals is the desired number of Decimal places in the Mantissa, defaults to 4
 @cat StringFloatConv
}

function SciFloat2EStr2 (const X: Extended; const Decimals: Byte = 4): string; overload;
function SciFloat2EStr2 (const X: Double; const Decimals: Byte = 4): string; overload;
function SciFloat2EStr2 (const X: Single; const Decimals: Byte = 4): string; overload;

{: Converts a Float into a string in Scientific Notation of length Len with
 <See Var=ESBNumPadCh> Padding to the Left. This is of the form d.dddEnn.
 <See Var=ESBBlankWhenZero> can be set to True to have Zero returned as an
 Empty string, where Zero is dependent upon <See Var=ESBTolerance>.
 @param X Value to Convert to String.
 @param Len is the length of the resultant string. If it is too small then
  valid digits will be truncated from the right.
 @param Decimals is the desired number of Decimal places, defaults to 4
 @cat StringFloatConv
 }
function SciFloat2Str (const X: Extended; const Len: Byte;
     const Decimals: Byte = 4): string; overload;
function SciFloat2Str (const X: Double; const Len: Byte;
     const Decimals: Byte = 4): string; overload;
function SciFloat2Str (const X: Single; const Len: Byte;
     const Decimals: Byte = 4): string; overload;

{--- Boolean Conversions ---}

{: Converts a Boolean Value into the corresponding Character.
 Returns for True = 'T' and  False = 'F'.
 @param B Boolean Value to convert.
 @cat BooleanConv
}
function Boolean2TF (const B: Boolean): Char;

{: Converts a Boolean Value into the corresponding Character.
 Returns for True = 'Y' and  False = 'N'.
 @param B Boolean Value to convert.
 @cat BooleanConv
}
function Boolean2YN (const B: Boolean): Char;

{: Converts a Boolean Value into the corresponding Character.
 Returns for True = 'Y' and False = ' '  (ie Blank).
 @param B Boolean Value to convert.
 @cat BooleanConv
}
function Boolean2YB (const B: Boolean): Char;

{: Converts a Boolean Value into the corresponding Character.
 @param B Boolean Value to convert.
 @param TrueChar Character to return when True.
 @param FalseChar Character to return when False.
 @cat BooleanConv
}
function Boolean2Char (const B: Boolean; const TrueChar, FalseChar: Char): Char;

{: Converts a Character Value into its corresponding Boolean value.
 Returns 'T', 't'  for True,  Otherwise = False.
 @param Ch Character to convert.
 @cat BooleanConv
}
function TF2Boolean (const Ch: Char): Boolean;

{: Converts a Character Value into its corresponding Boolean value.
 Returns 'Y', 'y'  for True,  Otherwise = False.
 @param Ch Character to convert.
 @cat BooleanConv
}
function YN2Boolean (const Ch: Char): Boolean;

{: Converts a Boolean Value into the corresponding Character.
 Returns for True = 'True' and  False = 'False'.
 @param B Boolean Value to convert.
 @cat BooleanConv
}
function Boolean2TFStr (const B: Boolean): string;

{: Converts a Boolean Value into the corresponding Character.
 Returns for True = 'Yes' and  False = 'No'.
 @param B Boolean Value to convert.
 @cat BooleanConv
}
function Boolean2YNStr (const B: Boolean): string;

{: Converts a Boolean Value into the corresponding Character.
 Returns for True = 'On' and  False = 'Off'.
 @param B Boolean Value to convert.
 @cat BooleanConv
}
function Boolean2OnOffStr (const B: Boolean): string;

{--- SpreadSheet related ---}

{: Takes a Alphabetic Column Heading like that used in MS Excel
 and converts it to its Numeric Equivalent. Only at most
 2 characters processed.
 @param AlphaCol Alphabetic Column Heading like 'A' or 'BC'
 @Returns Numeric Column, where first column 'A' is 1
 @cat StringIntConv
}
function AlphaCol2Int (const AlphaCol: string): Word;

{: Takes a Numeric Column Heading and converts it to its Alphabetic
 Column Heading like that used in Excel. If Col is 0 then empty string
 returned. If Col evaluates to greater than 'ZZ' then '**' is returned.
 @param Col Column Number, where 1 is the first Column
 @Returns Alphabetic Column Heading like 'A' or 'BC'.
 @cat StringIntConv
}
function IntCol2Alpha (const Col: Word): string;

{--- IP Address Routines ---}

{: Converts a String representing an IP Address 'xxx.xxx.xxx.xxx' into the
 equivalent LongWord. If <See Var=ESBRaiseIPError> is true then an Exception
 is raised if a IP Address Conversion error occurs, if false just returns 0.
 @param IPAddr String representing an IP Address.
 @cat StringIntConv
}
function IPStr2LWord (const IPAddr: string): LongWord;

{: Converts a LongWord representing an IP Address and returns the equivalent
 string representation 'xxx.xxx.xxx.xxx'. No Padding is applied.
 @param IPAddr LongWord representing an IP Address.
 @cat StringIntConv
}
function LWord2IPStr (const IPAddr: LongWord): string;

{--- String Tests ---}

{: Returns True if the string is not empty and is only made of Characters
 in the specificed CharSet.
 @cat ExtraStrings
}
function IsCharSetStr (const S: string; const CharSet: TESBCharSet): Boolean;

{: Returns True if the string is not empty and is only made of Digits '0'
 through '9'.
 @cat ExtraStrings
}
function IsDigitStr (const S: string): Boolean;

{: Returns True if the string is not empty and is only made of Standard
 Alphabetic characters 'A'through 'Z' and 'a' through 'z'.
 @cat ExtraStrings
}
function IsAlphaStr (const S: string): Boolean;

{: Returns True if the string is not empty and is only made of Standard
 Alphabetic characters 'A'through 'Z' and 'a' through 'z' or of Digits
 '0' through '9'.
 @cat ExtraStrings
}
function IsAlphaNumericStr (const S: string): Boolean;

{: Retrieves the P'th Value in a String containing several values separated
 by given Separator which defaults to a semicolon. Thus 'Mon;Tue;Wed' has
 'Mon' as the 1st string, 'Tue' as the 2nd string, etc. If you request a
 Value exceeding the number of values present then an Empty String is returned.

 @param ValuesStr String containing Values separated by given Separator.
 @parm P The Value you wish to get - 1 is the First Value. 0 is treated the same as 1.
 @param Separator Character used to separate values, defaults to ';'.
 @cat ExtraStrings
}
function ExtractValue (const ValuesStr: string; var P: Integer;
     const Separator: Char = ';'): string;

{: Retrieves True if the specified Value is in a String containing several values
 separated by given Separator which defaults to a semicolon. Thus
 'Mon;Tue;Wed' has 'Mon' as the 1st string, 'Tue' as the 2nd string, etc.
     Values are compared without case sensitivity.

 @param ValuesStr String containing Values separated by given Separator.
 @parm Value The Value you wish to search for.
 @param Separator Character used to separate values, defaults to ';'.
 @cat ExtraStrings
}
function ValueMatch (const ValuesStr, Value: string;
     const Separator: Char = ';'): Boolean;

{--- Angle Conversions ---}

{ Converts an angle into a formatted Degree/Minute/Second String using the
 <See Var=ESBDegreeStr>, <See Var=ESBMinuteStr> and <See Var=ESBSecondStr>.
     @param Degrees If Degrees is 0 then Sign is used to identify whether the value is Positive or Negative.
 @param Minutes If Minutes are negative or larger than 59 then an exception is raised.
 @param Seconds If Seconds are negative or larger than 59.99999999 then an exception is raised.
 @param DecimalPlaces Number of Decimal Places to use with Seconds portion.
 @param Sign -1 if Value is Negative, 0 if Value is Zero, 1 if Value is Positive.
 @cat StringFloatConv
}
function Angle2Str (const Angle: Extended; const DecimalPlaces: Byte = 2): string; overload;
function Angle2Str (const Degrees, Minutes: Integer; const Seconds: Extended;
     const Sign: Shortint = 1; const DecimalPlaces: Byte = 2): string; overload;

implementation

uses
     SysUtils,
     {$IFDEF MSWINDOWS}
     Windows,
     {$ENDIF}
     {$IFDEF LINUX}
     Libc,
     {$ENDIF}
     QESBPCS_RS_Globals, QESBPCS_RS_Math,
     QESBPCSMath, QESBPCSSystem;

{--- Extra string Operations ---}

function AnsiUpCase (const Ch: Char): Char;
{$IFDEF MSWINDOWS}
var
     PC: PChar;
begin
     New (PC);
     try
          PC^ := Ch; // Convert Char to PChar
          Result := CharUpper (PC)^; // Call Windows function
     finally
          Dispose (PC);
     end;
end;
{$ENDIF}
{$IFDEF LINUX}
begin
     Result := Char (towupper (UCS4Char (Ch)));
end;
{$ENDIF}

function ESBPosCh (const Ch: Char; const S: string; Start: Integer = 1): Integer;
var
     I, N: Integer;
begin
     Result := 0;
     N := Length (S);
     if Start < 1 then
          Start := 1;
     if (N = 0) or (Start > N) then
          Exit;

     for I := Start to N do
     begin
          if S [I] = Ch then
          begin
               Result := I;
               Exit;
          end;
     end;
end;

function ESBPosNCh (const Ch: Char; const S: string; const N: Integer;
     Start: Integer = 1): Integer;
var
     I, Len, Count: Integer;
begin
     Result := 0;
     Len := Length (S);
     if Start < 1 then
          Start := 1;
     if (Len = 0) or (Start > Len) or (N < 1) then
          Exit;

     Count := 0;
     for I := Start to Len do
     begin
          if S [I] = Ch then
          begin
               Inc (Count);
               if Count = N then
               begin
                    Result := I;
                    Exit;
               end;
          end;
     end;
end;

function ESBLastPosCh (const Ch: Char; const S: string; Start: Integer = 0): Integer;
var
     I, N: Integer;
begin
     Result := 0;
     N := Length (S);
     if Start < 1 then
          Start := N;
     if (N = 0) or (Start > N) then
          Exit;

     for I := Start downto 1 do
     begin
          if S [I] = Ch then
          begin
               Result := I;
               Exit;
          end;
     end;
end;

function CentreChStr (const S: string; const Ch: Char;
     const Len: LongWord): string;
var
     N, M: LongWord;
begin
     N := Length (S);
     if N < Len then
     begin
          M := Len - N; // Length of padding needed
          N := M div 2; // Half on either side
          if Odd (M) then // Handle Odd differently to Even
               Result := FillStr (Ch, N) + S
                    + FillStr (Ch, N + 1)
          else
               Result := FillStr (Ch, N) + S
                    + FillStr (Ch, N);
     end
     else
          Result := S;
end;

function CentreStr (const S: string; const Len: LongWord): string;
begin
     Result := CentreChStr (S, ' ', Len);
end;

function ESBProperStr (const S: string): string;
var
     I: LongWord;
     First: Boolean;
begin
     First := True;
     Result := AnsiLowerCase (S); // Convert string to lower case
     for I := 1 to Length (S) do
          if not (Result [I] in WordSepSet) then // check for character that is part of word
          begin
               if First then // Only capitalise if first word
               begin
                    First := False;
                    Result [I] := AnsiUpCase (Result [I]);
               end;
          end
          else // Character is a Word Separator and we start again
               First := True;
end;

function FillStr (const Ch: Char; const N: LongWord): string;
begin
     SetLength (Result, N); // Set Length of string
     FillChar (Result [1], N, Ch); // Fill with Character
end;

function BlankStr (const N: LongWord): string;
begin
     Result := FillStr (' ', N);
end;

function DashStr (const N: LongWord): string;
begin
     Result := FillStr ('-', N);
end;

function StarStr (const N: LongWord): string;
begin
     Result := FillStr ('*', N);
end;

function UnderscoreStr (const N: LongWord): string;
begin
     Result := FillStr ('_', N);
end;

function LeftAlignStr (const S: string; const N: LongWord): string;
begin
     Result := PadRightStr (LeftStr (S, N), N);
end;

function LeftStr (const S: string; const N: LongWord): string;
begin
     Result := Copy (S, 1, N); // Return the first N Characters
end;

function LeftTillChStr (const S: string; const Ch: Char): string;
var
     M: Integer;
begin
     M := ESBPosCh (Ch, S); // Find first position of Character
     if M < 2 then
          Result := '' // If not found or is in the first position return ''
     else
          Result := LeftStr (S, M - 1); // Otherwise return the Left portion
end;

function RightTillChStr (const S: string; const Ch: Char): string;
var
     M: Integer;
begin
     M := ESBLastPosCh (Ch, S); // Find Last position of Character
     if (M = 0) or (M >= Length (S)) then
          Result := '' // If not found or is in the last position return ''
     else
          Result := RightAfterStr (S, M); // Otherwise return the Right portion
end;

function PadChLeftStr (const S: string; const Ch: Char;
     const Len: LongWord): string;
var
     N: LongWord;
begin
     N := Length (S);
     if N < Len then
          Result := FillStr (Ch, Len - N) + S
     else
          Result := S;
end;

function PadLeftStr (const S: string; const Len: LongWord): string;
begin
     Result := PadChLeftStr (S, ' ', Len);
end;

function PadChRightStr (const S: string; const Ch: Char;
     const Len: LongWord): string;
var
     N: LongWord;
begin
     N := Length (S);
     if N < Len then
          Result := S + FillStr (Ch, Len - N)
     else
          Result := S;
end;

function PadRightStr (const S: string; const Len: LongWord): string;
begin
     Result := PadChRightStr (S, ' ', Len);
end;

function RightAlignStr (const S: string; const N: LongWord): string;
begin
     Result := PadLeftStr (LeftStr (S, N), N);
end;

function RightStr (const S: string; const N: LongWord): string;
var
     M: LongWord;
begin
     M := Int64 (Length (S)) - N + 1; // Calculate the starting point
     if M < 1 then // If longer then string, return string
          M := 1;
     Result := Copy (S, M, N); // Return last N characters
end;

function RightAfterStr (const S: string; const N: LongWord): string;
begin
     Result := Copy (S, N + 1, Int64 (Length (S)) - N); // Return the string remaining after the Nth character
end;

function RightAfterChStr (const S: string; const Ch: Char): string;
var
     M: LongWord;
begin
     M := ESBPosCh (Ch, S); // Find first position of Character
     if M = 0 then
          Result := '' // If not found then return ''
     else
          Result := RightAfterStr (S, M); // Return the Right portion
end;

function StripTChStr (const S: string; const Ch: Char): string;
var
     Len: LongWord;
begin
     Len := Length (S);
     while (Len > 0) and (S [Len] = Ch) do
          Dec (Len);
     if Len = 0 then
          Result := ''
     else
          Result := LeftStr (S, Len);
end;

function StripTChStr (const S: string; const Chars: TESBCharSet): string;
var
     Len: Integer;
begin
     if Chars = [] then
          Result := S
     else
     begin
          Len := Length (S);
          while (Len > 0) and (S [Len] in Chars) do
               Dec (Len);
          if Len = 0 then
               Result := ''
          else
               Result := LeftStr (S, Len);
     end;
end;

function StripLChStr (const S: string; const Ch: Char): string;
var
     I, Len: LongWord;
begin
     Len := Length (S);
     I := 1;
     while (I <= Len) and (S [I] = Ch) do
          Inc (I);
     if (I > Len) then
          Result := ''
     else
          Result := Copy (S, I, Len - I + 1);
end;

function StripLChStr (const S: string; const Chars: TESBCharSet): string;
var
     Len, I: Integer;
begin
     if Chars = [] then
          Result := S
     else
     begin
          Len := Length (S);
          I := 1;
          while (I <= Len) and (S [I] in Chars) do
               Inc (I);
          if (I > Len) then
               Result := ''
          else
               Result := Copy (S, I, Len - I + 1);
     end;
end;

function StripChStr (const S: string; const Ch: Char): string;
begin
     Result := StripTChStr (StripLChStr (S, Ch), Ch);
end;

function StripChStr (const S: string; const Chars: TESBCharSet): string;
begin
     Result := StripTChStr (StripLChStr (S, Chars), Chars);
end;

function ReplaceChStr (const S: string; const OldCh, NewCh: Char): string;
var
     I: LongWord;
begin
     Result := S;
     if OldCh = NewCh then
          Exit;
     for I := 1 to Length (S) do
          if S [I] = OldCh then
               Result [I] := NewCh;
end;

function DisplayTabsInString (const S: string): string;
var
     P: Integer;
begin
     Result := S;
     P := ESBPosCh (#9, Result);
     while P > 0 do
     begin
          Result := LeftStr (Result, P - 1) + ESBTabStr
               + RightAfterStr (Result, P);
          P := ESBPosCh (#9, Result);
     end;
end;

function StripChFromStr (const S: string; const Ch: Char): string;
var
     LenS, N, I: Integer;
begin
     LenS := Length (S);
     SetLength (Result, LenS);
     N := 0;
     for I := 1 to LenS do
     begin
          if S [I] <> Ch then
          begin
               Inc (N);
               Result [N] := S [I];
          end;
     end;
     SetLength (Result, N);
end;

function StripChFromStr (const S: string; const Chars: TESBCharSet): string;
var
     LenS, N, I: Integer;
begin
     if Chars = [] then
          Result := S
     else
     begin
          LenS := Length (S);
          SetLength (Result, LenS);
          N := 0;
          for I := 1 to LenS do
          begin
               if not (S [I] in Chars) then
               begin
                    Inc (N);
                    Result [N] := S [I];
               end;
          end;
          SetLength (Result, N);
     end;
end;

{---  Convert string to Integers ---}

function StripThousandSeparators (const S: string): string;
begin
     Result := StripChFromStr (S, ThousandSeparator);
end;

function Str2Int64 (const S: string): Int64;
var
     S2: string;
     Error: Integer;
begin
     S2 := StripThousandSeparators (Trim (S)); // Remove Thousands Separators, if any
     try
          Val (S2, Result, Error);
          if Error <> 0 then
               Result := 0 // Return 0 for non-numeric
     except
          Result := 0; // Return 0 for non-numeric
     end;
end;

function Str2LInt (const S: string): LongInt;
var
     S2: string;
     L: Int64;
     Error: Integer;
begin
     S2 := StripThousandSeparators (Trim (S)); // Remove Thousands Separators, if any
     try
          Val (S2, L, Error);
          if Error <> 0 then
               Result := 0 // Return 0 for non-numeric
          else if L > High (LongInt) then // Check with in boundaries
               Result := High (LongInt)
          else if L < Low (LongInt) then
               Result := Low (LongInt)
          else
               Result := L; // Return Value
     except
          Result := 0; // Return 0 for non-numeric
     end;
end;

function Str2Int (const S: string): Integer;
var
     S2: string;
     L: Int64;
     Error: Integer;
begin
     S2 := StripThousandSeparators (Trim (S)); // Remove Thousands Separators, if any
     try
          Val (S2, L, Error);
          if Error <> 0 then
               Result := 0 // Return 0 for non-numeric
          else if L > High (Integer) then // Check with in boundaries
               Result := High (Integer)
          else if L < Low (Integer) then
               Result := Low (Integer)
          else
               Result := L; // Return Value
     except
          Result := 0; // Return 0 for non-numeric
     end;
end;

function Str2SmallInt (const S: string): SmallInt;
var
     S2: string;
     L: Int64;
     Error: Integer;
begin
     S2 := StripThousandSeparators (Trim (S)); // Remove Thousands Separators, if any
     try
          Val (S2, L, Error);
          if Error <> 0 then
               Result := 0 // Return 0 for non-numeric
          else if L > High (SmallInt) then // Check with in boundaries
               Result := High (SmallInt)
          else if L < Low (SmallInt) then
               Result := Low (SmallInt)
          else
               Result := L; // Return Value
     except
          Result := 0; // Return 0 for non-numeric
     end;
end;

function Str2SInt (const S: string): ShortInt;
var
     S2: string;
     L: Int64;
     Error: Integer;
begin
     S2 := StripThousandSeparators (Trim (S)); // Remove Thousands Separators, if any
     try
          Val (S2, L, Error);
          if Error <> 0 then
               Result := 0 // Return 0 for non-numeric
          else if L > High (ShortInt) then // Check with in boundaries
               Result := High (ShortInt)
          else if L < Low (ShortInt) then
               Result := Low (ShortInt)
          else
               Result := L; // Return Value
     except
          Result := 0; // Return 0 for non-numeric
     end;
end;

function Str2LWord (const S: string): LongWord;
var
     S2: string;
     L: Int64;
     Error: Integer;
begin
     S2 := StripThousandSeparators (Trim (S)); // Remove Thousands Separators, if any
     try
          Val (S2, L, Error);
          if Error <> 0 then
               Result := 0 // Return 0 for non-numeric
          else if L > High (LongWord) then // Check with in boundaries
               Result := High (LongWord)
          else if L < Low (LongWord) then
               Result := Low (LongWord)
          else
               Result := L; // Return Value
     except
          Result := 0; // Return 0 for non-numeric
     end;
end;

function Str2Cardinal (const S: string): Cardinal;
var
     S2: string;
     L: Int64;
     Error: Integer;
begin
     S2 := StripThousandSeparators (Trim (S)); // Remove Thousands Separators, if any
     try
          Val (S2, L, Error);
          if Error <> 0 then
               Result := 0 // Return 0 for non-numeric
          else if L > High (Cardinal) then // Check with in boundaries
               Result := High (Cardinal)
          else if L < Low (Cardinal) then
               Result := Low (Cardinal)
          else
               Result := L; // Return Value
     except
          Result := 0; // Return 0 for non-numeric
     end;
end;

function Str2Word (const S: string): Word;
var
     S2: string;
     L: Int64;
     Error: Integer;
begin
     S2 := StripThousandSeparators (Trim (S)); // Remove Thousands Separators, if any
     try
          Val (S2, L, Error);
          if Error <> 0 then
               Result := 0 // Return 0 for non-numeric
          else if L > High (Word) then // Check with in boundaries
               Result := High (Word)
          else if L < Low (Word) then
               Result := Low (Word)
          else
               Result := L; // Return Value
     except
          Result := 0; // Return 0 for non-numeric
     end;
end;

function Str2Byte (const S: string): Byte;
var
     S2: string;
     L: Int64;
     Error: Integer;
begin
     S2 := StripThousandSeparators (Trim (S)); // Remove Thousands Separators, if any
     try
          Val (S2, L, Error);
          if Error <> 0 then
               Result := 0 // Return 0 for non-numeric
          else if L > High (Byte) then // Check with in boundaries
               Result := High (Byte)
          else if L < Low (Byte) then
               Result := Low (Byte)
          else
               Result := L; // Return Value
     except
          Result := 0; // Return 0 for non-numeric
     end;
end;

function Hex2LWord (const S: string): LongWord;
var
     S2: string;
     L: Int64;
     Error: Integer;
begin
     S2 := StripChFromStr (S, WhiteSpaceSet);
     if (Length (S2) > 1) and (S2 [1] <> '$') then
          S2 := '$' + S2;
     try
          Val (S2, L, Error);
          if Error <> 0 then
               Result := 0 // Return 0 for non-numeric
          else if L > High (LongWord) then // Check with in boundaries
               Result := High (LongWord)
          else if L < Low (LongWord) then
               Result := Low (LongWord)
          else
               Result := L; // Return Value
     except
          Result := 0; // Return 0 for non-numeric
     end;
end;

function Hex2Int64 (const S: string): Int64;
var
     S2: string;
     L: Int64;
     Error: Integer;
begin
     S2 := UpperCase (StripChFromStr (S, WhiteSpaceSet));
     if (Length (S2) > 1) and (S2 [1] <> '$') then
          S2 := '$' + S2;

     { Some Delphi/Kylix versions have problems with the following so we
      handle them as special cases. }

     if S2 = '$FFFFFFFFFFFFFFFF' then
     begin
          Result := -1;
          Exit;
     end;

     if S2 = '$8000000000000000' then
     begin
          Result := Low (Int64);
          Exit;
     end;

     try
          Val (S2, L, Error);
          if Error <> 0 then
               Result := 0 // Return 0 for non-numeric
          else
               Result := L; // Return Value
     except
          Result := 0; // Return 0 for non-numeric
     end;
end;

function Hex2Int (const S: string): Integer;
var
     S2: string;
     L: Integer;
     Error: Integer;
begin
     S2 := StripChFromStr (S, WhiteSpaceSet);
     if (Length (S2) > 1) and (S2 [1] <> '$') then
          S2 := '$' + S2;
     try
          Val (S2, L, Error);
          if Error <> 0 then
               Result := 0 // Return 0 for non-numeric
          else
               Result := L; // Return Value
     except
          Result := 0; // Return 0 for non-numeric
     end;
end;

{---  Convert Integers to strings ---}

function Int2EStr (const L: LongInt): string;
begin
     if ESBBlankWhenZero and (L = 0) then
     begin
          Result := '';
          Exit;
     end;

     try
          FmtStr (Result, '%d', [L]); // Format the string
          if ESBNumPosSign and (L > 0) then // See if '+' needed
               Result := '+' + Result;
     except
          Result := '';
     end;
end;

function Int2EStr (const L: Int64): string;
begin
     if ESBBlankWhenZero and (L = 0) then
     begin
          Result := '';
          Exit;
     end;

     try
          FmtStr (Result, '%d', [L]); // Format the string
          if ESBNumPosSign and (L > 0) then // See if '+' needed
               Result := '+' + Result;
     except
          Result := '';
     end;
end;

function Int2EHex (const L: LongInt): string;
begin
     if ESBBlankWhenZero and (L = 0) then
     begin
          Result := '';
          Exit;
     end;

     try
          FmtStr (Result, '%x', [L]); // Format the string
     except
          Result := '';
     end;
end;

function Int2EHex (const L: Int64): string;
begin
     if ESBBlankWhenZero and (L = 0) then
     begin
          Result := '';
          Exit;
     end;

     try
          FmtStr (Result, '%x', [L]); // Format the string
     except
          Result := '';
     end;
end;

function Int2Str (const L: LongInt; const Len: Byte): string;
begin
     if ESBBlankWhenZero and (L = 0) then
     begin
          Result := BlankStr (Len);
          Exit;
     end;
     Result := PadChLeftStr (LeftStr (Int2EStr (L), Len), ESBNumPadCh, Len);
end;

function Int2Str (const L: Int64; const Len: Byte): string;
begin
     if ESBBlankWhenZero and (L = 0) then
     begin
          Result := BlankStr (Len);
          Exit;
     end;
     Result := PadChLeftStr (LeftStr (Int2EStr (L), Len), ESBNumPadCh, Len);
end;

function Int2Hex (const L: LongInt; const Len: Byte): string;
begin
     if ESBBlankWhenZero and (L = 0) then
     begin
          Result := BlankStr (Len);
          Exit;
     end;
     Result := PadChLeftStr (LeftStr (Int2EHex (L), Len), ESBNumPadCh, Len);
end;

function Int2Hex (const L: Int64; const Len: Byte): string;
begin
     if ESBBlankWhenZero and (L = 0) then
     begin
          Result := BlankStr (Len);
          Exit;
     end;
     Result := PadChLeftStr (LeftStr (Int2EHex (L), Len), ESBNumPadCh, Len);
end;

function Int2ZStr (const L: LongInt; const Len: Byte): string;
var
     Len2: Byte;
begin
     if ESBBlankWhenZero and (L = 0) then
     begin
          Result := BlankStr (Len);
          Exit;
     end;

     try
          FmtStr (Result, '%d', [abs (L)]); // Format the string
          if L = Low (LongInt) then
               Result := RightAfterStr (Result, 1);
          if (Len > 0) and ((L < 0) or ((L > 0) and ESBNumPosSign)) then
               Len2 := Len - 1 // Need to leave space for the sign
          else
               Len2 := Len;
          Result := PadChLeftStr (LeftStr (Result, Len2), '0', Len2); // Pad with Zeroes
          if L < 0 then // Add Sign if necessary
               Result := '-' + Result
          else if (L > 0) and ESBNumPosSign then
               Result := '+' + Result;
     except
          Result := '';
     end;
end;

function Int2ZStr (const L: Int64; const Len: Byte): string;
var
     Len2: Byte;
begin
     if ESBBlankWhenZero and (L = 0) then
     begin
          Result := BlankStr (Len);
          Exit;
     end;

     try
          FmtStr (Result, '%d', [abs (L)]); // Format the string
          if L = Low (Int64) then
               Result := RightAfterStr (Result, 1);
          if (Len > 0) and ((L < 0) or ((L > 0) and ESBNumPosSign)) then
               Len2 := Len - 1 // Need to leave space for the sign
          else
               Len2 := Len;
          Result := PadChLeftStr (LeftStr (Result, Len2), '0', Len2); // Pad with Zeroes
          if L < 0 then // Add Sign if necessary
               Result := '-' + Result
          else if (L > 0) and ESBNumPosSign then
               Result := '+' + Result;
     except
          Result := '';
     end;
end;

function Int2ZHex (const L: LongInt; const Len: Byte): string;
begin
     if ESBBlankWhenZero and (L = 0) then
     begin
          Result := BlankStr (Len);
          Exit;
     end;
     Result := PadChLeftStr (LeftStr (Int2EHex (L), Len), '0', Len);
end;

function Int2ZHex (const L: Int64; const Len: Byte): string;
begin
     if ESBBlankWhenZero and (L = 0) then
     begin
          Result := BlankStr (Len);
          Exit;
     end;
     Result := PadChLeftStr (LeftStr (Int2EHex (L), Len), '0', Len);
end;

function Int2CEStr (const L: LongInt): string;
var
     LS, L2, I: Integer;
     Temp: string;
begin
     if ESBBlankWhenZero and (L = 0) then
     begin
          Result := '';
          Exit;
     end;

     try
          FmtStr (Result, '%d', [abs (L)]); // Format the string
          if L = Low (LongInt) then
               Result := RightAfterStr (Result, 1);
          LS := Length (Result);
          L2 := (LS - 1) div 3; // Number of 'groups of three'
          Temp := '';
          for I := 1 to L2 do
               Temp := ThousandSeparator + Copy (Result, LS - 3 * I + 1, 3) + Temp;
          Result := Copy (Result, 1, (LS - 1) mod 3 + 1) + Temp;
          if L < 0 then // Add Sign if necessary
               Result := '-' + Result
          else if (L > 0) and ESBNumPosSign then
               Result := '+' + Result;
     except
          Result := ''
     end;
end;

function Int2CEStr (const L: Int64): string;
var
     LS, L2, I: Integer;
     Temp: string;
begin
     if ESBBlankWhenZero and (L = 0) then
     begin
          Result := '';
          Exit;
     end;

     try
          FmtStr (Result, '%d', [abs (L)]); // Format the string
          if L = Low (Int64) then
               Result := RightAfterStr (Result, 1);
          LS := Length (Result);
          L2 := (LS - 1) div 3; // Number of 'groups of three'
          Temp := '';
          for I := 1 to L2 do
               Temp := ThousandSeparator + Copy (Result, LS - 3 * I + 1, 3) + Temp;
          Result := Copy (Result, 1, (LS - 1) mod 3 + 1) + Temp;
          if L < 0 then // Add Sign if necessary
               Result := '-' + Result
          else if (L > 0) and ESBNumPosSign then
               Result := '+' + Result;
     except
          Result := ''
     end;
end;

function Int2CStr (const L: LongInt; const Len: Byte): string;
begin
     if ESBBlankWhenZero and (L = 0) then
     begin
          Result := BlankStr (Len);
          Exit;
     end;

     Result := PadChLeftStr (LeftStr (Int2CEStr (L), Len), ESBNumPadCh, Len);
end;

function Int2CStr (const L: Int64; const Len: Byte): string;
begin
     if ESBBlankWhenZero and (L = 0) then
     begin
          Result := BlankStr (Len);
          Exit;
     end;

     Result := PadChLeftStr (LeftStr (Int2CEStr (L), Len), ESBNumPadCh, Len);
end;

function Int2Placing (L: LongInt): string;
begin
     case abs (L) mod 10 of
          1: Result := 'st';
          2: Result := 'nd';
          3: Result := 'rd';
     else
          Result := 'th';
     end;
     case abs (L) mod 100 of
          11, 12, 13: Result := 'th';
     end;
end;

function Int2Placing (L: Int64): string;
begin
     case L mod 10 of
          1: Result := 'st';
          2: Result := 'nd';
          3: Result := 'rd';
     else
          Result := 'th';
     end;
     case L mod 100 of
          11, 12, 13: Result := 'th';
     end;
end;

{---  Convert string to Float ---}

function StripPercent (const Value: string): string;
// Removes '%' if it occurs in the string
var
     P: LongWord;
begin
     Result := Value;
     P := ESBPosCh ('%', Value);
     if P > 0 then
          Delete (Result, P, 1)
end;

function Str2Float (const S: string): Extended;
var
     S2: string;
     Found: Boolean;
begin
     try
          S2 := Trim (StripThousandSeparators (StripPercent (S)));
          if S2 = '' then
               Result := 0.0
          else
          begin
               repeat
                    Found := False;
                    if S2 [Length (S2)] in ['e', 'E', DecimalSeparator, '-', '+'] then
                    begin
                         S2 := LeftStr (S2, Length (S2) - 1);
                         Found := True;
                         S2 := Trim (S2);
                    end;
               until not Found or (S2 = '');
               if S2 = '' then
                    Result := 0.0
               else
                    Result := StrToFloat (S2);
          end;
     except
          if ESBRaiseFloatError then
               raise
          else
               Result := 0;
     end;
end;

function Str2Double (const S: string): Double;
var
     X: Extended;
begin
     X := Str2Float (S);
     if X > MaxDouble then // Check with in boundaries
          Result := MaxDouble
     else if X < -MaxDouble then
          Result := -MaxDouble
     else
          Result := X; // Return Value
end;

function Str2Single (const S: string): Single;
var
     X: Extended;
begin
     X := Str2Float (S);
     if X > MaxSingle then // Check with in boundaries
          Result := MaxSingle
     else if X < -MaxSingle then
          Result := -MaxSingle
     else
          Result := X; // Return Value
end;

{---  Convert Float to string ---}

function Float2EStr (const X: Extended; const Decimals: Byte = 4): string;
begin
     if ESBBlankWhenZero and FloatIsZero (X) then
     begin
          Result := '';
          Exit;
     end;

     try
          Result := FloatToStrF (X, ffFixed, 18, Decimals);

          if ESBNumPosSign and (X > 0) then // See if '+' needed
               Result := '+' + Result;
     except
          Result := '';
     end;
end;

function Float2EStr (const X: Double; const Decimals: Byte = 4): string;
begin
     if ESBBlankWhenZero and FloatIsZero (X) then
     begin
          Result := '';
          Exit;
     end;

     try
          Result := FloatToStrF (X, ffFixed, 15, Decimals);
          if ESBNumPosSign and (X > 0) then // See if '+' needed
               Result := '+' + Result;
     except
          Result := '';
     end;
end;

function Float2EStr (const X: Single; const Decimals: Byte = 4): string;
begin
     if ESBBlankWhenZero and FloatIsZero (X) then
     begin
          Result := '';
          Exit;
     end;

     try
          Result := FloatToStrF (X, ffFixed, 7, Decimals);
          if ESBNumPosSign and (X > 0) then // See if '+' needed
               Result := '+' + Result;
     except
          Result := '';
     end;
end;

function Float2EStr2 (const X: Extended; const Decimals: Byte = 4): string;
begin
     Result := StripTChStr (Float2EStr (X, Decimals), '0');
     if Result [Length (Result)] = '.' then
          Result := LeftStr (Result, Length (Result) - 1);
end;

function Float2EStr2 (const X: Double; const Decimals: Byte = 4): string;
begin
     Result := StripTChStr (Float2EStr (X, Decimals), '0');
     if Result [Length (Result)] = '.' then
          Result := LeftStr (Result, Length (Result) - 1);
end;

function Float2EStr2 (const X: Single; const Decimals: Byte = 4): string;
begin
     Result := StripTChStr (Float2EStr (X, Decimals), '0');
     if Result [Length (Result)] = '.' then
          Result := LeftStr (Result, Length (Result) - 1);
end;

function Float2CEStr (const X: Extended; const Decimals: Byte = 4): string;
begin
     if ESBBlankWhenZero and FloatIsZero (X) then
     begin
          Result := '';
          Exit;
     end;

     try
          Result := FloatToStrF (X, ffNumber, 18, Decimals);
          if ESBNumPosSign and (X > 0) then // See if '+' needed
               Result := '+' + Result;
     except
          Result := '';
     end;
end;

function Float2CEStr (const X: Double; const Decimals: Byte = 4): string;
begin
     if ESBBlankWhenZero and FloatIsZero (X) then
     begin
          Result := '';
          Exit;
     end;

     try
          Result := FloatToStrF (X, ffNumber, 15, Decimals);
          if ESBNumPosSign and (X > 0) then // See if '+' needed
               Result := '+' + Result;
     except
          Result := '';
     end;
end;

function Float2CEStr (const X: Single; const Decimals: Byte = 4): string;
begin
     if ESBBlankWhenZero and FloatIsZero (X) then
     begin
          Result := '';
          Exit;
     end;

     try
          Result := FloatToStrF (X, ffNumber, 7, Decimals);
          if ESBNumPosSign and (X > 0) then // See if '+' needed
               Result := '+' + Result;
     except
          Result := '';
     end;
end;

function Float2CEStr2 (const X: Extended; const Decimals: Byte = 4): string;
begin
     Result := StripTChStr (Float2CEStr (X, Decimals), '0');
     if Result [Length (Result)] = '.' then
          Result := LeftStr (Result, Length (Result) - 1);
end;

function Float2CEStr2 (const X: Double; const Decimals: Byte = 4): string;
begin
     Result := StripTChStr (Float2CEStr (X, Decimals), '0');
     if Result [Length (Result)] = '.' then
          Result := LeftStr (Result, Length (Result) - 1);
end;

function Float2CEStr2 (const X: Single; const Decimals: Byte = 4): string;
begin
     Result := StripTChStr (Float2CEStr (X, Decimals), '0');
     if Result [Length (Result)] = '.' then
          Result := LeftStr (Result, Length (Result) - 1);
end;

function Float2Str (const X: Extended; const Len: Byte;
     const Decimals: Byte = 4): string;
begin
     if ESBBlankWhenZero and FloatIsZero (X) then
     begin
          Result := BlankStr (Len);
          Exit;
     end;

     Result := PadChLeftStr (LeftStr (Float2EStr (X, Decimals), Len), ESBNumPadCh, Len);
end;

function Float2Str (const X: Double; const Len: Byte;
     const Decimals: Byte = 4): string;
begin
     if ESBBlankWhenZero and FloatIsZero (X) then
     begin
          Result := BlankStr (Len);
          Exit;
     end;

     Result := PadChLeftStr (LeftStr (Float2EStr (X, Decimals), Len), ESBNumPadCh, Len);
end;

function Float2Str (const X: Single; const Len: Byte;
     const Decimals: Byte = 4): string;
begin
     if ESBBlankWhenZero and FloatIsZero (X) then
     begin
          Result := BlankStr (Len);
          Exit;
     end;

     Result := PadChLeftStr (LeftStr (Float2EStr (X, Decimals), Len), ESBNumPadCh, Len);
end;

function Float2CStr (const X: Extended; const Len: Byte;
     const Decimals: Byte = 4): string;
begin
     if ESBBlankWhenZero and FloatIsZero (X) then
     begin
          Result := BlankStr (Len);
          Exit;
     end;

     Result := PadChLeftStr (LeftStr (Float2CEStr (X, Decimals), Len), ESBNumPadCh, Len);
end;

function Float2CStr (const X: Double; const Len: Byte;
     const Decimals: Byte = 4): string;
begin
     if ESBBlankWhenZero and FloatIsZero (X) then
     begin
          Result := BlankStr (Len);
          Exit;
     end;

     Result := PadChLeftStr (LeftStr (Float2CEStr (X, Decimals), Len), ESBNumPadCh, Len);
end;

function Float2CStr (const X: Single; const Len: Byte;
     const Decimals: Byte = 4): string;
begin
     if ESBBlankWhenZero and FloatIsZero (X) then
     begin
          Result := BlankStr (Len);
          Exit;
     end;

     Result := PadChLeftStr (LeftStr (Float2CEStr (X, Decimals), Len), ESBNumPadCh, Len);
end;

function Float2ZStr (const X: Extended; const Len: Byte;
     const Decimals: Byte = 4): string;
var
     Hold: Boolean;
     Len2: Byte;
begin
     if ESBBlankWhenZero and FloatIsZero (X) then
     begin
          Result := BlankStr (Len);
          Exit;
     end;

     Hold := ESBNumPosSign;
     try
          ESBNumPosSign := False;
          Result := Float2EStr (Abs (X), Decimals);
     finally
          ESBNumPosSign := Hold;
     end;

     if (Len > 0) and (FloatIsNegative (X) or (FloatIsPositive (X) and ESBNumPosSign)) then
          Len2 := Len - 1 // Need to leave space for the sign
     else
          Len2 := Len;
     Result := PadChLeftStr (LeftStr (Result, Len2), '0', Len2); // Pad with Zeroes
     if FloatIsNegative (X) then // Add Sign if necessary
          Result := '-' + Result
     else if FloatIsPositive (X) and ESBNumPosSign then
          Result := '+' + Result;
end;

function Float2ZStr (const X: Double; const Len: Byte;
     const Decimals: Byte = 4): string;
var
     Hold: Boolean;
     Len2: Byte;
begin
     if ESBBlankWhenZero and FloatIsZero (X) then
     begin
          Result := BlankStr (Len);
          Exit;
     end;

     Hold := ESBNumPosSign;
     try
          ESBNumPosSign := False;
          Result := Float2EStr (Abs (X), Decimals);
     finally
          ESBNumPosSign := Hold;
     end;

     if (Len > 0) and (FloatIsNegative (X) or (FloatIsPositive (X) and ESBNumPosSign)) then
          Len2 := Len - 1 // Need to leave space for the sign
     else
          Len2 := Len;
     Result := PadChLeftStr (LeftStr (Result, Len2), '0', Len2); // Pad with Zeroes
     if FloatIsNegative (X) then // Add Sign if necessary
          Result := '-' + Result
     else if FloatIsPositive (X) and ESBNumPosSign then
          Result := '+' + Result;
end;

function Float2ZStr (const X: Single; const Len: Byte;
     const Decimals: Byte = 4): string;
var
     Hold: Boolean;
     Len2: Byte;
begin
     if ESBBlankWhenZero and FloatIsZero (X) then
     begin
          Result := BlankStr (Len);
          Exit;
     end;

     Hold := ESBNumPosSign;
     try
          ESBNumPosSign := False;
          Result := Float2EStr (Abs (X), Decimals);
     finally
          ESBNumPosSign := Hold;
     end;

     if (Len > 0) and (FloatIsNegative (X) or (FloatIsPositive (X) and ESBNumPosSign)) then
          Len2 := Len - 1 // Need to leave space for the sign
     else
          Len2 := Len;
     Result := PadChLeftStr (LeftStr (Result, Len2), '0', Len2); // Pad with Zeroes
     if FloatIsNegative (X) then // Add Sign if necessary
          Result := '-' + Result
     else if FloatIsPositive (X) and ESBNumPosSign then
          Result := '+' + Result;
end;

function SciFloat2EStr (const X: Extended; const Decimals: Byte = 4): string;
begin
     if ESBBlankWhenZero and FloatIsZero (X) then
     begin
          Result := '';
          Exit;
     end;

     try
          Result := FloatToStrF (X, ffExponent, Decimals + 1, 0)
     except
          Result := '';
     end;
end;

function SciFloat2EStr (const X: Double; const Decimals: Byte = 4): string;
begin
     if ESBBlankWhenZero and FloatIsZero (X) then
     begin
          Result := '';
          Exit;
     end;

     try
          Result := FloatToStrF (X, ffExponent, Decimals + 1, 0)
     except
          Result := '';
     end;
end;

function SciFloat2EStr (const X: Single; const Decimals: Byte = 4): string;
begin
     if ESBBlankWhenZero and FloatIsZero (X) then
     begin
          Result := '';
          Exit;
     end;

     try
          Result := FloatToStrF (X, ffExponent, Decimals + 1, 0)
     except
          Result := '';
     end;
end;

function SciFloat2EStr2 (const X: Extended; const Decimals: Byte = 4): string;
var
     P: Integer;
     S1, S2: string;
begin
     if ESBBlankWhenZero and FloatIsZero (X) then
     begin
          Result := '';
          Exit;
     end;

     try
          Result := FloatToStrF (X, ffExponent, Decimals + 1, 0);
          P := ESBPosCh ('E', Result);
          if P > 0 then
          begin
               S1 := LeftStr (Result, P - 1);
               S2 := RightAfterStr (Result, P - 1);
               S1 := StripTChStr (S1, '0');
               if S1 [Length (S1)] = '.' then
                    S1 := LeftStr (S1, Length (S1) - 1);
               Result := S1 + S2;
          end;
     except
          Result := '';
     end;
end;

function SciFloat2EStr2 (const X: Double; const Decimals: Byte = 4): string;
var
     P: Integer;
     S1, S2: string;
begin
     if ESBBlankWhenZero and FloatIsZero (X) then
     begin
          Result := '';
          Exit;
     end;

     try
          Result := FloatToStrF (X, ffExponent, Decimals + 1, 0);
          P := ESBPosCh ('E', Result);
          if P > 0 then
          begin
               S1 := LeftStr (Result, P - 1);
               S2 := RightAfterStr (Result, P - 1);
               S1 := StripTChStr (S1, '0');
               if S1 [Length (S1)] = '.' then
                    S1 := LeftStr (S1, Length (S1) - 1);
               Result := S1 + S2;
          end;
     except
          Result := '';
     end;
end;

function SciFloat2EStr2 (const X: Single; const Decimals: Byte = 4): string;
var
     P: Integer;
     S1, S2: string;
begin
     if ESBBlankWhenZero and FloatIsZero (X) then
     begin
          Result := '';
          Exit;
     end;

     try
          Result := FloatToStrF (X, ffExponent, Decimals + 1, 0);
          P := ESBPosCh ('E', Result);
          if P > 0 then
          begin
               S1 := LeftStr (Result, P - 1);
               S2 := RightAfterStr (Result, P - 1);
               S1 := StripTChStr (S1, '0');
               if S1 [Length (S1)] = '.' then
                    S1 := LeftStr (S1, Length (S1) - 1);
               Result := S1 + S2;
          end;
     except
          Result := '';
     end;
end;

function SciFloat2Str (const X: Extended; const Len: Byte;
     const Decimals: Byte = 4): string;
begin
     if ESBBlankWhenZero and FloatIsZero (X) then
     begin
          Result := BlankStr (Len);
          Exit;
     end;

     Result := PadChLeftStr (LeftStr (SciFloat2EStr (X, Decimals), Len), ESBNumPadCh, Len);
end;

function SciFloat2Str (const X: Double; const Len: Byte;
     const Decimals: Byte = 4): string;
begin
     if ESBBlankWhenZero and FloatIsZero (X) then
     begin
          Result := BlankStr (Len);
          Exit;
     end;

     Result := PadChLeftStr (LeftStr (SciFloat2EStr (X, Decimals), Len), ESBNumPadCh, Len);
end;

function SciFloat2Str (const X: Single; const Len: Byte;
     const Decimals: Byte = 4): string;
begin
     if ESBBlankWhenZero and FloatIsZero (X) then
     begin
          Result := BlankStr (Len);
          Exit;
     end;

     Result := PadChLeftStr (LeftStr (SciFloat2EStr (X, Decimals), Len), ESBNumPadCh, Len);
end;

{--- Boolean Conversions ---}

function Boolean2TF (const B: Boolean): Char;
begin
     Result := iff (B, 'T', 'F');
end;

function Boolean2YN (const B: Boolean): Char;
begin
     Result := iff (B, 'Y', 'N');
end;

function Boolean2YB (const B: Boolean): Char;
begin
     Result := iff (B, 'Y', #32);
end;

function Boolean2Char (const B: Boolean; const TrueChar, FalseChar: Char): Char;
begin
     Result := iff (B, TrueChar, FalseChar);
end;

function TF2Boolean (const Ch: Char): Boolean;
begin
     Result := Ch in ['T', 't'];
end;

function YN2Boolean (const Ch: Char): Boolean; assembler;
begin
     Result := Ch in ['Y', 'y'];
end;

function Boolean2TFStr (const B: Boolean): string;
begin
     Result := iff (B, rsTrue, rsFalse);
end;

function Boolean2YNStr (const B: Boolean): string;
begin
     Result := iff (B, rsYes, rsNo);
end;

function Boolean2OnOffStr (const B: Boolean): string;
begin
     Result := iff (B, rsOn, rsOff);
end;

function AlphaCol2Int (const AlphaCol: string): Word;
var
     LA: Integer;
     S: string;
begin
     S := UpperCase (AlphaCol);
     LA := Length (S);
     if LA = 0 then
          Result := 0
     else if LA = 1 then
          Result := Ord (S [1]) - 64
     else
          Result := (Ord (S [1]) - 64) * 26 + Ord (S [2]) - 64;
end;

function IntCol2Alpha (const Col: Word): string;
var
     X: Word;
begin
     if Col > 0 then
     begin
          if Col < 27 then
               Result := Char (Col + 64)
          else
          begin
               X := (Col - 1) div 26;
               if X > 26 then
                    Result := '**'
               else
                    Result := Char (X + 64) + Char ((Col - 1) mod 26 + 65)
          end;
     end
     else
          Result := ''
end;

function IPStr2LWord (const IPAddr: string): LongWord;
var
     I, P: Integer;
     S: string;
     X: Longword;
begin
     Result := 0;
     S := IPAddr;
     for I := 1 to 4 do
     begin
          P := ESBPosCh ('.', S);
          if P > 0 then
          begin
               X := Str2LWord (LeftStr (S, P - 1));
               S := RightAfterStr (S, P);
          end
          else
          begin
               X := Str2LWord (S);
               S := '';
          end;

          if (X > 255) then
          begin
               Result := 0;
               if ESBRaiseIPError then
                    raise EMathError.Create (rsIPAddrComp)
          end
          else
               Result := Result * Int64 (256) + X;
     end;
end;

function LWord2IPStr (const IPAddr: LongWord): string;
var
     I: Integer;
     X: LongWord;
begin
     X := IPAddr;
     Result := '';
     for I := 1 to 4 do
     begin
          if I > 1 then
               Result := '.' + Result;
          Result := Int2EStr (X mod 256) + Result;
          X := X div 256;
     end;
end;

function IsCharSetStr (const S: string; const CharSet: TESBCharSet): Boolean;
var
     I: Integer;
begin
     if S = '' then
          Result := False
     else
     begin
          Result := True;
          for I := 1 to Length (S) do
          begin
               if not (S [I] in CharSet) then
               begin
                    Result := False;
                    Break;
               end;
          end;
     end;
end;

function IsDigitStr (const S: string): Boolean;
begin
     Result := IsCharSetStr (S, ['0'..'9']);
end;

function IsAlphaStr (const S: string): Boolean;
begin
     Result := IsCharSetStr (S, ['A'..'Z', 'a'..'z']);
end;

function IsAlphaNumericStr (const S: string): Boolean;
begin
     Result := IsCharSetStr (S, ['0'..'9', 'A'..'Z', 'a'..'z']);
end;

function ExtractValue (const ValuesStr: string; var P: Integer;
     const Separator: Char = ';'): string;
var
     I: Integer;
begin
     if P < 1 then
          P := 1;

     I := P;
     // Find next ';' or end of string
     while (I <= Length (ValuesStr)) and (ValuesStr [I] <> Separator) do
          Inc (I);

     // Grab the Trimmed Value
     Result := Trim (Copy (ValuesStr, P, I - P));

     // If at a ';' then jump to the next character
     if (I <= Length (ValuesStr)) and (ValuesStr [I] = Separator) then
          Inc (I);

     P := I;
end;

function ValueMatch (const ValuesStr, Value: string;
     const Separator: Char = ';'): Boolean;
var
     P: Integer;
begin
     Result := False;
     P := 1;
     while P <= Length (ValuesStr) do
     begin
          if AnsiCompareText (ExtractValue (ValuesStr, P, Separator), Value) = 0 then
          begin
               Result := True;
               Break;
          end;
     end;
end;

function Angle2Str (const Angle: Extended; const DecimalPlaces: Byte = 2): string;
var
     Degrees, Minutes: Integer;
     Seconds: Extended;
     Sign: Shortint;
begin
     Deg2DMS (Angle, Degrees, Minutes, Seconds, Sign);
     Result := Angle2Str (Degrees, Minutes, Seconds, Sign);
end;

function Angle2Str (const Degrees, Minutes: Integer; const Seconds: Extended;
     const Sign: Shortint = 1; const DecimalPlaces: Byte = 2): string;
begin
     if (Minutes < 0) or (Minutes > 59) then
          raise EConvertError.Create (rsInvalidAngle);

     if (Seconds < 0) or (Seconds >= 60) then
          raise EConvertError.Create (rsInvalidAngle);

     Result := Int2EStr (Degrees) + ESBDegreeStr
          + Int2EStr (Minutes) + ESBMinuteStr
          + Float2EStr (Seconds, DecimalPlaces) + ESBSecondStr;

     if (Degrees = 0) and (Sign = -1) then
          Result := '-' + Result;
end;

end.
