{$INCLUDE ..\cDefines.inc}
unit cUnicodeChar;

{                                                                              }
{                    Unicode character functions v3.03                         }
{                                                                              }
{      This unit is copyright © 2000-2003 by David Butler (david@e.co.za)      }
{                                                                              }
{                  This unit is part of Delphi Fundamentals.                   }
{                 Its original file name is cUnicodeChar.pas                   }
{       The latest version is available from the Fundamentals home page        }
{                     http://fundementals.sourceforge.net/                     }
{                                                                              }
{                I invite you to use this unit, free of charge.                }
{        I invite you to distibute this unit, but it must be for free.         }
{             I also invite you to contribute to its development,              }
{             but do not distribute a modified copy of this file.              }
{                                                                              }
{          A forum is available on SourceForge for general discussion          }
{             http://sourceforge.net/forum/forum.php?forum_id=2117             }
{                                                                              }
{                                                                              }
{ Description:                                                                 }
{   Unicode character constants.                                               }
{   Functions for checking unicode character properties.                       }
{   Functions to interpret unicode characters.                                 }
{   Unicode character case functions.                                          }
{                                                                              }
{ Notes:                                                                       }
{   Most functions in this unit work from tables in source code form.          }
{   All tables were generated from the Unicode 3.2 data.                       }
{                                                                              }
{   The source code is deceptively big, for example, the upper-lower case      }
{   table takes up more than 128K in source code form, but only 7K in          }
{   binary form.                                                               }
{                                                                              }
{ Revision history:                                                            }
{   19/04/2002  0.01  Initial version                                          }
{   21/04/2002  0.02  Added case and decomposition functions                   }
{   28/10/2002  3.03  Refactored for Fundamentals 3.                           }
{                                                                              }

interface

const
  UnitName      = 'cUnicodeChar';
  UnitVersion   = '3.03';
  UnitDesc      = 'Unicode character functions';


  
{                                                                              }
{ Unicode character constants                                                  }
{                                                                              }
const
  WideNULL = WideChar(#0);
  WideSOH  = WideChar(#1);
  WideSTX  = WideChar(#2);
  WideETX  = WideChar(#3);
  WideEOT  = WideChar(#4);
  WideENQ  = WideChar(#5);
  WideACK  = WideChar(#6);
  WideBEL  = WideChar(#7);
  WideBS   = WideChar(#8);
  WideHT   = WideChar(#9);
  WideLF   = WideChar(#10);
  WideVT   = WideChar(#11);
  WideFF   = WideChar(#12);
  WideCR   = WideChar(#13);
  WideNAK  = WideChar(#21);
  WideSYN  = WideChar(#22);
  WideCAN  = WideChar(#24);
  WideEOF  = WideChar(#26);
  WideESC  = WideChar(#27);
  WideSP   = WideChar(#32);

  WideCRLF : WideString = #13#10;

  WideSingleQuote = WideChar('''');
  WideDoubleQuote = WideChar('"');

  WideNoBreakSpace       = WideChar(#$00A0);
  WideLineSeparator      = WideChar(#$2028);
  WideParagraphSeparator = WideChar(#$2029);

  WideBOM_CPU            = WideChar(#$FFFE);
  WideBOM_Reversed       = WideChar(#$FEFF);

  WideObjectReplacement  = WideChar(#$FFFC);
  WideCharReplacement    = WideChar(#$FFFD);
  WideInvalid            = WideChar(#$FFFF);

  WideCopyrightSign      = WideChar(#$00A9);
  WideRegisteredSign     = WideChar(#$00AE);

  WideHighSurrogateFirst        = WideChar(#$D800);
  WideHighSurrogateLast         = WideChar(#$DB7F);
  WideLowSurrogateFirst         = WideChar(#$DC00);
  WideLowSurrogateLast          = WideChar(#$DFFF);
  WidePrivateHighSurrogateFirst = WideChar(#$DB80);
  WidePrivateHighSurrogateLast  = WideChar(#$DBFF);



{                                                                              }
{ Unicode character functions                                                  }
{                                                                              }
{$IFDEF DELPHI5}
type
  UCS4Char = LongWord;
{$ENDIF}

type
  WideCharMatchFunction = function (const Ch: WideChar): Boolean;

function  IsASCIIChar(const Ch: WideChar): Boolean;
function  IsWhiteSpace(const Ch: WideChar): Boolean;
function  IsControl(const Ch: WideChar): Boolean;
function  IsControlOrWhiteSpace(const Ch: WideChar): Boolean;
function  IsIgnorable(const Ch: UCS4Char): Boolean;

function  IsDash(const Ch: WideChar): Boolean;
function  IsHyphen(const Ch: WideChar): Boolean;
function  IsFullStop(const Ch: WideChar): Boolean;
function  IsComma(const Ch: WideChar): Boolean;
function  IsExclamationMark(const Ch: WideChar): Boolean;
function  IsQuestionMark(const Ch: WideChar): Boolean;

function  IsLeftParenthesis(const Ch: WideChar): Boolean;
function  IsLeftBracket(const Ch: WideChar): Boolean;
function  GetRightParenthesis(const LeftParenthesis: WideChar): WideChar;
function  GetRightBracket(const LeftBracket: WideChar): WideChar;

function  IsSingularQuotationMark(const Ch: WideChar): Boolean;
function  IsOpeningQuotationMark(const Ch: WideChar): Boolean;
function  IsClosingQuotationMark(const Ch: WideChar): Boolean;
function  GetClosingQuotationMark(const OpeningQuote: WideChar): WideChar;
function  GetOpeningQuotationMark(const ClosingQuote: WideChar): WideChar;

function  IsPunctuation(const Ch: WideChar): Boolean;

function  IsDecimalDigit(const Ch: UCS4Char): Boolean; overload;
function  IsDecimalDigit(const Ch: WideChar): Boolean; overload;
function  DecimalDigitValue(const Ch: UCS4Char): Integer; overload;
function  DecimalDigitValue(const Ch: WideChar): Integer; overload;
function  FractionCharacterValue(const Ch: WideChar; var A, B: Integer): Boolean;
function  RomanNumeralValue(const Ch: WideChar): Integer;

function  IsHexDigit(const Ch: UCS4Char): Boolean; overload;
function  IsHexDigit(const Ch: WideChar): Boolean; overload;
function  HexDigitValue(const Ch: UCS4Char): Integer; overload;
function  HexDigitValue(const Ch: WideChar): Integer; overload;

function  IsUpperCase(const Ch: WideChar): Boolean;
function  IsLowerCase(const Ch: WideChar): Boolean;
function  IsTitleCase(const Ch: WideChar): Boolean;
function  WideUpCase(const Ch: WideChar): WideChar;
function  WideLowCase(const Ch: WideChar): WideChar;
function  WideUpCaseFolding(const Ch: WideChar): WideString;
function  WideLowCaseFolding(const Ch: WideChar): WideString;
function  WideTitleCaseFolding(const Ch: WideChar): WideString;
function  WideIsEqualNoCase(const A, B: WideChar): Boolean;
function  IsLetter(const Ch: WideChar): Boolean;
function  IsAlphabetic(const Ch: WideChar): Boolean;

function  GetCombiningClass(const Ch: WideChar): Byte;
function  GetCharacterDecomposition(const Ch: UCS4Char): WideString; overload;
function  GetCharacterDecomposition(const Ch: WideChar): WideString; overload;



implementation



{                                                                              }
{ Character functions                                                          }
{                                                                              }
function IsASCIIChar(const Ch: WideChar): Boolean;
begin
  Result := Ord(Ch) <= $7F;
end;

function IsWhiteSpace(const Ch: WideChar): Boolean;
begin
  Case Ch of
    #$0009..#$000D,    // ASCII CONTROL
    #$0020,            // SPACE
    #$0085,            // <control>
    #$00A0,            // NO-BREAK SPACE
    #$1680,            // OGHAM SPACE MARK
    #$2000..#$200A,    // EN QUAD..HAIR SPACE
    #$2028,            // LINE SEPARATOR
    #$2029,            // PARAGRAPH SEPARATOR
    #$202F,            // NARROW NO-BREAK SPACE
    #$3000 :           // IDEOGRAPHIC SPACE
      Result := True;
  else
    Result := False;
  end;
end;

function IsControl(const Ch: WideChar): Boolean;
begin
  Case Ch of
    #$0000..#$001F,
    #$007F..#$009F :
      Result := True;
  else
    Result := False;
  end;
end;

function IsControlOrWhiteSpace(const Ch: WideChar): Boolean;
begin
  Result := IsControl(Ch) or IsWhiteSpace(Ch);
end;

// Derived from 'Cf' + 'Cc' + 'Cs' - White_Space
function IsIgnorable(const Ch: UCS4Char): Boolean;
begin
  Case Ch of
    $0000..$0008,     // # Cc   [9] <control>..<control>
    $000E..$001F,     // # Cc  [18] <control>..<control>
    $007F..$0084,     // # Cc   [6] <control>..<control>
    $0086..$009F,     // # Cc  [26] <control>..<control>
    $06DD,            // # Cf       ARABIC END OF AYAH
    $070F,            // # Cf       SYRIAC ABBREVIATION MARK
    $180B..$180D,     // # Mn   [3] MONGOLIAN FREE VARIATION SELECTOR ONE..MONGOLIAN FREE VARIATION SELECTOR THREE
    $180E,            // # Cf       MONGOLIAN VOWEL SEPARATOR
    $200C..$200F,     // # Cf   [4] ZERO WIDTH NON-JOINER..RIGHT-TO-LEFT MARK
    $202A..$202E,     // # Cf   [5] LEFT-TO-RIGHT EMBEDDING..RIGHT-TO-LEFT OVERRIDE
    $2060..$2063,     // # Cf   [4] WORD JOINER..INVISIBLE SEPARATOR
    $2064..$2069,     // # Cn   [6]
    $206A..$206F,     // # Cf   [6] INHIBIT SYMMETRIC SWAPPING..NOMINAL DIGIT SHAPES
    $D800..$DFFF,     // # Cs [2048]
    $FE00..$FE0F,     // # Mn  [16] VARIATION SELECTOR-1..VARIATION SELECTOR-16
    $FEFF,            // # Cf       ZERO WIDTH NO-BREAK SPACE
    $FFF0..$FFF8,     // # Cn   [9]
    $FFF9..$FFFB,     // # Cf   [3] INTERLINEAR ANNOTATION ANCHOR..INTERLINEAR ANNOTATION TERMINATOR
    $1D173..$1D17A,   // # Cf   [8] MUSICAL SYMBOL BEGIN BEAM..MUSICAL SYMBOL END PHRASE
    $E0000,           // # Cn
    $E0001,           // # Cf       LANGUAGE TAG
    $E0002..$E001F,   // # Cn  [30]
    $E0020..$E007F,   // # Cf  [96] TAG SPACE..CANCEL TAG
    $E0080..$E0FFF :  // # Cn [3968]
      Result := True;
  else
    Result := False;
  end;
end;

function IsDash(const Ch: WideChar): Boolean;
begin
  Case Ch of
    #$002D,            // HYPHEN-MINUS
    #$00AD,            // SOFT HYPHEN
    #$058A,            // ARMENIAN HYPHEN
    #$1806,            // MONGOLIAN TODO SOFT HYPHEN
    #$2010..#$2015,    // HYPHEN..HORIZONTAL BAR
    #$207B,            // SUPERSCRIPT MINUS
    #$208B,            // SUBSCRIPT MINUS
    #$2212,            // MINUS SIGN
    #$301C,            // WAVE DASH
    #$3030,            // WAVY DASH
    #$FE31..#$FE32,    // PRESENTATION FORM FOR VERTICAL EM DASH..PRESENTATION FORM FOR VERTICAL EN DASH
    #$FE58,            // SMALL EM DASH
    #$FE63,            // SMALL HYPHEN-MINUS
    #$FF0D :           // FULLWIDTH HYPHEN-MINUS
      Result := True;
  else
    Result := False;
  end;
end;

function IsHyphen(const Ch: WideChar): Boolean;
begin
  Case Ch of
    #$002D,            // HYPHEN-MINUS
    #$00AD,            // SOFT HYPHEN
    #$058A,            // ARMENIAN HYPHEN
    #$1806,            // MONGOLIAN TODO SOFT HYPHEN
    #$2010..#$2011,    // HYPHEN..NON-BREAKING HYPHEN
    #$30FB,            // KATAKANA MIDDLE DOT
    #$FE63,            // SMALL HYPHEN-MINUS
    #$FF0D,            // FULLWIDTH HYPHEN-MINUS
    #$FF65 :           // HALFWIDTH KATAKANA MIDDLE DOT
      Result := True;
  else
    Result := False;
  end;
end;

function IsFullStop(const Ch: WideChar): Boolean;
begin
  Case Ord(Ch) of
    $002E,  // FULL STOP
    $0589,  // ARMENIAN FULL STOP
    $06D4,  // ARABIC FULL STOP
    $0701,  // SYRIAC SUPRALINEAR FULL STOP
    $0702,  // SYRIAC SUBLINEAR FULL STOP
    $1362,  // ETHIOPIC FULL STOP
    $166E,  // CANADIAN SYLLABICS FULL STOP
    $1803,  // MONGOLIAN FULL STOP
    $1809,  // MONGOLIAN MANCHU FULL STOP
    $3002,  // IDEOGRAPHIC FULL STOP
    $FE52,  // SMALL FULL STOP
    $FF0E,  // FULLWIDTH FULL STOP
    $FF61 : // HALFWIDTH IDEOGRAPHIC FULL STOP
      Result := True;
  else
    Result := False;
  end;
end;

function IsComma(const Ch: WideChar): Boolean;
begin
  Case Ord(Ch) of
    $002C,  // COMMA
    $055D,  // ARMENIAN COMMA
    $060C,  // ARABIC COMMA
    $0F14,  // TIBETAN MARK GTER TSHEG
    $1363,  // ETHIOPIC COMMA
    $1802,  // MONGOLIAN COMMA
    $1808,  // MONGOLIAN MANCHU COMMA
    $3001,  // IDEOGRAPHIC COMMA
    $FE50,  // SMALL COMMA
    $FE51,  // SMALL IDEOGRAPHIC COMMA
    $FF0C,  // FULLWIDTH COMMA
    $FF64 : // HALFWIDTH IDEOGRAPHIC COMMA
      Result := True;
  else
    Result := False;
  end;
end;

function IsExclamationMark(const Ch: WideChar): Boolean;
begin
  Case Ord(Ch) of
    $0021,    // EXCLAMATION MARK
    $00A1,    // INVERTED EXCLAMATION MARK
    $055C,    // ARMENIAN EXCLAMATION MARK
    $203C,    // DOUBLE EXCLAMATION MARK
    $203D,    // INTERROBANG
    $2048,    // QUESTION EXCLAMATION MARK
    $2049,    // EXCLAMATION QUESTION MARK
    $FE57,    // SMALL EXCLAMATION MARK
    $FF01 :   // FULLWIDTH EXCLAMATION MARK
      Result := True;
  else
    Result := False;
  end;
end;

function IsQuestionMark(const Ch: WideChar): Boolean;
begin
  Case Ord(Ch) of
    $003F,    // QUESTION MARK
    $00BF,    // INVERTED QUESTION MARK
    $037E,    // GREEK QUESTION MARK
    $055E,    // ARMENIAN QUESTION MARK
    $061F,    // ARABIC QUESTION MARK
    $1367,    // ETHIOPIC QUESTION MARK
    $2049,    // EXCLAMATION QUESTION MARK
    $FE56,    // SMALL QUESTION MARK
    $FF1F :   // FULLWIDTH QUESTION MARK
      Result := True;
  else
    Result := False;
  end;
end;

function GetRightParenthesis(const LeftParenthesis: WideChar): WideChar;
begin
  Case Ord(LeftParenthesis) of
    $0028 : Result := #$0029;  // PARENTHESIS
    $207D : Result := #$207E;  // SUPERSCRIPT PARENTHESIS
    $208D : Result := #$208E;  // SUBSCRIPT PARENTHESIS
    $FD3E : Result := #$FD3F;  // ORNATE PARENTHESIS
    $FE35 : Result := #$FE36;  // PRESENTATION FORM FOR VERTICAL PARENTHESIS
    $FE59 : Result := #$FE5A;  // SMALL PARENTHESIS
    $FF08 : Result := #$FF09;  // FULLWIDTH PARENTHESIS
  else
    Result := #$0000;
  end;
end;

function IsLeftParenthesis(const Ch: WideChar): Boolean;
begin
  Result := GetRightParenthesis(Ch) <> #$0000;
end;

function GetRightBracket(const LeftBracket: WideChar): WideChar;
begin
  Case Ord(LeftBracket) of
    $005B : Result := #$005D;  // SQUARE BRACKET
    $007B : Result := #$007D;  // CURLY BRACKET
    $2045 : Result := #$2046;  // SQUARE BRACKET WITH QUILL
    $2329 : Result := #$232A;  // POINTING ANGLE BRACKET
    $3008 : Result := #$3009;  // ANGLE BRACKET
    $300A : Result := #$300B;  // DOUBLE ANGLE BRACKET
    $300C : Result := #$300D;  // CORNER BRACKET
    $300E : Result := #$300F;  // WHITE CORNER BRACKET
    $3010 : Result := #$3011;  // BLACK LENTICULAR BRACKET
    $3014 : Result := #$3015;  // TORTOISE SHELL BRACKET
    $3016 : Result := #$3017;  // WHITE LENTICULAR BRACKET
    $3018 : Result := #$3019;  // WHITE TORTOISE SHELL BRACKET
    $301A : Result := #$301B;  // WHITE SQUARE BRACKET
    $FE37 : Result := #$FE38;  // PRESENTATION FORM FOR VERTICAL CURLY BRACKET
    $FE39 : Result := #$FE3A;  // PRESENTATION FORM FOR VERTICAL TORTOISE SHELL BRACKET
    $FE3B : Result := #$FE3C;  // PRESENTATION FORM FOR VERTICAL BLACK LENTICULAR BRACKET
    $FE3D : Result := #$FE3E;  // PRESENTATION FORM FOR VERTICAL DOUBLE ANGLE BRACKET
    $FE3F : Result := #$FE40;  // PRESENTATION FORM FOR VERTICAL ANGLE BRACKET
    $FE41 : Result := #$FE42;  // PRESENTATION FORM FOR VERTICAL CORNER BRACKET
    $FE43 : Result := #$FE44;  // PRESENTATION FORM FOR VERTICAL WHITE CORNER BRACKET
    $FE5B : Result := #$FE5C;  // SMALL CURLY BRACKET
    $FE5D : Result := #$FE5E;  // SMALL TORTOISE SHELL BRACKET
    $FF3B : Result := #$FF3D;  // FULLWIDTH SQUARE BRACKET
    $FF5B : Result := #$FF5D;  // FULLWIDTH CURLY BRACKET
    $FF62 : Result := #$FF63;  // HALFWIDTH CORNER BRACKET
  else
    Result := #$0000;
  end;
end;

function IsLeftBracket(const Ch: WideChar): Boolean;
begin
  Result := GetRightBracket(Ch) <> #$0000;
end;

function IsSingularQuotationMark(const Ch: WideChar): Boolean;
begin
  Case Ord(Ch) of
    $0022,   //        QUOTATION MARK
    $0027,   //        APOSTROPHE
    $FF02,   //        FULLWIDTH QUOTATION MARK
    $FF07 :  //        FULLWIDTH APOSTROPHE
      Result := True;
  else
    Result := False;
  end;
end;

function GetClosingQuotationMark(const OpeningQuote: WideChar): WideChar;
begin
  Case Ord(OpeningQuote) of
    $00AB : Result := #$00BB;     // LEFT/RIGHT -POINTING DOUBLE ANGLE QUOTATION MARK
    $2018 : Result := #$2019;     // LEFT/RIGHT SINGLE QUOTATION MARK
    $201A : Result := #$201B;     // SINGLE LOW-9 QUOTATION MARK / SINGLE HIGH-REVERSED-9 QUOTATION MARK
    $201C : Result := #$201D;     // LEFT/RIGHT DOUBLE QUOTATION MARK
    $201E : Result := #$201F;     // DOUBLE LOW-9 QUOTATION MARK / DOUBLE HIGH-REVERSED-9 QUOTATION MARK
    $2039 : Result := #$203A;     // SINGLE LEFT/RIGHT -POINTING ANGLE QUOTATION MARK
    $301D : Result := #$301E;     // REVERSED DOUBLE PRIME QUOTATION MARK / DOUBLE PRIME QUOTATION MARK (also $301F)
  else
    Result := #$0000;
  end;
end;

function IsOpeningQuotationMark(const Ch: WideChar): Boolean;
begin
  Result := GetClosingQuotationMark(Ch) <> #$0000;
end;

function GetOpeningQuotationMark(const ClosingQuote: WideChar): WideChar;
begin
  Case Ord(ClosingQuote) of
    $00BB : Result := #$00AB;     // LEFT/RIGHT -POINTING DOUBLE ANGLE QUOTATION MARK
    $2019 : Result := #$2018;     // LEFT/RIGHT SINGLE QUOTATION MARK
    $201B : Result := #$201A;     // SINGLE LOW-9 QUOTATION MARK / SINGLE HIGH-REVERSED-9 QUOTATION MARK
    $201D : Result := #$201C;     // LEFT/RIGHT DOUBLE QUOTATION MARK
    $201F : Result := #$201E;     // DOUBLE LOW-9 QUOTATION MARK / DOUBLE HIGH-REVERSED-9 QUOTATION MARK
    $203A : Result := #$2039;     // SINGLE LEFT/RIGHT -POINTING ANGLE QUOTATION MARK
    $301E : Result := #$301D;     // REVERSED DOUBLE PRIME QUOTATION MARK / DOUBLE PRIME QUOTATION MARK
    $301F : Result := #$301D;     // REVERSED DOUBLE PRIME QUOTATION MARK / LOW DOUBLE PRIME QUOTATION MARK
  else
    Result := #$0000;
  end;
end;

function IsClosingQuotationMark(const Ch: WideChar): Boolean;
begin
  Result := GetOpeningQuotationMark(Ch) <> #$0000;
end;

function IsPunctuation(const Ch: WideChar): Boolean;
begin
  Case Ord(Ch) of
    $0021,   // EXCLAMATION MARK
    $0022,   // QUOTATION MARK
    $0023,   // NUMBER SIGN
    $0025,   // PERCENT SIGN
    $0026,   // AMPERSAND
    $0027,   // APOSTROPHE
    $0028,   // LEFT PARENTHESIS
    $0029,   // RIGHT PARENTHESIS
    $002A,   // ASTERISK
    $002C,   // COMMA
    $002D,   // HYPHEN-MINUS
    $002E,   // FULL STOP
    $002F,   // SOLIDUS
    $003A,   // COLON
    $003B,   // SEMICOLON
    $003F,   // QUESTION MARK
    $0040,   // COMMERCIAL AT
    $005B,   // LEFT SQUARE BRACKET
    $005C,   // REVERSE SOLIDUS
    $005D,   // RIGHT SQUARE BRACKET
    $005F,   // LOW LINE
    $007B,   // LEFT CURLY BRACKET
    $007D,   // RIGHT CURLY BRACKET
    $00A1,   // INVERTED EXCLAMATION MARK
    $00AB,   // LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
    $00AD,   // SOFT HYPHEN
    $00B7,   // MIDDLE DOT
    $00BB,   // RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
    $00BF,   // INVERTED QUESTION MARK
    $037E,   // GREEK QUESTION MARK
    $0387,   // GREEK ANO TELEIA
    $055A,   // ARMENIAN APOSTROPHE
    $055B,   // ARMENIAN EMPHASIS MARK
    $055C,   // ARMENIAN EXCLAMATION MARK
    $055D,   // ARMENIAN COMMA
    $055E,   // ARMENIAN QUESTION MARK
    $055F,   // ARMENIAN ABBREVIATION MARK
    $0589,   // ARMENIAN FULL STOP
    $058A,   // ARMENIAN HYPHEN
    $05BE,   // HEBREW PUNCTUATION MAQAF
    $05C0,   // HEBREW PUNCTUATION PASEQ
    $05C3,   // HEBREW PUNCTUATION SOF PASUQ
    $05F3,   // HEBREW PUNCTUATION GERESH
    $05F4,   // HEBREW PUNCTUATION GERSHAYIM
    $060C,   // ARABIC COMMA
    $061B,   // ARABIC SEMICOLON
    $061F,   // ARABIC QUESTION MARK
    $066A,   // ARABIC PERCENT SIGN
    $066B,   // ARABIC DECIMAL SEPARATOR
    $066C,   // ARABIC THOUSANDS SEPARATOR
    $066D,   // ARABIC FIVE POINTED STAR
    $06D4,   // ARABIC FULL STOP
    $0700,   // SYRIAC END OF PARAGRAPH
    $0701,   // SYRIAC SUPRALINEAR FULL STOP
    $0702,   // SYRIAC SUBLINEAR FULL STOP
    $0703,   // SYRIAC SUPRALINEAR COLON
    $0704,   // SYRIAC SUBLINEAR COLON
    $0705,   // SYRIAC HORIZONTAL COLON
    $0706,   // SYRIAC COLON SKEWED LEFT
    $0707,   // SYRIAC COLON SKEWED RIGHT
    $0708,   // SYRIAC SUPRALINEAR COLON SKEWED LEFT
    $0709,   // SYRIAC SUBLINEAR COLON SKEWED RIGHT
    $070A,   // SYRIAC CONTRACTION
    $070B,   // SYRIAC HARKLEAN OBELUS
    $070C,   // SYRIAC HARKLEAN METOBELUS
    $070D,   // SYRIAC HARKLEAN ASTERISCUS
    $0964,   // DEVANAGARI DANDA
    $0965,   // DEVANAGARI DOUBLE DANDA
    $0970,   // DEVANAGARI ABBREVIATION SIGN
    $0DF4,   // SINHALA PUNCTUATION KUNDDALIYA
    $0E4F,   // THAI CHARACTER FONGMAN
    $0E5A,   // THAI CHARACTER ANGKHANKHU
    $0E5B,   // THAI CHARACTER KHOMUT
    $0F04,   // TIBETAN MARK INITIAL YIG MGO MDUN MA
    $0F05,   // TIBETAN MARK CLOSING YIG MGO SGAB MA
    $0F06,   // TIBETAN MARK CARET YIG MGO PHUR SHAD MA
    $0F07,   // TIBETAN MARK YIG MGO TSHEG SHAD MA
    $0F08,   // TIBETAN MARK SBRUL SHAD
    $0F09,   // TIBETAN MARK BSKUR YIG MGO
    $0F0A,   // TIBETAN MARK BKA- SHOG YIG MGO
    $0F0B,   // TIBETAN MARK INTERSYLLABIC TSHEG
    $0F0C,   // TIBETAN MARK DELIMITER TSHEG BSTAR
    $0F0D,   // TIBETAN MARK SHAD
    $0F0E,   // TIBETAN MARK NYIS SHAD
    $0F0F,   // TIBETAN MARK TSHEG SHAD
    $0F10,   // TIBETAN MARK NYIS TSHEG SHAD
    $0F11,   // TIBETAN MARK RIN CHEN SPUNGS SHAD
    $0F12,   // TIBETAN MARK RGYA GRAM SHAD
    $0F3A,   // TIBETAN MARK GUG RTAGS GYON
    $0F3B,   // TIBETAN MARK GUG RTAGS GYAS
    $0F3C,   // TIBETAN MARK ANG KHANG GYON
    $0F3D,   // TIBETAN MARK ANG KHANG GYAS
    $0F85,   // TIBETAN MARK PALUTA
    $104A,   // MYANMAR SIGN LITTLE SECTION
    $104B,   // MYANMAR SIGN SECTION
    $104C,   // MYANMAR SYMBOL LOCATIVE
    $104D,   // MYANMAR SYMBOL COMPLETED
    $104E,   // MYANMAR SYMBOL AFOREMENTIONED
    $104F,   // MYANMAR SYMBOL GENITIVE
    $10FB,   // GEORGIAN PARAGRAPH SEPARATOR
    $1361,   // ETHIOPIC WORDSPACE
    $1362,   // ETHIOPIC FULL STOP
    $1363,   // ETHIOPIC COMMA
    $1364,   // ETHIOPIC SEMICOLON
    $1365,   // ETHIOPIC COLON
    $1366,   // ETHIOPIC PREFACE COLON
    $1367,   // ETHIOPIC QUESTION MARK
    $1368,   // ETHIOPIC PARAGRAPH SEPARATOR
    $166D,   // CANADIAN SYLLABICS CHI SIGN
    $166E,   // CANADIAN SYLLABICS FULL STOP
    $169B,   // OGHAM FEATHER MARK
    $169C,   // OGHAM REVERSED FEATHER MARK
    $16EB,   // RUNIC SINGLE PUNCTUATION
    $16EC,   // RUNIC MULTIPLE PUNCTUATION
    $16ED,   // RUNIC CROSS PUNCTUATION
    $17D4,   // KHMER SIGN KHAN
    $17D5,   // KHMER SIGN BARIYOOSAN
    $17D6,   // KHMER SIGN CAMNUC PII KUUH
    $17D7,   // KHMER SIGN LEK TOO
    $17D8,   // KHMER SIGN BEYYAL
    $17D9,   // KHMER SIGN PHNAEK MUAN
    $17DA,   // KHMER SIGN KOOMUUT
    $17DC,   // KHMER SIGN AVAKRAHASANYA
    $1800,   // MONGOLIAN BIRGA
    $1801,   // MONGOLIAN ELLIPSIS
    $1802,   // MONGOLIAN COMMA
    $1803,   // MONGOLIAN FULL STOP
    $1804,   // MONGOLIAN COLON
    $1805,   // MONGOLIAN FOUR DOTS
    $1806,   // MONGOLIAN TODO SOFT HYPHEN
    $1807,   // MONGOLIAN SIBE SYLLABLE BOUNDARY MARKER
    $1808,   // MONGOLIAN MANCHU COMMA
    $1809,   // MONGOLIAN MANCHU FULL STOP
    $180A,   // MONGOLIAN NIRUGU
    $2010,   // HYPHEN
    $2011,   // NON-BREAKING HYPHEN
    $2012,   // FIGURE DASH
    $2013,   // EN DASH
    $2014,   // EM DASH
    $2015,   // HORIZONTAL BAR
    $2016,   // DOUBLE VERTICAL LINE
    $2017,   // DOUBLE LOW LINE
    $2018,   // LEFT SINGLE QUOTATION MARK
    $2019,   // RIGHT SINGLE QUOTATION MARK
    $201A,   // SINGLE LOW-9 QUOTATION MARK
    $201B,   // SINGLE HIGH-REVERSED-9 QUOTATION MARK
    $201C,   // LEFT DOUBLE QUOTATION MARK
    $201D,   // RIGHT DOUBLE QUOTATION MARK
    $201E,   // DOUBLE LOW-9 QUOTATION MARK
    $201F,   // DOUBLE HIGH-REVERSED-9 QUOTATION MARK
    $2020,   // DAGGER
    $2021,   // DOUBLE DAGGER
    $2022,   // BULLET
    $2023,   // TRIANGULAR BULLET
    $2024,   // ONE DOT LEADER
    $2025,   // TWO DOT LEADER
    $2026,   // HORIZONTAL ELLIPSIS
    $2027,   // HYPHENATION POINT
    $2030,   // PER MILLE SIGN
    $2031,   // PER TEN THOUSAND SIGN
    $2032,   // PRIME
    $2033,   // DOUBLE PRIME
    $2034,   // TRIPLE PRIME
    $2035,   // REVERSED PRIME
    $2036,   // REVERSED DOUBLE PRIME
    $2037,   // REVERSED TRIPLE PRIME
    $2038,   // CARET
    $2039,   // SINGLE LEFT-POINTING ANGLE QUOTATION MARK
    $203A,   // SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
    $203B,   // REFERENCE MARK
    $203C,   // DOUBLE EXCLAMATION MARK
    $203D,   // INTERROBANG
    $203E,   // OVERLINE
    $203F,   // UNDERTIE
    $2040,   // CHARACTER TIE
    $2041,   // CARET INSERTION POINT
    $2042,   // ASTERISM
    $2043,   // HYPHEN BULLET
    $2045,   // LEFT SQUARE BRACKET WITH QUILL
    $2046,   // RIGHT SQUARE BRACKET WITH QUILL
    $2048,   // QUESTION EXCLAMATION MARK
    $2049,   // EXCLAMATION QUESTION MARK
    $204A,   // TIRONIAN SIGN ET
    $204B,   // REVERSED PILCROW SIGN
    $204C,   // BLACK LEFTWARDS BULLET
    $204D,   // BLACK RIGHTWARDS BULLET
    $207D,   // SUPERSCRIPT LEFT PARENTHESIS
    $207E,   // SUPERSCRIPT RIGHT PARENTHESIS
    $208D,   // SUBSCRIPT LEFT PARENTHESIS
    $208E,   // SUBSCRIPT RIGHT PARENTHESIS
    $2329,   // LEFT-POINTING ANGLE BRACKET
    $232A,   // RIGHT-POINTING ANGLE BRACKET
    $3001,   // IDEOGRAPHIC COMMA
    $3002,   // IDEOGRAPHIC FULL STOP
    $3003,   // DITTO MARK
    $3008,   // LEFT ANGLE BRACKET
    $3009,   // RIGHT ANGLE BRACKET
    $300A,   // LEFT DOUBLE ANGLE BRACKET
    $300B,   // RIGHT DOUBLE ANGLE BRACKET
    $300C,   // LEFT CORNER BRACKET
    $300D,   // RIGHT CORNER BRACKET
    $300E,   // LEFT WHITE CORNER BRACKET
    $300F,   // RIGHT WHITE CORNER BRACKET
    $3010,   // LEFT BLACK LENTICULAR BRACKET
    $3011,   // RIGHT BLACK LENTICULAR BRACKET
    $3014,   // LEFT TORTOISE SHELL BRACKET
    $3015,   // RIGHT TORTOISE SHELL BRACKET
    $3016,   // LEFT WHITE LENTICULAR BRACKET
    $3017,   // RIGHT WHITE LENTICULAR BRACKET
    $3018,   // LEFT WHITE TORTOISE SHELL BRACKET
    $3019,   // RIGHT WHITE TORTOISE SHELL BRACKET
    $301A,   // LEFT WHITE SQUARE BRACKET
    $301B,   // RIGHT WHITE SQUARE BRACKET
    $301C,   // WAVE DASH
    $301D,   // REVERSED DOUBLE PRIME QUOTATION MARK
    $301E,   // DOUBLE PRIME QUOTATION MARK
    $301F,   // LOW DOUBLE PRIME QUOTATION MARK
    $3030,   // WAVY DASH
    $30FB,   // KATAKANA MIDDLE DOT
    $FD3E,   // ORNATE LEFT PARENTHESIS
    $FD3F,   // ORNATE RIGHT PARENTHESIS
    $FE30,   // PRESENTATION FORM FOR VERTICAL TWO DOT LEADER
    $FE31,   // PRESENTATION FORM FOR VERTICAL EM DASH
    $FE32,   // PRESENTATION FORM FOR VERTICAL EN DASH
    $FE33,   // PRESENTATION FORM FOR VERTICAL LOW LINE
    $FE34,   // PRESENTATION FORM FOR VERTICAL WAVY LOW LINE
    $FE35,   // PRESENTATION FORM FOR VERTICAL LEFT PARENTHESIS
    $FE36,   // PRESENTATION FORM FOR VERTICAL RIGHT PARENTHESIS
    $FE37,   // PRESENTATION FORM FOR VERTICAL LEFT CURLY BRACKET
    $FE38,   // PRESENTATION FORM FOR VERTICAL RIGHT CURLY BRACKET
    $FE39,   // PRESENTATION FORM FOR VERTICAL LEFT TORTOISE SHELL BRACKET
    $FE3A,   // PRESENTATION FORM FOR VERTICAL RIGHT TORTOISE SHELL BRACKET
    $FE3B,   // PRESENTATION FORM FOR VERTICAL LEFT BLACK LENTICULAR BRACKET
    $FE3C,   // PRESENTATION FORM FOR VERTICAL RIGHT BLACK LENTICULAR BRACKET
    $FE3D,   // PRESENTATION FORM FOR VERTICAL LEFT DOUBLE ANGLE BRACKET
    $FE3E,   // PRESENTATION FORM FOR VERTICAL RIGHT DOUBLE ANGLE BRACKET
    $FE3F,   // PRESENTATION FORM FOR VERTICAL LEFT ANGLE BRACKET
    $FE40,   // PRESENTATION FORM FOR VERTICAL RIGHT ANGLE BRACKET
    $FE41,   // PRESENTATION FORM FOR VERTICAL LEFT CORNER BRACKET
    $FE42,   // PRESENTATION FORM FOR VERTICAL RIGHT CORNER BRACKET
    $FE43,   // PRESENTATION FORM FOR VERTICAL LEFT WHITE CORNER BRACKET
    $FE44,   // PRESENTATION FORM FOR VERTICAL RIGHT WHITE CORNER BRACKET
    $FE49,   // DASHED OVERLINE
    $FE4A,   // CENTRELINE OVERLINE
    $FE4B,   // WAVY OVERLINE
    $FE4C,   // DOUBLE WAVY OVERLINE
    $FE4D,   // DASHED LOW LINE
    $FE4E,   // CENTRELINE LOW LINE
    $FE4F,   // WAVY LOW LINE
    $FE50,   // SMALL COMMA
    $FE51,   // SMALL IDEOGRAPHIC COMMA
    $FE52,   // SMALL FULL STOP
    $FE54,   // SMALL SEMICOLON
    $FE55,   // SMALL COLON
    $FE56,   // SMALL QUESTION MARK
    $FE57,   // SMALL EXCLAMATION MARK
    $FE58,   // SMALL EM DASH
    $FE59,   // SMALL LEFT PARENTHESIS
    $FE5A,   // SMALL RIGHT PARENTHESIS
    $FE5B,   // SMALL LEFT CURLY BRACKET
    $FE5C,   // SMALL RIGHT CURLY BRACKET
    $FE5D,   // SMALL LEFT TORTOISE SHELL BRACKET
    $FE5E,   // SMALL RIGHT TORTOISE SHELL BRACKET
    $FE5F,   // SMALL NUMBER SIGN
    $FE60,   // SMALL AMPERSAND
    $FE61,   // SMALL ASTERISK
    $FE63,   // SMALL HYPHEN-MINUS
    $FE68,   // SMALL REVERSE SOLIDUS
    $FE6A,   // SMALL PERCENT SIGN
    $FE6B,   // SMALL COMMERCIAL AT
    $FF01,   // FULLWIDTH EXCLAMATION MARK
    $FF02,   // FULLWIDTH QUOTATION MARK
    $FF03,   // FULLWIDTH NUMBER SIGN
    $FF05,   // FULLWIDTH PERCENT SIGN
    $FF06,   // FULLWIDTH AMPERSAND
    $FF07,   // FULLWIDTH APOSTROPHE
    $FF08,   // FULLWIDTH LEFT PARENTHESIS
    $FF09,   // FULLWIDTH RIGHT PARENTHESIS
    $FF0A,   // FULLWIDTH ASTERISK
    $FF0C,   // FULLWIDTH COMMA
    $FF0D,   // FULLWIDTH HYPHEN-MINUS
    $FF0E,   // FULLWIDTH FULL STOP
    $FF0F,   // FULLWIDTH SOLIDUS
    $FF1A,   // FULLWIDTH COLON
    $FF1B,   // FULLWIDTH SEMICOLON
    $FF1F,   // FULLWIDTH QUESTION MARK
    $FF20,   // FULLWIDTH COMMERCIAL AT
    $FF3B,   // FULLWIDTH LEFT SQUARE BRACKET
    $FF3C,   // FULLWIDTH REVERSE SOLIDUS
    $FF3D,   // FULLWIDTH RIGHT SQUARE BRACKET
    $FF3F,   // FULLWIDTH LOW LINE
    $FF5B,   // FULLWIDTH LEFT CURLY BRACKET
    $FF5D,   // FULLWIDTH RIGHT CURLY BRACKET
    $FF61,   // HALFWIDTH IDEOGRAPHIC FULL STOP
    $FF62,   // HALFWIDTH LEFT CORNER BRACKET
    $FF63,   // HALFWIDTH RIGHT CORNER BRACKET
    $FF64,   // HALFWIDTH IDEOGRAPHIC COMMA
    $FF65 :  // HALFWIDTH KATAKANA MIDDLE DOT
      Result := True;
  else
    Result := False;
  end;
end;

function DecimalDigitBase(const Ch: UCS4Char): UCS4Char;
begin
  Case Ch of
    $0030..$0039   : Result := $0030;  // DIGIT
    $0660..$0669   : Result := $0660;  // ARABIC-INDIC DIGIT
    $06F0..$06F9   : Result := $06F0;  // EXTENDED ARABIC-INDIC DIGIT
    $0966..$096F   : Result := $0966;  // DEVANAGARI DIGIT
    $09E6..$09EF   : Result := $09E6;  // BENGALI DIGIT
    $0A66..$0A6F   : Result := $0A66;  // GURMUKHI DIGIT
    $0AE6..$0AEF   : Result := $0AE6;  // GUJARATI DIGIT
    $0B66..$0B6F   : Result := $0B66;  // ORIYA DIGIT
    $0C66..$0C6F   : Result := $0C66;  // TELUGU DIGIT
    $0CE6..$0CEF   : Result := $0CE6;  // KANNADA DIGIT
    $0D66..$0D6F   : Result := $0D66;  // MALAYALAM DIGIT
    $0E50..$0E59   : Result := $0E50;  // THAI DIGIT
    $0ED0..$0ED9   : Result := $0ED0;  // LAO DIGIT
    $0F20..$0F29   : Result := $0F20;  // TIBETAN DIGIT
    $1040..$1049   : Result := $1040;  // MYANMAR DIGIT
    $17E0..$17E9   : Result := $17E0;  // KHMER DIGIT
    $1810..$1819   : Result := $1810;  // MONGOLIAN DIGIT
    $2070..$2079   : Result := $2070;  // SUPERSCRIPT DIGIT
    $2080..$2089   : Result := $2080;  // SUBSCRIPT DIGIT
    $FF10..$FF19   : Result := $FF10;  // FULLWIDTH DIGIT
    $1D7CE..$1D7D7 : Result := $1D7CE; // MATHEMATICAL BOLD DIGIT
    $1D7D8..$1D7E1 : Result := $1D7D8; // MATHEMATICAL DOUBLE-STRUCK DIGIT
    $1D7E2..$1D7EB : Result := $1D7E2; // MATHEMATICAL SANS-SERIF DIGIT
    $1D7EC..$1D7F5 : Result := $1D7EC; // MATHEMATICAL SANS-SERIF BOLD DIGIT
    $1D7F6..$1D7FF : Result := $1D7F6; // MATHEMATICAL MONOSPACE DIGIT
  else
    Result := 0;
  end;
end;

function DecimalDigitValue(const Ch: UCS4Char): Integer;
var I : LongWord;
begin
  I := DecimalDigitBase(Ch);
  if I = 0 then
    Result := -1 else
    Result := Ch - I;
end;

function DecimalDigitValue(const Ch: WideChar): Integer;
begin
  Result := DecimalDigitValue(Ord(Ch));
end;

function IsDecimalDigit(const Ch: UCS4Char): Boolean;
begin
  Result := DecimalDigitBase(Ch) <> 0;
end;

function IsDecimalDigit(const Ch: WideChar): Boolean;
begin
  Result := DecimalDigitBase(Ord(Ch)) <> 0;
end;

function FractionCharacterValue(const Ch: WideChar; var A, B : Integer): Boolean;
begin
  Case Ord(Ch) of
    $00BC : begin A := 1; B := 4; end;       // # No       VULGAR FRACTION ONE QUARTER
    $00BD : begin A := 1; B := 2; end;       // # No       VULGAR FRACTION ONE HALF
    $00BE : begin A := 3; B := 4; end;       // # No       VULGAR FRACTION THREE QUARTERS
    $0F2A : begin A := 1; B := 2; end;       // # No       TIBETAN DIGIT HALF ONE
    $2153 : begin A := 1; B := 3; end;       // # No       VULGAR FRACTION ONE THIRD
    $2154 : begin A := 2; B := 3; end;       // # No       VULGAR FRACTION TWO THIRDS
    $2155 : begin A := 1; B := 5; end;       // # No       VULGAR FRACTION ONE FIFTH
    $2156 : begin A := 2; B := 5; end;       // # No       VULGAR FRACTION TWO FIFTHS
    $2157 : begin A := 3; B := 5; end;       // # No       VULGAR FRACTION THREE FIFTHS
    $2158 : begin A := 4; B := 5; end;       // # No       VULGAR FRACTION FOUR FIFTHS
    $2159 : begin A := 1; B := 6; end;       // # No       VULGAR FRACTION ONE SIXTH
    $215A : begin A := 5; B := 6; end;       // # No       VULGAR FRACTION FIVE SIXTHS
    $215B : begin A := 1; B := 8; end;       // # No       VULGAR FRACTION ONE EIGHTH
    $215C : begin A := 3; B := 8; end;       // # No       VULGAR FRACTION THREE EIGHTHS
    $215D : begin A := 5; B := 8; end;       // # No       VULGAR FRACTION FIVE EIGHTHS
    $215E : begin A := 7; B := 8; end;       // # No       VULGAR FRACTION SEVEN EIGHTHS
  else
    begin A := 0; B := 0; end;
  end;
  Result := B <> 0;
end;

function RomanNumeralValue(const Ch: WideChar): Integer;
begin
  Case Ord(Ch) of
    $2160        : Result := 1;     //  Nl       ROMAN NUMERAL ONE
    $2161        : Result := 2;     //  Nl       ROMAN NUMERAL TWO
    $2162        : Result := 3;     //  Nl       ROMAN NUMERAL THREE
    $2163        : Result := 4;     //  Nl       ROMAN NUMERAL FOUR
    $2164        : Result := 5;     //  Nl       ROMAN NUMERAL FIVE
    $2165        : Result := 6;     //  Nl       ROMAN NUMERAL SIX
    $2166        : Result := 7;     //  Nl       ROMAN NUMERAL SEVEN
    $2167        : Result := 8;     //  Nl       ROMAN NUMERAL EIGHT
    $2168        : Result := 9;     //  Nl       ROMAN NUMERAL NINE
    $2169        : Result := 10;    //  Nl       ROMAN NUMERAL TEN
    $216A        : Result := 11;    //  Nl       ROMAN NUMERAL ELEVEN
    $216B        : Result := 12;    //  Nl       ROMAN NUMERAL TWELVE
    $216C        : Result := 50;    //  Nl       ROMAN NUMERAL FIFTY
    $216D        : Result := 100;   //  Nl       ROMAN NUMERAL ONE HUNDRED
    $216E        : Result := 500;   //  Nl       ROMAN NUMERAL FIVE HUNDRED
    $216F        : Result := 1000;  //  Nl       ROMAN NUMERAL ONE THOUSAND
    $2170        : Result := 1;     //  Nl       SMALL ROMAN NUMERAL ONE
    $2171        : Result := 2;     //  Nl       SMALL ROMAN NUMERAL TWO
    $2172        : Result := 3;     //  Nl       SMALL ROMAN NUMERAL THREE
    $2173        : Result := 4;     //  Nl       SMALL ROMAN NUMERAL FOUR
    $2174        : Result := 5;     //  Nl       SMALL ROMAN NUMERAL FIVE
    $2175        : Result := 6;     //  Nl       SMALL ROMAN NUMERAL SIX
    $2176        : Result := 7;     //  Nl       SMALL ROMAN NUMERAL SEVEN
    $2177        : Result := 8;     //  Nl       SMALL ROMAN NUMERAL EIGHT
    $2178        : Result := 9;     //  Nl       SMALL ROMAN NUMERAL NINE
    $2179        : Result := 10;    //  Nl       SMALL ROMAN NUMERAL TEN
    $217A        : Result := 11;    //  Nl       SMALL ROMAN NUMERAL ELEVEN
    $217B        : Result := 12;    //  Nl       SMALL ROMAN NUMERAL TWELVE
    $217C        : Result := 50;    //  Nl       SMALL ROMAN NUMERAL FIFTY
    $217D        : Result := 100;   //  Nl       SMALL ROMAN NUMERAL ONE HUNDRED
    $217E        : Result := 500;   //  Nl       SMALL ROMAN NUMERAL FIVE HUNDRED
    $217F..$2180 : Result := 1000;  //  Nl   [2] SMALL ROMAN NUMERAL ONE THOUSAND..ROMAN NUMERAL ONE THOUSAND C D
    $2181        : Result := 5000;  //  Nl       ROMAN NUMERAL FIVE THOUSAND
    $2182        : Result := 10000; //  Nl       ROMAN NUMERAL TEN THOUSAND
  else
    Result := 0;
  end;
end;

function LatinAlphaCharBase(const Ch: WideChar): UCS4Char;
begin
  Case Ord(Ch) of
    $0041..$005A : Result := $0041;  // LATIN CAPITAL LETTER
    $0061..$007A : Result := $0061;  // LATIN SMALL LETTER
    $FF21..$FF3A : Result := $FF21;  // FULLWIDTH LATIN CAPITAL LETTER
    $FF41..$FF5A : Result := $FF41;  // FULLWIDTH LATIN SMALL LETTER
  else
    Result := 0;
  end;
end;

function HexAlphaDigitBase(const Ch: WideChar): UCS4Char; overload;
begin
  Result := LatinAlphaCharBase(Ch);
  if Result = 0 then
    exit;
  if Ord(Ch) - Result > 5 then
    Result := 0;
end;

function HexAlphaDigitBase(const Ch: UCS4Char): UCS4Char; overload;
begin
  if Ch <= $FFFF then
    Result := HexAlphaDigitBase(WideChar(Ch)) else
    Case Ch of
      $1D400..$1D405 : Result := $1D400;  // MATHEMATICAL BOLD CAPITAL
      $1D41A..$1D41F : Result := $1D41A;  // MATHEMATICAL BOLD SMALL
      $1D434..$1D439 : Result := $1D434;  // MATHEMATICAL ITALIC CAPITAL
      $1D44E..$1D453 : Result := $1D44E;  // MATHEMATICAL ITALIC SMALL
      $1D468..$1D46D : Result := $1D468;  // MATHEMATICAL BOLD ITALIC CAPITAL
      $1D482..$1D487 : Result := $1D482;  // MATHEMATICAL BOLD ITALIC SMALL
      $1D49C..$1D4A1 : Result := $1D49C;  // MATHEMATICAL SCRIPT CAPITAL
      $1D4B6..$1D4BB : Result := $1D4B6;  // MATHEMATICAL SCRIPT SMALL
      $1D4D0..$1D4D5 : Result := $1D4D0;  // MATHEMATICAL BOLD SCRIPT CAPITAL
      $1D4EA..$1D4EF : Result := $1D4EA;  // MATHEMATICAL BOLD SCRIPT SMALL
      $1D504..$1D509 : Result := $1D504;  // MATHEMATICAL FRAKTUR CAPITAL
      $1D51E..$1D523 : Result := $1D51E;  // MATHEMATICAL FRAKTUR SMALL
      $1D538..$1D53D : Result := $1D538;  // MATHEMATICAL DOUBLE-STRUCK CAPITAL
      $1D552..$1D557 : Result := $1D552;  // MATHEMATICAL DOUBLE-STRUCK SMALL
      $1D56C..$1D571 : Result := $1D56C;  // MATHEMATICAL BOLD FRAKTUR CAPITAL
      $1D586..$1D58B : Result := $1D586;  // MATHEMATICAL BOLD FRAKTUR SMALL
      $1D5A0..$1D5A5 : Result := $1D5A0;  // MATHEMATICAL SANS-SERIF CAPITAL
      $1D5BA..$1D5BF : Result := $1D5BA;  // MATHEMATICAL SANS-SERIF SMALL
      $1D5D4..$1D5D9 : Result := $1D5D4;  // MATHEMATICAL SANS-SERIF BOLD CAPITAL
      $1D5EE..$1D5F3 : Result := $1D5EE;  // MATHEMATICAL SANS-SERIF BOLD SMALL
      $1D608..$1D60D : Result := $1D608;  // MATHEMATICAL SANS-SERIF ITALIC CAPITAL
      $1D622..$1D627 : Result := $1D622;  // MATHEMATICAL SANS-SERIF ITALIC SMALL
      $1D63C..$1D641 : Result := $1D63C;  // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL
      $1D656..$1D65B : Result := $1D656;  // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL
      $1D670..$1D675 : Result := $1D670;  // MATHEMATICAL MONOSPACE CAPITAL
      $1D68A..$1D68F : Result := $1D68A;  // MATHEMATICAL MONOSPACE SMALL
      $E0041..$E0046 : Result := $E0041;  // TAG LATIN CAPITAL LETTER
    else
      Result := 0;
    end;
end;

function HexDigitValue(const Ch: UCS4Char): Integer;
var I : UCS4Char;
begin
  Result := DecimalDigitValue(Ch);
  if Result >= 0 then
    exit;
  I := HexAlphaDigitBase(Ch);
  if I > 0 then
    Result := Ch - I + 10;
end;

function HexDigitValue(const Ch: WideChar): Integer;
var I : UCS4Char;
begin
  Result := DecimalDigitValue(Ch);
  if Result >= 0 then
    exit;
  I := HexAlphaDigitBase(Ch);
  if I > 0 then
    Result := Ord(Ch) - I + 10;
end;

function IsHexDigit(const Ch: UCS4Char): Boolean;
begin
  Result := HexDigitValue(Ch) >= 0;
end;

function IsHexDigit(const Ch: WideChar): Boolean;
begin
  Result := HexDigitValue(Ch) >= 0;
end;

{ Unicode letter table                                                         }
type
  TUnicodeLetterAttr = (laUpper, laLower);
  TUnicodeLetterInfo = packed record
    Unicode  : WideChar;
    Attr     : TUnicodeLetterAttr;
    CaseCode : WideChar;
  end;
  PUnicodeLetterInfo = ^TUnicodeLetterInfo;

const
  // Derived from 'Lu' and 'Ll' class
  UnicodeLetterEntries = 1492; // 7K table
  UnicodeLetterInfo : Array[0..UnicodeLetterEntries - 1] of TUnicodeLetterInfo = (
    (Unicode:#$0041; Attr:laUpper; CaseCode:#$0061),   // LATIN CAPITAL LETTER A
    (Unicode:#$0042; Attr:laUpper; CaseCode:#$0062),   // LATIN CAPITAL LETTER B
    (Unicode:#$0043; Attr:laUpper; CaseCode:#$0063),   // LATIN CAPITAL LETTER C
    (Unicode:#$0044; Attr:laUpper; CaseCode:#$0064),   // LATIN CAPITAL LETTER D
    (Unicode:#$0045; Attr:laUpper; CaseCode:#$0065),   // LATIN CAPITAL LETTER E
    (Unicode:#$0046; Attr:laUpper; CaseCode:#$0066),   // LATIN CAPITAL LETTER F
    (Unicode:#$0047; Attr:laUpper; CaseCode:#$0067),   // LATIN CAPITAL LETTER G
    (Unicode:#$0048; Attr:laUpper; CaseCode:#$0068),   // LATIN CAPITAL LETTER H
    (Unicode:#$0049; Attr:laUpper; CaseCode:#$0069),   // LATIN CAPITAL LETTER I
    (Unicode:#$004A; Attr:laUpper; CaseCode:#$006A),   // LATIN CAPITAL LETTER J
    (Unicode:#$004B; Attr:laUpper; CaseCode:#$006B),   // LATIN CAPITAL LETTER K
    (Unicode:#$004C; Attr:laUpper; CaseCode:#$006C),   // LATIN CAPITAL LETTER L
    (Unicode:#$004D; Attr:laUpper; CaseCode:#$006D),   // LATIN CAPITAL LETTER M
    (Unicode:#$004E; Attr:laUpper; CaseCode:#$006E),   // LATIN CAPITAL LETTER N
    (Unicode:#$004F; Attr:laUpper; CaseCode:#$006F),   // LATIN CAPITAL LETTER O
    (Unicode:#$0050; Attr:laUpper; CaseCode:#$0070),   // LATIN CAPITAL LETTER P
    (Unicode:#$0051; Attr:laUpper; CaseCode:#$0071),   // LATIN CAPITAL LETTER Q
    (Unicode:#$0052; Attr:laUpper; CaseCode:#$0072),   // LATIN CAPITAL LETTER R
    (Unicode:#$0053; Attr:laUpper; CaseCode:#$0073),   // LATIN CAPITAL LETTER S
    (Unicode:#$0054; Attr:laUpper; CaseCode:#$0074),   // LATIN CAPITAL LETTER T
    (Unicode:#$0055; Attr:laUpper; CaseCode:#$0075),   // LATIN CAPITAL LETTER U
    (Unicode:#$0056; Attr:laUpper; CaseCode:#$0076),   // LATIN CAPITAL LETTER V
    (Unicode:#$0057; Attr:laUpper; CaseCode:#$0077),   // LATIN CAPITAL LETTER W
    (Unicode:#$0058; Attr:laUpper; CaseCode:#$0078),   // LATIN CAPITAL LETTER X
    (Unicode:#$0059; Attr:laUpper; CaseCode:#$0079),   // LATIN CAPITAL LETTER Y
    (Unicode:#$005A; Attr:laUpper; CaseCode:#$007A),   // LATIN CAPITAL LETTER Z
    (Unicode:#$0061; Attr:laLower; CaseCode:#$0041),   // LATIN SMALL LETTER A
    (Unicode:#$0062; Attr:laLower; CaseCode:#$0042),   // LATIN SMALL LETTER B
    (Unicode:#$0063; Attr:laLower; CaseCode:#$0043),   // LATIN SMALL LETTER C
    (Unicode:#$0064; Attr:laLower; CaseCode:#$0044),   // LATIN SMALL LETTER D
    (Unicode:#$0065; Attr:laLower; CaseCode:#$0045),   // LATIN SMALL LETTER E
    (Unicode:#$0066; Attr:laLower; CaseCode:#$0046),   // LATIN SMALL LETTER F
    (Unicode:#$0067; Attr:laLower; CaseCode:#$0047),   // LATIN SMALL LETTER G
    (Unicode:#$0068; Attr:laLower; CaseCode:#$0048),   // LATIN SMALL LETTER H
    (Unicode:#$0069; Attr:laLower; CaseCode:#$0049),   // LATIN SMALL LETTER I
    (Unicode:#$006A; Attr:laLower; CaseCode:#$004A),   // LATIN SMALL LETTER J
    (Unicode:#$006B; Attr:laLower; CaseCode:#$004B),   // LATIN SMALL LETTER K
    (Unicode:#$006C; Attr:laLower; CaseCode:#$004C),   // LATIN SMALL LETTER L
    (Unicode:#$006D; Attr:laLower; CaseCode:#$004D),   // LATIN SMALL LETTER M
    (Unicode:#$006E; Attr:laLower; CaseCode:#$004E),   // LATIN SMALL LETTER N
    (Unicode:#$006F; Attr:laLower; CaseCode:#$004F),   // LATIN SMALL LETTER O
    (Unicode:#$0070; Attr:laLower; CaseCode:#$0050),   // LATIN SMALL LETTER P
    (Unicode:#$0071; Attr:laLower; CaseCode:#$0051),   // LATIN SMALL LETTER Q
    (Unicode:#$0072; Attr:laLower; CaseCode:#$0052),   // LATIN SMALL LETTER R
    (Unicode:#$0073; Attr:laLower; CaseCode:#$0053),   // LATIN SMALL LETTER S
    (Unicode:#$0074; Attr:laLower; CaseCode:#$0054),   // LATIN SMALL LETTER T
    (Unicode:#$0075; Attr:laLower; CaseCode:#$0055),   // LATIN SMALL LETTER U
    (Unicode:#$0076; Attr:laLower; CaseCode:#$0056),   // LATIN SMALL LETTER V
    (Unicode:#$0077; Attr:laLower; CaseCode:#$0057),   // LATIN SMALL LETTER W
    (Unicode:#$0078; Attr:laLower; CaseCode:#$0058),   // LATIN SMALL LETTER X
    (Unicode:#$0079; Attr:laLower; CaseCode:#$0059),   // LATIN SMALL LETTER Y
    (Unicode:#$007A; Attr:laLower; CaseCode:#$005A),   // LATIN SMALL LETTER Z
    (Unicode:#$00AA; Attr:laLower; CaseCode:#$FFFF),   // FEMININE ORDINAL INDICATOR
    (Unicode:#$00B5; Attr:laLower; CaseCode:#$039C),   // MICRO SIGN
    (Unicode:#$00BA; Attr:laLower; CaseCode:#$FFFF),   // MASCULINE ORDINAL INDICATOR
    (Unicode:#$00C0; Attr:laUpper; CaseCode:#$00E0),   // LATIN CAPITAL LETTER A WITH GRAVE
    (Unicode:#$00C1; Attr:laUpper; CaseCode:#$00E1),   // LATIN CAPITAL LETTER A WITH ACUTE
    (Unicode:#$00C2; Attr:laUpper; CaseCode:#$00E2),   // LATIN CAPITAL LETTER A WITH CIRCUMFLEX
    (Unicode:#$00C3; Attr:laUpper; CaseCode:#$00E3),   // LATIN CAPITAL LETTER A WITH TILDE
    (Unicode:#$00C4; Attr:laUpper; CaseCode:#$00E4),   // LATIN CAPITAL LETTER A WITH DIAERESIS
    (Unicode:#$00C5; Attr:laUpper; CaseCode:#$00E5),   // LATIN CAPITAL LETTER A WITH RING ABOVE
    (Unicode:#$00C6; Attr:laUpper; CaseCode:#$00E6),   // LATIN CAPITAL LETTER AE
    (Unicode:#$00C7; Attr:laUpper; CaseCode:#$00E7),   // LATIN CAPITAL LETTER C WITH CEDILLA
    (Unicode:#$00C8; Attr:laUpper; CaseCode:#$00E8),   // LATIN CAPITAL LETTER E WITH GRAVE
    (Unicode:#$00C9; Attr:laUpper; CaseCode:#$00E9),   // LATIN CAPITAL LETTER E WITH ACUTE
    (Unicode:#$00CA; Attr:laUpper; CaseCode:#$00EA),   // LATIN CAPITAL LETTER E WITH CIRCUMFLEX
    (Unicode:#$00CB; Attr:laUpper; CaseCode:#$00EB),   // LATIN CAPITAL LETTER E WITH DIAERESIS
    (Unicode:#$00CC; Attr:laUpper; CaseCode:#$00EC),   // LATIN CAPITAL LETTER I WITH GRAVE
    (Unicode:#$00CD; Attr:laUpper; CaseCode:#$00ED),   // LATIN CAPITAL LETTER I WITH ACUTE
    (Unicode:#$00CE; Attr:laUpper; CaseCode:#$00EE),   // LATIN CAPITAL LETTER I WITH CIRCUMFLEX
    (Unicode:#$00CF; Attr:laUpper; CaseCode:#$00EF),   // LATIN CAPITAL LETTER I WITH DIAERESIS
    (Unicode:#$00D0; Attr:laUpper; CaseCode:#$00F0),   // LATIN CAPITAL LETTER ETH
    (Unicode:#$00D1; Attr:laUpper; CaseCode:#$00F1),   // LATIN CAPITAL LETTER N WITH TILDE
    (Unicode:#$00D2; Attr:laUpper; CaseCode:#$00F2),   // LATIN CAPITAL LETTER O WITH GRAVE
    (Unicode:#$00D3; Attr:laUpper; CaseCode:#$00F3),   // LATIN CAPITAL LETTER O WITH ACUTE
    (Unicode:#$00D4; Attr:laUpper; CaseCode:#$00F4),   // LATIN CAPITAL LETTER O WITH CIRCUMFLEX
    (Unicode:#$00D5; Attr:laUpper; CaseCode:#$00F5),   // LATIN CAPITAL LETTER O WITH TILDE
    (Unicode:#$00D6; Attr:laUpper; CaseCode:#$00F6),   // LATIN CAPITAL LETTER O WITH DIAERESIS
    (Unicode:#$00D8; Attr:laUpper; CaseCode:#$00F8),   // LATIN CAPITAL LETTER O WITH STROKE
    (Unicode:#$00D9; Attr:laUpper; CaseCode:#$00F9),   // LATIN CAPITAL LETTER U WITH GRAVE
    (Unicode:#$00DA; Attr:laUpper; CaseCode:#$00FA),   // LATIN CAPITAL LETTER U WITH ACUTE
    (Unicode:#$00DB; Attr:laUpper; CaseCode:#$00FB),   // LATIN CAPITAL LETTER U WITH CIRCUMFLEX
    (Unicode:#$00DC; Attr:laUpper; CaseCode:#$00FC),   // LATIN CAPITAL LETTER U WITH DIAERESIS
    (Unicode:#$00DD; Attr:laUpper; CaseCode:#$00FD),   // LATIN CAPITAL LETTER Y WITH ACUTE
    (Unicode:#$00DE; Attr:laUpper; CaseCode:#$00FE),   // LATIN CAPITAL LETTER THORN
    (Unicode:#$00DF; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER SHARP S
    (Unicode:#$00E0; Attr:laLower; CaseCode:#$00C0),   // LATIN SMALL LETTER A WITH GRAVE
    (Unicode:#$00E1; Attr:laLower; CaseCode:#$00C1),   // LATIN SMALL LETTER A WITH ACUTE
    (Unicode:#$00E2; Attr:laLower; CaseCode:#$00C2),   // LATIN SMALL LETTER A WITH CIRCUMFLEX
    (Unicode:#$00E3; Attr:laLower; CaseCode:#$00C3),   // LATIN SMALL LETTER A WITH TILDE
    (Unicode:#$00E4; Attr:laLower; CaseCode:#$00C4),   // LATIN SMALL LETTER A WITH DIAERESIS
    (Unicode:#$00E5; Attr:laLower; CaseCode:#$00C5),   // LATIN SMALL LETTER A WITH RING ABOVE
    (Unicode:#$00E6; Attr:laLower; CaseCode:#$00C6),   // LATIN SMALL LETTER AE
    (Unicode:#$00E7; Attr:laLower; CaseCode:#$00C7),   // LATIN SMALL LETTER C WITH CEDILLA
    (Unicode:#$00E8; Attr:laLower; CaseCode:#$00C8),   // LATIN SMALL LETTER E WITH GRAVE
    (Unicode:#$00E9; Attr:laLower; CaseCode:#$00C9),   // LATIN SMALL LETTER E WITH ACUTE
    (Unicode:#$00EA; Attr:laLower; CaseCode:#$00CA),   // LATIN SMALL LETTER E WITH CIRCUMFLEX
    (Unicode:#$00EB; Attr:laLower; CaseCode:#$00CB),   // LATIN SMALL LETTER E WITH DIAERESIS
    (Unicode:#$00EC; Attr:laLower; CaseCode:#$00CC),   // LATIN SMALL LETTER I WITH GRAVE
    (Unicode:#$00ED; Attr:laLower; CaseCode:#$00CD),   // LATIN SMALL LETTER I WITH ACUTE
    (Unicode:#$00EE; Attr:laLower; CaseCode:#$00CE),   // LATIN SMALL LETTER I WITH CIRCUMFLEX
    (Unicode:#$00EF; Attr:laLower; CaseCode:#$00CF),   // LATIN SMALL LETTER I WITH DIAERESIS
    (Unicode:#$00F0; Attr:laLower; CaseCode:#$00D0),   // LATIN SMALL LETTER ETH
    (Unicode:#$00F1; Attr:laLower; CaseCode:#$00D1),   // LATIN SMALL LETTER N WITH TILDE
    (Unicode:#$00F2; Attr:laLower; CaseCode:#$00D2),   // LATIN SMALL LETTER O WITH GRAVE
    (Unicode:#$00F3; Attr:laLower; CaseCode:#$00D3),   // LATIN SMALL LETTER O WITH ACUTE
    (Unicode:#$00F4; Attr:laLower; CaseCode:#$00D4),   // LATIN SMALL LETTER O WITH CIRCUMFLEX
    (Unicode:#$00F5; Attr:laLower; CaseCode:#$00D5),   // LATIN SMALL LETTER O WITH TILDE
    (Unicode:#$00F6; Attr:laLower; CaseCode:#$00D6),   // LATIN SMALL LETTER O WITH DIAERESIS
    (Unicode:#$00F8; Attr:laLower; CaseCode:#$00D8),   // LATIN SMALL LETTER O WITH STROKE
    (Unicode:#$00F9; Attr:laLower; CaseCode:#$00D9),   // LATIN SMALL LETTER U WITH GRAVE
    (Unicode:#$00FA; Attr:laLower; CaseCode:#$00DA),   // LATIN SMALL LETTER U WITH ACUTE
    (Unicode:#$00FB; Attr:laLower; CaseCode:#$00DB),   // LATIN SMALL LETTER U WITH CIRCUMFLEX
    (Unicode:#$00FC; Attr:laLower; CaseCode:#$00DC),   // LATIN SMALL LETTER U WITH DIAERESIS
    (Unicode:#$00FD; Attr:laLower; CaseCode:#$00DD),   // LATIN SMALL LETTER Y WITH ACUTE
    (Unicode:#$00FE; Attr:laLower; CaseCode:#$00DE),   // LATIN SMALL LETTER THORN
    (Unicode:#$00FF; Attr:laLower; CaseCode:#$0178),   // LATIN SMALL LETTER Y WITH DIAERESIS
    (Unicode:#$0100; Attr:laUpper; CaseCode:#$0101),   // LATIN CAPITAL LETTER A WITH MACRON
    (Unicode:#$0101; Attr:laLower; CaseCode:#$0100),   // LATIN SMALL LETTER A WITH MACRON
    (Unicode:#$0102; Attr:laUpper; CaseCode:#$0103),   // LATIN CAPITAL LETTER A WITH BREVE
    (Unicode:#$0103; Attr:laLower; CaseCode:#$0102),   // LATIN SMALL LETTER A WITH BREVE
    (Unicode:#$0104; Attr:laUpper; CaseCode:#$0105),   // LATIN CAPITAL LETTER A WITH OGONEK
    (Unicode:#$0105; Attr:laLower; CaseCode:#$0104),   // LATIN SMALL LETTER A WITH OGONEK
    (Unicode:#$0106; Attr:laUpper; CaseCode:#$0107),   // LATIN CAPITAL LETTER C WITH ACUTE
    (Unicode:#$0107; Attr:laLower; CaseCode:#$0106),   // LATIN SMALL LETTER C WITH ACUTE
    (Unicode:#$0108; Attr:laUpper; CaseCode:#$0109),   // LATIN CAPITAL LETTER C WITH CIRCUMFLEX
    (Unicode:#$0109; Attr:laLower; CaseCode:#$0108),   // LATIN SMALL LETTER C WITH CIRCUMFLEX
    (Unicode:#$010A; Attr:laUpper; CaseCode:#$010B),   // LATIN CAPITAL LETTER C WITH DOT ABOVE
    (Unicode:#$010B; Attr:laLower; CaseCode:#$010A),   // LATIN SMALL LETTER C WITH DOT ABOVE
    (Unicode:#$010C; Attr:laUpper; CaseCode:#$010D),   // LATIN CAPITAL LETTER C WITH CARON
    (Unicode:#$010D; Attr:laLower; CaseCode:#$010C),   // LATIN SMALL LETTER C WITH CARON
    (Unicode:#$010E; Attr:laUpper; CaseCode:#$010F),   // LATIN CAPITAL LETTER D WITH CARON
    (Unicode:#$010F; Attr:laLower; CaseCode:#$010E),   // LATIN SMALL LETTER D WITH CARON
    (Unicode:#$0110; Attr:laUpper; CaseCode:#$0111),   // LATIN CAPITAL LETTER D WITH STROKE
    (Unicode:#$0111; Attr:laLower; CaseCode:#$0110),   // LATIN SMALL LETTER D WITH STROKE
    (Unicode:#$0112; Attr:laUpper; CaseCode:#$0113),   // LATIN CAPITAL LETTER E WITH MACRON
    (Unicode:#$0113; Attr:laLower; CaseCode:#$0112),   // LATIN SMALL LETTER E WITH MACRON
    (Unicode:#$0114; Attr:laUpper; CaseCode:#$0115),   // LATIN CAPITAL LETTER E WITH BREVE
    (Unicode:#$0115; Attr:laLower; CaseCode:#$0114),   // LATIN SMALL LETTER E WITH BREVE
    (Unicode:#$0116; Attr:laUpper; CaseCode:#$0117),   // LATIN CAPITAL LETTER E WITH DOT ABOVE
    (Unicode:#$0117; Attr:laLower; CaseCode:#$0116),   // LATIN SMALL LETTER E WITH DOT ABOVE
    (Unicode:#$0118; Attr:laUpper; CaseCode:#$0119),   // LATIN CAPITAL LETTER E WITH OGONEK
    (Unicode:#$0119; Attr:laLower; CaseCode:#$0118),   // LATIN SMALL LETTER E WITH OGONEK
    (Unicode:#$011A; Attr:laUpper; CaseCode:#$011B),   // LATIN CAPITAL LETTER E WITH CARON
    (Unicode:#$011B; Attr:laLower; CaseCode:#$011A),   // LATIN SMALL LETTER E WITH CARON
    (Unicode:#$011C; Attr:laUpper; CaseCode:#$011D),   // LATIN CAPITAL LETTER G WITH CIRCUMFLEX
    (Unicode:#$011D; Attr:laLower; CaseCode:#$011C),   // LATIN SMALL LETTER G WITH CIRCUMFLEX
    (Unicode:#$011E; Attr:laUpper; CaseCode:#$011F),   // LATIN CAPITAL LETTER G WITH BREVE
    (Unicode:#$011F; Attr:laLower; CaseCode:#$011E),   // LATIN SMALL LETTER G WITH BREVE
    (Unicode:#$0120; Attr:laUpper; CaseCode:#$0121),   // LATIN CAPITAL LETTER G WITH DOT ABOVE
    (Unicode:#$0121; Attr:laLower; CaseCode:#$0120),   // LATIN SMALL LETTER G WITH DOT ABOVE
    (Unicode:#$0122; Attr:laUpper; CaseCode:#$0123),   // LATIN CAPITAL LETTER G WITH CEDILLA
    (Unicode:#$0123; Attr:laLower; CaseCode:#$0122),   // LATIN SMALL LETTER G WITH CEDILLA
    (Unicode:#$0124; Attr:laUpper; CaseCode:#$0125),   // LATIN CAPITAL LETTER H WITH CIRCUMFLEX
    (Unicode:#$0125; Attr:laLower; CaseCode:#$0124),   // LATIN SMALL LETTER H WITH CIRCUMFLEX
    (Unicode:#$0126; Attr:laUpper; CaseCode:#$0127),   // LATIN CAPITAL LETTER H WITH STROKE
    (Unicode:#$0127; Attr:laLower; CaseCode:#$0126),   // LATIN SMALL LETTER H WITH STROKE
    (Unicode:#$0128; Attr:laUpper; CaseCode:#$0129),   // LATIN CAPITAL LETTER I WITH TILDE
    (Unicode:#$0129; Attr:laLower; CaseCode:#$0128),   // LATIN SMALL LETTER I WITH TILDE
    (Unicode:#$012A; Attr:laUpper; CaseCode:#$012B),   // LATIN CAPITAL LETTER I WITH MACRON
    (Unicode:#$012B; Attr:laLower; CaseCode:#$012A),   // LATIN SMALL LETTER I WITH MACRON
    (Unicode:#$012C; Attr:laUpper; CaseCode:#$012D),   // LATIN CAPITAL LETTER I WITH BREVE
    (Unicode:#$012D; Attr:laLower; CaseCode:#$012C),   // LATIN SMALL LETTER I WITH BREVE
    (Unicode:#$012E; Attr:laUpper; CaseCode:#$012F),   // LATIN CAPITAL LETTER I WITH OGONEK
    (Unicode:#$012F; Attr:laLower; CaseCode:#$012E),   // LATIN SMALL LETTER I WITH OGONEK
    (Unicode:#$0130; Attr:laUpper; CaseCode:#$0069),   // LATIN CAPITAL LETTER I WITH DOT ABOVE
    (Unicode:#$0131; Attr:laLower; CaseCode:#$0049),   // LATIN SMALL LETTER DOTLESS I
    (Unicode:#$0132; Attr:laUpper; CaseCode:#$0133),   // LATIN CAPITAL LIGATURE IJ
    (Unicode:#$0133; Attr:laLower; CaseCode:#$0132),   // LATIN SMALL LIGATURE IJ
    (Unicode:#$0134; Attr:laUpper; CaseCode:#$0135),   // LATIN CAPITAL LETTER J WITH CIRCUMFLEX
    (Unicode:#$0135; Attr:laLower; CaseCode:#$0134),   // LATIN SMALL LETTER J WITH CIRCUMFLEX
    (Unicode:#$0136; Attr:laUpper; CaseCode:#$0137),   // LATIN CAPITAL LETTER K WITH CEDILLA
    (Unicode:#$0137; Attr:laLower; CaseCode:#$0136),   // LATIN SMALL LETTER K WITH CEDILLA
    (Unicode:#$0138; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER KRA
    (Unicode:#$0139; Attr:laUpper; CaseCode:#$013A),   // LATIN CAPITAL LETTER L WITH ACUTE
    (Unicode:#$013A; Attr:laLower; CaseCode:#$0139),   // LATIN SMALL LETTER L WITH ACUTE
    (Unicode:#$013B; Attr:laUpper; CaseCode:#$013C),   // LATIN CAPITAL LETTER L WITH CEDILLA
    (Unicode:#$013C; Attr:laLower; CaseCode:#$013B),   // LATIN SMALL LETTER L WITH CEDILLA
    (Unicode:#$013D; Attr:laUpper; CaseCode:#$013E),   // LATIN CAPITAL LETTER L WITH CARON
    (Unicode:#$013E; Attr:laLower; CaseCode:#$013D),   // LATIN SMALL LETTER L WITH CARON
    (Unicode:#$013F; Attr:laUpper; CaseCode:#$0140),   // LATIN CAPITAL LETTER L WITH MIDDLE DOT
    (Unicode:#$0140; Attr:laLower; CaseCode:#$013F),   // LATIN SMALL LETTER L WITH MIDDLE DOT
    (Unicode:#$0141; Attr:laUpper; CaseCode:#$0142),   // LATIN CAPITAL LETTER L WITH STROKE
    (Unicode:#$0142; Attr:laLower; CaseCode:#$0141),   // LATIN SMALL LETTER L WITH STROKE
    (Unicode:#$0143; Attr:laUpper; CaseCode:#$0144),   // LATIN CAPITAL LETTER N WITH ACUTE
    (Unicode:#$0144; Attr:laLower; CaseCode:#$0143),   // LATIN SMALL LETTER N WITH ACUTE
    (Unicode:#$0145; Attr:laUpper; CaseCode:#$0146),   // LATIN CAPITAL LETTER N WITH CEDILLA
    (Unicode:#$0146; Attr:laLower; CaseCode:#$0145),   // LATIN SMALL LETTER N WITH CEDILLA
    (Unicode:#$0147; Attr:laUpper; CaseCode:#$0148),   // LATIN CAPITAL LETTER N WITH CARON
    (Unicode:#$0148; Attr:laLower; CaseCode:#$0147),   // LATIN SMALL LETTER N WITH CARON
    (Unicode:#$0149; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER N PRECEDED BY APOSTROPHE
    (Unicode:#$014A; Attr:laUpper; CaseCode:#$014B),   // LATIN CAPITAL LETTER ENG
    (Unicode:#$014B; Attr:laLower; CaseCode:#$014A),   // LATIN SMALL LETTER ENG
    (Unicode:#$014C; Attr:laUpper; CaseCode:#$014D),   // LATIN CAPITAL LETTER O WITH MACRON
    (Unicode:#$014D; Attr:laLower; CaseCode:#$014C),   // LATIN SMALL LETTER O WITH MACRON
    (Unicode:#$014E; Attr:laUpper; CaseCode:#$014F),   // LATIN CAPITAL LETTER O WITH BREVE
    (Unicode:#$014F; Attr:laLower; CaseCode:#$014E),   // LATIN SMALL LETTER O WITH BREVE
    (Unicode:#$0150; Attr:laUpper; CaseCode:#$0151),   // LATIN CAPITAL LETTER O WITH DOUBLE ACUTE
    (Unicode:#$0151; Attr:laLower; CaseCode:#$0150),   // LATIN SMALL LETTER O WITH DOUBLE ACUTE
    (Unicode:#$0152; Attr:laUpper; CaseCode:#$0153),   // LATIN CAPITAL LIGATURE OE
    (Unicode:#$0153; Attr:laLower; CaseCode:#$0152),   // LATIN SMALL LIGATURE OE
    (Unicode:#$0154; Attr:laUpper; CaseCode:#$0155),   // LATIN CAPITAL LETTER R WITH ACUTE
    (Unicode:#$0155; Attr:laLower; CaseCode:#$0154),   // LATIN SMALL LETTER R WITH ACUTE
    (Unicode:#$0156; Attr:laUpper; CaseCode:#$0157),   // LATIN CAPITAL LETTER R WITH CEDILLA
    (Unicode:#$0157; Attr:laLower; CaseCode:#$0156),   // LATIN SMALL LETTER R WITH CEDILLA
    (Unicode:#$0158; Attr:laUpper; CaseCode:#$0159),   // LATIN CAPITAL LETTER R WITH CARON
    (Unicode:#$0159; Attr:laLower; CaseCode:#$0158),   // LATIN SMALL LETTER R WITH CARON
    (Unicode:#$015A; Attr:laUpper; CaseCode:#$015B),   // LATIN CAPITAL LETTER S WITH ACUTE
    (Unicode:#$015B; Attr:laLower; CaseCode:#$015A),   // LATIN SMALL LETTER S WITH ACUTE
    (Unicode:#$015C; Attr:laUpper; CaseCode:#$015D),   // LATIN CAPITAL LETTER S WITH CIRCUMFLEX
    (Unicode:#$015D; Attr:laLower; CaseCode:#$015C),   // LATIN SMALL LETTER S WITH CIRCUMFLEX
    (Unicode:#$015E; Attr:laUpper; CaseCode:#$015F),   // LATIN CAPITAL LETTER S WITH CEDILLA
    (Unicode:#$015F; Attr:laLower; CaseCode:#$015E),   // LATIN SMALL LETTER S WITH CEDILLA
    (Unicode:#$0160; Attr:laUpper; CaseCode:#$0161),   // LATIN CAPITAL LETTER S WITH CARON
    (Unicode:#$0161; Attr:laLower; CaseCode:#$0160),   // LATIN SMALL LETTER S WITH CARON
    (Unicode:#$0162; Attr:laUpper; CaseCode:#$0163),   // LATIN CAPITAL LETTER T WITH CEDILLA
    (Unicode:#$0163; Attr:laLower; CaseCode:#$0162),   // LATIN SMALL LETTER T WITH CEDILLA
    (Unicode:#$0164; Attr:laUpper; CaseCode:#$0165),   // LATIN CAPITAL LETTER T WITH CARON
    (Unicode:#$0165; Attr:laLower; CaseCode:#$0164),   // LATIN SMALL LETTER T WITH CARON
    (Unicode:#$0166; Attr:laUpper; CaseCode:#$0167),   // LATIN CAPITAL LETTER T WITH STROKE
    (Unicode:#$0167; Attr:laLower; CaseCode:#$0166),   // LATIN SMALL LETTER T WITH STROKE
    (Unicode:#$0168; Attr:laUpper; CaseCode:#$0169),   // LATIN CAPITAL LETTER U WITH TILDE
    (Unicode:#$0169; Attr:laLower; CaseCode:#$0168),   // LATIN SMALL LETTER U WITH TILDE
    (Unicode:#$016A; Attr:laUpper; CaseCode:#$016B),   // LATIN CAPITAL LETTER U WITH MACRON
    (Unicode:#$016B; Attr:laLower; CaseCode:#$016A),   // LATIN SMALL LETTER U WITH MACRON
    (Unicode:#$016C; Attr:laUpper; CaseCode:#$016D),   // LATIN CAPITAL LETTER U WITH BREVE
    (Unicode:#$016D; Attr:laLower; CaseCode:#$016C),   // LATIN SMALL LETTER U WITH BREVE
    (Unicode:#$016E; Attr:laUpper; CaseCode:#$016F),   // LATIN CAPITAL LETTER U WITH RING ABOVE
    (Unicode:#$016F; Attr:laLower; CaseCode:#$016E),   // LATIN SMALL LETTER U WITH RING ABOVE
    (Unicode:#$0170; Attr:laUpper; CaseCode:#$0171),   // LATIN CAPITAL LETTER U WITH DOUBLE ACUTE
    (Unicode:#$0171; Attr:laLower; CaseCode:#$0170),   // LATIN SMALL LETTER U WITH DOUBLE ACUTE
    (Unicode:#$0172; Attr:laUpper; CaseCode:#$0173),   // LATIN CAPITAL LETTER U WITH OGONEK
    (Unicode:#$0173; Attr:laLower; CaseCode:#$0172),   // LATIN SMALL LETTER U WITH OGONEK
    (Unicode:#$0174; Attr:laUpper; CaseCode:#$0175),   // LATIN CAPITAL LETTER W WITH CIRCUMFLEX
    (Unicode:#$0175; Attr:laLower; CaseCode:#$0174),   // LATIN SMALL LETTER W WITH CIRCUMFLEX
    (Unicode:#$0176; Attr:laUpper; CaseCode:#$0177),   // LATIN CAPITAL LETTER Y WITH CIRCUMFLEX
    (Unicode:#$0177; Attr:laLower; CaseCode:#$0176),   // LATIN SMALL LETTER Y WITH CIRCUMFLEX
    (Unicode:#$0178; Attr:laUpper; CaseCode:#$00FF),   // LATIN CAPITAL LETTER Y WITH DIAERESIS
    (Unicode:#$0179; Attr:laUpper; CaseCode:#$017A),   // LATIN CAPITAL LETTER Z WITH ACUTE
    (Unicode:#$017A; Attr:laLower; CaseCode:#$0179),   // LATIN SMALL LETTER Z WITH ACUTE
    (Unicode:#$017B; Attr:laUpper; CaseCode:#$017C),   // LATIN CAPITAL LETTER Z WITH DOT ABOVE
    (Unicode:#$017C; Attr:laLower; CaseCode:#$017B),   // LATIN SMALL LETTER Z WITH DOT ABOVE
    (Unicode:#$017D; Attr:laUpper; CaseCode:#$017E),   // LATIN CAPITAL LETTER Z WITH CARON
    (Unicode:#$017E; Attr:laLower; CaseCode:#$017D),   // LATIN SMALL LETTER Z WITH CARON
    (Unicode:#$017F; Attr:laLower; CaseCode:#$0053),   // LATIN SMALL LETTER LONG S
    (Unicode:#$0180; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER B WITH STROKE
    (Unicode:#$0181; Attr:laUpper; CaseCode:#$0253),   // LATIN CAPITAL LETTER B WITH HOOK
    (Unicode:#$0182; Attr:laUpper; CaseCode:#$0183),   // LATIN CAPITAL LETTER B WITH TOPBAR
    (Unicode:#$0183; Attr:laLower; CaseCode:#$0182),   // LATIN SMALL LETTER B WITH TOPBAR
    (Unicode:#$0184; Attr:laUpper; CaseCode:#$0185),   // LATIN CAPITAL LETTER TONE SIX
    (Unicode:#$0185; Attr:laLower; CaseCode:#$0184),   // LATIN SMALL LETTER TONE SIX
    (Unicode:#$0186; Attr:laUpper; CaseCode:#$0254),   // LATIN CAPITAL LETTER OPEN O
    (Unicode:#$0187; Attr:laUpper; CaseCode:#$0188),   // LATIN CAPITAL LETTER C WITH HOOK
    (Unicode:#$0188; Attr:laLower; CaseCode:#$0187),   // LATIN SMALL LETTER C WITH HOOK
    (Unicode:#$0189; Attr:laUpper; CaseCode:#$0256),   // LATIN CAPITAL LETTER AFRICAN D
    (Unicode:#$018A; Attr:laUpper; CaseCode:#$0257),   // LATIN CAPITAL LETTER D WITH HOOK
    (Unicode:#$018B; Attr:laUpper; CaseCode:#$018C),   // LATIN CAPITAL LETTER D WITH TOPBAR
    (Unicode:#$018C; Attr:laLower; CaseCode:#$018B),   // LATIN SMALL LETTER D WITH TOPBAR
    (Unicode:#$018D; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER TURNED DELTA
    (Unicode:#$018E; Attr:laUpper; CaseCode:#$01DD),   // LATIN CAPITAL LETTER REVERSED E
    (Unicode:#$018F; Attr:laUpper; CaseCode:#$0259),   // LATIN CAPITAL LETTER SCHWA
    (Unicode:#$0190; Attr:laUpper; CaseCode:#$025B),   // LATIN CAPITAL LETTER OPEN E
    (Unicode:#$0191; Attr:laUpper; CaseCode:#$0192),   // LATIN CAPITAL LETTER F WITH HOOK
    (Unicode:#$0192; Attr:laLower; CaseCode:#$0191),   // LATIN SMALL LETTER F WITH HOOK
    (Unicode:#$0193; Attr:laUpper; CaseCode:#$0260),   // LATIN CAPITAL LETTER G WITH HOOK
    (Unicode:#$0194; Attr:laUpper; CaseCode:#$0263),   // LATIN CAPITAL LETTER GAMMA
    (Unicode:#$0195; Attr:laLower; CaseCode:#$01F6),   // LATIN SMALL LETTER HV
    (Unicode:#$0196; Attr:laUpper; CaseCode:#$0269),   // LATIN CAPITAL LETTER IOTA
    (Unicode:#$0197; Attr:laUpper; CaseCode:#$0268),   // LATIN CAPITAL LETTER I WITH STROKE
    (Unicode:#$0198; Attr:laUpper; CaseCode:#$0199),   // LATIN CAPITAL LETTER K WITH HOOK
    (Unicode:#$0199; Attr:laLower; CaseCode:#$0198),   // LATIN SMALL LETTER K WITH HOOK
    (Unicode:#$019A; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER L WITH BAR
    (Unicode:#$019B; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER LAMBDA WITH STROKE
    (Unicode:#$019C; Attr:laUpper; CaseCode:#$026F),   // LATIN CAPITAL LETTER TURNED M
    (Unicode:#$019D; Attr:laUpper; CaseCode:#$0272),   // LATIN CAPITAL LETTER N WITH LEFT HOOK
    (Unicode:#$019E; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER N WITH LONG RIGHT LEG
    (Unicode:#$019F; Attr:laUpper; CaseCode:#$0275),   // LATIN CAPITAL LETTER O WITH MIDDLE TILDE
    (Unicode:#$01A0; Attr:laUpper; CaseCode:#$01A1),   // LATIN CAPITAL LETTER O WITH HORN
    (Unicode:#$01A1; Attr:laLower; CaseCode:#$01A0),   // LATIN SMALL LETTER O WITH HORN
    (Unicode:#$01A2; Attr:laUpper; CaseCode:#$01A3),   // LATIN CAPITAL LETTER OI
    (Unicode:#$01A3; Attr:laLower; CaseCode:#$01A2),   // LATIN SMALL LETTER OI
    (Unicode:#$01A4; Attr:laUpper; CaseCode:#$01A5),   // LATIN CAPITAL LETTER P WITH HOOK
    (Unicode:#$01A5; Attr:laLower; CaseCode:#$01A4),   // LATIN SMALL LETTER P WITH HOOK
    (Unicode:#$01A6; Attr:laUpper; CaseCode:#$0280),   // LATIN LETTER YR
    (Unicode:#$01A7; Attr:laUpper; CaseCode:#$01A8),   // LATIN CAPITAL LETTER TONE TWO
    (Unicode:#$01A8; Attr:laLower; CaseCode:#$01A7),   // LATIN SMALL LETTER TONE TWO
    (Unicode:#$01A9; Attr:laUpper; CaseCode:#$0283),   // LATIN CAPITAL LETTER ESH
    (Unicode:#$01AA; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER REVERSED ESH LOOP
    (Unicode:#$01AB; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER T WITH PALATAL HOOK
    (Unicode:#$01AC; Attr:laUpper; CaseCode:#$01AD),   // LATIN CAPITAL LETTER T WITH HOOK
    (Unicode:#$01AD; Attr:laLower; CaseCode:#$01AC),   // LATIN SMALL LETTER T WITH HOOK
    (Unicode:#$01AE; Attr:laUpper; CaseCode:#$0288),   // LATIN CAPITAL LETTER T WITH RETROFLEX HOOK
    (Unicode:#$01AF; Attr:laUpper; CaseCode:#$01B0),   // LATIN CAPITAL LETTER U WITH HORN
    (Unicode:#$01B0; Attr:laLower; CaseCode:#$01AF),   // LATIN SMALL LETTER U WITH HORN
    (Unicode:#$01B1; Attr:laUpper; CaseCode:#$028A),   // LATIN CAPITAL LETTER UPSILON
    (Unicode:#$01B2; Attr:laUpper; CaseCode:#$028B),   // LATIN CAPITAL LETTER V WITH HOOK
    (Unicode:#$01B3; Attr:laUpper; CaseCode:#$01B4),   // LATIN CAPITAL LETTER Y WITH HOOK
    (Unicode:#$01B4; Attr:laLower; CaseCode:#$01B3),   // LATIN SMALL LETTER Y WITH HOOK
    (Unicode:#$01B5; Attr:laUpper; CaseCode:#$01B6),   // LATIN CAPITAL LETTER Z WITH STROKE
    (Unicode:#$01B6; Attr:laLower; CaseCode:#$01B5),   // LATIN SMALL LETTER Z WITH STROKE
    (Unicode:#$01B7; Attr:laUpper; CaseCode:#$0292),   // LATIN CAPITAL LETTER EZH
    (Unicode:#$01B8; Attr:laUpper; CaseCode:#$01B9),   // LATIN CAPITAL LETTER EZH REVERSED
    (Unicode:#$01B9; Attr:laLower; CaseCode:#$01B8),   // LATIN SMALL LETTER EZH REVERSED
    (Unicode:#$01BA; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER EZH WITH TAIL
    (Unicode:#$01BC; Attr:laUpper; CaseCode:#$01BD),   // LATIN CAPITAL LETTER TONE FIVE
    (Unicode:#$01BD; Attr:laLower; CaseCode:#$01BC),   // LATIN SMALL LETTER TONE FIVE
    (Unicode:#$01BE; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER INVERTED GLOTTAL STOP WITH STROKE
    (Unicode:#$01BF; Attr:laLower; CaseCode:#$01F7),   // LATIN LETTER WYNN
    (Unicode:#$01C4; Attr:laUpper; CaseCode:#$01C6),   // LATIN CAPITAL LETTER DZ WITH CARON
    (Unicode:#$01C6; Attr:laLower; CaseCode:#$01C4),   // LATIN SMALL LETTER DZ WITH CARON
    (Unicode:#$01C7; Attr:laUpper; CaseCode:#$01C9),   // LATIN CAPITAL LETTER LJ
    (Unicode:#$01C9; Attr:laLower; CaseCode:#$01C7),   // LATIN SMALL LETTER LJ
    (Unicode:#$01CA; Attr:laUpper; CaseCode:#$01CC),   // LATIN CAPITAL LETTER NJ
    (Unicode:#$01CC; Attr:laLower; CaseCode:#$01CA),   // LATIN SMALL LETTER NJ
    (Unicode:#$01CD; Attr:laUpper; CaseCode:#$01CE),   // LATIN CAPITAL LETTER A WITH CARON
    (Unicode:#$01CE; Attr:laLower; CaseCode:#$01CD),   // LATIN SMALL LETTER A WITH CARON
    (Unicode:#$01CF; Attr:laUpper; CaseCode:#$01D0),   // LATIN CAPITAL LETTER I WITH CARON
    (Unicode:#$01D0; Attr:laLower; CaseCode:#$01CF),   // LATIN SMALL LETTER I WITH CARON
    (Unicode:#$01D1; Attr:laUpper; CaseCode:#$01D2),   // LATIN CAPITAL LETTER O WITH CARON
    (Unicode:#$01D2; Attr:laLower; CaseCode:#$01D1),   // LATIN SMALL LETTER O WITH CARON
    (Unicode:#$01D3; Attr:laUpper; CaseCode:#$01D4),   // LATIN CAPITAL LETTER U WITH CARON
    (Unicode:#$01D4; Attr:laLower; CaseCode:#$01D3),   // LATIN SMALL LETTER U WITH CARON
    (Unicode:#$01D5; Attr:laUpper; CaseCode:#$01D6),   // LATIN CAPITAL LETTER U WITH DIAERESIS AND MACRON
    (Unicode:#$01D6; Attr:laLower; CaseCode:#$01D5),   // LATIN SMALL LETTER U WITH DIAERESIS AND MACRON
    (Unicode:#$01D7; Attr:laUpper; CaseCode:#$01D8),   // LATIN CAPITAL LETTER U WITH DIAERESIS AND ACUTE
    (Unicode:#$01D8; Attr:laLower; CaseCode:#$01D7),   // LATIN SMALL LETTER U WITH DIAERESIS AND ACUTE
    (Unicode:#$01D9; Attr:laUpper; CaseCode:#$01DA),   // LATIN CAPITAL LETTER U WITH DIAERESIS AND CARON
    (Unicode:#$01DA; Attr:laLower; CaseCode:#$01D9),   // LATIN SMALL LETTER U WITH DIAERESIS AND CARON
    (Unicode:#$01DB; Attr:laUpper; CaseCode:#$01DC),   // LATIN CAPITAL LETTER U WITH DIAERESIS AND GRAVE
    (Unicode:#$01DC; Attr:laLower; CaseCode:#$01DB),   // LATIN SMALL LETTER U WITH DIAERESIS AND GRAVE
    (Unicode:#$01DD; Attr:laLower; CaseCode:#$018E),   // LATIN SMALL LETTER TURNED E
    (Unicode:#$01DE; Attr:laUpper; CaseCode:#$01DF),   // LATIN CAPITAL LETTER A WITH DIAERESIS AND MACRON
    (Unicode:#$01DF; Attr:laLower; CaseCode:#$01DE),   // LATIN SMALL LETTER A WITH DIAERESIS AND MACRON
    (Unicode:#$01E0; Attr:laUpper; CaseCode:#$01E1),   // LATIN CAPITAL LETTER A WITH DOT ABOVE AND MACRON
    (Unicode:#$01E1; Attr:laLower; CaseCode:#$01E0),   // LATIN SMALL LETTER A WITH DOT ABOVE AND MACRON
    (Unicode:#$01E2; Attr:laUpper; CaseCode:#$01E3),   // LATIN CAPITAL LETTER AE WITH MACRON
    (Unicode:#$01E3; Attr:laLower; CaseCode:#$01E2),   // LATIN SMALL LETTER AE WITH MACRON
    (Unicode:#$01E4; Attr:laUpper; CaseCode:#$01E5),   // LATIN CAPITAL LETTER G WITH STROKE
    (Unicode:#$01E5; Attr:laLower; CaseCode:#$01E4),   // LATIN SMALL LETTER G WITH STROKE
    (Unicode:#$01E6; Attr:laUpper; CaseCode:#$01E7),   // LATIN CAPITAL LETTER G WITH CARON
    (Unicode:#$01E7; Attr:laLower; CaseCode:#$01E6),   // LATIN SMALL LETTER G WITH CARON
    (Unicode:#$01E8; Attr:laUpper; CaseCode:#$01E9),   // LATIN CAPITAL LETTER K WITH CARON
    (Unicode:#$01E9; Attr:laLower; CaseCode:#$01E8),   // LATIN SMALL LETTER K WITH CARON
    (Unicode:#$01EA; Attr:laUpper; CaseCode:#$01EB),   // LATIN CAPITAL LETTER O WITH OGONEK
    (Unicode:#$01EB; Attr:laLower; CaseCode:#$01EA),   // LATIN SMALL LETTER O WITH OGONEK
    (Unicode:#$01EC; Attr:laUpper; CaseCode:#$01ED),   // LATIN CAPITAL LETTER O WITH OGONEK AND MACRON
    (Unicode:#$01ED; Attr:laLower; CaseCode:#$01EC),   // LATIN SMALL LETTER O WITH OGONEK AND MACRON
    (Unicode:#$01EE; Attr:laUpper; CaseCode:#$01EF),   // LATIN CAPITAL LETTER EZH WITH CARON
    (Unicode:#$01EF; Attr:laLower; CaseCode:#$01EE),   // LATIN SMALL LETTER EZH WITH CARON
    (Unicode:#$01F0; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER J WITH CARON
    (Unicode:#$01F1; Attr:laUpper; CaseCode:#$01F3),   // LATIN CAPITAL LETTER DZ
    (Unicode:#$01F3; Attr:laLower; CaseCode:#$01F1),   // LATIN SMALL LETTER DZ
    (Unicode:#$01F4; Attr:laUpper; CaseCode:#$01F5),   // LATIN CAPITAL LETTER G WITH ACUTE
    (Unicode:#$01F5; Attr:laLower; CaseCode:#$01F4),   // LATIN SMALL LETTER G WITH ACUTE
    (Unicode:#$01F6; Attr:laUpper; CaseCode:#$0195),   // LATIN CAPITAL LETTER HWAIR
    (Unicode:#$01F7; Attr:laUpper; CaseCode:#$01BF),   // LATIN CAPITAL LETTER WYNN
    (Unicode:#$01F8; Attr:laUpper; CaseCode:#$01F9),   // LATIN CAPITAL LETTER N WITH GRAVE
    (Unicode:#$01F9; Attr:laLower; CaseCode:#$01F8),   // LATIN SMALL LETTER N WITH GRAVE
    (Unicode:#$01FA; Attr:laUpper; CaseCode:#$01FB),   // LATIN CAPITAL LETTER A WITH RING ABOVE AND ACUTE
    (Unicode:#$01FB; Attr:laLower; CaseCode:#$01FA),   // LATIN SMALL LETTER A WITH RING ABOVE AND ACUTE
    (Unicode:#$01FC; Attr:laUpper; CaseCode:#$01FD),   // LATIN CAPITAL LETTER AE WITH ACUTE
    (Unicode:#$01FD; Attr:laLower; CaseCode:#$01FC),   // LATIN SMALL LETTER AE WITH ACUTE
    (Unicode:#$01FE; Attr:laUpper; CaseCode:#$01FF),   // LATIN CAPITAL LETTER O WITH STROKE AND ACUTE
    (Unicode:#$01FF; Attr:laLower; CaseCode:#$01FE),   // LATIN SMALL LETTER O WITH STROKE AND ACUTE
    (Unicode:#$0200; Attr:laUpper; CaseCode:#$0201),   // LATIN CAPITAL LETTER A WITH DOUBLE GRAVE
    (Unicode:#$0201; Attr:laLower; CaseCode:#$0200),   // LATIN SMALL LETTER A WITH DOUBLE GRAVE
    (Unicode:#$0202; Attr:laUpper; CaseCode:#$0203),   // LATIN CAPITAL LETTER A WITH INVERTED BREVE
    (Unicode:#$0203; Attr:laLower; CaseCode:#$0202),   // LATIN SMALL LETTER A WITH INVERTED BREVE
    (Unicode:#$0204; Attr:laUpper; CaseCode:#$0205),   // LATIN CAPITAL LETTER E WITH DOUBLE GRAVE
    (Unicode:#$0205; Attr:laLower; CaseCode:#$0204),   // LATIN SMALL LETTER E WITH DOUBLE GRAVE
    (Unicode:#$0206; Attr:laUpper; CaseCode:#$0207),   // LATIN CAPITAL LETTER E WITH INVERTED BREVE
    (Unicode:#$0207; Attr:laLower; CaseCode:#$0206),   // LATIN SMALL LETTER E WITH INVERTED BREVE
    (Unicode:#$0208; Attr:laUpper; CaseCode:#$0209),   // LATIN CAPITAL LETTER I WITH DOUBLE GRAVE
    (Unicode:#$0209; Attr:laLower; CaseCode:#$0208),   // LATIN SMALL LETTER I WITH DOUBLE GRAVE
    (Unicode:#$020A; Attr:laUpper; CaseCode:#$020B),   // LATIN CAPITAL LETTER I WITH INVERTED BREVE
    (Unicode:#$020B; Attr:laLower; CaseCode:#$020A),   // LATIN SMALL LETTER I WITH INVERTED BREVE
    (Unicode:#$020C; Attr:laUpper; CaseCode:#$020D),   // LATIN CAPITAL LETTER O WITH DOUBLE GRAVE
    (Unicode:#$020D; Attr:laLower; CaseCode:#$020C),   // LATIN SMALL LETTER O WITH DOUBLE GRAVE
    (Unicode:#$020E; Attr:laUpper; CaseCode:#$020F),   // LATIN CAPITAL LETTER O WITH INVERTED BREVE
    (Unicode:#$020F; Attr:laLower; CaseCode:#$020E),   // LATIN SMALL LETTER O WITH INVERTED BREVE
    (Unicode:#$0210; Attr:laUpper; CaseCode:#$0211),   // LATIN CAPITAL LETTER R WITH DOUBLE GRAVE
    (Unicode:#$0211; Attr:laLower; CaseCode:#$0210),   // LATIN SMALL LETTER R WITH DOUBLE GRAVE
    (Unicode:#$0212; Attr:laUpper; CaseCode:#$0213),   // LATIN CAPITAL LETTER R WITH INVERTED BREVE
    (Unicode:#$0213; Attr:laLower; CaseCode:#$0212),   // LATIN SMALL LETTER R WITH INVERTED BREVE
    (Unicode:#$0214; Attr:laUpper; CaseCode:#$0215),   // LATIN CAPITAL LETTER U WITH DOUBLE GRAVE
    (Unicode:#$0215; Attr:laLower; CaseCode:#$0214),   // LATIN SMALL LETTER U WITH DOUBLE GRAVE
    (Unicode:#$0216; Attr:laUpper; CaseCode:#$0217),   // LATIN CAPITAL LETTER U WITH INVERTED BREVE
    (Unicode:#$0217; Attr:laLower; CaseCode:#$0216),   // LATIN SMALL LETTER U WITH INVERTED BREVE
    (Unicode:#$0218; Attr:laUpper; CaseCode:#$0219),   // LATIN CAPITAL LETTER S WITH COMMA BELOW
    (Unicode:#$0219; Attr:laLower; CaseCode:#$0218),   // LATIN SMALL LETTER S WITH COMMA BELOW
    (Unicode:#$021A; Attr:laUpper; CaseCode:#$021B),   // LATIN CAPITAL LETTER T WITH COMMA BELOW
    (Unicode:#$021B; Attr:laLower; CaseCode:#$021A),   // LATIN SMALL LETTER T WITH COMMA BELOW
    (Unicode:#$021C; Attr:laUpper; CaseCode:#$021D),   // LATIN CAPITAL LETTER YOGH
    (Unicode:#$021D; Attr:laLower; CaseCode:#$021C),   // LATIN SMALL LETTER YOGH
    (Unicode:#$021E; Attr:laUpper; CaseCode:#$021F),   // LATIN CAPITAL LETTER H WITH CARON
    (Unicode:#$021F; Attr:laLower; CaseCode:#$021E),   // LATIN SMALL LETTER H WITH CARON
    (Unicode:#$0222; Attr:laUpper; CaseCode:#$0223),   // LATIN CAPITAL LETTER OU
    (Unicode:#$0223; Attr:laLower; CaseCode:#$0222),   // LATIN SMALL LETTER OU
    (Unicode:#$0224; Attr:laUpper; CaseCode:#$0225),   // LATIN CAPITAL LETTER Z WITH HOOK
    (Unicode:#$0225; Attr:laLower; CaseCode:#$0224),   // LATIN SMALL LETTER Z WITH HOOK
    (Unicode:#$0226; Attr:laUpper; CaseCode:#$0227),   // LATIN CAPITAL LETTER A WITH DOT ABOVE
    (Unicode:#$0227; Attr:laLower; CaseCode:#$0226),   // LATIN SMALL LETTER A WITH DOT ABOVE
    (Unicode:#$0228; Attr:laUpper; CaseCode:#$0229),   // LATIN CAPITAL LETTER E WITH CEDILLA
    (Unicode:#$0229; Attr:laLower; CaseCode:#$0228),   // LATIN SMALL LETTER E WITH CEDILLA
    (Unicode:#$022A; Attr:laUpper; CaseCode:#$022B),   // LATIN CAPITAL LETTER O WITH DIAERESIS AND MACRON
    (Unicode:#$022B; Attr:laLower; CaseCode:#$022A),   // LATIN SMALL LETTER O WITH DIAERESIS AND MACRON
    (Unicode:#$022C; Attr:laUpper; CaseCode:#$022D),   // LATIN CAPITAL LETTER O WITH TILDE AND MACRON
    (Unicode:#$022D; Attr:laLower; CaseCode:#$022C),   // LATIN SMALL LETTER O WITH TILDE AND MACRON
    (Unicode:#$022E; Attr:laUpper; CaseCode:#$022F),   // LATIN CAPITAL LETTER O WITH DOT ABOVE
    (Unicode:#$022F; Attr:laLower; CaseCode:#$022E),   // LATIN SMALL LETTER O WITH DOT ABOVE
    (Unicode:#$0230; Attr:laUpper; CaseCode:#$0231),   // LATIN CAPITAL LETTER O WITH DOT ABOVE AND MACRON
    (Unicode:#$0231; Attr:laLower; CaseCode:#$0230),   // LATIN SMALL LETTER O WITH DOT ABOVE AND MACRON
    (Unicode:#$0232; Attr:laUpper; CaseCode:#$0233),   // LATIN CAPITAL LETTER Y WITH MACRON
    (Unicode:#$0233; Attr:laLower; CaseCode:#$0232),   // LATIN SMALL LETTER Y WITH MACRON
    (Unicode:#$0250; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER TURNED A
    (Unicode:#$0251; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER ALPHA
    (Unicode:#$0252; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER TURNED ALPHA
    (Unicode:#$0253; Attr:laLower; CaseCode:#$0181),   // LATIN SMALL LETTER B WITH HOOK
    (Unicode:#$0254; Attr:laLower; CaseCode:#$0186),   // LATIN SMALL LETTER OPEN O
    (Unicode:#$0255; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER C WITH CURL
    (Unicode:#$0256; Attr:laLower; CaseCode:#$0189),   // LATIN SMALL LETTER D WITH TAIL
    (Unicode:#$0257; Attr:laLower; CaseCode:#$018A),   // LATIN SMALL LETTER D WITH HOOK
    (Unicode:#$0258; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER REVERSED E
    (Unicode:#$0259; Attr:laLower; CaseCode:#$018F),   // LATIN SMALL LETTER SCHWA
    (Unicode:#$025A; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER SCHWA WITH HOOK
    (Unicode:#$025B; Attr:laLower; CaseCode:#$0190),   // LATIN SMALL LETTER OPEN E
    (Unicode:#$025C; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER REVERSED OPEN E
    (Unicode:#$025D; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER REVERSED OPEN E WITH HOOK
    (Unicode:#$025E; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER CLOSED REVERSED OPEN E
    (Unicode:#$025F; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER DOTLESS J WITH STROKE
    (Unicode:#$0260; Attr:laLower; CaseCode:#$0193),   // LATIN SMALL LETTER G WITH HOOK
    (Unicode:#$0261; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER SCRIPT G
    (Unicode:#$0262; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER SMALL CAPITAL G
    (Unicode:#$0263; Attr:laLower; CaseCode:#$0194),   // LATIN SMALL LETTER GAMMA
    (Unicode:#$0264; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER RAMS HORN
    (Unicode:#$0265; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER TURNED H
    (Unicode:#$0266; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER H WITH HOOK
    (Unicode:#$0267; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER HENG WITH HOOK
    (Unicode:#$0268; Attr:laLower; CaseCode:#$0197),   // LATIN SMALL LETTER I WITH STROKE
    (Unicode:#$0269; Attr:laLower; CaseCode:#$0196),   // LATIN SMALL LETTER IOTA
    (Unicode:#$026A; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER SMALL CAPITAL I
    (Unicode:#$026B; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER L WITH MIDDLE TILDE
    (Unicode:#$026C; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER L WITH BELT
    (Unicode:#$026D; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER L WITH RETROFLEX HOOK
    (Unicode:#$026E; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER LEZH
    (Unicode:#$026F; Attr:laLower; CaseCode:#$019C),   // LATIN SMALL LETTER TURNED M
    (Unicode:#$0270; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER TURNED M WITH LONG LEG
    (Unicode:#$0271; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER M WITH HOOK
    (Unicode:#$0272; Attr:laLower; CaseCode:#$019D),   // LATIN SMALL LETTER N WITH LEFT HOOK
    (Unicode:#$0273; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER N WITH RETROFLEX HOOK
    (Unicode:#$0274; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER SMALL CAPITAL N
    (Unicode:#$0275; Attr:laLower; CaseCode:#$019F),   // LATIN SMALL LETTER BARRED O
    (Unicode:#$0276; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER SMALL CAPITAL OE
    (Unicode:#$0277; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER CLOSED OMEGA
    (Unicode:#$0278; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER PHI
    (Unicode:#$0279; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER TURNED R
    (Unicode:#$027A; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER TURNED R WITH LONG LEG
    (Unicode:#$027B; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER TURNED R WITH HOOK
    (Unicode:#$027C; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER R WITH LONG LEG
    (Unicode:#$027D; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER R WITH TAIL
    (Unicode:#$027E; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER R WITH FISHHOOK
    (Unicode:#$027F; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER REVERSED R WITH FISHHOOK
    (Unicode:#$0280; Attr:laLower; CaseCode:#$01A6),   // LATIN LETTER SMALL CAPITAL R
    (Unicode:#$0281; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER SMALL CAPITAL INVERTED R
    (Unicode:#$0282; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER S WITH HOOK
    (Unicode:#$0283; Attr:laLower; CaseCode:#$01A9),   // LATIN SMALL LETTER ESH
    (Unicode:#$0284; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER DOTLESS J WITH STROKE AND HOOK
    (Unicode:#$0285; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER SQUAT REVERSED ESH
    (Unicode:#$0286; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER ESH WITH CURL
    (Unicode:#$0287; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER TURNED T
    (Unicode:#$0288; Attr:laLower; CaseCode:#$01AE),   // LATIN SMALL LETTER T WITH RETROFLEX HOOK
    (Unicode:#$0289; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER U BAR
    (Unicode:#$028A; Attr:laLower; CaseCode:#$01B1),   // LATIN SMALL LETTER UPSILON
    (Unicode:#$028B; Attr:laLower; CaseCode:#$01B2),   // LATIN SMALL LETTER V WITH HOOK
    (Unicode:#$028C; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER TURNED V
    (Unicode:#$028D; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER TURNED W
    (Unicode:#$028E; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER TURNED Y
    (Unicode:#$028F; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER SMALL CAPITAL Y
    (Unicode:#$0290; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER Z WITH RETROFLEX HOOK
    (Unicode:#$0291; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER Z WITH CURL
    (Unicode:#$0292; Attr:laLower; CaseCode:#$01B7),   // LATIN SMALL LETTER EZH
    (Unicode:#$0293; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER EZH WITH CURL
    (Unicode:#$0294; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER GLOTTAL STOP
    (Unicode:#$0295; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER PHARYNGEAL VOICED FRICATIVE
    (Unicode:#$0296; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER INVERTED GLOTTAL STOP
    (Unicode:#$0297; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER STRETCHED C
    (Unicode:#$0298; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER BILABIAL CLICK
    (Unicode:#$0299; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER SMALL CAPITAL B
    (Unicode:#$029A; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER CLOSED OPEN E
    (Unicode:#$029B; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER SMALL CAPITAL G WITH HOOK
    (Unicode:#$029C; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER SMALL CAPITAL H
    (Unicode:#$029D; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER J WITH CROSSED-TAIL
    (Unicode:#$029E; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER TURNED K
    (Unicode:#$029F; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER SMALL CAPITAL L
    (Unicode:#$02A0; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER Q WITH HOOK
    (Unicode:#$02A1; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER GLOTTAL STOP WITH STROKE
    (Unicode:#$02A2; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER REVERSED GLOTTAL STOP WITH STROKE
    (Unicode:#$02A3; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER DZ DIGRAPH
    (Unicode:#$02A4; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER DEZH DIGRAPH
    (Unicode:#$02A5; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER DZ DIGRAPH WITH CURL
    (Unicode:#$02A6; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER TS DIGRAPH
    (Unicode:#$02A7; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER TESH DIGRAPH
    (Unicode:#$02A8; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER TC DIGRAPH WITH CURL
    (Unicode:#$02A9; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER FENG DIGRAPH
    (Unicode:#$02AA; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER LS DIGRAPH
    (Unicode:#$02AB; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER LZ DIGRAPH
    (Unicode:#$02AC; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER BILABIAL PERCUSSIVE
    (Unicode:#$02AD; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER BIDENTAL PERCUSSIVE
    (Unicode:#$0386; Attr:laUpper; CaseCode:#$03AC),   // GREEK CAPITAL LETTER ALPHA WITH TONOS
    (Unicode:#$0388; Attr:laUpper; CaseCode:#$03AD),   // GREEK CAPITAL LETTER EPSILON WITH TONOS
    (Unicode:#$0389; Attr:laUpper; CaseCode:#$03AE),   // GREEK CAPITAL LETTER ETA WITH TONOS
    (Unicode:#$038A; Attr:laUpper; CaseCode:#$03AF),   // GREEK CAPITAL LETTER IOTA WITH TONOS
    (Unicode:#$038C; Attr:laUpper; CaseCode:#$03CC),   // GREEK CAPITAL LETTER OMICRON WITH TONOS
    (Unicode:#$038E; Attr:laUpper; CaseCode:#$03CD),   // GREEK CAPITAL LETTER UPSILON WITH TONOS
    (Unicode:#$038F; Attr:laUpper; CaseCode:#$03CE),   // GREEK CAPITAL LETTER OMEGA WITH TONOS
    (Unicode:#$0390; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS
    (Unicode:#$0391; Attr:laUpper; CaseCode:#$03B1),   // GREEK CAPITAL LETTER ALPHA
    (Unicode:#$0392; Attr:laUpper; CaseCode:#$03B2),   // GREEK CAPITAL LETTER BETA
    (Unicode:#$0393; Attr:laUpper; CaseCode:#$03B3),   // GREEK CAPITAL LETTER GAMMA
    (Unicode:#$0394; Attr:laUpper; CaseCode:#$03B4),   // GREEK CAPITAL LETTER DELTA
    (Unicode:#$0395; Attr:laUpper; CaseCode:#$03B5),   // GREEK CAPITAL LETTER EPSILON
    (Unicode:#$0396; Attr:laUpper; CaseCode:#$03B6),   // GREEK CAPITAL LETTER ZETA
    (Unicode:#$0397; Attr:laUpper; CaseCode:#$03B7),   // GREEK CAPITAL LETTER ETA
    (Unicode:#$0398; Attr:laUpper; CaseCode:#$03B8),   // GREEK CAPITAL LETTER THETA
    (Unicode:#$0399; Attr:laUpper; CaseCode:#$03B9),   // GREEK CAPITAL LETTER IOTA
    (Unicode:#$039A; Attr:laUpper; CaseCode:#$03BA),   // GREEK CAPITAL LETTER KAPPA
    (Unicode:#$039B; Attr:laUpper; CaseCode:#$03BB),   // GREEK CAPITAL LETTER LAMDA
    (Unicode:#$039C; Attr:laUpper; CaseCode:#$03BC),   // GREEK CAPITAL LETTER MU
    (Unicode:#$039D; Attr:laUpper; CaseCode:#$03BD),   // GREEK CAPITAL LETTER NU
    (Unicode:#$039E; Attr:laUpper; CaseCode:#$03BE),   // GREEK CAPITAL LETTER XI
    (Unicode:#$039F; Attr:laUpper; CaseCode:#$03BF),   // GREEK CAPITAL LETTER OMICRON
    (Unicode:#$03A0; Attr:laUpper; CaseCode:#$03C0),   // GREEK CAPITAL LETTER PI
    (Unicode:#$03A1; Attr:laUpper; CaseCode:#$03C1),   // GREEK CAPITAL LETTER RHO
    (Unicode:#$03A3; Attr:laUpper; CaseCode:#$03C3),   // GREEK CAPITAL LETTER SIGMA
    (Unicode:#$03A4; Attr:laUpper; CaseCode:#$03C4),   // GREEK CAPITAL LETTER TAU
    (Unicode:#$03A5; Attr:laUpper; CaseCode:#$03C5),   // GREEK CAPITAL LETTER UPSILON
    (Unicode:#$03A6; Attr:laUpper; CaseCode:#$03C6),   // GREEK CAPITAL LETTER PHI
    (Unicode:#$03A7; Attr:laUpper; CaseCode:#$03C7),   // GREEK CAPITAL LETTER CHI
    (Unicode:#$03A8; Attr:laUpper; CaseCode:#$03C8),   // GREEK CAPITAL LETTER PSI
    (Unicode:#$03A9; Attr:laUpper; CaseCode:#$03C9),   // GREEK CAPITAL LETTER OMEGA
    (Unicode:#$03AA; Attr:laUpper; CaseCode:#$03CA),   // GREEK CAPITAL LETTER IOTA WITH DIALYTIKA
    (Unicode:#$03AB; Attr:laUpper; CaseCode:#$03CB),   // GREEK CAPITAL LETTER UPSILON WITH DIALYTIKA
    (Unicode:#$03AC; Attr:laLower; CaseCode:#$0386),   // GREEK SMALL LETTER ALPHA WITH TONOS
    (Unicode:#$03AD; Attr:laLower; CaseCode:#$0388),   // GREEK SMALL LETTER EPSILON WITH TONOS
    (Unicode:#$03AE; Attr:laLower; CaseCode:#$0389),   // GREEK SMALL LETTER ETA WITH TONOS
    (Unicode:#$03AF; Attr:laLower; CaseCode:#$038A),   // GREEK SMALL LETTER IOTA WITH TONOS
    (Unicode:#$03B0; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS
    (Unicode:#$03B1; Attr:laLower; CaseCode:#$0391),   // GREEK SMALL LETTER ALPHA
    (Unicode:#$03B2; Attr:laLower; CaseCode:#$0392),   // GREEK SMALL LETTER BETA
    (Unicode:#$03B3; Attr:laLower; CaseCode:#$0393),   // GREEK SMALL LETTER GAMMA
    (Unicode:#$03B4; Attr:laLower; CaseCode:#$0394),   // GREEK SMALL LETTER DELTA
    (Unicode:#$03B5; Attr:laLower; CaseCode:#$0395),   // GREEK SMALL LETTER EPSILON
    (Unicode:#$03B6; Attr:laLower; CaseCode:#$0396),   // GREEK SMALL LETTER ZETA
    (Unicode:#$03B7; Attr:laLower; CaseCode:#$0397),   // GREEK SMALL LETTER ETA
    (Unicode:#$03B8; Attr:laLower; CaseCode:#$0398),   // GREEK SMALL LETTER THETA
    (Unicode:#$03B9; Attr:laLower; CaseCode:#$0399),   // GREEK SMALL LETTER IOTA
    (Unicode:#$03BA; Attr:laLower; CaseCode:#$039A),   // GREEK SMALL LETTER KAPPA
    (Unicode:#$03BB; Attr:laLower; CaseCode:#$039B),   // GREEK SMALL LETTER LAMDA
    (Unicode:#$03BC; Attr:laLower; CaseCode:#$039C),   // GREEK SMALL LETTER MU
    (Unicode:#$03BD; Attr:laLower; CaseCode:#$039D),   // GREEK SMALL LETTER NU
    (Unicode:#$03BE; Attr:laLower; CaseCode:#$039E),   // GREEK SMALL LETTER XI
    (Unicode:#$03BF; Attr:laLower; CaseCode:#$039F),   // GREEK SMALL LETTER OMICRON
    (Unicode:#$03C0; Attr:laLower; CaseCode:#$03A0),   // GREEK SMALL LETTER PI
    (Unicode:#$03C1; Attr:laLower; CaseCode:#$03A1),   // GREEK SMALL LETTER RHO
    (Unicode:#$03C2; Attr:laLower; CaseCode:#$03A3),   // GREEK SMALL LETTER FINAL SIGMA
    (Unicode:#$03C3; Attr:laLower; CaseCode:#$03A3),   // GREEK SMALL LETTER SIGMA
    (Unicode:#$03C4; Attr:laLower; CaseCode:#$03A4),   // GREEK SMALL LETTER TAU
    (Unicode:#$03C5; Attr:laLower; CaseCode:#$03A5),   // GREEK SMALL LETTER UPSILON
    (Unicode:#$03C6; Attr:laLower; CaseCode:#$03A6),   // GREEK SMALL LETTER PHI
    (Unicode:#$03C7; Attr:laLower; CaseCode:#$03A7),   // GREEK SMALL LETTER CHI
    (Unicode:#$03C8; Attr:laLower; CaseCode:#$03A8),   // GREEK SMALL LETTER PSI
    (Unicode:#$03C9; Attr:laLower; CaseCode:#$03A9),   // GREEK SMALL LETTER OMEGA
    (Unicode:#$03CA; Attr:laLower; CaseCode:#$03AA),   // GREEK SMALL LETTER IOTA WITH DIALYTIKA
    (Unicode:#$03CB; Attr:laLower; CaseCode:#$03AB),   // GREEK SMALL LETTER UPSILON WITH DIALYTIKA
    (Unicode:#$03CC; Attr:laLower; CaseCode:#$038C),   // GREEK SMALL LETTER OMICRON WITH TONOS
    (Unicode:#$03CD; Attr:laLower; CaseCode:#$038E),   // GREEK SMALL LETTER UPSILON WITH TONOS
    (Unicode:#$03CE; Attr:laLower; CaseCode:#$038F),   // GREEK SMALL LETTER OMEGA WITH TONOS
    (Unicode:#$03D0; Attr:laLower; CaseCode:#$0392),   // GREEK BETA SYMBOL
    (Unicode:#$03D1; Attr:laLower; CaseCode:#$0398),   // GREEK THETA SYMBOL
    (Unicode:#$03D2; Attr:laUpper; CaseCode:#$FFFF),   // GREEK UPSILON WITH HOOK SYMBOL
    (Unicode:#$03D3; Attr:laUpper; CaseCode:#$FFFF),   // GREEK UPSILON WITH ACUTE AND HOOK SYMBOL
    (Unicode:#$03D4; Attr:laUpper; CaseCode:#$FFFF),   // GREEK UPSILON WITH DIAERESIS AND HOOK SYMBOL
    (Unicode:#$03D5; Attr:laLower; CaseCode:#$03A6),   // GREEK PHI SYMBOL
    (Unicode:#$03D6; Attr:laLower; CaseCode:#$03A0),   // GREEK PI SYMBOL
    (Unicode:#$03D7; Attr:laLower; CaseCode:#$FFFF),   // GREEK KAI SYMBOL
    (Unicode:#$03DA; Attr:laUpper; CaseCode:#$03DB),   // GREEK LETTER STIGMA
    (Unicode:#$03DB; Attr:laLower; CaseCode:#$03DA),   // GREEK SMALL LETTER STIGMA
    (Unicode:#$03DC; Attr:laUpper; CaseCode:#$03DD),   // GREEK LETTER DIGAMMA
    (Unicode:#$03DD; Attr:laLower; CaseCode:#$03DC),   // GREEK SMALL LETTER DIGAMMA
    (Unicode:#$03DE; Attr:laUpper; CaseCode:#$03DF),   // GREEK LETTER KOPPA
    (Unicode:#$03DF; Attr:laLower; CaseCode:#$03DE),   // GREEK SMALL LETTER KOPPA
    (Unicode:#$03E0; Attr:laUpper; CaseCode:#$03E1),   // GREEK LETTER SAMPI
    (Unicode:#$03E1; Attr:laLower; CaseCode:#$03E0),   // GREEK SMALL LETTER SAMPI
    (Unicode:#$03E2; Attr:laUpper; CaseCode:#$03E3),   // COPTIC CAPITAL LETTER SHEI
    (Unicode:#$03E3; Attr:laLower; CaseCode:#$03E2),   // COPTIC SMALL LETTER SHEI
    (Unicode:#$03E4; Attr:laUpper; CaseCode:#$03E5),   // COPTIC CAPITAL LETTER FEI
    (Unicode:#$03E5; Attr:laLower; CaseCode:#$03E4),   // COPTIC SMALL LETTER FEI
    (Unicode:#$03E6; Attr:laUpper; CaseCode:#$03E7),   // COPTIC CAPITAL LETTER KHEI
    (Unicode:#$03E7; Attr:laLower; CaseCode:#$03E6),   // COPTIC SMALL LETTER KHEI
    (Unicode:#$03E8; Attr:laUpper; CaseCode:#$03E9),   // COPTIC CAPITAL LETTER HORI
    (Unicode:#$03E9; Attr:laLower; CaseCode:#$03E8),   // COPTIC SMALL LETTER HORI
    (Unicode:#$03EA; Attr:laUpper; CaseCode:#$03EB),   // COPTIC CAPITAL LETTER GANGIA
    (Unicode:#$03EB; Attr:laLower; CaseCode:#$03EA),   // COPTIC SMALL LETTER GANGIA
    (Unicode:#$03EC; Attr:laUpper; CaseCode:#$03ED),   // COPTIC CAPITAL LETTER SHIMA
    (Unicode:#$03ED; Attr:laLower; CaseCode:#$03EC),   // COPTIC SMALL LETTER SHIMA
    (Unicode:#$03EE; Attr:laUpper; CaseCode:#$03EF),   // COPTIC CAPITAL LETTER DEI
    (Unicode:#$03EF; Attr:laLower; CaseCode:#$03EE),   // COPTIC SMALL LETTER DEI
    (Unicode:#$03F0; Attr:laLower; CaseCode:#$039A),   // GREEK KAPPA SYMBOL
    (Unicode:#$03F1; Attr:laLower; CaseCode:#$03A1),   // GREEK RHO SYMBOL
    (Unicode:#$03F2; Attr:laLower; CaseCode:#$03A3),   // GREEK LUNATE SIGMA SYMBOL
    (Unicode:#$03F3; Attr:laLower; CaseCode:#$FFFF),   // GREEK LETTER YOT
    (Unicode:#$03F4; Attr:laUpper; CaseCode:#$03B8),   // GREEK CAPITAL THETA SYMBOL
    (Unicode:#$03F5; Attr:laLower; CaseCode:#$0395),   // GREEK LUNATE EPSILON SYMBOL
    (Unicode:#$0400; Attr:laUpper; CaseCode:#$0450),   // CYRILLIC CAPITAL LETTER IE WITH GRAVE
    (Unicode:#$0401; Attr:laUpper; CaseCode:#$0451),   // CYRILLIC CAPITAL LETTER IO
    (Unicode:#$0402; Attr:laUpper; CaseCode:#$0452),   // CYRILLIC CAPITAL LETTER DJE
    (Unicode:#$0403; Attr:laUpper; CaseCode:#$0453),   // CYRILLIC CAPITAL LETTER GJE
    (Unicode:#$0404; Attr:laUpper; CaseCode:#$0454),   // CYRILLIC CAPITAL LETTER UKRAINIAN IE
    (Unicode:#$0405; Attr:laUpper; CaseCode:#$0455),   // CYRILLIC CAPITAL LETTER DZE
    (Unicode:#$0406; Attr:laUpper; CaseCode:#$0456),   // CYRILLIC CAPITAL LETTER BYELORUSSIAN-UKRAINIAN I
    (Unicode:#$0407; Attr:laUpper; CaseCode:#$0457),   // CYRILLIC CAPITAL LETTER YI
    (Unicode:#$0408; Attr:laUpper; CaseCode:#$0458),   // CYRILLIC CAPITAL LETTER JE
    (Unicode:#$0409; Attr:laUpper; CaseCode:#$0459),   // CYRILLIC CAPITAL LETTER LJE
    (Unicode:#$040A; Attr:laUpper; CaseCode:#$045A),   // CYRILLIC CAPITAL LETTER NJE
    (Unicode:#$040B; Attr:laUpper; CaseCode:#$045B),   // CYRILLIC CAPITAL LETTER TSHE
    (Unicode:#$040C; Attr:laUpper; CaseCode:#$045C),   // CYRILLIC CAPITAL LETTER KJE
    (Unicode:#$040D; Attr:laUpper; CaseCode:#$045D),   // CYRILLIC CAPITAL LETTER I WITH GRAVE
    (Unicode:#$040E; Attr:laUpper; CaseCode:#$045E),   // CYRILLIC CAPITAL LETTER SHORT U
    (Unicode:#$040F; Attr:laUpper; CaseCode:#$045F),   // CYRILLIC CAPITAL LETTER DZHE
    (Unicode:#$0410; Attr:laUpper; CaseCode:#$0430),   // CYRILLIC CAPITAL LETTER A
    (Unicode:#$0411; Attr:laUpper; CaseCode:#$0431),   // CYRILLIC CAPITAL LETTER BE
    (Unicode:#$0412; Attr:laUpper; CaseCode:#$0432),   // CYRILLIC CAPITAL LETTER VE
    (Unicode:#$0413; Attr:laUpper; CaseCode:#$0433),   // CYRILLIC CAPITAL LETTER GHE
    (Unicode:#$0414; Attr:laUpper; CaseCode:#$0434),   // CYRILLIC CAPITAL LETTER DE
    (Unicode:#$0415; Attr:laUpper; CaseCode:#$0435),   // CYRILLIC CAPITAL LETTER IE
    (Unicode:#$0416; Attr:laUpper; CaseCode:#$0436),   // CYRILLIC CAPITAL LETTER ZHE
    (Unicode:#$0417; Attr:laUpper; CaseCode:#$0437),   // CYRILLIC CAPITAL LETTER ZE
    (Unicode:#$0418; Attr:laUpper; CaseCode:#$0438),   // CYRILLIC CAPITAL LETTER I
    (Unicode:#$0419; Attr:laUpper; CaseCode:#$0439),   // CYRILLIC CAPITAL LETTER SHORT I
    (Unicode:#$041A; Attr:laUpper; CaseCode:#$043A),   // CYRILLIC CAPITAL LETTER KA
    (Unicode:#$041B; Attr:laUpper; CaseCode:#$043B),   // CYRILLIC CAPITAL LETTER EL
    (Unicode:#$041C; Attr:laUpper; CaseCode:#$043C),   // CYRILLIC CAPITAL LETTER EM
    (Unicode:#$041D; Attr:laUpper; CaseCode:#$043D),   // CYRILLIC CAPITAL LETTER EN
    (Unicode:#$041E; Attr:laUpper; CaseCode:#$043E),   // CYRILLIC CAPITAL LETTER O
    (Unicode:#$041F; Attr:laUpper; CaseCode:#$043F),   // CYRILLIC CAPITAL LETTER PE
    (Unicode:#$0420; Attr:laUpper; CaseCode:#$0440),   // CYRILLIC CAPITAL LETTER ER
    (Unicode:#$0421; Attr:laUpper; CaseCode:#$0441),   // CYRILLIC CAPITAL LETTER ES
    (Unicode:#$0422; Attr:laUpper; CaseCode:#$0442),   // CYRILLIC CAPITAL LETTER TE
    (Unicode:#$0423; Attr:laUpper; CaseCode:#$0443),   // CYRILLIC CAPITAL LETTER U
    (Unicode:#$0424; Attr:laUpper; CaseCode:#$0444),   // CYRILLIC CAPITAL LETTER EF
    (Unicode:#$0425; Attr:laUpper; CaseCode:#$0445),   // CYRILLIC CAPITAL LETTER HA
    (Unicode:#$0426; Attr:laUpper; CaseCode:#$0446),   // CYRILLIC CAPITAL LETTER TSE
    (Unicode:#$0427; Attr:laUpper; CaseCode:#$0447),   // CYRILLIC CAPITAL LETTER CHE
    (Unicode:#$0428; Attr:laUpper; CaseCode:#$0448),   // CYRILLIC CAPITAL LETTER SHA
    (Unicode:#$0429; Attr:laUpper; CaseCode:#$0449),   // CYRILLIC CAPITAL LETTER SHCHA
    (Unicode:#$042A; Attr:laUpper; CaseCode:#$044A),   // CYRILLIC CAPITAL LETTER HARD SIGN
    (Unicode:#$042B; Attr:laUpper; CaseCode:#$044B),   // CYRILLIC CAPITAL LETTER YERU
    (Unicode:#$042C; Attr:laUpper; CaseCode:#$044C),   // CYRILLIC CAPITAL LETTER SOFT SIGN
    (Unicode:#$042D; Attr:laUpper; CaseCode:#$044D),   // CYRILLIC CAPITAL LETTER E
    (Unicode:#$042E; Attr:laUpper; CaseCode:#$044E),   // CYRILLIC CAPITAL LETTER YU
    (Unicode:#$042F; Attr:laUpper; CaseCode:#$044F),   // CYRILLIC CAPITAL LETTER YA
    (Unicode:#$0430; Attr:laLower; CaseCode:#$0410),   // CYRILLIC SMALL LETTER A
    (Unicode:#$0431; Attr:laLower; CaseCode:#$0411),   // CYRILLIC SMALL LETTER BE
    (Unicode:#$0432; Attr:laLower; CaseCode:#$0412),   // CYRILLIC SMALL LETTER VE
    (Unicode:#$0433; Attr:laLower; CaseCode:#$0413),   // CYRILLIC SMALL LETTER GHE
    (Unicode:#$0434; Attr:laLower; CaseCode:#$0414),   // CYRILLIC SMALL LETTER DE
    (Unicode:#$0435; Attr:laLower; CaseCode:#$0415),   // CYRILLIC SMALL LETTER IE
    (Unicode:#$0436; Attr:laLower; CaseCode:#$0416),   // CYRILLIC SMALL LETTER ZHE
    (Unicode:#$0437; Attr:laLower; CaseCode:#$0417),   // CYRILLIC SMALL LETTER ZE
    (Unicode:#$0438; Attr:laLower; CaseCode:#$0418),   // CYRILLIC SMALL LETTER I
    (Unicode:#$0439; Attr:laLower; CaseCode:#$0419),   // CYRILLIC SMALL LETTER SHORT I
    (Unicode:#$043A; Attr:laLower; CaseCode:#$041A),   // CYRILLIC SMALL LETTER KA
    (Unicode:#$043B; Attr:laLower; CaseCode:#$041B),   // CYRILLIC SMALL LETTER EL
    (Unicode:#$043C; Attr:laLower; CaseCode:#$041C),   // CYRILLIC SMALL LETTER EM
    (Unicode:#$043D; Attr:laLower; CaseCode:#$041D),   // CYRILLIC SMALL LETTER EN
    (Unicode:#$043E; Attr:laLower; CaseCode:#$041E),   // CYRILLIC SMALL LETTER O
    (Unicode:#$043F; Attr:laLower; CaseCode:#$041F),   // CYRILLIC SMALL LETTER PE
    (Unicode:#$0440; Attr:laLower; CaseCode:#$0420),   // CYRILLIC SMALL LETTER ER
    (Unicode:#$0441; Attr:laLower; CaseCode:#$0421),   // CYRILLIC SMALL LETTER ES
    (Unicode:#$0442; Attr:laLower; CaseCode:#$0422),   // CYRILLIC SMALL LETTER TE
    (Unicode:#$0443; Attr:laLower; CaseCode:#$0423),   // CYRILLIC SMALL LETTER U
    (Unicode:#$0444; Attr:laLower; CaseCode:#$0424),   // CYRILLIC SMALL LETTER EF
    (Unicode:#$0445; Attr:laLower; CaseCode:#$0425),   // CYRILLIC SMALL LETTER HA
    (Unicode:#$0446; Attr:laLower; CaseCode:#$0426),   // CYRILLIC SMALL LETTER TSE
    (Unicode:#$0447; Attr:laLower; CaseCode:#$0427),   // CYRILLIC SMALL LETTER CHE
    (Unicode:#$0448; Attr:laLower; CaseCode:#$0428),   // CYRILLIC SMALL LETTER SHA
    (Unicode:#$0449; Attr:laLower; CaseCode:#$0429),   // CYRILLIC SMALL LETTER SHCHA
    (Unicode:#$044A; Attr:laLower; CaseCode:#$042A),   // CYRILLIC SMALL LETTER HARD SIGN
    (Unicode:#$044B; Attr:laLower; CaseCode:#$042B),   // CYRILLIC SMALL LETTER YERU
    (Unicode:#$044C; Attr:laLower; CaseCode:#$042C),   // CYRILLIC SMALL LETTER SOFT SIGN
    (Unicode:#$044D; Attr:laLower; CaseCode:#$042D),   // CYRILLIC SMALL LETTER E
    (Unicode:#$044E; Attr:laLower; CaseCode:#$042E),   // CYRILLIC SMALL LETTER YU
    (Unicode:#$044F; Attr:laLower; CaseCode:#$042F),   // CYRILLIC SMALL LETTER YA
    (Unicode:#$0450; Attr:laLower; CaseCode:#$0400),   // CYRILLIC SMALL LETTER IE WITH GRAVE
    (Unicode:#$0451; Attr:laLower; CaseCode:#$0401),   // CYRILLIC SMALL LETTER IO
    (Unicode:#$0452; Attr:laLower; CaseCode:#$0402),   // CYRILLIC SMALL LETTER DJE
    (Unicode:#$0453; Attr:laLower; CaseCode:#$0403),   // CYRILLIC SMALL LETTER GJE
    (Unicode:#$0454; Attr:laLower; CaseCode:#$0404),   // CYRILLIC SMALL LETTER UKRAINIAN IE
    (Unicode:#$0455; Attr:laLower; CaseCode:#$0405),   // CYRILLIC SMALL LETTER DZE
    (Unicode:#$0456; Attr:laLower; CaseCode:#$0406),   // CYRILLIC SMALL LETTER BYELORUSSIAN-UKRAINIAN I
    (Unicode:#$0457; Attr:laLower; CaseCode:#$0407),   // CYRILLIC SMALL LETTER YI
    (Unicode:#$0458; Attr:laLower; CaseCode:#$0408),   // CYRILLIC SMALL LETTER JE
    (Unicode:#$0459; Attr:laLower; CaseCode:#$0409),   // CYRILLIC SMALL LETTER LJE
    (Unicode:#$045A; Attr:laLower; CaseCode:#$040A),   // CYRILLIC SMALL LETTER NJE
    (Unicode:#$045B; Attr:laLower; CaseCode:#$040B),   // CYRILLIC SMALL LETTER TSHE
    (Unicode:#$045C; Attr:laLower; CaseCode:#$040C),   // CYRILLIC SMALL LETTER KJE
    (Unicode:#$045D; Attr:laLower; CaseCode:#$040D),   // CYRILLIC SMALL LETTER I WITH GRAVE
    (Unicode:#$045E; Attr:laLower; CaseCode:#$040E),   // CYRILLIC SMALL LETTER SHORT U
    (Unicode:#$045F; Attr:laLower; CaseCode:#$040F),   // CYRILLIC SMALL LETTER DZHE
    (Unicode:#$0460; Attr:laUpper; CaseCode:#$0461),   // CYRILLIC CAPITAL LETTER OMEGA
    (Unicode:#$0461; Attr:laLower; CaseCode:#$0460),   // CYRILLIC SMALL LETTER OMEGA
    (Unicode:#$0462; Attr:laUpper; CaseCode:#$0463),   // CYRILLIC CAPITAL LETTER YAT
    (Unicode:#$0463; Attr:laLower; CaseCode:#$0462),   // CYRILLIC SMALL LETTER YAT
    (Unicode:#$0464; Attr:laUpper; CaseCode:#$0465),   // CYRILLIC CAPITAL LETTER IOTIFIED E
    (Unicode:#$0465; Attr:laLower; CaseCode:#$0464),   // CYRILLIC SMALL LETTER IOTIFIED E
    (Unicode:#$0466; Attr:laUpper; CaseCode:#$0467),   // CYRILLIC CAPITAL LETTER LITTLE YUS
    (Unicode:#$0467; Attr:laLower; CaseCode:#$0466),   // CYRILLIC SMALL LETTER LITTLE YUS
    (Unicode:#$0468; Attr:laUpper; CaseCode:#$0469),   // CYRILLIC CAPITAL LETTER IOTIFIED LITTLE YUS
    (Unicode:#$0469; Attr:laLower; CaseCode:#$0468),   // CYRILLIC SMALL LETTER IOTIFIED LITTLE YUS
    (Unicode:#$046A; Attr:laUpper; CaseCode:#$046B),   // CYRILLIC CAPITAL LETTER BIG YUS
    (Unicode:#$046B; Attr:laLower; CaseCode:#$046A),   // CYRILLIC SMALL LETTER BIG YUS
    (Unicode:#$046C; Attr:laUpper; CaseCode:#$046D),   // CYRILLIC CAPITAL LETTER IOTIFIED BIG YUS
    (Unicode:#$046D; Attr:laLower; CaseCode:#$046C),   // CYRILLIC SMALL LETTER IOTIFIED BIG YUS
    (Unicode:#$046E; Attr:laUpper; CaseCode:#$046F),   // CYRILLIC CAPITAL LETTER KSI
    (Unicode:#$046F; Attr:laLower; CaseCode:#$046E),   // CYRILLIC SMALL LETTER KSI
    (Unicode:#$0470; Attr:laUpper; CaseCode:#$0471),   // CYRILLIC CAPITAL LETTER PSI
    (Unicode:#$0471; Attr:laLower; CaseCode:#$0470),   // CYRILLIC SMALL LETTER PSI
    (Unicode:#$0472; Attr:laUpper; CaseCode:#$0473),   // CYRILLIC CAPITAL LETTER FITA
    (Unicode:#$0473; Attr:laLower; CaseCode:#$0472),   // CYRILLIC SMALL LETTER FITA
    (Unicode:#$0474; Attr:laUpper; CaseCode:#$0475),   // CYRILLIC CAPITAL LETTER IZHITSA
    (Unicode:#$0475; Attr:laLower; CaseCode:#$0474),   // CYRILLIC SMALL LETTER IZHITSA
    (Unicode:#$0476; Attr:laUpper; CaseCode:#$0477),   // CYRILLIC CAPITAL LETTER IZHITSA WITH DOUBLE GRAVE ACCENT
    (Unicode:#$0477; Attr:laLower; CaseCode:#$0476),   // CYRILLIC SMALL LETTER IZHITSA WITH DOUBLE GRAVE ACCENT
    (Unicode:#$0478; Attr:laUpper; CaseCode:#$0479),   // CYRILLIC CAPITAL LETTER UK
    (Unicode:#$0479; Attr:laLower; CaseCode:#$0478),   // CYRILLIC SMALL LETTER UK
    (Unicode:#$047A; Attr:laUpper; CaseCode:#$047B),   // CYRILLIC CAPITAL LETTER ROUND OMEGA
    (Unicode:#$047B; Attr:laLower; CaseCode:#$047A),   // CYRILLIC SMALL LETTER ROUND OMEGA
    (Unicode:#$047C; Attr:laUpper; CaseCode:#$047D),   // CYRILLIC CAPITAL LETTER OMEGA WITH TITLO
    (Unicode:#$047D; Attr:laLower; CaseCode:#$047C),   // CYRILLIC SMALL LETTER OMEGA WITH TITLO
    (Unicode:#$047E; Attr:laUpper; CaseCode:#$047F),   // CYRILLIC CAPITAL LETTER OT
    (Unicode:#$047F; Attr:laLower; CaseCode:#$047E),   // CYRILLIC SMALL LETTER OT
    (Unicode:#$0480; Attr:laUpper; CaseCode:#$0481),   // CYRILLIC CAPITAL LETTER KOPPA
    (Unicode:#$0481; Attr:laLower; CaseCode:#$0480),   // CYRILLIC SMALL LETTER KOPPA
    (Unicode:#$048C; Attr:laUpper; CaseCode:#$048D),   // CYRILLIC CAPITAL LETTER SEMISOFT SIGN
    (Unicode:#$048D; Attr:laLower; CaseCode:#$048C),   // CYRILLIC SMALL LETTER SEMISOFT SIGN
    (Unicode:#$048E; Attr:laUpper; CaseCode:#$048F),   // CYRILLIC CAPITAL LETTER ER WITH TICK
    (Unicode:#$048F; Attr:laLower; CaseCode:#$048E),   // CYRILLIC SMALL LETTER ER WITH TICK
    (Unicode:#$0490; Attr:laUpper; CaseCode:#$0491),   // CYRILLIC CAPITAL LETTER GHE WITH UPTURN
    (Unicode:#$0491; Attr:laLower; CaseCode:#$0490),   // CYRILLIC SMALL LETTER GHE WITH UPTURN
    (Unicode:#$0492; Attr:laUpper; CaseCode:#$0493),   // CYRILLIC CAPITAL LETTER GHE WITH STROKE
    (Unicode:#$0493; Attr:laLower; CaseCode:#$0492),   // CYRILLIC SMALL LETTER GHE WITH STROKE
    (Unicode:#$0494; Attr:laUpper; CaseCode:#$0495),   // CYRILLIC CAPITAL LETTER GHE WITH MIDDLE HOOK
    (Unicode:#$0495; Attr:laLower; CaseCode:#$0494),   // CYRILLIC SMALL LETTER GHE WITH MIDDLE HOOK
    (Unicode:#$0496; Attr:laUpper; CaseCode:#$0497),   // CYRILLIC CAPITAL LETTER ZHE WITH DESCENDER
    (Unicode:#$0497; Attr:laLower; CaseCode:#$0496),   // CYRILLIC SMALL LETTER ZHE WITH DESCENDER
    (Unicode:#$0498; Attr:laUpper; CaseCode:#$0499),   // CYRILLIC CAPITAL LETTER ZE WITH DESCENDER
    (Unicode:#$0499; Attr:laLower; CaseCode:#$0498),   // CYRILLIC SMALL LETTER ZE WITH DESCENDER
    (Unicode:#$049A; Attr:laUpper; CaseCode:#$049B),   // CYRILLIC CAPITAL LETTER KA WITH DESCENDER
    (Unicode:#$049B; Attr:laLower; CaseCode:#$049A),   // CYRILLIC SMALL LETTER KA WITH DESCENDER
    (Unicode:#$049C; Attr:laUpper; CaseCode:#$049D),   // CYRILLIC CAPITAL LETTER KA WITH VERTICAL STROKE
    (Unicode:#$049D; Attr:laLower; CaseCode:#$049C),   // CYRILLIC SMALL LETTER KA WITH VERTICAL STROKE
    (Unicode:#$049E; Attr:laUpper; CaseCode:#$049F),   // CYRILLIC CAPITAL LETTER KA WITH STROKE
    (Unicode:#$049F; Attr:laLower; CaseCode:#$049E),   // CYRILLIC SMALL LETTER KA WITH STROKE
    (Unicode:#$04A0; Attr:laUpper; CaseCode:#$04A1),   // CYRILLIC CAPITAL LETTER BASHKIR KA
    (Unicode:#$04A1; Attr:laLower; CaseCode:#$04A0),   // CYRILLIC SMALL LETTER BASHKIR KA
    (Unicode:#$04A2; Attr:laUpper; CaseCode:#$04A3),   // CYRILLIC CAPITAL LETTER EN WITH DESCENDER
    (Unicode:#$04A3; Attr:laLower; CaseCode:#$04A2),   // CYRILLIC SMALL LETTER EN WITH DESCENDER
    (Unicode:#$04A4; Attr:laUpper; CaseCode:#$04A5),   // CYRILLIC CAPITAL LIGATURE EN GHE
    (Unicode:#$04A5; Attr:laLower; CaseCode:#$04A4),   // CYRILLIC SMALL LIGATURE EN GHE
    (Unicode:#$04A6; Attr:laUpper; CaseCode:#$04A7),   // CYRILLIC CAPITAL LETTER PE WITH MIDDLE HOOK
    (Unicode:#$04A7; Attr:laLower; CaseCode:#$04A6),   // CYRILLIC SMALL LETTER PE WITH MIDDLE HOOK
    (Unicode:#$04A8; Attr:laUpper; CaseCode:#$04A9),   // CYRILLIC CAPITAL LETTER ABKHASIAN HA
    (Unicode:#$04A9; Attr:laLower; CaseCode:#$04A8),   // CYRILLIC SMALL LETTER ABKHASIAN HA
    (Unicode:#$04AA; Attr:laUpper; CaseCode:#$04AB),   // CYRILLIC CAPITAL LETTER ES WITH DESCENDER
    (Unicode:#$04AB; Attr:laLower; CaseCode:#$04AA),   // CYRILLIC SMALL LETTER ES WITH DESCENDER
    (Unicode:#$04AC; Attr:laUpper; CaseCode:#$04AD),   // CYRILLIC CAPITAL LETTER TE WITH DESCENDER
    (Unicode:#$04AD; Attr:laLower; CaseCode:#$04AC),   // CYRILLIC SMALL LETTER TE WITH DESCENDER
    (Unicode:#$04AE; Attr:laUpper; CaseCode:#$04AF),   // CYRILLIC CAPITAL LETTER STRAIGHT U
    (Unicode:#$04AF; Attr:laLower; CaseCode:#$04AE),   // CYRILLIC SMALL LETTER STRAIGHT U
    (Unicode:#$04B0; Attr:laUpper; CaseCode:#$04B1),   // CYRILLIC CAPITAL LETTER STRAIGHT U WITH STROKE
    (Unicode:#$04B1; Attr:laLower; CaseCode:#$04B0),   // CYRILLIC SMALL LETTER STRAIGHT U WITH STROKE
    (Unicode:#$04B2; Attr:laUpper; CaseCode:#$04B3),   // CYRILLIC CAPITAL LETTER HA WITH DESCENDER
    (Unicode:#$04B3; Attr:laLower; CaseCode:#$04B2),   // CYRILLIC SMALL LETTER HA WITH DESCENDER
    (Unicode:#$04B4; Attr:laUpper; CaseCode:#$04B5),   // CYRILLIC CAPITAL LIGATURE TE TSE
    (Unicode:#$04B5; Attr:laLower; CaseCode:#$04B4),   // CYRILLIC SMALL LIGATURE TE TSE
    (Unicode:#$04B6; Attr:laUpper; CaseCode:#$04B7),   // CYRILLIC CAPITAL LETTER CHE WITH DESCENDER
    (Unicode:#$04B7; Attr:laLower; CaseCode:#$04B6),   // CYRILLIC SMALL LETTER CHE WITH DESCENDER
    (Unicode:#$04B8; Attr:laUpper; CaseCode:#$04B9),   // CYRILLIC CAPITAL LETTER CHE WITH VERTICAL STROKE
    (Unicode:#$04B9; Attr:laLower; CaseCode:#$04B8),   // CYRILLIC SMALL LETTER CHE WITH VERTICAL STROKE
    (Unicode:#$04BA; Attr:laUpper; CaseCode:#$04BB),   // CYRILLIC CAPITAL LETTER SHHA
    (Unicode:#$04BB; Attr:laLower; CaseCode:#$04BA),   // CYRILLIC SMALL LETTER SHHA
    (Unicode:#$04BC; Attr:laUpper; CaseCode:#$04BD),   // CYRILLIC CAPITAL LETTER ABKHASIAN CHE
    (Unicode:#$04BD; Attr:laLower; CaseCode:#$04BC),   // CYRILLIC SMALL LETTER ABKHASIAN CHE
    (Unicode:#$04BE; Attr:laUpper; CaseCode:#$04BF),   // CYRILLIC CAPITAL LETTER ABKHASIAN CHE WITH DESCENDER
    (Unicode:#$04BF; Attr:laLower; CaseCode:#$04BE),   // CYRILLIC SMALL LETTER ABKHASIAN CHE WITH DESCENDER
    (Unicode:#$04C0; Attr:laUpper; CaseCode:#$FFFF),   // CYRILLIC LETTER PALOCHKA
    (Unicode:#$04C1; Attr:laUpper; CaseCode:#$04C2),   // CYRILLIC CAPITAL LETTER ZHE WITH BREVE
    (Unicode:#$04C2; Attr:laLower; CaseCode:#$04C1),   // CYRILLIC SMALL LETTER ZHE WITH BREVE
    (Unicode:#$04C3; Attr:laUpper; CaseCode:#$04C4),   // CYRILLIC CAPITAL LETTER KA WITH HOOK
    (Unicode:#$04C4; Attr:laLower; CaseCode:#$04C3),   // CYRILLIC SMALL LETTER KA WITH HOOK
    (Unicode:#$04C7; Attr:laUpper; CaseCode:#$04C8),   // CYRILLIC CAPITAL LETTER EN WITH HOOK
    (Unicode:#$04C8; Attr:laLower; CaseCode:#$04C7),   // CYRILLIC SMALL LETTER EN WITH HOOK
    (Unicode:#$04CB; Attr:laUpper; CaseCode:#$04CC),   // CYRILLIC CAPITAL LETTER KHAKASSIAN CHE
    (Unicode:#$04CC; Attr:laLower; CaseCode:#$04CB),   // CYRILLIC SMALL LETTER KHAKASSIAN CHE
    (Unicode:#$04D0; Attr:laUpper; CaseCode:#$04D1),   // CYRILLIC CAPITAL LETTER A WITH BREVE
    (Unicode:#$04D1; Attr:laLower; CaseCode:#$04D0),   // CYRILLIC SMALL LETTER A WITH BREVE
    (Unicode:#$04D2; Attr:laUpper; CaseCode:#$04D3),   // CYRILLIC CAPITAL LETTER A WITH DIAERESIS
    (Unicode:#$04D3; Attr:laLower; CaseCode:#$04D2),   // CYRILLIC SMALL LETTER A WITH DIAERESIS
    (Unicode:#$04D4; Attr:laUpper; CaseCode:#$04D5),   // CYRILLIC CAPITAL LIGATURE A IE
    (Unicode:#$04D5; Attr:laLower; CaseCode:#$04D4),   // CYRILLIC SMALL LIGATURE A IE
    (Unicode:#$04D6; Attr:laUpper; CaseCode:#$04D7),   // CYRILLIC CAPITAL LETTER IE WITH BREVE
    (Unicode:#$04D7; Attr:laLower; CaseCode:#$04D6),   // CYRILLIC SMALL LETTER IE WITH BREVE
    (Unicode:#$04D8; Attr:laUpper; CaseCode:#$04D9),   // CYRILLIC CAPITAL LETTER SCHWA
    (Unicode:#$04D9; Attr:laLower; CaseCode:#$04D8),   // CYRILLIC SMALL LETTER SCHWA
    (Unicode:#$04DA; Attr:laUpper; CaseCode:#$04DB),   // CYRILLIC CAPITAL LETTER SCHWA WITH DIAERESIS
    (Unicode:#$04DB; Attr:laLower; CaseCode:#$04DA),   // CYRILLIC SMALL LETTER SCHWA WITH DIAERESIS
    (Unicode:#$04DC; Attr:laUpper; CaseCode:#$04DD),   // CYRILLIC CAPITAL LETTER ZHE WITH DIAERESIS
    (Unicode:#$04DD; Attr:laLower; CaseCode:#$04DC),   // CYRILLIC SMALL LETTER ZHE WITH DIAERESIS
    (Unicode:#$04DE; Attr:laUpper; CaseCode:#$04DF),   // CYRILLIC CAPITAL LETTER ZE WITH DIAERESIS
    (Unicode:#$04DF; Attr:laLower; CaseCode:#$04DE),   // CYRILLIC SMALL LETTER ZE WITH DIAERESIS
    (Unicode:#$04E0; Attr:laUpper; CaseCode:#$04E1),   // CYRILLIC CAPITAL LETTER ABKHASIAN DZE
    (Unicode:#$04E1; Attr:laLower; CaseCode:#$04E0),   // CYRILLIC SMALL LETTER ABKHASIAN DZE
    (Unicode:#$04E2; Attr:laUpper; CaseCode:#$04E3),   // CYRILLIC CAPITAL LETTER I WITH MACRON
    (Unicode:#$04E3; Attr:laLower; CaseCode:#$04E2),   // CYRILLIC SMALL LETTER I WITH MACRON
    (Unicode:#$04E4; Attr:laUpper; CaseCode:#$04E5),   // CYRILLIC CAPITAL LETTER I WITH DIAERESIS
    (Unicode:#$04E5; Attr:laLower; CaseCode:#$04E4),   // CYRILLIC SMALL LETTER I WITH DIAERESIS
    (Unicode:#$04E6; Attr:laUpper; CaseCode:#$04E7),   // CYRILLIC CAPITAL LETTER O WITH DIAERESIS
    (Unicode:#$04E7; Attr:laLower; CaseCode:#$04E6),   // CYRILLIC SMALL LETTER O WITH DIAERESIS
    (Unicode:#$04E8; Attr:laUpper; CaseCode:#$04E9),   // CYRILLIC CAPITAL LETTER BARRED O
    (Unicode:#$04E9; Attr:laLower; CaseCode:#$04E8),   // CYRILLIC SMALL LETTER BARRED O
    (Unicode:#$04EA; Attr:laUpper; CaseCode:#$04EB),   // CYRILLIC CAPITAL LETTER BARRED O WITH DIAERESIS
    (Unicode:#$04EB; Attr:laLower; CaseCode:#$04EA),   // CYRILLIC SMALL LETTER BARRED O WITH DIAERESIS
    (Unicode:#$04EC; Attr:laUpper; CaseCode:#$04ED),   // CYRILLIC CAPITAL LETTER E WITH DIAERESIS
    (Unicode:#$04ED; Attr:laLower; CaseCode:#$04EC),   // CYRILLIC SMALL LETTER E WITH DIAERESIS
    (Unicode:#$04EE; Attr:laUpper; CaseCode:#$04EF),   // CYRILLIC CAPITAL LETTER U WITH MACRON
    (Unicode:#$04EF; Attr:laLower; CaseCode:#$04EE),   // CYRILLIC SMALL LETTER U WITH MACRON
    (Unicode:#$04F0; Attr:laUpper; CaseCode:#$04F1),   // CYRILLIC CAPITAL LETTER U WITH DIAERESIS
    (Unicode:#$04F1; Attr:laLower; CaseCode:#$04F0),   // CYRILLIC SMALL LETTER U WITH DIAERESIS
    (Unicode:#$04F2; Attr:laUpper; CaseCode:#$04F3),   // CYRILLIC CAPITAL LETTER U WITH DOUBLE ACUTE
    (Unicode:#$04F3; Attr:laLower; CaseCode:#$04F2),   // CYRILLIC SMALL LETTER U WITH DOUBLE ACUTE
    (Unicode:#$04F4; Attr:laUpper; CaseCode:#$04F5),   // CYRILLIC CAPITAL LETTER CHE WITH DIAERESIS
    (Unicode:#$04F5; Attr:laLower; CaseCode:#$04F4),   // CYRILLIC SMALL LETTER CHE WITH DIAERESIS
    (Unicode:#$04F8; Attr:laUpper; CaseCode:#$04F9),   // CYRILLIC CAPITAL LETTER YERU WITH DIAERESIS
    (Unicode:#$04F9; Attr:laLower; CaseCode:#$04F8),   // CYRILLIC SMALL LETTER YERU WITH DIAERESIS
    (Unicode:#$0531; Attr:laUpper; CaseCode:#$0561),   // ARMENIAN CAPITAL LETTER AYB
    (Unicode:#$0532; Attr:laUpper; CaseCode:#$0562),   // ARMENIAN CAPITAL LETTER BEN
    (Unicode:#$0533; Attr:laUpper; CaseCode:#$0563),   // ARMENIAN CAPITAL LETTER GIM
    (Unicode:#$0534; Attr:laUpper; CaseCode:#$0564),   // ARMENIAN CAPITAL LETTER DA
    (Unicode:#$0535; Attr:laUpper; CaseCode:#$0565),   // ARMENIAN CAPITAL LETTER ECH
    (Unicode:#$0536; Attr:laUpper; CaseCode:#$0566),   // ARMENIAN CAPITAL LETTER ZA
    (Unicode:#$0537; Attr:laUpper; CaseCode:#$0567),   // ARMENIAN CAPITAL LETTER EH
    (Unicode:#$0538; Attr:laUpper; CaseCode:#$0568),   // ARMENIAN CAPITAL LETTER ET
    (Unicode:#$0539; Attr:laUpper; CaseCode:#$0569),   // ARMENIAN CAPITAL LETTER TO
    (Unicode:#$053A; Attr:laUpper; CaseCode:#$056A),   // ARMENIAN CAPITAL LETTER ZHE
    (Unicode:#$053B; Attr:laUpper; CaseCode:#$056B),   // ARMENIAN CAPITAL LETTER INI
    (Unicode:#$053C; Attr:laUpper; CaseCode:#$056C),   // ARMENIAN CAPITAL LETTER LIWN
    (Unicode:#$053D; Attr:laUpper; CaseCode:#$056D),   // ARMENIAN CAPITAL LETTER XEH
    (Unicode:#$053E; Attr:laUpper; CaseCode:#$056E),   // ARMENIAN CAPITAL LETTER CA
    (Unicode:#$053F; Attr:laUpper; CaseCode:#$056F),   // ARMENIAN CAPITAL LETTER KEN
    (Unicode:#$0540; Attr:laUpper; CaseCode:#$0570),   // ARMENIAN CAPITAL LETTER HO
    (Unicode:#$0541; Attr:laUpper; CaseCode:#$0571),   // ARMENIAN CAPITAL LETTER JA
    (Unicode:#$0542; Attr:laUpper; CaseCode:#$0572),   // ARMENIAN CAPITAL LETTER GHAD
    (Unicode:#$0543; Attr:laUpper; CaseCode:#$0573),   // ARMENIAN CAPITAL LETTER CHEH
    (Unicode:#$0544; Attr:laUpper; CaseCode:#$0574),   // ARMENIAN CAPITAL LETTER MEN
    (Unicode:#$0545; Attr:laUpper; CaseCode:#$0575),   // ARMENIAN CAPITAL LETTER YI
    (Unicode:#$0546; Attr:laUpper; CaseCode:#$0576),   // ARMENIAN CAPITAL LETTER NOW
    (Unicode:#$0547; Attr:laUpper; CaseCode:#$0577),   // ARMENIAN CAPITAL LETTER SHA
    (Unicode:#$0548; Attr:laUpper; CaseCode:#$0578),   // ARMENIAN CAPITAL LETTER VO
    (Unicode:#$0549; Attr:laUpper; CaseCode:#$0579),   // ARMENIAN CAPITAL LETTER CHA
    (Unicode:#$054A; Attr:laUpper; CaseCode:#$057A),   // ARMENIAN CAPITAL LETTER PEH
    (Unicode:#$054B; Attr:laUpper; CaseCode:#$057B),   // ARMENIAN CAPITAL LETTER JHEH
    (Unicode:#$054C; Attr:laUpper; CaseCode:#$057C),   // ARMENIAN CAPITAL LETTER RA
    (Unicode:#$054D; Attr:laUpper; CaseCode:#$057D),   // ARMENIAN CAPITAL LETTER SEH
    (Unicode:#$054E; Attr:laUpper; CaseCode:#$057E),   // ARMENIAN CAPITAL LETTER VEW
    (Unicode:#$054F; Attr:laUpper; CaseCode:#$057F),   // ARMENIAN CAPITAL LETTER TIWN
    (Unicode:#$0550; Attr:laUpper; CaseCode:#$0580),   // ARMENIAN CAPITAL LETTER REH
    (Unicode:#$0551; Attr:laUpper; CaseCode:#$0581),   // ARMENIAN CAPITAL LETTER CO
    (Unicode:#$0552; Attr:laUpper; CaseCode:#$0582),   // ARMENIAN CAPITAL LETTER YIWN
    (Unicode:#$0553; Attr:laUpper; CaseCode:#$0583),   // ARMENIAN CAPITAL LETTER PIWR
    (Unicode:#$0554; Attr:laUpper; CaseCode:#$0584),   // ARMENIAN CAPITAL LETTER KEH
    (Unicode:#$0555; Attr:laUpper; CaseCode:#$0585),   // ARMENIAN CAPITAL LETTER OH
    (Unicode:#$0556; Attr:laUpper; CaseCode:#$0586),   // ARMENIAN CAPITAL LETTER FEH
    (Unicode:#$0561; Attr:laLower; CaseCode:#$0531),   // ARMENIAN SMALL LETTER AYB
    (Unicode:#$0562; Attr:laLower; CaseCode:#$0532),   // ARMENIAN SMALL LETTER BEN
    (Unicode:#$0563; Attr:laLower; CaseCode:#$0533),   // ARMENIAN SMALL LETTER GIM
    (Unicode:#$0564; Attr:laLower; CaseCode:#$0534),   // ARMENIAN SMALL LETTER DA
    (Unicode:#$0565; Attr:laLower; CaseCode:#$0535),   // ARMENIAN SMALL LETTER ECH
    (Unicode:#$0566; Attr:laLower; CaseCode:#$0536),   // ARMENIAN SMALL LETTER ZA
    (Unicode:#$0567; Attr:laLower; CaseCode:#$0537),   // ARMENIAN SMALL LETTER EH
    (Unicode:#$0568; Attr:laLower; CaseCode:#$0538),   // ARMENIAN SMALL LETTER ET
    (Unicode:#$0569; Attr:laLower; CaseCode:#$0539),   // ARMENIAN SMALL LETTER TO
    (Unicode:#$056A; Attr:laLower; CaseCode:#$053A),   // ARMENIAN SMALL LETTER ZHE
    (Unicode:#$056B; Attr:laLower; CaseCode:#$053B),   // ARMENIAN SMALL LETTER INI
    (Unicode:#$056C; Attr:laLower; CaseCode:#$053C),   // ARMENIAN SMALL LETTER LIWN
    (Unicode:#$056D; Attr:laLower; CaseCode:#$053D),   // ARMENIAN SMALL LETTER XEH
    (Unicode:#$056E; Attr:laLower; CaseCode:#$053E),   // ARMENIAN SMALL LETTER CA
    (Unicode:#$056F; Attr:laLower; CaseCode:#$053F),   // ARMENIAN SMALL LETTER KEN
    (Unicode:#$0570; Attr:laLower; CaseCode:#$0540),   // ARMENIAN SMALL LETTER HO
    (Unicode:#$0571; Attr:laLower; CaseCode:#$0541),   // ARMENIAN SMALL LETTER JA
    (Unicode:#$0572; Attr:laLower; CaseCode:#$0542),   // ARMENIAN SMALL LETTER GHAD
    (Unicode:#$0573; Attr:laLower; CaseCode:#$0543),   // ARMENIAN SMALL LETTER CHEH
    (Unicode:#$0574; Attr:laLower; CaseCode:#$0544),   // ARMENIAN SMALL LETTER MEN
    (Unicode:#$0575; Attr:laLower; CaseCode:#$0545),   // ARMENIAN SMALL LETTER YI
    (Unicode:#$0576; Attr:laLower; CaseCode:#$0546),   // ARMENIAN SMALL LETTER NOW
    (Unicode:#$0577; Attr:laLower; CaseCode:#$0547),   // ARMENIAN SMALL LETTER SHA
    (Unicode:#$0578; Attr:laLower; CaseCode:#$0548),   // ARMENIAN SMALL LETTER VO
    (Unicode:#$0579; Attr:laLower; CaseCode:#$0549),   // ARMENIAN SMALL LETTER CHA
    (Unicode:#$057A; Attr:laLower; CaseCode:#$054A),   // ARMENIAN SMALL LETTER PEH
    (Unicode:#$057B; Attr:laLower; CaseCode:#$054B),   // ARMENIAN SMALL LETTER JHEH
    (Unicode:#$057C; Attr:laLower; CaseCode:#$054C),   // ARMENIAN SMALL LETTER RA
    (Unicode:#$057D; Attr:laLower; CaseCode:#$054D),   // ARMENIAN SMALL LETTER SEH
    (Unicode:#$057E; Attr:laLower; CaseCode:#$054E),   // ARMENIAN SMALL LETTER VEW
    (Unicode:#$057F; Attr:laLower; CaseCode:#$054F),   // ARMENIAN SMALL LETTER TIWN
    (Unicode:#$0580; Attr:laLower; CaseCode:#$0550),   // ARMENIAN SMALL LETTER REH
    (Unicode:#$0581; Attr:laLower; CaseCode:#$0551),   // ARMENIAN SMALL LETTER CO
    (Unicode:#$0582; Attr:laLower; CaseCode:#$0552),   // ARMENIAN SMALL LETTER YIWN
    (Unicode:#$0583; Attr:laLower; CaseCode:#$0553),   // ARMENIAN SMALL LETTER PIWR
    (Unicode:#$0584; Attr:laLower; CaseCode:#$0554),   // ARMENIAN SMALL LETTER KEH
    (Unicode:#$0585; Attr:laLower; CaseCode:#$0555),   // ARMENIAN SMALL LETTER OH
    (Unicode:#$0586; Attr:laLower; CaseCode:#$0556),   // ARMENIAN SMALL LETTER FEH
    (Unicode:#$0587; Attr:laLower; CaseCode:#$FFFF),   // ARMENIAN SMALL LIGATURE ECH YIWN
    (Unicode:#$10A0; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER AN
    (Unicode:#$10A1; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER BAN
    (Unicode:#$10A2; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER GAN
    (Unicode:#$10A3; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER DON
    (Unicode:#$10A4; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER EN
    (Unicode:#$10A5; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER VIN
    (Unicode:#$10A6; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER ZEN
    (Unicode:#$10A7; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER TAN
    (Unicode:#$10A8; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER IN
    (Unicode:#$10A9; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER KAN
    (Unicode:#$10AA; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER LAS
    (Unicode:#$10AB; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER MAN
    (Unicode:#$10AC; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER NAR
    (Unicode:#$10AD; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER ON
    (Unicode:#$10AE; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER PAR
    (Unicode:#$10AF; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER ZHAR
    (Unicode:#$10B0; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER RAE
    (Unicode:#$10B1; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER SAN
    (Unicode:#$10B2; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER TAR
    (Unicode:#$10B3; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER UN
    (Unicode:#$10B4; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER PHAR
    (Unicode:#$10B5; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER KHAR
    (Unicode:#$10B6; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER GHAN
    (Unicode:#$10B7; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER QAR
    (Unicode:#$10B8; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER SHIN
    (Unicode:#$10B9; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER CHIN
    (Unicode:#$10BA; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER CAN
    (Unicode:#$10BB; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER JIL
    (Unicode:#$10BC; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER CIL
    (Unicode:#$10BD; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER CHAR
    (Unicode:#$10BE; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER XAN
    (Unicode:#$10BF; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER JHAN
    (Unicode:#$10C0; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER HAE
    (Unicode:#$10C1; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER HE
    (Unicode:#$10C2; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER HIE
    (Unicode:#$10C3; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER WE
    (Unicode:#$10C4; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER HAR
    (Unicode:#$10C5; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER HOE
    (Unicode:#$1E00; Attr:laUpper; CaseCode:#$1E01),   // LATIN CAPITAL LETTER A WITH RING BELOW
    (Unicode:#$1E01; Attr:laLower; CaseCode:#$1E00),   // LATIN SMALL LETTER A WITH RING BELOW
    (Unicode:#$1E02; Attr:laUpper; CaseCode:#$1E03),   // LATIN CAPITAL LETTER B WITH DOT ABOVE
    (Unicode:#$1E03; Attr:laLower; CaseCode:#$1E02),   // LATIN SMALL LETTER B WITH DOT ABOVE
    (Unicode:#$1E04; Attr:laUpper; CaseCode:#$1E05),   // LATIN CAPITAL LETTER B WITH DOT BELOW
    (Unicode:#$1E05; Attr:laLower; CaseCode:#$1E04),   // LATIN SMALL LETTER B WITH DOT BELOW
    (Unicode:#$1E06; Attr:laUpper; CaseCode:#$1E07),   // LATIN CAPITAL LETTER B WITH LINE BELOW
    (Unicode:#$1E07; Attr:laLower; CaseCode:#$1E06),   // LATIN SMALL LETTER B WITH LINE BELOW
    (Unicode:#$1E08; Attr:laUpper; CaseCode:#$1E09),   // LATIN CAPITAL LETTER C WITH CEDILLA AND ACUTE
    (Unicode:#$1E09; Attr:laLower; CaseCode:#$1E08),   // LATIN SMALL LETTER C WITH CEDILLA AND ACUTE
    (Unicode:#$1E0A; Attr:laUpper; CaseCode:#$1E0B),   // LATIN CAPITAL LETTER D WITH DOT ABOVE
    (Unicode:#$1E0B; Attr:laLower; CaseCode:#$1E0A),   // LATIN SMALL LETTER D WITH DOT ABOVE
    (Unicode:#$1E0C; Attr:laUpper; CaseCode:#$1E0D),   // LATIN CAPITAL LETTER D WITH DOT BELOW
    (Unicode:#$1E0D; Attr:laLower; CaseCode:#$1E0C),   // LATIN SMALL LETTER D WITH DOT BELOW
    (Unicode:#$1E0E; Attr:laUpper; CaseCode:#$1E0F),   // LATIN CAPITAL LETTER D WITH LINE BELOW
    (Unicode:#$1E0F; Attr:laLower; CaseCode:#$1E0E),   // LATIN SMALL LETTER D WITH LINE BELOW
    (Unicode:#$1E10; Attr:laUpper; CaseCode:#$1E11),   // LATIN CAPITAL LETTER D WITH CEDILLA
    (Unicode:#$1E11; Attr:laLower; CaseCode:#$1E10),   // LATIN SMALL LETTER D WITH CEDILLA
    (Unicode:#$1E12; Attr:laUpper; CaseCode:#$1E13),   // LATIN CAPITAL LETTER D WITH CIRCUMFLEX BELOW
    (Unicode:#$1E13; Attr:laLower; CaseCode:#$1E12),   // LATIN SMALL LETTER D WITH CIRCUMFLEX BELOW
    (Unicode:#$1E14; Attr:laUpper; CaseCode:#$1E15),   // LATIN CAPITAL LETTER E WITH MACRON AND GRAVE
    (Unicode:#$1E15; Attr:laLower; CaseCode:#$1E14),   // LATIN SMALL LETTER E WITH MACRON AND GRAVE
    (Unicode:#$1E16; Attr:laUpper; CaseCode:#$1E17),   // LATIN CAPITAL LETTER E WITH MACRON AND ACUTE
    (Unicode:#$1E17; Attr:laLower; CaseCode:#$1E16),   // LATIN SMALL LETTER E WITH MACRON AND ACUTE
    (Unicode:#$1E18; Attr:laUpper; CaseCode:#$1E19),   // LATIN CAPITAL LETTER E WITH CIRCUMFLEX BELOW
    (Unicode:#$1E19; Attr:laLower; CaseCode:#$1E18),   // LATIN SMALL LETTER E WITH CIRCUMFLEX BELOW
    (Unicode:#$1E1A; Attr:laUpper; CaseCode:#$1E1B),   // LATIN CAPITAL LETTER E WITH TILDE BELOW
    (Unicode:#$1E1B; Attr:laLower; CaseCode:#$1E1A),   // LATIN SMALL LETTER E WITH TILDE BELOW
    (Unicode:#$1E1C; Attr:laUpper; CaseCode:#$1E1D),   // LATIN CAPITAL LETTER E WITH CEDILLA AND BREVE
    (Unicode:#$1E1D; Attr:laLower; CaseCode:#$1E1C),   // LATIN SMALL LETTER E WITH CEDILLA AND BREVE
    (Unicode:#$1E1E; Attr:laUpper; CaseCode:#$1E1F),   // LATIN CAPITAL LETTER F WITH DOT ABOVE
    (Unicode:#$1E1F; Attr:laLower; CaseCode:#$1E1E),   // LATIN SMALL LETTER F WITH DOT ABOVE
    (Unicode:#$1E20; Attr:laUpper; CaseCode:#$1E21),   // LATIN CAPITAL LETTER G WITH MACRON
    (Unicode:#$1E21; Attr:laLower; CaseCode:#$1E20),   // LATIN SMALL LETTER G WITH MACRON
    (Unicode:#$1E22; Attr:laUpper; CaseCode:#$1E23),   // LATIN CAPITAL LETTER H WITH DOT ABOVE
    (Unicode:#$1E23; Attr:laLower; CaseCode:#$1E22),   // LATIN SMALL LETTER H WITH DOT ABOVE
    (Unicode:#$1E24; Attr:laUpper; CaseCode:#$1E25),   // LATIN CAPITAL LETTER H WITH DOT BELOW
    (Unicode:#$1E25; Attr:laLower; CaseCode:#$1E24),   // LATIN SMALL LETTER H WITH DOT BELOW
    (Unicode:#$1E26; Attr:laUpper; CaseCode:#$1E27),   // LATIN CAPITAL LETTER H WITH DIAERESIS
    (Unicode:#$1E27; Attr:laLower; CaseCode:#$1E26),   // LATIN SMALL LETTER H WITH DIAERESIS
    (Unicode:#$1E28; Attr:laUpper; CaseCode:#$1E29),   // LATIN CAPITAL LETTER H WITH CEDILLA
    (Unicode:#$1E29; Attr:laLower; CaseCode:#$1E28),   // LATIN SMALL LETTER H WITH CEDILLA
    (Unicode:#$1E2A; Attr:laUpper; CaseCode:#$1E2B),   // LATIN CAPITAL LETTER H WITH BREVE BELOW
    (Unicode:#$1E2B; Attr:laLower; CaseCode:#$1E2A),   // LATIN SMALL LETTER H WITH BREVE BELOW
    (Unicode:#$1E2C; Attr:laUpper; CaseCode:#$1E2D),   // LATIN CAPITAL LETTER I WITH TILDE BELOW
    (Unicode:#$1E2D; Attr:laLower; CaseCode:#$1E2C),   // LATIN SMALL LETTER I WITH TILDE BELOW
    (Unicode:#$1E2E; Attr:laUpper; CaseCode:#$1E2F),   // LATIN CAPITAL LETTER I WITH DIAERESIS AND ACUTE
    (Unicode:#$1E2F; Attr:laLower; CaseCode:#$1E2E),   // LATIN SMALL LETTER I WITH DIAERESIS AND ACUTE
    (Unicode:#$1E30; Attr:laUpper; CaseCode:#$1E31),   // LATIN CAPITAL LETTER K WITH ACUTE
    (Unicode:#$1E31; Attr:laLower; CaseCode:#$1E30),   // LATIN SMALL LETTER K WITH ACUTE
    (Unicode:#$1E32; Attr:laUpper; CaseCode:#$1E33),   // LATIN CAPITAL LETTER K WITH DOT BELOW
    (Unicode:#$1E33; Attr:laLower; CaseCode:#$1E32),   // LATIN SMALL LETTER K WITH DOT BELOW
    (Unicode:#$1E34; Attr:laUpper; CaseCode:#$1E35),   // LATIN CAPITAL LETTER K WITH LINE BELOW
    (Unicode:#$1E35; Attr:laLower; CaseCode:#$1E34),   // LATIN SMALL LETTER K WITH LINE BELOW
    (Unicode:#$1E36; Attr:laUpper; CaseCode:#$1E37),   // LATIN CAPITAL LETTER L WITH DOT BELOW
    (Unicode:#$1E37; Attr:laLower; CaseCode:#$1E36),   // LATIN SMALL LETTER L WITH DOT BELOW
    (Unicode:#$1E38; Attr:laUpper; CaseCode:#$1E39),   // LATIN CAPITAL LETTER L WITH DOT BELOW AND MACRON
    (Unicode:#$1E39; Attr:laLower; CaseCode:#$1E38),   // LATIN SMALL LETTER L WITH DOT BELOW AND MACRON
    (Unicode:#$1E3A; Attr:laUpper; CaseCode:#$1E3B),   // LATIN CAPITAL LETTER L WITH LINE BELOW
    (Unicode:#$1E3B; Attr:laLower; CaseCode:#$1E3A),   // LATIN SMALL LETTER L WITH LINE BELOW
    (Unicode:#$1E3C; Attr:laUpper; CaseCode:#$1E3D),   // LATIN CAPITAL LETTER L WITH CIRCUMFLEX BELOW
    (Unicode:#$1E3D; Attr:laLower; CaseCode:#$1E3C),   // LATIN SMALL LETTER L WITH CIRCUMFLEX BELOW
    (Unicode:#$1E3E; Attr:laUpper; CaseCode:#$1E3F),   // LATIN CAPITAL LETTER M WITH ACUTE
    (Unicode:#$1E3F; Attr:laLower; CaseCode:#$1E3E),   // LATIN SMALL LETTER M WITH ACUTE
    (Unicode:#$1E40; Attr:laUpper; CaseCode:#$1E41),   // LATIN CAPITAL LETTER M WITH DOT ABOVE
    (Unicode:#$1E41; Attr:laLower; CaseCode:#$1E40),   // LATIN SMALL LETTER M WITH DOT ABOVE
    (Unicode:#$1E42; Attr:laUpper; CaseCode:#$1E43),   // LATIN CAPITAL LETTER M WITH DOT BELOW
    (Unicode:#$1E43; Attr:laLower; CaseCode:#$1E42),   // LATIN SMALL LETTER M WITH DOT BELOW
    (Unicode:#$1E44; Attr:laUpper; CaseCode:#$1E45),   // LATIN CAPITAL LETTER N WITH DOT ABOVE
    (Unicode:#$1E45; Attr:laLower; CaseCode:#$1E44),   // LATIN SMALL LETTER N WITH DOT ABOVE
    (Unicode:#$1E46; Attr:laUpper; CaseCode:#$1E47),   // LATIN CAPITAL LETTER N WITH DOT BELOW
    (Unicode:#$1E47; Attr:laLower; CaseCode:#$1E46),   // LATIN SMALL LETTER N WITH DOT BELOW
    (Unicode:#$1E48; Attr:laUpper; CaseCode:#$1E49),   // LATIN CAPITAL LETTER N WITH LINE BELOW
    (Unicode:#$1E49; Attr:laLower; CaseCode:#$1E48),   // LATIN SMALL LETTER N WITH LINE BELOW
    (Unicode:#$1E4A; Attr:laUpper; CaseCode:#$1E4B),   // LATIN CAPITAL LETTER N WITH CIRCUMFLEX BELOW
    (Unicode:#$1E4B; Attr:laLower; CaseCode:#$1E4A),   // LATIN SMALL LETTER N WITH CIRCUMFLEX BELOW
    (Unicode:#$1E4C; Attr:laUpper; CaseCode:#$1E4D),   // LATIN CAPITAL LETTER O WITH TILDE AND ACUTE
    (Unicode:#$1E4D; Attr:laLower; CaseCode:#$1E4C),   // LATIN SMALL LETTER O WITH TILDE AND ACUTE
    (Unicode:#$1E4E; Attr:laUpper; CaseCode:#$1E4F),   // LATIN CAPITAL LETTER O WITH TILDE AND DIAERESIS
    (Unicode:#$1E4F; Attr:laLower; CaseCode:#$1E4E),   // LATIN SMALL LETTER O WITH TILDE AND DIAERESIS
    (Unicode:#$1E50; Attr:laUpper; CaseCode:#$1E51),   // LATIN CAPITAL LETTER O WITH MACRON AND GRAVE
    (Unicode:#$1E51; Attr:laLower; CaseCode:#$1E50),   // LATIN SMALL LETTER O WITH MACRON AND GRAVE
    (Unicode:#$1E52; Attr:laUpper; CaseCode:#$1E53),   // LATIN CAPITAL LETTER O WITH MACRON AND ACUTE
    (Unicode:#$1E53; Attr:laLower; CaseCode:#$1E52),   // LATIN SMALL LETTER O WITH MACRON AND ACUTE
    (Unicode:#$1E54; Attr:laUpper; CaseCode:#$1E55),   // LATIN CAPITAL LETTER P WITH ACUTE
    (Unicode:#$1E55; Attr:laLower; CaseCode:#$1E54),   // LATIN SMALL LETTER P WITH ACUTE
    (Unicode:#$1E56; Attr:laUpper; CaseCode:#$1E57),   // LATIN CAPITAL LETTER P WITH DOT ABOVE
    (Unicode:#$1E57; Attr:laLower; CaseCode:#$1E56),   // LATIN SMALL LETTER P WITH DOT ABOVE
    (Unicode:#$1E58; Attr:laUpper; CaseCode:#$1E59),   // LATIN CAPITAL LETTER R WITH DOT ABOVE
    (Unicode:#$1E59; Attr:laLower; CaseCode:#$1E58),   // LATIN SMALL LETTER R WITH DOT ABOVE
    (Unicode:#$1E5A; Attr:laUpper; CaseCode:#$1E5B),   // LATIN CAPITAL LETTER R WITH DOT BELOW
    (Unicode:#$1E5B; Attr:laLower; CaseCode:#$1E5A),   // LATIN SMALL LETTER R WITH DOT BELOW
    (Unicode:#$1E5C; Attr:laUpper; CaseCode:#$1E5D),   // LATIN CAPITAL LETTER R WITH DOT BELOW AND MACRON
    (Unicode:#$1E5D; Attr:laLower; CaseCode:#$1E5C),   // LATIN SMALL LETTER R WITH DOT BELOW AND MACRON
    (Unicode:#$1E5E; Attr:laUpper; CaseCode:#$1E5F),   // LATIN CAPITAL LETTER R WITH LINE BELOW
    (Unicode:#$1E5F; Attr:laLower; CaseCode:#$1E5E),   // LATIN SMALL LETTER R WITH LINE BELOW
    (Unicode:#$1E60; Attr:laUpper; CaseCode:#$1E61),   // LATIN CAPITAL LETTER S WITH DOT ABOVE
    (Unicode:#$1E61; Attr:laLower; CaseCode:#$1E60),   // LATIN SMALL LETTER S WITH DOT ABOVE
    (Unicode:#$1E62; Attr:laUpper; CaseCode:#$1E63),   // LATIN CAPITAL LETTER S WITH DOT BELOW
    (Unicode:#$1E63; Attr:laLower; CaseCode:#$1E62),   // LATIN SMALL LETTER S WITH DOT BELOW
    (Unicode:#$1E64; Attr:laUpper; CaseCode:#$1E65),   // LATIN CAPITAL LETTER S WITH ACUTE AND DOT ABOVE
    (Unicode:#$1E65; Attr:laLower; CaseCode:#$1E64),   // LATIN SMALL LETTER S WITH ACUTE AND DOT ABOVE
    (Unicode:#$1E66; Attr:laUpper; CaseCode:#$1E67),   // LATIN CAPITAL LETTER S WITH CARON AND DOT ABOVE
    (Unicode:#$1E67; Attr:laLower; CaseCode:#$1E66),   // LATIN SMALL LETTER S WITH CARON AND DOT ABOVE
    (Unicode:#$1E68; Attr:laUpper; CaseCode:#$1E69),   // LATIN CAPITAL LETTER S WITH DOT BELOW AND DOT ABOVE
    (Unicode:#$1E69; Attr:laLower; CaseCode:#$1E68),   // LATIN SMALL LETTER S WITH DOT BELOW AND DOT ABOVE
    (Unicode:#$1E6A; Attr:laUpper; CaseCode:#$1E6B),   // LATIN CAPITAL LETTER T WITH DOT ABOVE
    (Unicode:#$1E6B; Attr:laLower; CaseCode:#$1E6A),   // LATIN SMALL LETTER T WITH DOT ABOVE
    (Unicode:#$1E6C; Attr:laUpper; CaseCode:#$1E6D),   // LATIN CAPITAL LETTER T WITH DOT BELOW
    (Unicode:#$1E6D; Attr:laLower; CaseCode:#$1E6C),   // LATIN SMALL LETTER T WITH DOT BELOW
    (Unicode:#$1E6E; Attr:laUpper; CaseCode:#$1E6F),   // LATIN CAPITAL LETTER T WITH LINE BELOW
    (Unicode:#$1E6F; Attr:laLower; CaseCode:#$1E6E),   // LATIN SMALL LETTER T WITH LINE BELOW
    (Unicode:#$1E70; Attr:laUpper; CaseCode:#$1E71),   // LATIN CAPITAL LETTER T WITH CIRCUMFLEX BELOW
    (Unicode:#$1E71; Attr:laLower; CaseCode:#$1E70),   // LATIN SMALL LETTER T WITH CIRCUMFLEX BELOW
    (Unicode:#$1E72; Attr:laUpper; CaseCode:#$1E73),   // LATIN CAPITAL LETTER U WITH DIAERESIS BELOW
    (Unicode:#$1E73; Attr:laLower; CaseCode:#$1E72),   // LATIN SMALL LETTER U WITH DIAERESIS BELOW
    (Unicode:#$1E74; Attr:laUpper; CaseCode:#$1E75),   // LATIN CAPITAL LETTER U WITH TILDE BELOW
    (Unicode:#$1E75; Attr:laLower; CaseCode:#$1E74),   // LATIN SMALL LETTER U WITH TILDE BELOW
    (Unicode:#$1E76; Attr:laUpper; CaseCode:#$1E77),   // LATIN CAPITAL LETTER U WITH CIRCUMFLEX BELOW
    (Unicode:#$1E77; Attr:laLower; CaseCode:#$1E76),   // LATIN SMALL LETTER U WITH CIRCUMFLEX BELOW
    (Unicode:#$1E78; Attr:laUpper; CaseCode:#$1E79),   // LATIN CAPITAL LETTER U WITH TILDE AND ACUTE
    (Unicode:#$1E79; Attr:laLower; CaseCode:#$1E78),   // LATIN SMALL LETTER U WITH TILDE AND ACUTE
    (Unicode:#$1E7A; Attr:laUpper; CaseCode:#$1E7B),   // LATIN CAPITAL LETTER U WITH MACRON AND DIAERESIS
    (Unicode:#$1E7B; Attr:laLower; CaseCode:#$1E7A),   // LATIN SMALL LETTER U WITH MACRON AND DIAERESIS
    (Unicode:#$1E7C; Attr:laUpper; CaseCode:#$1E7D),   // LATIN CAPITAL LETTER V WITH TILDE
    (Unicode:#$1E7D; Attr:laLower; CaseCode:#$1E7C),   // LATIN SMALL LETTER V WITH TILDE
    (Unicode:#$1E7E; Attr:laUpper; CaseCode:#$1E7F),   // LATIN CAPITAL LETTER V WITH DOT BELOW
    (Unicode:#$1E7F; Attr:laLower; CaseCode:#$1E7E),   // LATIN SMALL LETTER V WITH DOT BELOW
    (Unicode:#$1E80; Attr:laUpper; CaseCode:#$1E81),   // LATIN CAPITAL LETTER W WITH GRAVE
    (Unicode:#$1E81; Attr:laLower; CaseCode:#$1E80),   // LATIN SMALL LETTER W WITH GRAVE
    (Unicode:#$1E82; Attr:laUpper; CaseCode:#$1E83),   // LATIN CAPITAL LETTER W WITH ACUTE
    (Unicode:#$1E83; Attr:laLower; CaseCode:#$1E82),   // LATIN SMALL LETTER W WITH ACUTE
    (Unicode:#$1E84; Attr:laUpper; CaseCode:#$1E85),   // LATIN CAPITAL LETTER W WITH DIAERESIS
    (Unicode:#$1E85; Attr:laLower; CaseCode:#$1E84),   // LATIN SMALL LETTER W WITH DIAERESIS
    (Unicode:#$1E86; Attr:laUpper; CaseCode:#$1E87),   // LATIN CAPITAL LETTER W WITH DOT ABOVE
    (Unicode:#$1E87; Attr:laLower; CaseCode:#$1E86),   // LATIN SMALL LETTER W WITH DOT ABOVE
    (Unicode:#$1E88; Attr:laUpper; CaseCode:#$1E89),   // LATIN CAPITAL LETTER W WITH DOT BELOW
    (Unicode:#$1E89; Attr:laLower; CaseCode:#$1E88),   // LATIN SMALL LETTER W WITH DOT BELOW
    (Unicode:#$1E8A; Attr:laUpper; CaseCode:#$1E8B),   // LATIN CAPITAL LETTER X WITH DOT ABOVE
    (Unicode:#$1E8B; Attr:laLower; CaseCode:#$1E8A),   // LATIN SMALL LETTER X WITH DOT ABOVE
    (Unicode:#$1E8C; Attr:laUpper; CaseCode:#$1E8D),   // LATIN CAPITAL LETTER X WITH DIAERESIS
    (Unicode:#$1E8D; Attr:laLower; CaseCode:#$1E8C),   // LATIN SMALL LETTER X WITH DIAERESIS
    (Unicode:#$1E8E; Attr:laUpper; CaseCode:#$1E8F),   // LATIN CAPITAL LETTER Y WITH DOT ABOVE
    (Unicode:#$1E8F; Attr:laLower; CaseCode:#$1E8E),   // LATIN SMALL LETTER Y WITH DOT ABOVE
    (Unicode:#$1E90; Attr:laUpper; CaseCode:#$1E91),   // LATIN CAPITAL LETTER Z WITH CIRCUMFLEX
    (Unicode:#$1E91; Attr:laLower; CaseCode:#$1E90),   // LATIN SMALL LETTER Z WITH CIRCUMFLEX
    (Unicode:#$1E92; Attr:laUpper; CaseCode:#$1E93),   // LATIN CAPITAL LETTER Z WITH DOT BELOW
    (Unicode:#$1E93; Attr:laLower; CaseCode:#$1E92),   // LATIN SMALL LETTER Z WITH DOT BELOW
    (Unicode:#$1E94; Attr:laUpper; CaseCode:#$1E95),   // LATIN CAPITAL LETTER Z WITH LINE BELOW
    (Unicode:#$1E95; Attr:laLower; CaseCode:#$1E94),   // LATIN SMALL LETTER Z WITH LINE BELOW
    (Unicode:#$1E96; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER H WITH LINE BELOW
    (Unicode:#$1E97; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER T WITH DIAERESIS
    (Unicode:#$1E98; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER W WITH RING ABOVE
    (Unicode:#$1E99; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER Y WITH RING ABOVE
    (Unicode:#$1E9A; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER A WITH RIGHT HALF RING
    (Unicode:#$1E9B; Attr:laLower; CaseCode:#$1E60),   // LATIN SMALL LETTER LONG S WITH DOT ABOVE
    (Unicode:#$1EA0; Attr:laUpper; CaseCode:#$1EA1),   // LATIN CAPITAL LETTER A WITH DOT BELOW
    (Unicode:#$1EA1; Attr:laLower; CaseCode:#$1EA0),   // LATIN SMALL LETTER A WITH DOT BELOW
    (Unicode:#$1EA2; Attr:laUpper; CaseCode:#$1EA3),   // LATIN CAPITAL LETTER A WITH HOOK ABOVE
    (Unicode:#$1EA3; Attr:laLower; CaseCode:#$1EA2),   // LATIN SMALL LETTER A WITH HOOK ABOVE
    (Unicode:#$1EA4; Attr:laUpper; CaseCode:#$1EA5),   // LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND ACUTE
    (Unicode:#$1EA5; Attr:laLower; CaseCode:#$1EA4),   // LATIN SMALL LETTER A WITH CIRCUMFLEX AND ACUTE
    (Unicode:#$1EA6; Attr:laUpper; CaseCode:#$1EA7),   // LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND GRAVE
    (Unicode:#$1EA7; Attr:laLower; CaseCode:#$1EA6),   // LATIN SMALL LETTER A WITH CIRCUMFLEX AND GRAVE
    (Unicode:#$1EA8; Attr:laUpper; CaseCode:#$1EA9),   // LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE
    (Unicode:#$1EA9; Attr:laLower; CaseCode:#$1EA8),   // LATIN SMALL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE
    (Unicode:#$1EAA; Attr:laUpper; CaseCode:#$1EAB),   // LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND TILDE
    (Unicode:#$1EAB; Attr:laLower; CaseCode:#$1EAA),   // LATIN SMALL LETTER A WITH CIRCUMFLEX AND TILDE
    (Unicode:#$1EAC; Attr:laUpper; CaseCode:#$1EAD),   // LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND DOT BELOW
    (Unicode:#$1EAD; Attr:laLower; CaseCode:#$1EAC),   // LATIN SMALL LETTER A WITH CIRCUMFLEX AND DOT BELOW
    (Unicode:#$1EAE; Attr:laUpper; CaseCode:#$1EAF),   // LATIN CAPITAL LETTER A WITH BREVE AND ACUTE
    (Unicode:#$1EAF; Attr:laLower; CaseCode:#$1EAE),   // LATIN SMALL LETTER A WITH BREVE AND ACUTE
    (Unicode:#$1EB0; Attr:laUpper; CaseCode:#$1EB1),   // LATIN CAPITAL LETTER A WITH BREVE AND GRAVE
    (Unicode:#$1EB1; Attr:laLower; CaseCode:#$1EB0),   // LATIN SMALL LETTER A WITH BREVE AND GRAVE
    (Unicode:#$1EB2; Attr:laUpper; CaseCode:#$1EB3),   // LATIN CAPITAL LETTER A WITH BREVE AND HOOK ABOVE
    (Unicode:#$1EB3; Attr:laLower; CaseCode:#$1EB2),   // LATIN SMALL LETTER A WITH BREVE AND HOOK ABOVE
    (Unicode:#$1EB4; Attr:laUpper; CaseCode:#$1EB5),   // LATIN CAPITAL LETTER A WITH BREVE AND TILDE
    (Unicode:#$1EB5; Attr:laLower; CaseCode:#$1EB4),   // LATIN SMALL LETTER A WITH BREVE AND TILDE
    (Unicode:#$1EB6; Attr:laUpper; CaseCode:#$1EB7),   // LATIN CAPITAL LETTER A WITH BREVE AND DOT BELOW
    (Unicode:#$1EB7; Attr:laLower; CaseCode:#$1EB6),   // LATIN SMALL LETTER A WITH BREVE AND DOT BELOW
    (Unicode:#$1EB8; Attr:laUpper; CaseCode:#$1EB9),   // LATIN CAPITAL LETTER E WITH DOT BELOW
    (Unicode:#$1EB9; Attr:laLower; CaseCode:#$1EB8),   // LATIN SMALL LETTER E WITH DOT BELOW
    (Unicode:#$1EBA; Attr:laUpper; CaseCode:#$1EBB),   // LATIN CAPITAL LETTER E WITH HOOK ABOVE
    (Unicode:#$1EBB; Attr:laLower; CaseCode:#$1EBA),   // LATIN SMALL LETTER E WITH HOOK ABOVE
    (Unicode:#$1EBC; Attr:laUpper; CaseCode:#$1EBD),   // LATIN CAPITAL LETTER E WITH TILDE
    (Unicode:#$1EBD; Attr:laLower; CaseCode:#$1EBC),   // LATIN SMALL LETTER E WITH TILDE
    (Unicode:#$1EBE; Attr:laUpper; CaseCode:#$1EBF),   // LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND ACUTE
    (Unicode:#$1EBF; Attr:laLower; CaseCode:#$1EBE),   // LATIN SMALL LETTER E WITH CIRCUMFLEX AND ACUTE
    (Unicode:#$1EC0; Attr:laUpper; CaseCode:#$1EC1),   // LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND GRAVE
    (Unicode:#$1EC1; Attr:laLower; CaseCode:#$1EC0),   // LATIN SMALL LETTER E WITH CIRCUMFLEX AND GRAVE
    (Unicode:#$1EC2; Attr:laUpper; CaseCode:#$1EC3),   // LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE
    (Unicode:#$1EC3; Attr:laLower; CaseCode:#$1EC2),   // LATIN SMALL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE
    (Unicode:#$1EC4; Attr:laUpper; CaseCode:#$1EC5),   // LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND TILDE
    (Unicode:#$1EC5; Attr:laLower; CaseCode:#$1EC4),   // LATIN SMALL LETTER E WITH CIRCUMFLEX AND TILDE
    (Unicode:#$1EC6; Attr:laUpper; CaseCode:#$1EC7),   // LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND DOT BELOW
    (Unicode:#$1EC7; Attr:laLower; CaseCode:#$1EC6),   // LATIN SMALL LETTER E WITH CIRCUMFLEX AND DOT BELOW
    (Unicode:#$1EC8; Attr:laUpper; CaseCode:#$1EC9),   // LATIN CAPITAL LETTER I WITH HOOK ABOVE
    (Unicode:#$1EC9; Attr:laLower; CaseCode:#$1EC8),   // LATIN SMALL LETTER I WITH HOOK ABOVE
    (Unicode:#$1ECA; Attr:laUpper; CaseCode:#$1ECB),   // LATIN CAPITAL LETTER I WITH DOT BELOW
    (Unicode:#$1ECB; Attr:laLower; CaseCode:#$1ECA),   // LATIN SMALL LETTER I WITH DOT BELOW
    (Unicode:#$1ECC; Attr:laUpper; CaseCode:#$1ECD),   // LATIN CAPITAL LETTER O WITH DOT BELOW
    (Unicode:#$1ECD; Attr:laLower; CaseCode:#$1ECC),   // LATIN SMALL LETTER O WITH DOT BELOW
    (Unicode:#$1ECE; Attr:laUpper; CaseCode:#$1ECF),   // LATIN CAPITAL LETTER O WITH HOOK ABOVE
    (Unicode:#$1ECF; Attr:laLower; CaseCode:#$1ECE),   // LATIN SMALL LETTER O WITH HOOK ABOVE
    (Unicode:#$1ED0; Attr:laUpper; CaseCode:#$1ED1),   // LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND ACUTE
    (Unicode:#$1ED1; Attr:laLower; CaseCode:#$1ED0),   // LATIN SMALL LETTER O WITH CIRCUMFLEX AND ACUTE
    (Unicode:#$1ED2; Attr:laUpper; CaseCode:#$1ED3),   // LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND GRAVE
    (Unicode:#$1ED3; Attr:laLower; CaseCode:#$1ED2),   // LATIN SMALL LETTER O WITH CIRCUMFLEX AND GRAVE
    (Unicode:#$1ED4; Attr:laUpper; CaseCode:#$1ED5),   // LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE
    (Unicode:#$1ED5; Attr:laLower; CaseCode:#$1ED4),   // LATIN SMALL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE
    (Unicode:#$1ED6; Attr:laUpper; CaseCode:#$1ED7),   // LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND TILDE
    (Unicode:#$1ED7; Attr:laLower; CaseCode:#$1ED6),   // LATIN SMALL LETTER O WITH CIRCUMFLEX AND TILDE
    (Unicode:#$1ED8; Attr:laUpper; CaseCode:#$1ED9),   // LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND DOT BELOW
    (Unicode:#$1ED9; Attr:laLower; CaseCode:#$1ED8),   // LATIN SMALL LETTER O WITH CIRCUMFLEX AND DOT BELOW
    (Unicode:#$1EDA; Attr:laUpper; CaseCode:#$1EDB),   // LATIN CAPITAL LETTER O WITH HORN AND ACUTE
    (Unicode:#$1EDB; Attr:laLower; CaseCode:#$1EDA),   // LATIN SMALL LETTER O WITH HORN AND ACUTE
    (Unicode:#$1EDC; Attr:laUpper; CaseCode:#$1EDD),   // LATIN CAPITAL LETTER O WITH HORN AND GRAVE
    (Unicode:#$1EDD; Attr:laLower; CaseCode:#$1EDC),   // LATIN SMALL LETTER O WITH HORN AND GRAVE
    (Unicode:#$1EDE; Attr:laUpper; CaseCode:#$1EDF),   // LATIN CAPITAL LETTER O WITH HORN AND HOOK ABOVE
    (Unicode:#$1EDF; Attr:laLower; CaseCode:#$1EDE),   // LATIN SMALL LETTER O WITH HORN AND HOOK ABOVE
    (Unicode:#$1EE0; Attr:laUpper; CaseCode:#$1EE1),   // LATIN CAPITAL LETTER O WITH HORN AND TILDE
    (Unicode:#$1EE1; Attr:laLower; CaseCode:#$1EE0),   // LATIN SMALL LETTER O WITH HORN AND TILDE
    (Unicode:#$1EE2; Attr:laUpper; CaseCode:#$1EE3),   // LATIN CAPITAL LETTER O WITH HORN AND DOT BELOW
    (Unicode:#$1EE3; Attr:laLower; CaseCode:#$1EE2),   // LATIN SMALL LETTER O WITH HORN AND DOT BELOW
    (Unicode:#$1EE4; Attr:laUpper; CaseCode:#$1EE5),   // LATIN CAPITAL LETTER U WITH DOT BELOW
    (Unicode:#$1EE5; Attr:laLower; CaseCode:#$1EE4),   // LATIN SMALL LETTER U WITH DOT BELOW
    (Unicode:#$1EE6; Attr:laUpper; CaseCode:#$1EE7),   // LATIN CAPITAL LETTER U WITH HOOK ABOVE
    (Unicode:#$1EE7; Attr:laLower; CaseCode:#$1EE6),   // LATIN SMALL LETTER U WITH HOOK ABOVE
    (Unicode:#$1EE8; Attr:laUpper; CaseCode:#$1EE9),   // LATIN CAPITAL LETTER U WITH HORN AND ACUTE
    (Unicode:#$1EE9; Attr:laLower; CaseCode:#$1EE8),   // LATIN SMALL LETTER U WITH HORN AND ACUTE
    (Unicode:#$1EEA; Attr:laUpper; CaseCode:#$1EEB),   // LATIN CAPITAL LETTER U WITH HORN AND GRAVE
    (Unicode:#$1EEB; Attr:laLower; CaseCode:#$1EEA),   // LATIN SMALL LETTER U WITH HORN AND GRAVE
    (Unicode:#$1EEC; Attr:laUpper; CaseCode:#$1EED),   // LATIN CAPITAL LETTER U WITH HORN AND HOOK ABOVE
    (Unicode:#$1EED; Attr:laLower; CaseCode:#$1EEC),   // LATIN SMALL LETTER U WITH HORN AND HOOK ABOVE
    (Unicode:#$1EEE; Attr:laUpper; CaseCode:#$1EEF),   // LATIN CAPITAL LETTER U WITH HORN AND TILDE
    (Unicode:#$1EEF; Attr:laLower; CaseCode:#$1EEE),   // LATIN SMALL LETTER U WITH HORN AND TILDE
    (Unicode:#$1EF0; Attr:laUpper; CaseCode:#$1EF1),   // LATIN CAPITAL LETTER U WITH HORN AND DOT BELOW
    (Unicode:#$1EF1; Attr:laLower; CaseCode:#$1EF0),   // LATIN SMALL LETTER U WITH HORN AND DOT BELOW
    (Unicode:#$1EF2; Attr:laUpper; CaseCode:#$1EF3),   // LATIN CAPITAL LETTER Y WITH GRAVE
    (Unicode:#$1EF3; Attr:laLower; CaseCode:#$1EF2),   // LATIN SMALL LETTER Y WITH GRAVE
    (Unicode:#$1EF4; Attr:laUpper; CaseCode:#$1EF5),   // LATIN CAPITAL LETTER Y WITH DOT BELOW
    (Unicode:#$1EF5; Attr:laLower; CaseCode:#$1EF4),   // LATIN SMALL LETTER Y WITH DOT BELOW
    (Unicode:#$1EF6; Attr:laUpper; CaseCode:#$1EF7),   // LATIN CAPITAL LETTER Y WITH HOOK ABOVE
    (Unicode:#$1EF7; Attr:laLower; CaseCode:#$1EF6),   // LATIN SMALL LETTER Y WITH HOOK ABOVE
    (Unicode:#$1EF8; Attr:laUpper; CaseCode:#$1EF9),   // LATIN CAPITAL LETTER Y WITH TILDE
    (Unicode:#$1EF9; Attr:laLower; CaseCode:#$1EF8),   // LATIN SMALL LETTER Y WITH TILDE
    (Unicode:#$1F00; Attr:laLower; CaseCode:#$1F08),   // GREEK SMALL LETTER ALPHA WITH PSILI
    (Unicode:#$1F01; Attr:laLower; CaseCode:#$1F09),   // GREEK SMALL LETTER ALPHA WITH DASIA
    (Unicode:#$1F02; Attr:laLower; CaseCode:#$1F0A),   // GREEK SMALL LETTER ALPHA WITH PSILI AND VARIA
    (Unicode:#$1F03; Attr:laLower; CaseCode:#$1F0B),   // GREEK SMALL LETTER ALPHA WITH DASIA AND VARIA
    (Unicode:#$1F04; Attr:laLower; CaseCode:#$1F0C),   // GREEK SMALL LETTER ALPHA WITH PSILI AND OXIA
    (Unicode:#$1F05; Attr:laLower; CaseCode:#$1F0D),   // GREEK SMALL LETTER ALPHA WITH DASIA AND OXIA
    (Unicode:#$1F06; Attr:laLower; CaseCode:#$1F0E),   // GREEK SMALL LETTER ALPHA WITH PSILI AND PERISPOMENI
    (Unicode:#$1F07; Attr:laLower; CaseCode:#$1F0F),   // GREEK SMALL LETTER ALPHA WITH DASIA AND PERISPOMENI
    (Unicode:#$1F08; Attr:laUpper; CaseCode:#$1F00),   // GREEK CAPITAL LETTER ALPHA WITH PSILI
    (Unicode:#$1F09; Attr:laUpper; CaseCode:#$1F01),   // GREEK CAPITAL LETTER ALPHA WITH DASIA
    (Unicode:#$1F0A; Attr:laUpper; CaseCode:#$1F02),   // GREEK CAPITAL LETTER ALPHA WITH PSILI AND VARIA
    (Unicode:#$1F0B; Attr:laUpper; CaseCode:#$1F03),   // GREEK CAPITAL LETTER ALPHA WITH DASIA AND VARIA
    (Unicode:#$1F0C; Attr:laUpper; CaseCode:#$1F04),   // GREEK CAPITAL LETTER ALPHA WITH PSILI AND OXIA
    (Unicode:#$1F0D; Attr:laUpper; CaseCode:#$1F05),   // GREEK CAPITAL LETTER ALPHA WITH DASIA AND OXIA
    (Unicode:#$1F0E; Attr:laUpper; CaseCode:#$1F06),   // GREEK CAPITAL LETTER ALPHA WITH PSILI AND PERISPOMENI
    (Unicode:#$1F0F; Attr:laUpper; CaseCode:#$1F07),   // GREEK CAPITAL LETTER ALPHA WITH DASIA AND PERISPOMENI
    (Unicode:#$1F10; Attr:laLower; CaseCode:#$1F18),   // GREEK SMALL LETTER EPSILON WITH PSILI
    (Unicode:#$1F11; Attr:laLower; CaseCode:#$1F19),   // GREEK SMALL LETTER EPSILON WITH DASIA
    (Unicode:#$1F12; Attr:laLower; CaseCode:#$1F1A),   // GREEK SMALL LETTER EPSILON WITH PSILI AND VARIA
    (Unicode:#$1F13; Attr:laLower; CaseCode:#$1F1B),   // GREEK SMALL LETTER EPSILON WITH DASIA AND VARIA
    (Unicode:#$1F14; Attr:laLower; CaseCode:#$1F1C),   // GREEK SMALL LETTER EPSILON WITH PSILI AND OXIA
    (Unicode:#$1F15; Attr:laLower; CaseCode:#$1F1D),   // GREEK SMALL LETTER EPSILON WITH DASIA AND OXIA
    (Unicode:#$1F18; Attr:laUpper; CaseCode:#$1F10),   // GREEK CAPITAL LETTER EPSILON WITH PSILI
    (Unicode:#$1F19; Attr:laUpper; CaseCode:#$1F11),   // GREEK CAPITAL LETTER EPSILON WITH DASIA
    (Unicode:#$1F1A; Attr:laUpper; CaseCode:#$1F12),   // GREEK CAPITAL LETTER EPSILON WITH PSILI AND VARIA
    (Unicode:#$1F1B; Attr:laUpper; CaseCode:#$1F13),   // GREEK CAPITAL LETTER EPSILON WITH DASIA AND VARIA
    (Unicode:#$1F1C; Attr:laUpper; CaseCode:#$1F14),   // GREEK CAPITAL LETTER EPSILON WITH PSILI AND OXIA
    (Unicode:#$1F1D; Attr:laUpper; CaseCode:#$1F15),   // GREEK CAPITAL LETTER EPSILON WITH DASIA AND OXIA
    (Unicode:#$1F20; Attr:laLower; CaseCode:#$1F28),   // GREEK SMALL LETTER ETA WITH PSILI
    (Unicode:#$1F21; Attr:laLower; CaseCode:#$1F29),   // GREEK SMALL LETTER ETA WITH DASIA
    (Unicode:#$1F22; Attr:laLower; CaseCode:#$1F2A),   // GREEK SMALL LETTER ETA WITH PSILI AND VARIA
    (Unicode:#$1F23; Attr:laLower; CaseCode:#$1F2B),   // GREEK SMALL LETTER ETA WITH DASIA AND VARIA
    (Unicode:#$1F24; Attr:laLower; CaseCode:#$1F2C),   // GREEK SMALL LETTER ETA WITH PSILI AND OXIA
    (Unicode:#$1F25; Attr:laLower; CaseCode:#$1F2D),   // GREEK SMALL LETTER ETA WITH DASIA AND OXIA
    (Unicode:#$1F26; Attr:laLower; CaseCode:#$1F2E),   // GREEK SMALL LETTER ETA WITH PSILI AND PERISPOMENI
    (Unicode:#$1F27; Attr:laLower; CaseCode:#$1F2F),   // GREEK SMALL LETTER ETA WITH DASIA AND PERISPOMENI
    (Unicode:#$1F28; Attr:laUpper; CaseCode:#$1F20),   // GREEK CAPITAL LETTER ETA WITH PSILI
    (Unicode:#$1F29; Attr:laUpper; CaseCode:#$1F21),   // GREEK CAPITAL LETTER ETA WITH DASIA
    (Unicode:#$1F2A; Attr:laUpper; CaseCode:#$1F22),   // GREEK CAPITAL LETTER ETA WITH PSILI AND VARIA
    (Unicode:#$1F2B; Attr:laUpper; CaseCode:#$1F23),   // GREEK CAPITAL LETTER ETA WITH DASIA AND VARIA
    (Unicode:#$1F2C; Attr:laUpper; CaseCode:#$1F24),   // GREEK CAPITAL LETTER ETA WITH PSILI AND OXIA
    (Unicode:#$1F2D; Attr:laUpper; CaseCode:#$1F25),   // GREEK CAPITAL LETTER ETA WITH DASIA AND OXIA
    (Unicode:#$1F2E; Attr:laUpper; CaseCode:#$1F26),   // GREEK CAPITAL LETTER ETA WITH PSILI AND PERISPOMENI
    (Unicode:#$1F2F; Attr:laUpper; CaseCode:#$1F27),   // GREEK CAPITAL LETTER ETA WITH DASIA AND PERISPOMENI
    (Unicode:#$1F30; Attr:laLower; CaseCode:#$1F38),   // GREEK SMALL LETTER IOTA WITH PSILI
    (Unicode:#$1F31; Attr:laLower; CaseCode:#$1F39),   // GREEK SMALL LETTER IOTA WITH DASIA
    (Unicode:#$1F32; Attr:laLower; CaseCode:#$1F3A),   // GREEK SMALL LETTER IOTA WITH PSILI AND VARIA
    (Unicode:#$1F33; Attr:laLower; CaseCode:#$1F3B),   // GREEK SMALL LETTER IOTA WITH DASIA AND VARIA
    (Unicode:#$1F34; Attr:laLower; CaseCode:#$1F3C),   // GREEK SMALL LETTER IOTA WITH PSILI AND OXIA
    (Unicode:#$1F35; Attr:laLower; CaseCode:#$1F3D),   // GREEK SMALL LETTER IOTA WITH DASIA AND OXIA
    (Unicode:#$1F36; Attr:laLower; CaseCode:#$1F3E),   // GREEK SMALL LETTER IOTA WITH PSILI AND PERISPOMENI
    (Unicode:#$1F37; Attr:laLower; CaseCode:#$1F3F),   // GREEK SMALL LETTER IOTA WITH DASIA AND PERISPOMENI
    (Unicode:#$1F38; Attr:laUpper; CaseCode:#$1F30),   // GREEK CAPITAL LETTER IOTA WITH PSILI
    (Unicode:#$1F39; Attr:laUpper; CaseCode:#$1F31),   // GREEK CAPITAL LETTER IOTA WITH DASIA
    (Unicode:#$1F3A; Attr:laUpper; CaseCode:#$1F32),   // GREEK CAPITAL LETTER IOTA WITH PSILI AND VARIA
    (Unicode:#$1F3B; Attr:laUpper; CaseCode:#$1F33),   // GREEK CAPITAL LETTER IOTA WITH DASIA AND VARIA
    (Unicode:#$1F3C; Attr:laUpper; CaseCode:#$1F34),   // GREEK CAPITAL LETTER IOTA WITH PSILI AND OXIA
    (Unicode:#$1F3D; Attr:laUpper; CaseCode:#$1F35),   // GREEK CAPITAL LETTER IOTA WITH DASIA AND OXIA
    (Unicode:#$1F3E; Attr:laUpper; CaseCode:#$1F36),   // GREEK CAPITAL LETTER IOTA WITH PSILI AND PERISPOMENI
    (Unicode:#$1F3F; Attr:laUpper; CaseCode:#$1F37),   // GREEK CAPITAL LETTER IOTA WITH DASIA AND PERISPOMENI
    (Unicode:#$1F40; Attr:laLower; CaseCode:#$1F48),   // GREEK SMALL LETTER OMICRON WITH PSILI
    (Unicode:#$1F41; Attr:laLower; CaseCode:#$1F49),   // GREEK SMALL LETTER OMICRON WITH DASIA
    (Unicode:#$1F42; Attr:laLower; CaseCode:#$1F4A),   // GREEK SMALL LETTER OMICRON WITH PSILI AND VARIA
    (Unicode:#$1F43; Attr:laLower; CaseCode:#$1F4B),   // GREEK SMALL LETTER OMICRON WITH DASIA AND VARIA
    (Unicode:#$1F44; Attr:laLower; CaseCode:#$1F4C),   // GREEK SMALL LETTER OMICRON WITH PSILI AND OXIA
    (Unicode:#$1F45; Attr:laLower; CaseCode:#$1F4D),   // GREEK SMALL LETTER OMICRON WITH DASIA AND OXIA
    (Unicode:#$1F48; Attr:laUpper; CaseCode:#$1F40),   // GREEK CAPITAL LETTER OMICRON WITH PSILI
    (Unicode:#$1F49; Attr:laUpper; CaseCode:#$1F41),   // GREEK CAPITAL LETTER OMICRON WITH DASIA
    (Unicode:#$1F4A; Attr:laUpper; CaseCode:#$1F42),   // GREEK CAPITAL LETTER OMICRON WITH PSILI AND VARIA
    (Unicode:#$1F4B; Attr:laUpper; CaseCode:#$1F43),   // GREEK CAPITAL LETTER OMICRON WITH DASIA AND VARIA
    (Unicode:#$1F4C; Attr:laUpper; CaseCode:#$1F44),   // GREEK CAPITAL LETTER OMICRON WITH PSILI AND OXIA
    (Unicode:#$1F4D; Attr:laUpper; CaseCode:#$1F45),   // GREEK CAPITAL LETTER OMICRON WITH DASIA AND OXIA
    (Unicode:#$1F50; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER UPSILON WITH PSILI
    (Unicode:#$1F51; Attr:laLower; CaseCode:#$1F59),   // GREEK SMALL LETTER UPSILON WITH DASIA
    (Unicode:#$1F52; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER UPSILON WITH PSILI AND VARIA
    (Unicode:#$1F53; Attr:laLower; CaseCode:#$1F5B),   // GREEK SMALL LETTER UPSILON WITH DASIA AND VARIA
    (Unicode:#$1F54; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER UPSILON WITH PSILI AND OXIA
    (Unicode:#$1F55; Attr:laLower; CaseCode:#$1F5D),   // GREEK SMALL LETTER UPSILON WITH DASIA AND OXIA
    (Unicode:#$1F56; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER UPSILON WITH PSILI AND PERISPOMENI
    (Unicode:#$1F57; Attr:laLower; CaseCode:#$1F5F),   // GREEK SMALL LETTER UPSILON WITH DASIA AND PERISPOMENI
    (Unicode:#$1F59; Attr:laUpper; CaseCode:#$1F51),   // GREEK CAPITAL LETTER UPSILON WITH DASIA
    (Unicode:#$1F5B; Attr:laUpper; CaseCode:#$1F53),   // GREEK CAPITAL LETTER UPSILON WITH DASIA AND VARIA
    (Unicode:#$1F5D; Attr:laUpper; CaseCode:#$1F55),   // GREEK CAPITAL LETTER UPSILON WITH DASIA AND OXIA
    (Unicode:#$1F5F; Attr:laUpper; CaseCode:#$1F57),   // GREEK CAPITAL LETTER UPSILON WITH DASIA AND PERISPOMENI
    (Unicode:#$1F60; Attr:laLower; CaseCode:#$1F68),   // GREEK SMALL LETTER OMEGA WITH PSILI
    (Unicode:#$1F61; Attr:laLower; CaseCode:#$1F69),   // GREEK SMALL LETTER OMEGA WITH DASIA
    (Unicode:#$1F62; Attr:laLower; CaseCode:#$1F6A),   // GREEK SMALL LETTER OMEGA WITH PSILI AND VARIA
    (Unicode:#$1F63; Attr:laLower; CaseCode:#$1F6B),   // GREEK SMALL LETTER OMEGA WITH DASIA AND VARIA
    (Unicode:#$1F64; Attr:laLower; CaseCode:#$1F6C),   // GREEK SMALL LETTER OMEGA WITH PSILI AND OXIA
    (Unicode:#$1F65; Attr:laLower; CaseCode:#$1F6D),   // GREEK SMALL LETTER OMEGA WITH DASIA AND OXIA
    (Unicode:#$1F66; Attr:laLower; CaseCode:#$1F6E),   // GREEK SMALL LETTER OMEGA WITH PSILI AND PERISPOMENI
    (Unicode:#$1F67; Attr:laLower; CaseCode:#$1F6F),   // GREEK SMALL LETTER OMEGA WITH DASIA AND PERISPOMENI
    (Unicode:#$1F68; Attr:laUpper; CaseCode:#$1F60),   // GREEK CAPITAL LETTER OMEGA WITH PSILI
    (Unicode:#$1F69; Attr:laUpper; CaseCode:#$1F61),   // GREEK CAPITAL LETTER OMEGA WITH DASIA
    (Unicode:#$1F6A; Attr:laUpper; CaseCode:#$1F62),   // GREEK CAPITAL LETTER OMEGA WITH PSILI AND VARIA
    (Unicode:#$1F6B; Attr:laUpper; CaseCode:#$1F63),   // GREEK CAPITAL LETTER OMEGA WITH DASIA AND VARIA
    (Unicode:#$1F6C; Attr:laUpper; CaseCode:#$1F64),   // GREEK CAPITAL LETTER OMEGA WITH PSILI AND OXIA
    (Unicode:#$1F6D; Attr:laUpper; CaseCode:#$1F65),   // GREEK CAPITAL LETTER OMEGA WITH DASIA AND OXIA
    (Unicode:#$1F6E; Attr:laUpper; CaseCode:#$1F66),   // GREEK CAPITAL LETTER OMEGA WITH PSILI AND PERISPOMENI
    (Unicode:#$1F6F; Attr:laUpper; CaseCode:#$1F67),   // GREEK CAPITAL LETTER OMEGA WITH DASIA AND PERISPOMENI
    (Unicode:#$1F70; Attr:laLower; CaseCode:#$1FBA),   // GREEK SMALL LETTER ALPHA WITH VARIA
    (Unicode:#$1F71; Attr:laLower; CaseCode:#$1FBB),   // GREEK SMALL LETTER ALPHA WITH OXIA
    (Unicode:#$1F72; Attr:laLower; CaseCode:#$1FC8),   // GREEK SMALL LETTER EPSILON WITH VARIA
    (Unicode:#$1F73; Attr:laLower; CaseCode:#$1FC9),   // GREEK SMALL LETTER EPSILON WITH OXIA
    (Unicode:#$1F74; Attr:laLower; CaseCode:#$1FCA),   // GREEK SMALL LETTER ETA WITH VARIA
    (Unicode:#$1F75; Attr:laLower; CaseCode:#$1FCB),   // GREEK SMALL LETTER ETA WITH OXIA
    (Unicode:#$1F76; Attr:laLower; CaseCode:#$1FDA),   // GREEK SMALL LETTER IOTA WITH VARIA
    (Unicode:#$1F77; Attr:laLower; CaseCode:#$1FDB),   // GREEK SMALL LETTER IOTA WITH OXIA
    (Unicode:#$1F78; Attr:laLower; CaseCode:#$1FF8),   // GREEK SMALL LETTER OMICRON WITH VARIA
    (Unicode:#$1F79; Attr:laLower; CaseCode:#$1FF9),   // GREEK SMALL LETTER OMICRON WITH OXIA
    (Unicode:#$1F7A; Attr:laLower; CaseCode:#$1FEA),   // GREEK SMALL LETTER UPSILON WITH VARIA
    (Unicode:#$1F7B; Attr:laLower; CaseCode:#$1FEB),   // GREEK SMALL LETTER UPSILON WITH OXIA
    (Unicode:#$1F7C; Attr:laLower; CaseCode:#$1FFA),   // GREEK SMALL LETTER OMEGA WITH VARIA
    (Unicode:#$1F7D; Attr:laLower; CaseCode:#$1FFB),   // GREEK SMALL LETTER OMEGA WITH OXIA
    (Unicode:#$1F80; Attr:laLower; CaseCode:#$1F88),   // GREEK SMALL LETTER ALPHA WITH PSILI AND YPOGEGRAMMENI
    (Unicode:#$1F81; Attr:laLower; CaseCode:#$1F89),   // GREEK SMALL LETTER ALPHA WITH DASIA AND YPOGEGRAMMENI
    (Unicode:#$1F82; Attr:laLower; CaseCode:#$1F8A),   // GREEK SMALL LETTER ALPHA WITH PSILI AND VARIA AND YPOGEGRAMMENI
    (Unicode:#$1F83; Attr:laLower; CaseCode:#$1F8B),   // GREEK SMALL LETTER ALPHA WITH DASIA AND VARIA AND YPOGEGRAMMENI
    (Unicode:#$1F84; Attr:laLower; CaseCode:#$1F8C),   // GREEK SMALL LETTER ALPHA WITH PSILI AND OXIA AND YPOGEGRAMMENI
    (Unicode:#$1F85; Attr:laLower; CaseCode:#$1F8D),   // GREEK SMALL LETTER ALPHA WITH DASIA AND OXIA AND YPOGEGRAMMENI
    (Unicode:#$1F86; Attr:laLower; CaseCode:#$1F8E),   // GREEK SMALL LETTER ALPHA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
    (Unicode:#$1F87; Attr:laLower; CaseCode:#$1F8F),   // GREEK SMALL LETTER ALPHA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
    (Unicode:#$1F90; Attr:laLower; CaseCode:#$1F98),   // GREEK SMALL LETTER ETA WITH PSILI AND YPOGEGRAMMENI
    (Unicode:#$1F91; Attr:laLower; CaseCode:#$1F99),   // GREEK SMALL LETTER ETA WITH DASIA AND YPOGEGRAMMENI
    (Unicode:#$1F92; Attr:laLower; CaseCode:#$1F9A),   // GREEK SMALL LETTER ETA WITH PSILI AND VARIA AND YPOGEGRAMMENI
    (Unicode:#$1F93; Attr:laLower; CaseCode:#$1F9B),   // GREEK SMALL LETTER ETA WITH DASIA AND VARIA AND YPOGEGRAMMENI
    (Unicode:#$1F94; Attr:laLower; CaseCode:#$1F9C),   // GREEK SMALL LETTER ETA WITH PSILI AND OXIA AND YPOGEGRAMMENI
    (Unicode:#$1F95; Attr:laLower; CaseCode:#$1F9D),   // GREEK SMALL LETTER ETA WITH DASIA AND OXIA AND YPOGEGRAMMENI
    (Unicode:#$1F96; Attr:laLower; CaseCode:#$1F9E),   // GREEK SMALL LETTER ETA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
    (Unicode:#$1F97; Attr:laLower; CaseCode:#$1F9F),   // GREEK SMALL LETTER ETA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
    (Unicode:#$1FA0; Attr:laLower; CaseCode:#$1FA8),   // GREEK SMALL LETTER OMEGA WITH PSILI AND YPOGEGRAMMENI
    (Unicode:#$1FA1; Attr:laLower; CaseCode:#$1FA9),   // GREEK SMALL LETTER OMEGA WITH DASIA AND YPOGEGRAMMENI
    (Unicode:#$1FA2; Attr:laLower; CaseCode:#$1FAA),   // GREEK SMALL LETTER OMEGA WITH PSILI AND VARIA AND YPOGEGRAMMENI
    (Unicode:#$1FA3; Attr:laLower; CaseCode:#$1FAB),   // GREEK SMALL LETTER OMEGA WITH DASIA AND VARIA AND YPOGEGRAMMENI
    (Unicode:#$1FA4; Attr:laLower; CaseCode:#$1FAC),   // GREEK SMALL LETTER OMEGA WITH PSILI AND OXIA AND YPOGEGRAMMENI
    (Unicode:#$1FA5; Attr:laLower; CaseCode:#$1FAD),   // GREEK SMALL LETTER OMEGA WITH DASIA AND OXIA AND YPOGEGRAMMENI
    (Unicode:#$1FA6; Attr:laLower; CaseCode:#$1FAE),   // GREEK SMALL LETTER OMEGA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
    (Unicode:#$1FA7; Attr:laLower; CaseCode:#$1FAF),   // GREEK SMALL LETTER OMEGA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
    (Unicode:#$1FB0; Attr:laLower; CaseCode:#$1FB8),   // GREEK SMALL LETTER ALPHA WITH VRACHY
    (Unicode:#$1FB1; Attr:laLower; CaseCode:#$1FB9),   // GREEK SMALL LETTER ALPHA WITH MACRON
    (Unicode:#$1FB2; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER ALPHA WITH VARIA AND YPOGEGRAMMENI
    (Unicode:#$1FB3; Attr:laLower; CaseCode:#$1FBC),   // GREEK SMALL LETTER ALPHA WITH YPOGEGRAMMENI
    (Unicode:#$1FB4; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER ALPHA WITH OXIA AND YPOGEGRAMMENI
    (Unicode:#$1FB6; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER ALPHA WITH PERISPOMENI
    (Unicode:#$1FB7; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER ALPHA WITH PERISPOMENI AND YPOGEGRAMMENI
    (Unicode:#$1FB8; Attr:laUpper; CaseCode:#$1FB0),   // GREEK CAPITAL LETTER ALPHA WITH VRACHY
    (Unicode:#$1FB9; Attr:laUpper; CaseCode:#$1FB1),   // GREEK CAPITAL LETTER ALPHA WITH MACRON
    (Unicode:#$1FBA; Attr:laUpper; CaseCode:#$1F70),   // GREEK CAPITAL LETTER ALPHA WITH VARIA
    (Unicode:#$1FBB; Attr:laUpper; CaseCode:#$1F71),   // GREEK CAPITAL LETTER ALPHA WITH OXIA
    (Unicode:#$1FBE; Attr:laLower; CaseCode:#$0399),   // GREEK PROSGEGRAMMENI
    (Unicode:#$1FC2; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER ETA WITH VARIA AND YPOGEGRAMMENI
    (Unicode:#$1FC3; Attr:laLower; CaseCode:#$1FCC),   // GREEK SMALL LETTER ETA WITH YPOGEGRAMMENI
    (Unicode:#$1FC4; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER ETA WITH OXIA AND YPOGEGRAMMENI
    (Unicode:#$1FC6; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER ETA WITH PERISPOMENI
    (Unicode:#$1FC7; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER ETA WITH PERISPOMENI AND YPOGEGRAMMENI
    (Unicode:#$1FC8; Attr:laUpper; CaseCode:#$1F72),   // GREEK CAPITAL LETTER EPSILON WITH VARIA
    (Unicode:#$1FC9; Attr:laUpper; CaseCode:#$1F73),   // GREEK CAPITAL LETTER EPSILON WITH OXIA
    (Unicode:#$1FCA; Attr:laUpper; CaseCode:#$1F74),   // GREEK CAPITAL LETTER ETA WITH VARIA
    (Unicode:#$1FCB; Attr:laUpper; CaseCode:#$1F75),   // GREEK CAPITAL LETTER ETA WITH OXIA
    (Unicode:#$1FD0; Attr:laLower; CaseCode:#$1FD8),   // GREEK SMALL LETTER IOTA WITH VRACHY
    (Unicode:#$1FD1; Attr:laLower; CaseCode:#$1FD9),   // GREEK SMALL LETTER IOTA WITH MACRON
    (Unicode:#$1FD2; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER IOTA WITH DIALYTIKA AND VARIA
    (Unicode:#$1FD3; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER IOTA WITH DIALYTIKA AND OXIA
    (Unicode:#$1FD6; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER IOTA WITH PERISPOMENI
    (Unicode:#$1FD7; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER IOTA WITH DIALYTIKA AND PERISPOMENI
    (Unicode:#$1FD8; Attr:laUpper; CaseCode:#$1FD0),   // GREEK CAPITAL LETTER IOTA WITH VRACHY
    (Unicode:#$1FD9; Attr:laUpper; CaseCode:#$1FD1),   // GREEK CAPITAL LETTER IOTA WITH MACRON
    (Unicode:#$1FDA; Attr:laUpper; CaseCode:#$1F76),   // GREEK CAPITAL LETTER IOTA WITH VARIA
    (Unicode:#$1FDB; Attr:laUpper; CaseCode:#$1F77),   // GREEK CAPITAL LETTER IOTA WITH OXIA
    (Unicode:#$1FE0; Attr:laLower; CaseCode:#$1FE8),   // GREEK SMALL LETTER UPSILON WITH VRACHY
    (Unicode:#$1FE1; Attr:laLower; CaseCode:#$1FE9),   // GREEK SMALL LETTER UPSILON WITH MACRON
    (Unicode:#$1FE2; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND VARIA
    (Unicode:#$1FE3; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND OXIA
    (Unicode:#$1FE4; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER RHO WITH PSILI
    (Unicode:#$1FE5; Attr:laLower; CaseCode:#$1FEC),   // GREEK SMALL LETTER RHO WITH DASIA
    (Unicode:#$1FE6; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER UPSILON WITH PERISPOMENI
    (Unicode:#$1FE7; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND PERISPOMENI
    (Unicode:#$1FE8; Attr:laUpper; CaseCode:#$1FE0),   // GREEK CAPITAL LETTER UPSILON WITH VRACHY
    (Unicode:#$1FE9; Attr:laUpper; CaseCode:#$1FE1),   // GREEK CAPITAL LETTER UPSILON WITH MACRON
    (Unicode:#$1FEA; Attr:laUpper; CaseCode:#$1F7A),   // GREEK CAPITAL LETTER UPSILON WITH VARIA
    (Unicode:#$1FEB; Attr:laUpper; CaseCode:#$1F7B),   // GREEK CAPITAL LETTER UPSILON WITH OXIA
    (Unicode:#$1FEC; Attr:laUpper; CaseCode:#$1FE5),   // GREEK CAPITAL LETTER RHO WITH DASIA
    (Unicode:#$1FF2; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER OMEGA WITH VARIA AND YPOGEGRAMMENI
    (Unicode:#$1FF3; Attr:laLower; CaseCode:#$1FFC),   // GREEK SMALL LETTER OMEGA WITH YPOGEGRAMMENI
    (Unicode:#$1FF4; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER OMEGA WITH OXIA AND YPOGEGRAMMENI
    (Unicode:#$1FF6; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER OMEGA WITH PERISPOMENI
    (Unicode:#$1FF7; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER OMEGA WITH PERISPOMENI AND YPOGEGRAMMENI
    (Unicode:#$1FF8; Attr:laUpper; CaseCode:#$1F78),   // GREEK CAPITAL LETTER OMICRON WITH VARIA
    (Unicode:#$1FF9; Attr:laUpper; CaseCode:#$1F79),   // GREEK CAPITAL LETTER OMICRON WITH OXIA
    (Unicode:#$1FFA; Attr:laUpper; CaseCode:#$1F7C),   // GREEK CAPITAL LETTER OMEGA WITH VARIA
    (Unicode:#$1FFB; Attr:laUpper; CaseCode:#$1F7D),   // GREEK CAPITAL LETTER OMEGA WITH OXIA
    (Unicode:#$207F; Attr:laLower; CaseCode:#$FFFF),   // SUPERSCRIPT LATIN SMALL LETTER N
    (Unicode:#$2102; Attr:laUpper; CaseCode:#$FFFF),   // DOUBLE-STRUCK CAPITAL C
    (Unicode:#$2107; Attr:laUpper; CaseCode:#$FFFF),   // EULER CONSTANT
    (Unicode:#$210A; Attr:laLower; CaseCode:#$FFFF),   // SCRIPT SMALL G
    (Unicode:#$210B; Attr:laUpper; CaseCode:#$FFFF),   // SCRIPT CAPITAL H
    (Unicode:#$210C; Attr:laUpper; CaseCode:#$FFFF),   // BLACK-LETTER CAPITAL H
    (Unicode:#$210D; Attr:laUpper; CaseCode:#$FFFF),   // DOUBLE-STRUCK CAPITAL H
    (Unicode:#$210E; Attr:laLower; CaseCode:#$FFFF),   // PLANCK CONSTANT
    (Unicode:#$210F; Attr:laLower; CaseCode:#$FFFF),   // PLANCK CONSTANT OVER TWO PI
    (Unicode:#$2110; Attr:laUpper; CaseCode:#$FFFF),   // SCRIPT CAPITAL I
    (Unicode:#$2111; Attr:laUpper; CaseCode:#$FFFF),   // BLACK-LETTER CAPITAL I
    (Unicode:#$2112; Attr:laUpper; CaseCode:#$FFFF),   // SCRIPT CAPITAL L
    (Unicode:#$2113; Attr:laLower; CaseCode:#$FFFF),   // SCRIPT SMALL L
    (Unicode:#$2115; Attr:laUpper; CaseCode:#$FFFF),   // DOUBLE-STRUCK CAPITAL N
    (Unicode:#$2119; Attr:laUpper; CaseCode:#$FFFF),   // DOUBLE-STRUCK CAPITAL P
    (Unicode:#$211A; Attr:laUpper; CaseCode:#$FFFF),   // DOUBLE-STRUCK CAPITAL Q
    (Unicode:#$211B; Attr:laUpper; CaseCode:#$FFFF),   // SCRIPT CAPITAL R
    (Unicode:#$211C; Attr:laUpper; CaseCode:#$FFFF),   // BLACK-LETTER CAPITAL R
    (Unicode:#$211D; Attr:laUpper; CaseCode:#$FFFF),   // DOUBLE-STRUCK CAPITAL R
    (Unicode:#$2124; Attr:laUpper; CaseCode:#$FFFF),   // DOUBLE-STRUCK CAPITAL Z
    (Unicode:#$2126; Attr:laUpper; CaseCode:#$03C9),   // OHM SIGN
    (Unicode:#$2128; Attr:laUpper; CaseCode:#$FFFF),   // BLACK-LETTER CAPITAL Z
    (Unicode:#$212A; Attr:laUpper; CaseCode:#$006B),   // KELVIN SIGN
    (Unicode:#$212B; Attr:laUpper; CaseCode:#$00E5),   // ANGSTROM SIGN
    (Unicode:#$212C; Attr:laUpper; CaseCode:#$FFFF),   // SCRIPT CAPITAL B
    (Unicode:#$212D; Attr:laUpper; CaseCode:#$FFFF),   // BLACK-LETTER CAPITAL C
    (Unicode:#$212F; Attr:laLower; CaseCode:#$FFFF),   // SCRIPT SMALL E
    (Unicode:#$2130; Attr:laUpper; CaseCode:#$FFFF),   // SCRIPT CAPITAL E
    (Unicode:#$2131; Attr:laUpper; CaseCode:#$FFFF),   // SCRIPT CAPITAL F
    (Unicode:#$2133; Attr:laUpper; CaseCode:#$FFFF),   // SCRIPT CAPITAL M
    (Unicode:#$2134; Attr:laLower; CaseCode:#$FFFF),   // SCRIPT SMALL O
    (Unicode:#$2139; Attr:laLower; CaseCode:#$FFFF),   // INFORMATION SOURCE
    (Unicode:#$FB00; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LIGATURE FF
    (Unicode:#$FB01; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LIGATURE FI
    (Unicode:#$FB02; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LIGATURE FL
    (Unicode:#$FB03; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LIGATURE FFI
    (Unicode:#$FB04; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LIGATURE FFL
    (Unicode:#$FB05; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LIGATURE LONG S T
    (Unicode:#$FB06; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LIGATURE ST
    (Unicode:#$FB13; Attr:laLower; CaseCode:#$FFFF),   // ARMENIAN SMALL LIGATURE MEN NOW
    (Unicode:#$FB14; Attr:laLower; CaseCode:#$FFFF),   // ARMENIAN SMALL LIGATURE MEN ECH
    (Unicode:#$FB15; Attr:laLower; CaseCode:#$FFFF),   // ARMENIAN SMALL LIGATURE MEN INI
    (Unicode:#$FB16; Attr:laLower; CaseCode:#$FFFF),   // ARMENIAN SMALL LIGATURE VEW NOW
    (Unicode:#$FB17; Attr:laLower; CaseCode:#$FFFF),   // ARMENIAN SMALL LIGATURE MEN XEH
    (Unicode:#$FF21; Attr:laUpper; CaseCode:#$FF41),   // FULLWIDTH LATIN CAPITAL LETTER A
    (Unicode:#$FF22; Attr:laUpper; CaseCode:#$FF42),   // FULLWIDTH LATIN CAPITAL LETTER B
    (Unicode:#$FF23; Attr:laUpper; CaseCode:#$FF43),   // FULLWIDTH LATIN CAPITAL LETTER C
    (Unicode:#$FF24; Attr:laUpper; CaseCode:#$FF44),   // FULLWIDTH LATIN CAPITAL LETTER D
    (Unicode:#$FF25; Attr:laUpper; CaseCode:#$FF45),   // FULLWIDTH LATIN CAPITAL LETTER E
    (Unicode:#$FF26; Attr:laUpper; CaseCode:#$FF46),   // FULLWIDTH LATIN CAPITAL LETTER F
    (Unicode:#$FF27; Attr:laUpper; CaseCode:#$FF47),   // FULLWIDTH LATIN CAPITAL LETTER G
    (Unicode:#$FF28; Attr:laUpper; CaseCode:#$FF48),   // FULLWIDTH LATIN CAPITAL LETTER H
    (Unicode:#$FF29; Attr:laUpper; CaseCode:#$FF49),   // FULLWIDTH LATIN CAPITAL LETTER I
    (Unicode:#$FF2A; Attr:laUpper; CaseCode:#$FF4A),   // FULLWIDTH LATIN CAPITAL LETTER J
    (Unicode:#$FF2B; Attr:laUpper; CaseCode:#$FF4B),   // FULLWIDTH LATIN CAPITAL LETTER K
    (Unicode:#$FF2C; Attr:laUpper; CaseCode:#$FF4C),   // FULLWIDTH LATIN CAPITAL LETTER L
    (Unicode:#$FF2D; Attr:laUpper; CaseCode:#$FF4D),   // FULLWIDTH LATIN CAPITAL LETTER M
    (Unicode:#$FF2E; Attr:laUpper; CaseCode:#$FF4E),   // FULLWIDTH LATIN CAPITAL LETTER N
    (Unicode:#$FF2F; Attr:laUpper; CaseCode:#$FF4F),   // FULLWIDTH LATIN CAPITAL LETTER O
    (Unicode:#$FF30; Attr:laUpper; CaseCode:#$FF50),   // FULLWIDTH LATIN CAPITAL LETTER P
    (Unicode:#$FF31; Attr:laUpper; CaseCode:#$FF51),   // FULLWIDTH LATIN CAPITAL LETTER Q
    (Unicode:#$FF32; Attr:laUpper; CaseCode:#$FF52),   // FULLWIDTH LATIN CAPITAL LETTER R
    (Unicode:#$FF33; Attr:laUpper; CaseCode:#$FF53),   // FULLWIDTH LATIN CAPITAL LETTER S
    (Unicode:#$FF34; Attr:laUpper; CaseCode:#$FF54),   // FULLWIDTH LATIN CAPITAL LETTER T
    (Unicode:#$FF35; Attr:laUpper; CaseCode:#$FF55),   // FULLWIDTH LATIN CAPITAL LETTER U
    (Unicode:#$FF36; Attr:laUpper; CaseCode:#$FF56),   // FULLWIDTH LATIN CAPITAL LETTER V
    (Unicode:#$FF37; Attr:laUpper; CaseCode:#$FF57),   // FULLWIDTH LATIN CAPITAL LETTER W
    (Unicode:#$FF38; Attr:laUpper; CaseCode:#$FF58),   // FULLWIDTH LATIN CAPITAL LETTER X
    (Unicode:#$FF39; Attr:laUpper; CaseCode:#$FF59),   // FULLWIDTH LATIN CAPITAL LETTER Y
    (Unicode:#$FF3A; Attr:laUpper; CaseCode:#$FF5A),   // FULLWIDTH LATIN CAPITAL LETTER Z
    (Unicode:#$FF41; Attr:laLower; CaseCode:#$FF21),   // FULLWIDTH LATIN SMALL LETTER A
    (Unicode:#$FF42; Attr:laLower; CaseCode:#$FF22),   // FULLWIDTH LATIN SMALL LETTER B
    (Unicode:#$FF43; Attr:laLower; CaseCode:#$FF23),   // FULLWIDTH LATIN SMALL LETTER C
    (Unicode:#$FF44; Attr:laLower; CaseCode:#$FF24),   // FULLWIDTH LATIN SMALL LETTER D
    (Unicode:#$FF45; Attr:laLower; CaseCode:#$FF25),   // FULLWIDTH LATIN SMALL LETTER E
    (Unicode:#$FF46; Attr:laLower; CaseCode:#$FF26),   // FULLWIDTH LATIN SMALL LETTER F
    (Unicode:#$FF47; Attr:laLower; CaseCode:#$FF27),   // FULLWIDTH LATIN SMALL LETTER G
    (Unicode:#$FF48; Attr:laLower; CaseCode:#$FF28),   // FULLWIDTH LATIN SMALL LETTER H
    (Unicode:#$FF49; Attr:laLower; CaseCode:#$FF29),   // FULLWIDTH LATIN SMALL LETTER I
    (Unicode:#$FF4A; Attr:laLower; CaseCode:#$FF2A),   // FULLWIDTH LATIN SMALL LETTER J
    (Unicode:#$FF4B; Attr:laLower; CaseCode:#$FF2B),   // FULLWIDTH LATIN SMALL LETTER K
    (Unicode:#$FF4C; Attr:laLower; CaseCode:#$FF2C),   // FULLWIDTH LATIN SMALL LETTER L
    (Unicode:#$FF4D; Attr:laLower; CaseCode:#$FF2D),   // FULLWIDTH LATIN SMALL LETTER M
    (Unicode:#$FF4E; Attr:laLower; CaseCode:#$FF2E),   // FULLWIDTH LATIN SMALL LETTER N
    (Unicode:#$FF4F; Attr:laLower; CaseCode:#$FF2F),   // FULLWIDTH LATIN SMALL LETTER O
    (Unicode:#$FF50; Attr:laLower; CaseCode:#$FF30),   // FULLWIDTH LATIN SMALL LETTER P
    (Unicode:#$FF51; Attr:laLower; CaseCode:#$FF31),   // FULLWIDTH LATIN SMALL LETTER Q
    (Unicode:#$FF52; Attr:laLower; CaseCode:#$FF32),   // FULLWIDTH LATIN SMALL LETTER R
    (Unicode:#$FF53; Attr:laLower; CaseCode:#$FF33),   // FULLWIDTH LATIN SMALL LETTER S
    (Unicode:#$FF54; Attr:laLower; CaseCode:#$FF34),   // FULLWIDTH LATIN SMALL LETTER T
    (Unicode:#$FF55; Attr:laLower; CaseCode:#$FF35),   // FULLWIDTH LATIN SMALL LETTER U
    (Unicode:#$FF56; Attr:laLower; CaseCode:#$FF36),   // FULLWIDTH LATIN SMALL LETTER V
    (Unicode:#$FF57; Attr:laLower; CaseCode:#$FF37),   // FULLWIDTH LATIN SMALL LETTER W
    (Unicode:#$FF58; Attr:laLower; CaseCode:#$FF38),   // FULLWIDTH LATIN SMALL LETTER X
    (Unicode:#$FF59; Attr:laLower; CaseCode:#$FF39),   // FULLWIDTH LATIN SMALL LETTER Y
    (Unicode:#$FF5A; Attr:laLower; CaseCode:#$FF3A)    // FULLWIDTH LATIN SMALL LETTER Z
    );

function LocateLetterInfo(const Ch: WideChar): PUnicodeLetterInfo;
var L, H, I : Integer;
    D : WideChar;
begin
  // Binary search [Avg number of comparisons = Log2(UnicodeLetterEntries) = 10]
  L := 0;
  H := UnicodeLetterEntries - 1;
  Repeat
    I := (L + H) div 2;
    D := UnicodeLetterInfo[I].Unicode;
    if D = Ch then
      begin
        Result := @UnicodeLetterInfo[I];
        exit;
      end else
    if D > Ch then
      H := I - 1 else
      L := I + 1;
  Until L > H;
  Result := nil;
end;

function LocateOtherLowerCase(const Ch: WideChar): WideChar;
begin
  Case Ord(Ch) of
    $2170..$217F : Result := WideChar(Ord(Ch) - $2170 + $2160);    // # Nl  [16] SMALL ROMAN NUMERAL ONE..SMALL ROMAN NUMERAL ONE THOUSAND
    $24D0..$24E9 : Result := WideChar(Ord(Ch) - $24D0 + $24B6);    // # So  [26] CIRCLED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
  else
    Result := #$0000;
  end;
end;

function LocateOtherUpperCase(const Ch: WideChar): WideChar;
begin
  Case Ord(Ch) of
    $2160..$216F : Result := WideChar(Ord(Ch) - $2160 + $2170);    // # Nl  [16] ROMAN NUMERAL ONE..ROMAN NUMERAL ONE THOUSAND
    $24B6..$24CF : Result := WideChar(Ord(Ch) - $24B6 + $24D0);    // # So  [26] CIRCLED LATIN CAPITAL LETTER A..CIRCLED LATIN CAPITAL LETTER Z
  else
    Result := #$0000;
  end;
end;

function LocateFoldingTitleCase(const Ch: WideChar): WideString;
begin
  if Ord(Ch) < $00DF then
    Result := '' else
  if Ord(Ch) <= $0587 then
    Case Ord(Ch) of
      $00DF : Result := #$0053#$0073;         // # LATIN SMALL LETTER SHARP S
      $0149 : Result := #$02BC#$004E;         // # LATIN SMALL LETTER N PRECEDED BY APOSTROPHE
      $01F0 : Result := #$004A#$030C;         // # LATIN SMALL LETTER J WITH CARON
      $0390 : Result := #$0399#$0308#$0301;   // # GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS
      $03B0 : Result := #$03A5#$0308#$0301;   // # GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS
      $0587 : Result := #$0535#$0582;         // # ARMENIAN SMALL LIGATURE ECH YIWN
    else
      Result := '';
    end else
  if Ord(Ch) < $1E96 then
    Result := '' else
  if Ord(Ch) <= $1FF7 then
    Case Ord(Ch) of
      $1E96 : Result := #$0048#$0331;         // # LATIN SMALL LETTER H WITH LINE BELOW
      $1E97 : Result := #$0054#$0308;         // # LATIN SMALL LETTER T WITH DIAERESIS
      $1E98 : Result := #$0057#$030A;         // # LATIN SMALL LETTER W WITH RING ABOVE
      $1E99 : Result := #$0059#$030A;         // # LATIN SMALL LETTER Y WITH RING ABOVE
      $1E9A : Result := #$0041#$02BE;         // # LATIN SMALL LETTER A WITH RIGHT HALF RING
      $1F50 : Result := #$03A5#$0313;         // # GREEK SMALL LETTER UPSILON WITH PSILI
      $1F52 : Result := #$03A5#$0313#$0300;   // # GREEK SMALL LETTER UPSILON WITH PSILI AND VARIA
      $1F54 : Result := #$03A5#$0313#$0301;   // # GREEK SMALL LETTER UPSILON WITH PSILI AND OXIA
      $1F56 : Result := #$03A5#$0313#$0342;   // # GREEK SMALL LETTER UPSILON WITH PSILI AND PERISPOMENI
      $1FB2 : Result := #$1FBA#$0345;         // # GREEK SMALL LETTER ALPHA WITH VARIA AND YPOGEGRAMMENI
      $1FB4 : Result := #$0386#$0345;         // # GREEK SMALL LETTER ALPHA WITH OXIA AND YPOGEGRAMMENI
      $1FB6 : Result := #$0391#$0342;         // # GREEK SMALL LETTER ALPHA WITH PERISPOMENI
      $1FB7 : Result := #$0391#$0342#$0345;   // # GREEK SMALL LETTER ALPHA WITH PERISPOMENI AND YPOGEGRAMMENI
      $1FC2 : Result := #$1FCA#$0345;         // # GREEK SMALL LETTER ETA WITH VARIA AND YPOGEGRAMMENI
      $1FC4 : Result := #$0389#$0345;         // # GREEK SMALL LETTER ETA WITH OXIA AND YPOGEGRAMMENI
      $1FC6 : Result := #$0397#$0342;         // # GREEK SMALL LETTER ETA WITH PERISPOMENI
      $1FC7 : Result := #$0397#$0342#$0345;   // # GREEK SMALL LETTER ETA WITH PERISPOMENI AND YPOGEGRAMMENI
      $1FD2 : Result := #$0399#$0308#$0300;   // # GREEK SMALL LETTER IOTA WITH DIALYTIKA AND VARIA
      $1FD3 : Result := #$0399#$0308#$0301;   // # GREEK SMALL LETTER IOTA WITH DIALYTIKA AND OXIA
      $1FD6 : Result := #$0399#$0342;         // # GREEK SMALL LETTER IOTA WITH PERISPOMENI
      $1FD7 : Result := #$0399#$0308#$0342;   // # GREEK SMALL LETTER IOTA WITH DIALYTIKA AND PERISPOMENI
      $1FE2 : Result := #$03A5#$0308#$0300;   // # GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND VARIA
      $1FE3 : Result := #$03A5#$0308#$0301;   // # GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND OXIA
      $1FE4 : Result := #$03A1#$0313;         // # GREEK SMALL LETTER RHO WITH PSILI
      $1FE6 : Result := #$03A5#$0342;         // # GREEK SMALL LETTER UPSILON WITH PERISPOMENI
      $1FE7 : Result := #$03A5#$0308#$0342;   // # GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND PERISPOMENI
      $1FF2 : Result := #$1FFA#$0345;         // # GREEK SMALL LETTER OMEGA WITH VARIA AND YPOGEGRAMMENI
      $1FF4 : Result := #$038F#$0345;         // # GREEK SMALL LETTER OMEGA WITH OXIA AND YPOGEGRAMMENI
      $1FF6 : Result := #$03A9#$0342;         // # GREEK SMALL LETTER OMEGA WITH PERISPOMENI
      $1FF7 : Result := #$03A9#$0342#$0345;   // # GREEK SMALL LETTER OMEGA WITH PERISPOMENI AND YPOGEGRAMMENI
    else
      Result := '';
    end else
  if Ord(Ch) < $FB00 then
    Result := '' else
  if Ord(Ch) <= $FB17 then
    Case Ord(Ch) of
      $FB00 : Result := #$0046#$0066;         // # LATIN SMALL LIGATURE FF
      $FB01 : Result := #$0046#$0069;         // # LATIN SMALL LIGATURE FI
      $FB02 : Result := #$0046#$006C;         // # LATIN SMALL LIGATURE FL
      $FB03 : Result := #$0046#$0066#$0069;   // # LATIN SMALL LIGATURE FFI
      $FB04 : Result := #$0046#$0066#$006C;   // # LATIN SMALL LIGATURE FFL
      $FB05 : Result := #$0053#$0074;         // # LATIN SMALL LIGATURE LONG S T
      $FB06 : Result := #$0053#$0074;         // # LATIN SMALL LIGATURE ST
      $FB13 : Result := #$0544#$0576;         // # ARMENIAN SMALL LIGATURE MEN NOW
      $FB14 : Result := #$0544#$0565;         // # ARMENIAN SMALL LIGATURE MEN ECH
      $FB15 : Result := #$0544#$056B;         // # ARMENIAN SMALL LIGATURE MEN INI
      $FB16 : Result := #$054E#$0576;         // # ARMENIAN SMALL LIGATURE VEW NOW
      $FB17 : Result := #$0544#$056D;         // # ARMENIAN SMALL LIGATURE MEN XEH
    else
      Result := '';
    end else
    Result := '';
end;

function LocateFoldingUpperCase(const Ch: WideChar): WideString;
begin
  if Ord(Ch) < $00DF then
    Result := '' else
  if Ord(Ch) <= $0587 then
    Case Ord(Ch) of
      $00DF : Result := #$0053#$0053;         // # LATIN SMALL LETTER SHARP S
      $0149 : Result := #$02BC#$004E;         // # LATIN SMALL LETTER N PRECEDED BY APOSTROPHE
      $01F0 : Result := #$004A#$030C;         // # LATIN SMALL LETTER J WITH CARON
      $0390 : Result := #$0399#$0308#$0301;   // # GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS
      $03B0 : Result := #$03A5#$0308#$0301;   // # GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS
      $0587 : Result := #$0535#$0552;         // # ARMENIAN SMALL LIGATURE ECH YIWN
    else
      Result := '';
    end else
  if Ord(Ch) < $1E96 then
    Result := '' else
  if Ord(Ch) <= $1FFC then
    Case Ord(Ch) of
      $1E96 : Result := #$0048#$0331;         // # LATIN SMALL LETTER H WITH LINE BELOW
      $1E97 : Result := #$0054#$0308;         // # LATIN SMALL LETTER T WITH DIAERESIS
      $1E98 : Result := #$0057#$030A;         // # LATIN SMALL LETTER W WITH RING ABOVE
      $1E99 : Result := #$0059#$030A;         // # LATIN SMALL LETTER Y WITH RING ABOVE
      $1E9A : Result := #$0041#$02BE;         // # LATIN SMALL LETTER A WITH RIGHT HALF RING
      $1F50 : Result := #$03A5#$0313;         // # GREEK SMALL LETTER UPSILON WITH PSILI
      $1F52 : Result := #$03A5#$0313#$0300;   // # GREEK SMALL LETTER UPSILON WITH PSILI AND VARIA
      $1F54 : Result := #$03A5#$0313#$0301;   // # GREEK SMALL LETTER UPSILON WITH PSILI AND OXIA
      $1F56 : Result := #$03A5#$0313#$0342;   // # GREEK SMALL LETTER UPSILON WITH PSILI AND PERISPOMENI
      $1F80 : Result := #$1F08#$0399;         // # GREEK SMALL LETTER ALPHA WITH PSILI AND YPOGEGRAMMENI
      $1F81 : Result := #$1F09#$0399;         // # GREEK SMALL LETTER ALPHA WITH DASIA AND YPOGEGRAMMENI
      $1F82 : Result := #$1F0A#$0399;         // # GREEK SMALL LETTER ALPHA WITH PSILI AND VARIA AND YPOGEGRAMMENI
      $1F83 : Result := #$1F0B#$0399;         // # GREEK SMALL LETTER ALPHA WITH DASIA AND VARIA AND YPOGEGRAMMENI
      $1F84 : Result := #$1F0C#$0399;         // # GREEK SMALL LETTER ALPHA WITH PSILI AND OXIA AND YPOGEGRAMMENI
      $1F85 : Result := #$1F0D#$0399;         // # GREEK SMALL LETTER ALPHA WITH DASIA AND OXIA AND YPOGEGRAMMENI
      $1F86 : Result := #$1F0E#$0399;         // # GREEK SMALL LETTER ALPHA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
      $1F87 : Result := #$1F0F#$0399;         // # GREEK SMALL LETTER ALPHA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
      $1F88 : Result := #$1F08#$0399;         // # GREEK CAPITAL LETTER ALPHA WITH PSILI AND PROSGEGRAMMENI
      $1F89 : Result := #$1F09#$0399;         // # GREEK CAPITAL LETTER ALPHA WITH DASIA AND PROSGEGRAMMENI
      $1F8A : Result := #$1F0A#$0399;         // # GREEK CAPITAL LETTER ALPHA WITH PSILI AND VARIA AND PROSGEGRAMMENI
      $1F8B : Result := #$1F0B#$0399;         // # GREEK CAPITAL LETTER ALPHA WITH DASIA AND VARIA AND PROSGEGRAMMENI
      $1F8C : Result := #$1F0C#$0399;         // # GREEK CAPITAL LETTER ALPHA WITH PSILI AND OXIA AND PROSGEGRAMMENI
      $1F8D : Result := #$1F0D#$0399;         // # GREEK CAPITAL LETTER ALPHA WITH DASIA AND OXIA AND PROSGEGRAMMENI
      $1F8E : Result := #$1F0E#$0399;         // # GREEK CAPITAL LETTER ALPHA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
      $1F8F : Result := #$1F0F#$0399;         // # GREEK CAPITAL LETTER ALPHA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
      $1F90 : Result := #$1F28#$0399;         // # GREEK SMALL LETTER ETA WITH PSILI AND YPOGEGRAMMENI
      $1F91 : Result := #$1F29#$0399;         // # GREEK SMALL LETTER ETA WITH DASIA AND YPOGEGRAMMENI
      $1F92 : Result := #$1F2A#$0399;         // # GREEK SMALL LETTER ETA WITH PSILI AND VARIA AND YPOGEGRAMMENI
      $1F93 : Result := #$1F2B#$0399;         // # GREEK SMALL LETTER ETA WITH DASIA AND VARIA AND YPOGEGRAMMENI
      $1F94 : Result := #$1F2C#$0399;         // # GREEK SMALL LETTER ETA WITH PSILI AND OXIA AND YPOGEGRAMMENI
      $1F95 : Result := #$1F2D#$0399;         // # GREEK SMALL LETTER ETA WITH DASIA AND OXIA AND YPOGEGRAMMENI
      $1F96 : Result := #$1F2E#$0399;         // # GREEK SMALL LETTER ETA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
      $1F97 : Result := #$1F2F#$0399;         // # GREEK SMALL LETTER ETA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
      $1F98 : Result := #$1F28#$0399;         // # GREEK CAPITAL LETTER ETA WITH PSILI AND PROSGEGRAMMENI
      $1F99 : Result := #$1F29#$0399;         // # GREEK CAPITAL LETTER ETA WITH DASIA AND PROSGEGRAMMENI
      $1F9A : Result := #$1F2A#$0399;         // # GREEK CAPITAL LETTER ETA WITH PSILI AND VARIA AND PROSGEGRAMMENI
      $1F9B : Result := #$1F2B#$0399;         // # GREEK CAPITAL LETTER ETA WITH DASIA AND VARIA AND PROSGEGRAMMENI
      $1F9C : Result := #$1F2C#$0399;         // # GREEK CAPITAL LETTER ETA WITH PSILI AND OXIA AND PROSGEGRAMMENI
      $1F9D : Result := #$1F2D#$0399;         // # GREEK CAPITAL LETTER ETA WITH DASIA AND OXIA AND PROSGEGRAMMENI
      $1F9E : Result := #$1F2E#$0399;         // # GREEK CAPITAL LETTER ETA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
      $1F9F : Result := #$1F2F#$0399;         // # GREEK CAPITAL LETTER ETA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
      $1FA0 : Result := #$1F68#$0399;         // # GREEK SMALL LETTER OMEGA WITH PSILI AND YPOGEGRAMMENI
      $1FA1 : Result := #$1F69#$0399;         // # GREEK SMALL LETTER OMEGA WITH DASIA AND YPOGEGRAMMENI
      $1FA2 : Result := #$1F6A#$0399;         // # GREEK SMALL LETTER OMEGA WITH PSILI AND VARIA AND YPOGEGRAMMENI
      $1FA3 : Result := #$1F6B#$0399;         // # GREEK SMALL LETTER OMEGA WITH DASIA AND VARIA AND YPOGEGRAMMENI
      $1FA4 : Result := #$1F6C#$0399;         // # GREEK SMALL LETTER OMEGA WITH PSILI AND OXIA AND YPOGEGRAMMENI
      $1FA5 : Result := #$1F6D#$0399;         // # GREEK SMALL LETTER OMEGA WITH DASIA AND OXIA AND YPOGEGRAMMENI
      $1FA6 : Result := #$1F6E#$0399;         // # GREEK SMALL LETTER OMEGA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
      $1FA7 : Result := #$1F6F#$0399;         // # GREEK SMALL LETTER OMEGA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
      $1FA8 : Result := #$1F68#$0399;         // # GREEK CAPITAL LETTER OMEGA WITH PSILI AND PROSGEGRAMMENI
      $1FA9 : Result := #$1F69#$0399;         // # GREEK CAPITAL LETTER OMEGA WITH DASIA AND PROSGEGRAMMENI
      $1FAA : Result := #$1F6A#$0399;         // # GREEK CAPITAL LETTER OMEGA WITH PSILI AND VARIA AND PROSGEGRAMMENI
      $1FAB : Result := #$1F6B#$0399;         // # GREEK CAPITAL LETTER OMEGA WITH DASIA AND VARIA AND PROSGEGRAMMENI
      $1FAC : Result := #$1F6C#$0399;         // # GREEK CAPITAL LETTER OMEGA WITH PSILI AND OXIA AND PROSGEGRAMMENI
      $1FAD : Result := #$1F6D#$0399;         // # GREEK CAPITAL LETTER OMEGA WITH DASIA AND OXIA AND PROSGEGRAMMENI
      $1FAE : Result := #$1F6E#$0399;         // # GREEK CAPITAL LETTER OMEGA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
      $1FAF : Result := #$1F6F#$0399;         // # GREEK CAPITAL LETTER OMEGA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
      $1FB2 : Result := #$1FBA#$0399;         // # GREEK SMALL LETTER ALPHA WITH VARIA AND YPOGEGRAMMENI
      $1FB3 : Result := #$0391#$0399;         // # GREEK SMALL LETTER ALPHA WITH YPOGEGRAMMENI
      $1FB4 : Result := #$0386#$0399;         // # GREEK SMALL LETTER ALPHA WITH OXIA AND YPOGEGRAMMENI
      $1FB6 : Result := #$0391#$0342;         // # GREEK SMALL LETTER ALPHA WITH PERISPOMENI
      $1FB7 : Result := #$0391#$0342#$0399;   // # GREEK SMALL LETTER ALPHA WITH PERISPOMENI AND YPOGEGRAMMENI
      $1FBC : Result := #$0391#$0399;         // # GREEK CAPITAL LETTER ALPHA WITH PROSGEGRAMMENI
      $1FC2 : Result := #$1FCA#$0399;         // # GREEK SMALL LETTER ETA WITH VARIA AND YPOGEGRAMMENI
      $1FC3 : Result := #$0397#$0399;         // # GREEK SMALL LETTER ETA WITH YPOGEGRAMMENI
      $1FC4 : Result := #$0389#$0399;         // # GREEK SMALL LETTER ETA WITH OXIA AND YPOGEGRAMMENI
      $1FC6 : Result := #$0397#$0342;         // # GREEK SMALL LETTER ETA WITH PERISPOMENI
      $1FC7 : Result := #$0397#$0342#$0399;   // # GREEK SMALL LETTER ETA WITH PERISPOMENI AND YPOGEGRAMMENI
      $1FCC : Result := #$0397#$0399;         // # GREEK CAPITAL LETTER ETA WITH PROSGEGRAMMENI
      $1FD2 : Result := #$0399#$0308#$0300;   // # GREEK SMALL LETTER IOTA WITH DIALYTIKA AND VARIA
      $1FD3 : Result := #$0399#$0308#$0301;   // # GREEK SMALL LETTER IOTA WITH DIALYTIKA AND OXIA
      $1FD6 : Result := #$0399#$0342;         // # GREEK SMALL LETTER IOTA WITH PERISPOMENI
      $1FD7 : Result := #$0399#$0308#$0342;   // # GREEK SMALL LETTER IOTA WITH DIALYTIKA AND PERISPOMENI
      $1FE2 : Result := #$03A5#$0308#$0300;   // # GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND VARIA
      $1FE3 : Result := #$03A5#$0308#$0301;   // # GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND OXIA
      $1FE4 : Result := #$03A1#$0313;         // # GREEK SMALL LETTER RHO WITH PSILI
      $1FE6 : Result := #$03A5#$0342;         // # GREEK SMALL LETTER UPSILON WITH PERISPOMENI
      $1FE7 : Result := #$03A5#$0308#$0342;   // # GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND PERISPOMENI
      $1FF2 : Result := #$1FFA#$0399;         // # GREEK SMALL LETTER OMEGA WITH VARIA AND YPOGEGRAMMENI
      $1FF3 : Result := #$03A9#$0399;         // # GREEK SMALL LETTER OMEGA WITH YPOGEGRAMMENI
      $1FF4 : Result := #$038F#$0399;         // # GREEK SMALL LETTER OMEGA WITH OXIA AND YPOGEGRAMMENI
      $1FF6 : Result := #$03A9#$0342;         // # GREEK SMALL LETTER OMEGA WITH PERISPOMENI
      $1FF7 : Result := #$03A9#$0342#$0399;   // # GREEK SMALL LETTER OMEGA WITH PERISPOMENI AND YPOGEGRAMMENI
      $1FFC : Result := #$03A9#$0399;         // # GREEK CAPITAL LETTER OMEGA WITH PROSGEGRAMMENI
    else
      Result := '';
    end else
  if Ord(Ch) < $FB00 then
    Result := '' else
  if Ord(Ch) <= $FB17 then
    Case Ord(Ch) of
      $FB00 : Result := #$0046#$0046;         // # LATIN SMALL LIGATURE FF
      $FB01 : Result := #$0046#$0049;         // # LATIN SMALL LIGATURE FI
      $FB02 : Result := #$0046#$004C;         // # LATIN SMALL LIGATURE FL
      $FB03 : Result := #$0046#$0046#$0049;   // # LATIN SMALL LIGATURE FFI
      $FB04 : Result := #$0046#$0046#$004C;   // # LATIN SMALL LIGATURE FFL
      $FB05 : Result := #$0053#$0054;         // # LATIN SMALL LIGATURE LONG S T
      $FB06 : Result := #$0053#$0054;         // # LATIN SMALL LIGATURE ST
      $FB13 : Result := #$0544#$0546;         // # ARMENIAN SMALL LIGATURE MEN NOW
      $FB14 : Result := #$0544#$0535;         // # ARMENIAN SMALL LIGATURE MEN ECH
      $FB15 : Result := #$0544#$053B;         // # ARMENIAN SMALL LIGATURE MEN INI
      $FB16 : Result := #$054E#$0546;         // # ARMENIAN SMALL LIGATURE VEW NOW
      $FB17 : Result := #$0544#$053D;         // # ARMENIAN SMALL LIGATURE MEN XEH
    else
      Result := '';
    end else
    Result := '';
end;

function LocateFoldingLowerCase(const Ch: WideChar): WideString;
begin
  if Ch = #$0130 then
    Result := #$0069#$0307 else
    Result := '';
end;

function IsUpperCase(const Ch: WideChar): Boolean;
var I : PUnicodeLetterInfo;
begin
  I := LocateLetterInfo(Ch);
  if Assigned(I) then
    Result := I^.Attr = laUpper else
    Result := LocateOtherUpperCase(Ch) <> #$0000;
end;

function IsLowerCase(const Ch: WideChar): Boolean;
var I : PUnicodeLetterInfo;
begin
  I := LocateLetterInfo(Ch);
  if Assigned(I) then
    Result := I.Attr = laLower else
    Result := LocateOtherLowerCase(Ch) <> #$0000;
end;

type
  TUnicodeTitleCaseLetterInfo = packed record
    Unicode : WideChar;
    Upper   : WideChar;
    Lower   : WideChar;
  end;
  PUnicodeTitleCaseLetterInfo = ^TUnicodeTitleCaseLetterInfo;

const
  // Derived from 'Lt' class
  UnicodeTitleCaseLetterEntries = 31;
  UnicodeTitleCaseLetterInfo : Array[0..UnicodeTitleCaseLetterEntries - 1] of TUnicodeTitleCaseLetterInfo = (
    (Unicode:#$01C5; Upper:#$01C4; Lower:#$01C6),   // LATIN CAPITAL LETTER D WITH SMALL LETTER Z WITH CARON
    (Unicode:#$01C8; Upper:#$01C7; Lower:#$01C9),   // LATIN CAPITAL LETTER L WITH SMALL LETTER J
    (Unicode:#$01CB; Upper:#$01CA; Lower:#$01CC),   // LATIN CAPITAL LETTER N WITH SMALL LETTER J
    (Unicode:#$01F2; Upper:#$01F1; Lower:#$01F3),   // LATIN CAPITAL LETTER D WITH SMALL LETTER Z
    (Unicode:#$1F88; Upper:#$FFFF; Lower:#$1F80),   // GREEK CAPITAL LETTER ALPHA WITH PSILI AND PROSGEGRAMMENI
    (Unicode:#$1F89; Upper:#$FFFF; Lower:#$1F81),   // GREEK CAPITAL LETTER ALPHA WITH DASIA AND PROSGEGRAMMENI
    (Unicode:#$1F8A; Upper:#$FFFF; Lower:#$1F82),   // GREEK CAPITAL LETTER ALPHA WITH PSILI AND VARIA AND PROSGEGRAMMENI
    (Unicode:#$1F8B; Upper:#$FFFF; Lower:#$1F83),   // GREEK CAPITAL LETTER ALPHA WITH DASIA AND VARIA AND PROSGEGRAMMENI
    (Unicode:#$1F8C; Upper:#$FFFF; Lower:#$1F84),   // GREEK CAPITAL LETTER ALPHA WITH PSILI AND OXIA AND PROSGEGRAMMENI
    (Unicode:#$1F8D; Upper:#$FFFF; Lower:#$1F85),   // GREEK CAPITAL LETTER ALPHA WITH DASIA AND OXIA AND PROSGEGRAMMENI
    (Unicode:#$1F8E; Upper:#$FFFF; Lower:#$1F86),   // GREEK CAPITAL LETTER ALPHA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
    (Unicode:#$1F8F; Upper:#$FFFF; Lower:#$1F87),   // GREEK CAPITAL LETTER ALPHA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
    (Unicode:#$1F98; Upper:#$FFFF; Lower:#$1F90),   // GREEK CAPITAL LETTER ETA WITH PSILI AND PROSGEGRAMMENI
    (Unicode:#$1F99; Upper:#$FFFF; Lower:#$1F91),   // GREEK CAPITAL LETTER ETA WITH DASIA AND PROSGEGRAMMENI
    (Unicode:#$1F9A; Upper:#$FFFF; Lower:#$1F92),   // GREEK CAPITAL LETTER ETA WITH PSILI AND VARIA AND PROSGEGRAMMENI
    (Unicode:#$1F9B; Upper:#$FFFF; Lower:#$1F93),   // GREEK CAPITAL LETTER ETA WITH DASIA AND VARIA AND PROSGEGRAMMENI
    (Unicode:#$1F9C; Upper:#$FFFF; Lower:#$1F94),   // GREEK CAPITAL LETTER ETA WITH PSILI AND OXIA AND PROSGEGRAMMENI
    (Unicode:#$1F9D; Upper:#$FFFF; Lower:#$1F95),   // GREEK CAPITAL LETTER ETA WITH DASIA AND OXIA AND PROSGEGRAMMENI
    (Unicode:#$1F9E; Upper:#$FFFF; Lower:#$1F96),   // GREEK CAPITAL LETTER ETA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
    (Unicode:#$1F9F; Upper:#$FFFF; Lower:#$1F97),   // GREEK CAPITAL LETTER ETA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
    (Unicode:#$1FA8; Upper:#$FFFF; Lower:#$1FA0),   // GREEK CAPITAL LETTER OMEGA WITH PSILI AND PROSGEGRAMMENI
    (Unicode:#$1FA9; Upper:#$FFFF; Lower:#$1FA1),   // GREEK CAPITAL LETTER OMEGA WITH DASIA AND PROSGEGRAMMENI
    (Unicode:#$1FAA; Upper:#$FFFF; Lower:#$1FA2),   // GREEK CAPITAL LETTER OMEGA WITH PSILI AND VARIA AND PROSGEGRAMMENI
    (Unicode:#$1FAB; Upper:#$FFFF; Lower:#$1FA3),   // GREEK CAPITAL LETTER OMEGA WITH DASIA AND VARIA AND PROSGEGRAMMENI
    (Unicode:#$1FAC; Upper:#$FFFF; Lower:#$1FA4),   // GREEK CAPITAL LETTER OMEGA WITH PSILI AND OXIA AND PROSGEGRAMMENI
    (Unicode:#$1FAD; Upper:#$FFFF; Lower:#$1FA5),   // GREEK CAPITAL LETTER OMEGA WITH DASIA AND OXIA AND PROSGEGRAMMENI
    (Unicode:#$1FAE; Upper:#$FFFF; Lower:#$1FA6),   // GREEK CAPITAL LETTER OMEGA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
    (Unicode:#$1FAF; Upper:#$FFFF; Lower:#$1FA7),   // GREEK CAPITAL LETTER OMEGA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
    (Unicode:#$1FBC; Upper:#$FFFF; Lower:#$1FB3),   // GREEK CAPITAL LETTER ALPHA WITH PROSGEGRAMMENI
    (Unicode:#$1FCC; Upper:#$FFFF; Lower:#$1FC3),   // GREEK CAPITAL LETTER ETA WITH PROSGEGRAMMENI
    (Unicode:#$1FFC; Upper:#$FFFF; Lower:#$1FF3)    // GREEK CAPITAL LETTER OMEGA WITH PROSGEGRAMMENI
    );

function LocateTitleCaseLetterInfo(const Ch: WideChar): PUnicodeTitleCaseLetterInfo;
var I : Integer;
begin
  if (Ord(Ch) < $01C5) or (Ord(Ch) > $1FFC) then
    Result := nil else
  if (Ord(Ch) > $01F2) and (Ord(Ch) < $1F88) then
    Result := nil else
    begin
      For I := 0 to UnicodeTitleCaseLetterEntries - 1 do
        if UnicodeTitleCaseLetterInfo[I].Unicode = Ch then
          begin
            Result := @UnicodeTitleCaseLetterInfo[I];
            exit;
          end;
      Result := nil;
    end;
end;

function IsTitleCase(const Ch: WideChar): Boolean;
begin
  Result := Assigned(LocateTitleCaseLetterInfo(Ch));
end;

function WideUpCase(const Ch: WideChar): WideChar;
var I : PUnicodeLetterInfo;
    J : PUnicodeTitleCaseLetterInfo;
    C : WideChar;
begin
  if Ord(Ch) < $80 then // ASCII short-cut
    begin
      if Char(Ord(Ch)) in ['a'..'z'] then
        Result := WideChar(Ord(Ch) - (Ord('a') - Ord('A'))) else
        Result := Ch;
    end else
    begin
      I := LocateLetterInfo(Ch);
      if Assigned(I) then
        begin
          if I^.Attr = laUpper then
            Result := Ch else
            if I^.CaseCode = #$FFFF then
              Result := Ch else
              Result := I^.CaseCode;
        end else
        begin
          J := LocateTitleCaseLetterInfo(Ch);
          if Assigned(J) then
            begin
              if J^.Upper = #$FFFF then
                Result := Ch else
                Result := J^.Upper;
            end else
            begin
              C := LocateOtherLowerCase(Ch);
              if C = #$0000 then
                Result := Ch else
                Result := C;
            end;
        end;
    end;
end;

function WideUpCaseFolding(const Ch: WideChar): WideString;
var R : WideChar;
begin
  R := WideUpCase(Ch);
  if R = Ch then
    begin
      Result := LocateFoldingUpperCase(Ch);
      if Result = '' then
        Result := Ch;
    end else
    Result := R;
end;

function WideLowCase(const Ch: WideChar): WideChar;
var I : PUnicodeLetterInfo;
    J : PUnicodeTitleCaseLetterInfo;
    C : WideChar;
begin
  if Ord(Ch) < $80 then // ASCII short-cut
    begin
      if Char(Ord(Ch)) in ['A'..'Z'] then
        Result := WideChar(Ord(Ch) + (Ord('a') - Ord('A'))) else
        Result := Ch;
    end else
    begin
      I := LocateLetterInfo(Ch);
      if Assigned(I) then
        begin
          if I^.Attr = laLower then
            Result := Ch else
            if I^.CaseCode = #$FFFF then
              Result := Ch else
              Result := I^.CaseCode;
        end else
        begin
          J := LocateTitleCaseLetterInfo(Ch);
          if Assigned(J) then
            begin
              if J^.Lower = #$FFFF then
                Result := Ch else
                Result := J^.Lower;
            end else
            begin
              C := LocateOtherUpperCase(Ch);
              if C = #$0000 then
                Result := Ch else
                Result := C;
            end;
        end;
    end;
end;

function WideLowCaseFolding(const Ch: WideChar): WideString;
var R : WideChar;
begin
  R := WideLowCase(Ch);
  if R = Ch then
    begin
      Result := LocateFoldingLowerCase(Ch);
      if Result = '' then
        Result := Ch;
    end else
    Result := R;
end;

function WideTitleCaseFolding(const Ch: WideChar): WideString;
begin
  Result := LocateFoldingTitleCase(Ch);
  if Result = '' then
    Result := Ch;
end;

function WideIsEqualNoCase(const A, B: WideChar): Boolean;
var I : PUnicodeLetterInfo;
    J : PUnicodeTitleCaseLetterInfo;
    C, D : Char;
    E : WideChar;
begin
  Result := A = B;
  if Result then
    exit;
  if (Ord(A) < $80) and (Ord(B) < $80) then // ASCII short-cut
    begin
      if Char(Ord(A)) in ['A'..'Z'] then
        C := Char(Byte(Ord(A)) + (Ord('a') - Ord('A'))) else
        C := Char(Ord(A));
      if Char(Ord(B)) in ['A'..'Z'] then
        D := Char(Byte (Ord(B)) + (Ord('a') - Ord('A'))) else
        D := Char(Ord(B));
      Result := C = D;
      exit;
    end;
  I := LocateLetterInfo(A);
  if Assigned(I) then
    begin
      if I^.CaseCode = #$FFFF then
        Result := False else
        Result := I^.CaseCode = B;
      exit;
    end;
  J := LocateTitleCaseLetterInfo(A);
  if Assigned(J) then
    begin
      Result := ((J^.Upper <> #$FFFF) and (J^.Upper = B)) or
                ((J^.Lower <> #$FFFF) and (J^.Lower = B));
      exit;
    end;
  E := LocateOtherLowerCase(A);
  if E <> #$0000 then
    Result := E = B else
    Result := False;
end;

// Derived from 'Lo' class
function IsOtherLetter(const Ch: UCS4Char): Boolean;
begin
  Case Ch of
    $01BB,              //       LATIN LETTER TWO WITH STROKE
    $01C0..$01C3,       //   [4] LATIN LETTER DENTAL CLICK..LATIN LETTER RETROFLEX CLICK
    $05D0..$05EA,       //  [27] HEBREW LETTER ALEF..HEBREW LETTER TAV
    $05F0..$05F2,       //   [3] HEBREW LIGATURE YIDDISH DOUBLE VAV..HEBREW LIGATURE YIDDISH DOUBLE YOD
    $0621..$063A,       //  [26] ARABIC LETTER HAMZA..ARABIC LETTER GHAIN
    $0641..$064A,       //  [10] ARABIC LETTER FEH..ARABIC LETTER YEH
    $066E..$066F,       //   [2] ARABIC LETTER DOTLESS BEH..ARABIC LETTER DOTLESS QAF
    $0671..$06D3,       //  [99] ARABIC LETTER ALEF WASLA..ARABIC LETTER YEH BARREE WITH HAMZA ABOVE
    $06D5,              //       ARABIC LETTER AE
    $06FA..$06FC,       //   [3] ARABIC LETTER SHEEN WITH DOT BELOW..ARABIC LETTER GHAIN WITH DOT BELOW
    $0710,              //       SYRIAC LETTER ALAPH
    $0712..$072C,       //  [27] SYRIAC LETTER BETH..SYRIAC LETTER TAW
    $0780..$07A5,       //  [38] THAANA LETTER HAA..THAANA LETTER WAAVU
    $07B1,              //       THAANA LETTER NAA
    $0905..$0939,       //  [53] DEVANAGARI LETTER A..DEVANAGARI LETTER HA
    $093D,              //       DEVANAGARI SIGN AVAGRAHA
    $0950,              //       DEVANAGARI OM
    $0958..$0961,       //  [10] DEVANAGARI LETTER QA..DEVANAGARI LETTER VOCALIC LL
    $0985..$098C,       //   [8] BENGALI LETTER A..BENGALI LETTER VOCALIC L
    $098F..$0990,       //   [2] BENGALI LETTER E..BENGALI LETTER AI
    $0993..$09A8,       //  [22] BENGALI LETTER O..BENGALI LETTER NA
    $09AA..$09B0,       //   [7] BENGALI LETTER PA..BENGALI LETTER RA
    $09B2,              //       BENGALI LETTER LA
    $09B6..$09B9,       //   [4] BENGALI LETTER SHA..BENGALI LETTER HA
    $09DC..$09DD,       //   [2] BENGALI LETTER RRA..BENGALI LETTER RHA
    $09DF..$09E1,       //   [3] BENGALI LETTER YYA..BENGALI LETTER VOCALIC LL
    $09F0..$09F1,       //   [2] BENGALI LETTER RA WITH MIDDLE DIAGONAL..BENGALI LETTER RA WITH LOWER DIAGONAL
    $0A05..$0A0A,       //   [6] GURMUKHI LETTER A..GURMUKHI LETTER UU
    $0A0F..$0A10,       //   [2] GURMUKHI LETTER EE..GURMUKHI LETTER AI
    $0A13..$0A28,       //  [22] GURMUKHI LETTER OO..GURMUKHI LETTER NA
    $0A2A..$0A30,       //   [7] GURMUKHI LETTER PA..GURMUKHI LETTER RA
    $0A32..$0A33,       //   [2] GURMUKHI LETTER LA..GURMUKHI LETTER LLA
    $0A35..$0A36,       //   [2] GURMUKHI LETTER VA..GURMUKHI LETTER SHA
    $0A38..$0A39,       //   [2] GURMUKHI LETTER SA..GURMUKHI LETTER HA
    $0A59..$0A5C,       //   [4] GURMUKHI LETTER KHHA..GURMUKHI LETTER RRA
    $0A5E,              //       GURMUKHI LETTER FA
    $0A72..$0A74,       //   [3] GURMUKHI IRI..GURMUKHI EK ONKAR
    $0A85..$0A8B,       //   [7] GUJARATI LETTER A..GUJARATI LETTER VOCALIC R
    $0A8D,              //       GUJARATI VOWEL CANDRA E
    $0A8F..$0A91,       //   [3] GUJARATI LETTER E..GUJARATI VOWEL CANDRA O
    $0A93..$0AA8,       //  [22] GUJARATI LETTER O..GUJARATI LETTER NA
    $0AAA..$0AB0,       //   [7] GUJARATI LETTER PA..GUJARATI LETTER RA
    $0AB2..$0AB3,       //   [2] GUJARATI LETTER LA..GUJARATI LETTER LLA
    $0AB5..$0AB9,       //   [5] GUJARATI LETTER VA..GUJARATI LETTER HA
    $0ABD,              //       GUJARATI SIGN AVAGRAHA
    $0AD0,              //       GUJARATI OM
    $0AE0,              //       GUJARATI LETTER VOCALIC RR
    $0B05..$0B0C,       //   [8] ORIYA LETTER A..ORIYA LETTER VOCALIC L
    $0B0F..$0B10,       //   [2] ORIYA LETTER E..ORIYA LETTER AI
    $0B13..$0B28,       //  [22] ORIYA LETTER O..ORIYA LETTER NA
    $0B2A..$0B30,       //   [7] ORIYA LETTER PA..ORIYA LETTER RA
    $0B32..$0B33,       //   [2] ORIYA LETTER LA..ORIYA LETTER LLA
    $0B36..$0B39,       //   [4] ORIYA LETTER SHA..ORIYA LETTER HA
    $0B3D,              //       ORIYA SIGN AVAGRAHA
    $0B5C..$0B5D,       //   [2] ORIYA LETTER RRA..ORIYA LETTER RHA
    $0B5F..$0B61,       //   [3] ORIYA LETTER YYA..ORIYA LETTER VOCALIC LL
    $0B83,              //       TAMIL SIGN VISARGA
    $0B85..$0B8A,       //   [6] TAMIL LETTER A..TAMIL LETTER UU
    $0B8E..$0B90,       //   [3] TAMIL LETTER E..TAMIL LETTER AI
    $0B92..$0B95,       //   [4] TAMIL LETTER O..TAMIL LETTER KA
    $0B99..$0B9A,       //   [2] TAMIL LETTER NGA..TAMIL LETTER CA
    $0B9C,              //       TAMIL LETTER JA
    $0B9E..$0B9F,       //   [2] TAMIL LETTER NYA..TAMIL LETTER TTA
    $0BA3..$0BA4,       //   [2] TAMIL LETTER NNA..TAMIL LETTER TA
    $0BA8..$0BAA,       //   [3] TAMIL LETTER NA..TAMIL LETTER PA
    $0BAE..$0BB5,       //   [8] TAMIL LETTER MA..TAMIL LETTER VA
    $0BB7..$0BB9,       //   [3] TAMIL LETTER SSA..TAMIL LETTER HA
    $0C05..$0C0C,       //   [8] TELUGU LETTER A..TELUGU LETTER VOCALIC L
    $0C0E..$0C10,       //   [3] TELUGU LETTER E..TELUGU LETTER AI
    $0C12..$0C28,       //  [23] TELUGU LETTER O..TELUGU LETTER NA
    $0C2A..$0C33,       //  [10] TELUGU LETTER PA..TELUGU LETTER LLA
    $0C35..$0C39,       //   [5] TELUGU LETTER VA..TELUGU LETTER HA
    $0C60..$0C61,       //   [2] TELUGU LETTER VOCALIC RR..TELUGU LETTER VOCALIC LL
    $0C85..$0C8C,       //   [8] KANNADA LETTER A..KANNADA LETTER VOCALIC L
    $0C8E..$0C90,       //   [3] KANNADA LETTER E..KANNADA LETTER AI
    $0C92..$0CA8,       //  [23] KANNADA LETTER O..KANNADA LETTER NA
    $0CAA..$0CB3,       //  [10] KANNADA LETTER PA..KANNADA LETTER LLA
    $0CB5..$0CB9,       //   [5] KANNADA LETTER VA..KANNADA LETTER HA
    $0CDE,              //       KANNADA LETTER FA
    $0CE0..$0CE1,       //   [2] KANNADA LETTER VOCALIC RR..KANNADA LETTER VOCALIC LL
    $0D05..$0D0C,       //   [8] MALAYALAM LETTER A..MALAYALAM LETTER VOCALIC L
    $0D0E..$0D10,       //   [3] MALAYALAM LETTER E..MALAYALAM LETTER AI
    $0D12..$0D28,       //  [23] MALAYALAM LETTER O..MALAYALAM LETTER NA
    $0D2A..$0D39,       //  [16] MALAYALAM LETTER PA..MALAYALAM LETTER HA
    $0D60..$0D61,       //   [2] MALAYALAM LETTER VOCALIC RR..MALAYALAM LETTER VOCALIC LL
    $0D85..$0D96,       //  [18] SINHALA LETTER AYANNA..SINHALA LETTER AUYANNA
    $0D9A..$0DB1,       //  [24] SINHALA LETTER ALPAPRAANA KAYANNA..SINHALA LETTER DANTAJA NAYANNA
    $0DB3..$0DBB,       //   [9] SINHALA LETTER SANYAKA DAYANNA..SINHALA LETTER RAYANNA
    $0DBD,              //       SINHALA LETTER DANTAJA LAYANNA
    $0DC0..$0DC6,       //   [7] SINHALA LETTER VAYANNA..SINHALA LETTER FAYANNA
    $0E01..$0E30,       //  [48] THAI CHARACTER KO KAI..THAI CHARACTER SARA A
    $0E32..$0E33,       //   [2] THAI CHARACTER SARA AA..THAI CHARACTER SARA AM
    $0E40..$0E45,       //   [6] THAI CHARACTER SARA E..THAI CHARACTER LAKKHANGYAO
    $0E81..$0E82,       //   [2] LAO LETTER KO..LAO LETTER KHO SUNG
    $0E84,              //       LAO LETTER KHO TAM
    $0E87..$0E88,       //   [2] LAO LETTER NGO..LAO LETTER CO
    $0E8A,              //       LAO LETTER SO TAM
    $0E8D,              //       LAO LETTER NYO
    $0E94..$0E97,       //   [4] LAO LETTER DO..LAO LETTER THO TAM
    $0E99..$0E9F,       //   [7] LAO LETTER NO..LAO LETTER FO SUNG
    $0EA1..$0EA3,       //   [3] LAO LETTER MO..LAO LETTER LO LING
    $0EA5,              //       LAO LETTER LO LOOT
    $0EA7,              //       LAO LETTER WO
    $0EAA..$0EAB,       //   [2] LAO LETTER SO SUNG..LAO LETTER HO SUNG
    $0EAD..$0EB0,       //   [4] LAO LETTER O..LAO VOWEL SIGN A
    $0EB2..$0EB3,       //   [2] LAO VOWEL SIGN AA..LAO VOWEL SIGN AM
    $0EBD,              //       LAO SEMIVOWEL SIGN NYO
    $0EC0..$0EC4,       //   [5] LAO VOWEL SIGN E..LAO VOWEL SIGN AI
    $0EDC..$0EDD,       //   [2] LAO HO NO..LAO HO MO
    $0F00,              //       TIBETAN SYLLABLE OM
    $0F40..$0F47,       //   [8] TIBETAN LETTER KA..TIBETAN LETTER JA
    $0F49..$0F6A,       //  [34] TIBETAN LETTER NYA..TIBETAN LETTER FIXED-FORM RA
    $0F88..$0F8B,       //   [4] TIBETAN SIGN LCE TSA CAN..TIBETAN SIGN GRU MED RGYINGS
    $1000..$1021,       //  [34] MYANMAR LETTER KA..MYANMAR LETTER A
    $1023..$1027,       //   [5] MYANMAR LETTER I..MYANMAR LETTER E
    $1029..$102A,       //   [2] MYANMAR LETTER O..MYANMAR LETTER AU
    $1050..$1055,       //   [6] MYANMAR LETTER SHA..MYANMAR LETTER VOCALIC LL
    $10D0..$10F8,       //  [41] GEORGIAN LETTER AN..GEORGIAN LETTER ELIFI
    $1100..$1159,       //  [90] HANGUL CHOSEONG KIYEOK..HANGUL CHOSEONG YEORINHIEUH
    $115F..$11A2,       //  [68] HANGUL CHOSEONG FILLER..HANGUL JUNGSEONG SSANGARAEA
    $11A8..$11F9,       //  [82] HANGUL JONGSEONG KIYEOK..HANGUL JONGSEONG YEORINHIEUH
    $1200..$1206,       //   [7] ETHIOPIC SYLLABLE HA..ETHIOPIC SYLLABLE HO
    $1208..$1246,       //  [63] ETHIOPIC SYLLABLE LA..ETHIOPIC SYLLABLE QO
    $1248,              //       ETHIOPIC SYLLABLE QWA
    $124A..$124D,       //   [4] ETHIOPIC SYLLABLE QWI..ETHIOPIC SYLLABLE QWE
    $1250..$1256,       //   [7] ETHIOPIC SYLLABLE QHA..ETHIOPIC SYLLABLE QHO
    $1258,              //       ETHIOPIC SYLLABLE QHWA
    $125A..$125D,       //   [4] ETHIOPIC SYLLABLE QHWI..ETHIOPIC SYLLABLE QHWE
    $1260..$1286,       //  [39] ETHIOPIC SYLLABLE BA..ETHIOPIC SYLLABLE XO
    $1288,              //       ETHIOPIC SYLLABLE XWA
    $128A..$128D,       //   [4] ETHIOPIC SYLLABLE XWI..ETHIOPIC SYLLABLE XWE
    $1290..$12AE,       //  [31] ETHIOPIC SYLLABLE NA..ETHIOPIC SYLLABLE KO
    $12B0,              //       ETHIOPIC SYLLABLE KWA
    $12B2..$12B5,       //   [4] ETHIOPIC SYLLABLE KWI..ETHIOPIC SYLLABLE KWE
    $12B8..$12BE,       //   [7] ETHIOPIC SYLLABLE KXA..ETHIOPIC SYLLABLE KXO
    $12C0,              //       ETHIOPIC SYLLABLE KXWA
    $12C2..$12C5,       //   [4] ETHIOPIC SYLLABLE KXWI..ETHIOPIC SYLLABLE KXWE
    $12C8..$12CE,       //   [7] ETHIOPIC SYLLABLE WA..ETHIOPIC SYLLABLE WO
    $12D0..$12D6,       //   [7] ETHIOPIC SYLLABLE PHARYNGEAL A..ETHIOPIC SYLLABLE PHARYNGEAL O
    $12D8..$12EE,       //  [23] ETHIOPIC SYLLABLE ZA..ETHIOPIC SYLLABLE YO
    $12F0..$130E,       //  [31] ETHIOPIC SYLLABLE DA..ETHIOPIC SYLLABLE GO
    $1310,              //       ETHIOPIC SYLLABLE GWA
    $1312..$1315,       //   [4] ETHIOPIC SYLLABLE GWI..ETHIOPIC SYLLABLE GWE
    $1318..$131E,       //   [7] ETHIOPIC SYLLABLE GGA..ETHIOPIC SYLLABLE GGO
    $1320..$1346,       //  [39] ETHIOPIC SYLLABLE THA..ETHIOPIC SYLLABLE TZO
    $1348..$135A,       //  [19] ETHIOPIC SYLLABLE FA..ETHIOPIC SYLLABLE FYA
    $13A0..$13F4,       //  [85] CHEROKEE LETTER A..CHEROKEE LETTER YV
    $1401..$166C,       // [620] CANADIAN SYLLABICS E..CANADIAN SYLLABICS CARRIER TTSA
    $166F..$1676,       //   [8] CANADIAN SYLLABICS QAI..CANADIAN SYLLABICS NNGAA
    $1681..$169A,       //  [26] OGHAM LETTER BEITH..OGHAM LETTER PEITH
    $16A0..$16EA,       //  [75] RUNIC LETTER FEHU FEOH FE F..RUNIC LETTER X
    $1700..$170C,       //  [13] TAGALOG LETTER A..TAGALOG LETTER YA
    $170E..$1711,       //   [4] TAGALOG LETTER LA..TAGALOG LETTER HA
    $1720..$1731,       //  [18] HANUNOO LETTER A..HANUNOO LETTER HA
    $1740..$1751,       //  [18] BUHID LETTER A..BUHID LETTER HA
    $1760..$176C,       //  [13] TAGBANWA LETTER A..TAGBANWA LETTER YA
    $176E..$1770,       //   [3] TAGBANWA LETTER LA..TAGBANWA LETTER SA
    $1780..$17B3,       //  [52] KHMER LETTER KA..KHMER INDEPENDENT VOWEL QAU
    $17DC,              //       KHMER SIGN AVAKRAHASANYA
    $1820..$1842,       //  [35] MONGOLIAN LETTER A..MONGOLIAN LETTER CHI
    $1844..$1877,       //  [52] MONGOLIAN LETTER TODO E..MONGOLIAN LETTER MANCHU ZHA
    $1880..$18A8,       //  [41] MONGOLIAN LETTER ALI GALI ANUSVARA ONE..MONGOLIAN LETTER MANCHU ALI GALI BHA
    $2135..$2138,       //   [4] ALEF SYMBOL..DALET SYMBOL
    $3006,              //       IDEOGRAPHIC CLOSING MARK
    $303C,              //       MASU MARK
    $3041..$3096,       //  [86] HIRAGANA LETTER SMALL A..HIRAGANA LETTER SMALL KE
    $309F,              //       HIRAGANA DIGRAPH YORI
    $30A1..$30FA,       //  [90] KATAKANA LETTER SMALL A..KATAKANA LETTER VO
    $30FF,              //       KATAKANA DIGRAPH KOTO
    $3105..$312C,       //  [40] BOPOMOFO LETTER B..BOPOMOFO LETTER GN
    $3131..$318E,       //  [94] HANGUL LETTER KIYEOK..HANGUL LETTER ARAEAE
    $31A0..$31B7,       //  [24] BOPOMOFO LETTER BU..BOPOMOFO FINAL LETTER H
    $31F0..$31FF,       //  [16] KATAKANA LETTER SMALL KU..KATAKANA LETTER SMALL RO
    $3400..$4DB5,       // [6582] CJK UNIFIED IDEOGRAPH-3400..CJK UNIFIED IDEOGRAPH-4DB5
    $4E00..$9FA5,       // [20902] CJK UNIFIED IDEOGRAPH-4E00..CJK UNIFIED IDEOGRAPH-9FA5
    $A000..$A48C,       // [1165] YI SYLLABLE IT..YI SYLLABLE YYR
    $AC00..$D7A3,       // [11172] HANGUL SYLLABLE GA..HANGUL SYLLABLE HIH
    $F900..$FA2D,       // [302] CJK COMPATIBILITY IDEOGRAPH-F900..CJK COMPATIBILITY IDEOGRAPH-FA2D
    $FA30..$FA6A,       //  [59] CJK COMPATIBILITY IDEOGRAPH-FA30..CJK COMPATIBILITY IDEOGRAPH-FA6A
    $FB1D,              //       HEBREW LETTER YOD WITH HIRIQ
    $FB1F..$FB28,       //  [10] HEBREW LIGATURE YIDDISH YOD YOD PATAH..HEBREW LETTER WIDE TAV
    $FB2A..$FB36,       //  [13] HEBREW LETTER SHIN WITH SHIN DOT..HEBREW LETTER ZAYIN WITH DAGESH
    $FB38..$FB3C,       //   [5] HEBREW LETTER TET WITH DAGESH..HEBREW LETTER LAMED WITH DAGESH
    $FB3E,              //       HEBREW LETTER MEM WITH DAGESH
    $FB40..$FB41,       //   [2] HEBREW LETTER NUN WITH DAGESH..HEBREW LETTER SAMEKH WITH DAGESH
    $FB43..$FB44,       //   [2] HEBREW LETTER FINAL PE WITH DAGESH..HEBREW LETTER PE WITH DAGESH
    $FB46..$FBB1,       // [108] HEBREW LETTER TSADI WITH DAGESH..ARABIC LETTER YEH BARREE WITH HAMZA ABOVE FINAL FORM
    $FBD3..$FD3D,       // [363] ARABIC LETTER NG ISOLATED FORM..ARABIC LIGATURE ALEF WITH FATHATAN ISOLATED FORM
    $FD50..$FD8F,       //  [64] ARABIC LIGATURE TEH WITH JEEM WITH MEEM INITIAL FORM..ARABIC LIGATURE MEEM WITH KHAH WITH MEEM INITIAL FORM
    $FD92..$FDC7,       //  [54] ARABIC LIGATURE MEEM WITH JEEM WITH KHAH INITIAL FORM..ARABIC LIGATURE NOON WITH JEEM WITH YEH FINAL FORM
    $FDF0..$FDFB,       //  [12] ARABIC LIGATURE SALLA USED AS KORANIC STOP SIGN ISOLATED FORM..ARABIC LIGATURE JALLAJALALOUHOU
    $FE70..$FE74,       //   [5] ARABIC FATHATAN ISOLATED FORM..ARABIC KASRATAN ISOLATED FORM
    $FE76..$FEFC,       // [135] ARABIC FATHA ISOLATED FORM..ARABIC LIGATURE LAM WITH ALEF FINAL FORM
    $FF66..$FF6F,       //  [10] HALFWIDTH KATAKANA LETTER WO..HALFWIDTH KATAKANA LETTER SMALL TU
    $FF71..$FF9D,       //  [45] HALFWIDTH KATAKANA LETTER A..HALFWIDTH KATAKANA LETTER N
    $FFA0..$FFBE,       //  [31] HALFWIDTH HANGUL FILLER..HALFWIDTH HANGUL LETTER HIEUH
    $FFC2..$FFC7,       //   [6] HALFWIDTH HANGUL LETTER A..HALFWIDTH HANGUL LETTER E
    $FFCA..$FFCF,       //   [6] HALFWIDTH HANGUL LETTER YEO..HALFWIDTH HANGUL LETTER OE
    $FFD2..$FFD7,       //   [6] HALFWIDTH HANGUL LETTER YO..HALFWIDTH HANGUL LETTER YU
    $FFDA..$FFDC,       //   [3] HALFWIDTH HANGUL LETTER EU..HALFWIDTH HANGUL LETTER I
    $10300..$1031E,     //  [31] OLD ITALIC LETTER A..OLD ITALIC LETTER UU
    $10330..$10349,     //  [26] GOTHIC LETTER AHSA..GOTHIC LETTER OTHAL
    $20000..$2A6D6,     // [42711] CJK UNIFIED IDEOGRAPH-20000..CJK UNIFIED IDEOGRAPH-2A6D6
    $2F800..$2FA1D :    // [542] CJK COMPATIBILITY IDEOGRAPH-2F800..CJK COMPATIBILITY IDEOGRAPH-2FA1D
      Result := True;
  else
    Result := False;
  end;
end;

function IsLetter(const Ch: WideChar): Boolean;
begin
  if Ord(Ch) < $80 then // ASCII short-cut
    Result := Char(Ord(Ch)) in ['A'..'Z', 'a'..'z'] else
    begin
      Result := Assigned(LocateLetterInfo(Ch));
      if Result then
        exit;
      Result := Assigned(LocateTitleCaseLetterInfo(Ch));
      if Result then
        exit;
      Result := IsOtherLetter(Ord(Ch));
    end;
end;

function IsAlphabetic(const Ch: WideChar): Boolean;
begin
  Result := IsLetter(Ch);
  if Result then
    exit;
  Case Ord(Ch) of
    $02B0..$02B8,   // # Lm   [9] MODIFIER LETTER SMALL H..MODIFIER LETTER SMALL Y
    $02BB..$02C1,   // # Lm   [7] MODIFIER LETTER TURNED COMMA..MODIFIER LETTER REVERSED GLOTTAL STOP
    $02D0..$02D1,   // # Lm   [2] MODIFIER LETTER TRIANGULAR COLON..MODIFIER LETTER HALF TRIANGULAR COLON
    $02E0..$02E4,   // # Lm   [5] MODIFIER LETTER SMALL GAMMA..MODIFIER LETTER SMALL REVERSED GLOTTAL STOP
    $02EE,          // # Lm       MODIFIER LETTER DOUBLE APOSTROPHE
    $0345,          // # Mn       COMBINING GREEK YPOGEGRAMMENI
    $037A,          // # Lm       GREEK YPOGEGRAMMENI
    $0559,          // # Lm       ARMENIAN MODIFIER LETTER LEFT HALF RING
    $05B0..$05B9,   // # Mn  [10] HEBREW POINT SHEVA..HEBREW POINT HOLAM
    $05BB..$05BD,   // # Mn   [3] HEBREW POINT QUBUTS..HEBREW POINT METEG
    $05BF,          // # Mn       HEBREW POINT RAFE
    $05C1..$05C2,   // # Mn   [2] HEBREW POINT SHIN DOT..HEBREW POINT SIN DOT
    $05C4,          // # Mn       HEBREW MARK UPPER DOT
    $0640,          // # Lm       ARABIC TATWEEL
    $064B..$0655,   // # Mn  [11] ARABIC FATHATAN..ARABIC HAMZA BELOW
    $0670,          // # Mn       ARABIC LETTER SUPERSCRIPT ALEF
    $06D6..$06DC,   // # Mn   [7] ARABIC SMALL HIGH LIGATURE SAD WITH LAM WITH ALEF MAKSURA..ARABIC SMALL HIGH SEEN
    $06E1..$06E4,   // # Mn   [4] ARABIC SMALL HIGH DOTLESS HEAD OF KHAH..ARABIC SMALL HIGH MADDA
    $06E5..$06E6,   // # Lm   [2] ARABIC SMALL WAW..ARABIC SMALL YEH
    $06E7..$06E8,   // # Mn   [2] ARABIC SMALL HIGH YEH..ARABIC SMALL HIGH NOON
    $06ED,          // # Mn       ARABIC SMALL LOW MEEM
    $0711,          // # Mn       SYRIAC LETTER SUPERSCRIPT ALAPH
    $0730..$073F,   // # Mn  [16] SYRIAC PTHAHA ABOVE..SYRIAC RWAHA
    $07A6..$07B0,   // # Mn  [11] THAANA ABAFILI..THAANA SUKUN
    $0901..$0902,   // # Mn   [2] DEVANAGARI SIGN CANDRABINDU..DEVANAGARI SIGN ANUSVARA
    $0903,          // # Mc       DEVANAGARI SIGN VISARGA
    $093E..$0940,   // # Mc   [3] DEVANAGARI VOWEL SIGN AA..DEVANAGARI VOWEL SIGN II
    $0941..$0948,   // # Mn   [8] DEVANAGARI VOWEL SIGN U..DEVANAGARI VOWEL SIGN AI
    $0949..$094C,   // # Mc   [4] DEVANAGARI VOWEL SIGN CANDRA O..DEVANAGARI VOWEL SIGN AU
    $0962..$0963,   // # Mn   [2] DEVANAGARI VOWEL SIGN VOCALIC L..DEVANAGARI VOWEL SIGN VOCALIC LL
    $0981,          // # Mn       BENGALI SIGN CANDRABINDU
    $0982..$0983,   // # Mc   [2] BENGALI SIGN ANUSVARA..BENGALI SIGN VISARGA
    $09BE..$09C0,   // # Mc   [3] BENGALI VOWEL SIGN AA..BENGALI VOWEL SIGN II
    $09C1..$09C4,   // # Mn   [4] BENGALI VOWEL SIGN U..BENGALI VOWEL SIGN VOCALIC RR
    $09C7..$09C8,   // # Mc   [2] BENGALI VOWEL SIGN E..BENGALI VOWEL SIGN AI
    $09CB..$09CC,   // # Mc   [2] BENGALI VOWEL SIGN O..BENGALI VOWEL SIGN AU
    $09D7,          // # Mc       BENGALI AU LENGTH MARK
    $09E2..$09E3,   // # Mn   [2] BENGALI VOWEL SIGN VOCALIC L..BENGALI VOWEL SIGN VOCALIC LL
    $0A02,          // # Mn       GURMUKHI SIGN BINDI
    $0A3E..$0A40,   // # Mc   [3] GURMUKHI VOWEL SIGN AA..GURMUKHI VOWEL SIGN II
    $0A41..$0A42,   // # Mn   [2] GURMUKHI VOWEL SIGN U..GURMUKHI VOWEL SIGN UU
    $0A47..$0A48,   // # Mn   [2] GURMUKHI VOWEL SIGN EE..GURMUKHI VOWEL SIGN AI
    $0A4B..$0A4C,   // # Mn   [2] GURMUKHI VOWEL SIGN OO..GURMUKHI VOWEL SIGN AU
    $0A70..$0A71,   // # Mn   [2] GURMUKHI TIPPI..GURMUKHI ADDAK
    $0A81..$0A82,   // # Mn   [2] GUJARATI SIGN CANDRABINDU..GUJARATI SIGN ANUSVARA
    $0A83,          // # Mc       GUJARATI SIGN VISARGA
    $0ABE..$0AC0,   // # Mc   [3] GUJARATI VOWEL SIGN AA..GUJARATI VOWEL SIGN II
    $0AC1..$0AC5,   // # Mn   [5] GUJARATI VOWEL SIGN U..GUJARATI VOWEL SIGN CANDRA E
    $0AC7..$0AC8,   // # Mn   [2] GUJARATI VOWEL SIGN E..GUJARATI VOWEL SIGN AI
    $0AC9,          // # Mc       GUJARATI VOWEL SIGN CANDRA O
    $0ACB..$0ACC,   // # Mc   [2] GUJARATI VOWEL SIGN O..GUJARATI VOWEL SIGN AU
    $0B01,          // # Mn       ORIYA SIGN CANDRABINDU
    $0B02..$0B03,   // # Mc   [2] ORIYA SIGN ANUSVARA..ORIYA SIGN VISARGA
    $0B3E,          // # Mc       ORIYA VOWEL SIGN AA
    $0B3F,          // # Mn       ORIYA VOWEL SIGN I
    $0B40,          // # Mc       ORIYA VOWEL SIGN II
    $0B41..$0B43,   // # Mn   [3] ORIYA VOWEL SIGN U..ORIYA VOWEL SIGN VOCALIC R
    $0B47..$0B48,   // # Mc   [2] ORIYA VOWEL SIGN E..ORIYA VOWEL SIGN AI
    $0B4B..$0B4C,   // # Mc   [2] ORIYA VOWEL SIGN O..ORIYA VOWEL SIGN AU
    $0B56,          // # Mn       ORIYA AI LENGTH MARK
    $0B57,          // # Mc       ORIYA AU LENGTH MARK
    $0B82,          // # Mn       TAMIL SIGN ANUSVARA
    $0BBE..$0BBF,   // # Mc   [2] TAMIL VOWEL SIGN AA..TAMIL VOWEL SIGN I
    $0BC0,          // # Mn       TAMIL VOWEL SIGN II
    $0BC1..$0BC2,   // # Mc   [2] TAMIL VOWEL SIGN U..TAMIL VOWEL SIGN UU
    $0BC6..$0BC8,   // # Mc   [3] TAMIL VOWEL SIGN E..TAMIL VOWEL SIGN AI
    $0BCA..$0BCC,   // # Mc   [3] TAMIL VOWEL SIGN O..TAMIL VOWEL SIGN AU
    $0BD7,          // # Mc       TAMIL AU LENGTH MARK
    $0C01..$0C03,   // # Mc   [3] TELUGU SIGN CANDRABINDU..TELUGU SIGN VISARGA
    $0C3E..$0C40,   // # Mn   [3] TELUGU VOWEL SIGN AA..TELUGU VOWEL SIGN II
    $0C41..$0C44,   // # Mc   [4] TELUGU VOWEL SIGN U..TELUGU VOWEL SIGN VOCALIC RR
    $0C46..$0C48,   // # Mn   [3] TELUGU VOWEL SIGN E..TELUGU VOWEL SIGN AI
    $0C4A..$0C4C,   // # Mn   [3] TELUGU VOWEL SIGN O..TELUGU VOWEL SIGN AU
    $0C55..$0C56,   // # Mn   [2] TELUGU LENGTH MARK..TELUGU AI LENGTH MARK
    $0C82..$0C83,   // # Mc   [2] KANNADA SIGN ANUSVARA..KANNADA SIGN VISARGA
    $0CBE,          // # Mc       KANNADA VOWEL SIGN AA
    $0CBF,          // # Mn       KANNADA VOWEL SIGN I
    $0CC0..$0CC4,   // # Mc   [5] KANNADA VOWEL SIGN II..KANNADA VOWEL SIGN VOCALIC RR
    $0CC6,          // # Mn       KANNADA VOWEL SIGN E
    $0CC7..$0CC8,   // # Mc   [2] KANNADA VOWEL SIGN EE..KANNADA VOWEL SIGN AI
    $0CCA..$0CCB,   // # Mc   [2] KANNADA VOWEL SIGN O..KANNADA VOWEL SIGN OO
    $0CCC,          // # Mn       KANNADA VOWEL SIGN AU
    $0CD5..$0CD6,   // # Mc   [2] KANNADA LENGTH MARK..KANNADA AI LENGTH MARK
    $0D02..$0D03,   // # Mc   [2] MALAYALAM SIGN ANUSVARA..MALAYALAM SIGN VISARGA
    $0D3E..$0D40,   // # Mc   [3] MALAYALAM VOWEL SIGN AA..MALAYALAM VOWEL SIGN II
    $0D41..$0D43,   // # Mn   [3] MALAYALAM VOWEL SIGN U..MALAYALAM VOWEL SIGN VOCALIC R
    $0D46..$0D48,   // # Mc   [3] MALAYALAM VOWEL SIGN E..MALAYALAM VOWEL SIGN AI
    $0D4A..$0D4C,   // # Mc   [3] MALAYALAM VOWEL SIGN O..MALAYALAM VOWEL SIGN AU
    $0D57,          // # Mc       MALAYALAM AU LENGTH MARK
    $0D82..$0D83,   // # Mc   [2] SINHALA SIGN ANUSVARAYA..SINHALA SIGN VISARGAYA
    $0DCF..$0DD1,   // # Mc   [3] SINHALA VOWEL SIGN AELA-PILLA..SINHALA VOWEL SIGN DIGA AEDA-PILLA
    $0DD2..$0DD4,   // # Mn   [3] SINHALA VOWEL SIGN KETTI IS-PILLA..SINHALA VOWEL SIGN KETTI PAA-PILLA
    $0DD6,          // # Mn       SINHALA VOWEL SIGN DIGA PAA-PILLA
    $0DD8..$0DDF,   // # Mc   [8] SINHALA VOWEL SIGN GAETTA-PILLA..SINHALA VOWEL SIGN GAYANUKITTA
    $0DF2..$0DF3,   // # Mc   [2] SINHALA VOWEL SIGN DIGA GAETTA-PILLA..SINHALA VOWEL SIGN DIGA GAYANUKITTA
    $0E31,          // # Mn       THAI CHARACTER MAI HAN-AKAT
    $0E34..$0E3A,   // # Mn   [7] THAI CHARACTER SARA I..THAI CHARACTER PHINTHU
    $0E46,          // # Lm       THAI CHARACTER MAIYAMOK
    $0E4D,          // # Mn       THAI CHARACTER NIKHAHIT
    $0EB1,          // # Mn       LAO VOWEL SIGN MAI KAN
    $0EB4..$0EB9,   // # Mn   [6] LAO VOWEL SIGN I..LAO VOWEL SIGN UU
    $0EBB..$0EBC,   // # Mn   [2] LAO VOWEL SIGN MAI KON..LAO SEMIVOWEL SIGN LO
    $0EC6,          // # Lm       LAO KO LA
    $0ECD,          // # Mn       LAO NIGGAHITA
    $0F71..$0F7E,   // # Mn  [14] TIBETAN VOWEL SIGN AA..TIBETAN SIGN RJES SU NGA RO
    $0F7F,          // # Mc       TIBETAN SIGN RNAM BCAD
    $0F80..$0F81,   // # Mn   [2] TIBETAN VOWEL SIGN REVERSED I..TIBETAN VOWEL SIGN REVERSED II
    $0F90..$0F97,   // # Mn   [8] TIBETAN SUBJOINED LETTER KA..TIBETAN SUBJOINED LETTER JA
    $0F99..$0FBC,   // # Mn  [36] TIBETAN SUBJOINED LETTER NYA..TIBETAN SUBJOINED LETTER FIXED-FORM RA
    $102C,          // # Mc       MYANMAR VOWEL SIGN AA
    $102D..$1030,   // # Mn   [4] MYANMAR VOWEL SIGN I..MYANMAR VOWEL SIGN UU
    $1031,          // # Mc       MYANMAR VOWEL SIGN E
    $1032,          // # Mn       MYANMAR VOWEL SIGN AI
    $1036,          // # Mn       MYANMAR SIGN ANUSVARA
    $1038,          // # Mc       MYANMAR SIGN VISARGA
    $1056..$1057,   // # Mc   [2] MYANMAR VOWEL SIGN VOCALIC R..MYANMAR VOWEL SIGN VOCALIC RR
    $1058..$1059,   // # Mn   [2] MYANMAR VOWEL SIGN VOCALIC L..MYANMAR VOWEL SIGN VOCALIC LL
    $16EE..$16F0,   // # Nl   [3] RUNIC ARLAUG SYMBOL..RUNIC BELGTHOR SYMBOL
    $1712..$1713,   // # Mn   [2] TAGALOG VOWEL SIGN I..TAGALOG VOWEL SIGN U
    $1732..$1733,   // # Mn   [2] HANUNOO VOWEL SIGN I..HANUNOO VOWEL SIGN U
    $1752..$1753,   // # Mn   [2] BUHID VOWEL SIGN I..BUHID VOWEL SIGN U
    $1772..$1773,   // # Mn   [2] TAGBANWA VOWEL SIGN I..TAGBANWA VOWEL SIGN U
    $17B4..$17B6,   // # Mc   [3] KHMER VOWEL INHERENT AQ..KHMER VOWEL SIGN AA
    $17B7..$17BD,   // # Mn   [7] KHMER VOWEL SIGN I..KHMER VOWEL SIGN UA
    $17BE..$17C5,   // # Mc   [8] KHMER VOWEL SIGN OE..KHMER VOWEL SIGN AU
    $17C6,          // # Mn       KHMER SIGN NIKAHIT
    $17C7..$17C8,   // # Mc   [2] KHMER SIGN REAHMUK..KHMER SIGN YUUKALEAPINTU
    $17D7,          // # Lm       KHMER SIGN LEK TOO
    $1843,          // # Lm       MONGOLIAN LETTER TODO LONG VOWEL SIGN
    $18A9,          // # Mn       MONGOLIAN LETTER ALI GALI DAGALGA
    $2160..$2183,   // # Nl  [36] ROMAN NUMERAL ONE..ROMAN NUMERAL REVERSED ONE HUNDRED
    $3005,          // # Lm       IDEOGRAPHIC ITERATION MARK
    $3007,          // # Nl       IDEOGRAPHIC NUMBER ZERO
    $3021..$3029,   // # Nl   [9] HANGZHOU NUMERAL ONE..HANGZHOU NUMERAL NINE
    $3031..$3035,   // # Lm   [5] VERTICAL KANA REPEAT MARK..VERTICAL KANA REPEAT MARK LOWER HALF
    $3038..$303A,   // # Nl   [3] HANGZHOU NUMERAL TEN..HANGZHOU NUMERAL THIRTY
    $303B,          // # Lm       VERTICAL IDEOGRAPHIC ITERATION MARK
    $309D..$309E,   // # Lm   [2] HIRAGANA ITERATION MARK..HIRAGANA VOICED ITERATION MARK
    $30FC..$30FE,   // # Lm   [3] KATAKANA-HIRAGANA PROLONGED SOUND MARK..KATAKANA VOICED ITERATION MARK
    $FB1E,          // # Mn       HEBREW POINT JUDEO-SPANISH VARIKA
    $FF70,          // # Lm       HALFWIDTH KATAKANA-HIRAGANA PROLONGED SOUND MARK
    $FF9E..$FF9F :  // # Lm   [2] HALFWIDTH KATAKANA VOICED SOUND MARK..HALFWIDTH KATAKANA SEMI-VOICED SOUND MARK
      Result := True;
  else
    Result := False;
  end;
end;

function GetCombiningClass(const Ch: WideChar): Byte;
begin
  if Ord(Ch) < $0300 then
    Result := 0 else
  Case Ord(Ch) of
    $0300..$0319 : Result := 230;
    $031A        : Result := 232;
    $031B        : Result := 216;
    $031C..$0320 : Result := 220;
    $0321..$0322 : Result := 202;
    $0323..$0326 : Result := 220;
    $0327..$0328 : Result := 202;
    $0329..$0333 : Result := 220;
    $0334..$0338 : Result := 1;
    $0339..$033C : Result := 220;
    $033D..$0344 : Result := 230;
    $0345        : Result := 240;
    $0346        : Result := 230;
    $0347..$0349 : Result := 220;
    $034A..$034C : Result := 230;
    $034D..$034E : Result := 220;
    $0360..$0361 : Result := 234;
    $0362        : Result := 233;
    $0483..$0486 : Result := 230;
    $0591        : Result := 220;
    $0592..$0595 : Result := 230;
    $0596        : Result := 220;
    $0597..$0599 : Result := 230;
    $059A        : Result := 222;
    $059B        : Result := 220;
    $059C..$05A1 : Result := 230;
    $05A3..$05A4 : Result := 220;
    $05A8..$05A9 : Result := 230;
    $05AA        : Result := 220;
    $05AB..$05AC : Result := 230;
    $05AD        : Result := 222;
    $05AE        : Result := 228;
    $05AF        : Result := 230;
    $05B0..$05B9 : Result := Ord(Ch) - $05B0 + 10;
    $05BB        : Result := 20;
    $05BC        : Result := 21;
    $05BD        : Result := 22;
    $05BF        : Result := 23;
    $05C1        : Result := 24;
    $05C2        : Result := 25;
    $05C4        : Result := 230;
    $064B..$0652 : Result := Ord(Ch) - $064B + 27;
    $0653..$0654 : Result := 230;
    $0655        : Result := 220;
    $0670        : Result := 35;
    $06D6..$06DC : Result := 230;
    $06DF..$06E2 : Result := 230;
    $06E3        : Result := 220;
    $06E4        : Result := 230;
    $06E7..$06E8 : Result := 230;
    $06EA        : Result := 220;
    $06EB..$06EC : Result := 230;
    $06ED        : Result := 220;
    $0711        : Result := 36;
    $0730        : Result := 230;
    $0731        : Result := 220;
    $0732..$0733 : Result := 230;
    $0734        : Result := 220;
    $0735..$0736 : Result := 230;
    $0737..$0739 : Result := 220;
    $073A        : Result := 230;
    $073B..$073C : Result := 220;
    $073D        : Result := 230;
    $073E        : Result := 220;
    $073F..$0741 : Result := 230;
    $0742        : Result := 220;
    $0743        : Result := 230;
    $0744        : Result := 220;
    $0745        : Result := 230;
    $0746        : Result := 220;
    $0747        : Result := 230;
    $0748        : Result := 220;
    $0749..$074A : Result := 230;
    $093C        : Result := 7;
    $094D        : Result := 9;
    $0951        : Result := 230;
    $0952        : Result := 220;
    $0953..$0954 : Result := 230;
    $09BC        : Result := 7;
    $09CD        : Result := 9;
    $0A3C        : Result := 7;
    $0A4D        : Result := 9;
    $0ABC        : Result := 7;
    $0ACD        : Result := 9;
    $0B3C        : Result := 7;
    $0B4D        : Result := 9;
    $0BCD        : Result := 9;
    $0C4D        : Result := 9;
    $0C55        : Result := 84;
    $0C56        : Result := 91;
    $0CCD        : Result := 9;
    $0D4D        : Result := 9;
    $0DCA        : Result := 9;
    $0E38..$0E39 : Result := 103;
    $0E3A        : Result := 9;
    $0E48..$0E4B : Result := 107;
    $0EB8..$0EB9 : Result := 118;
    $0EC8..$0ECB : Result := 122;
    $0F18..$0F19 : Result := 220;
    $0F35        : Result := 220;
    $0F37        : Result := 220;
    $0F39        : Result := 216;
    $0F71        : Result := 129;
    $0F72        : Result := 130;
    $0F74        : Result := 132;
    $0F7A..$0F7D : Result := 130;
    $0F80        : Result := 130;
    $0F82..$0F83 : Result := 230;
    $0F84        : Result := 9;
    $0F86..$0F87 : Result := 230;
    $0FC6        : Result := 220;
    $1037        : Result := 7;
    $1039        : Result := 9;
    $17D2        : Result := 9;
    $18A9        : Result := 228;
    $20D0..$20D1 : Result := 230;
    $20D2..$20D3 : Result := 1;
    $20D4..$20D7 : Result := 230;
    $20D8..$20DA : Result := 1;
    $20DB..$20DC : Result := 230;
    $20E1        : Result := 230;
    $302A        : Result := 218;
    $302B        : Result := 228;
    $302C        : Result := 232;
    $302D        : Result := 222;
    $302E..$302F : Result := 224;
    $3099        : Result := 8;
    $309A        : Result := 8;
    $FB1E        : Result := 26;
    $FE20..$FE23 : Result := 230;
  else
    Result := 0;
  end;
end;

type
  TUnicodeDecompositionAttr = (daNone, daNoBreak, daCompat, daSuper,
      daFraction, daSub, daFont, daCircle, daWide, daSquare, daIsolated,
      daInitial, daFinal, daMedial, daVertical, daSmall, daNarrow);
  TUnicodeDecompositionInfo = packed record
    Unicode : WideChar;
    Attr    : TUnicodeDecompositionAttr;
    Str     : WideString;
  end;
  PUnicodeDecompositionInfo = ^TUnicodeDecompositionInfo;

const
  UnicodeDecompositionEntries = 3485; // ~45K
  UnicodeDecompositionInfo : Array[0..UnicodeDecompositionEntries - 1] of TUnicodeDecompositionInfo = (
    (Unicode:#$00A0; Attr:daNoBreak; Str:#$0020),       // NO-BREAK SPACE
    (Unicode:#$00A8; Attr:daCompat; Str:#$0020#$0308),  // DIAERESIS
    (Unicode:#$00AA; Attr:daSuper; Str:#$0061),         // FEMININE ORDINAL INDICATOR
    (Unicode:#$00AF; Attr:daCompat; Str:#$0020#$0304),  // MACRON
    (Unicode:#$00B2; Attr:daSuper; Str:#$0032),         // SUPERSCRIPT TWO
    (Unicode:#$00B3; Attr:daSuper; Str:#$0033),         // SUPERSCRIPT THREE
    (Unicode:#$00B4; Attr:daCompat; Str:#$0020#$0301),  // ACUTE ACCENT
    (Unicode:#$00B5; Attr:daCompat; Str:#$03BC),        // MICRO SIGN
    (Unicode:#$00B8; Attr:daCompat; Str:#$0020#$0327),  // CEDILLA
    (Unicode:#$00B9; Attr:daSuper; Str:#$0031),         // SUPERSCRIPT ONE
    (Unicode:#$00BA; Attr:daSuper; Str:#$006F),         // MASCULINE ORDINAL INDICATOR
    (Unicode:#$00BC; Attr:daFraction; Str:#$0031#$2044#$0034), // VULGAR FRACTION ONE QUARTER
    (Unicode:#$00BD; Attr:daFraction; Str:#$0031#$2044#$0032), // VULGAR FRACTION ONE HALF
    (Unicode:#$00BE; Attr:daFraction; Str:#$0033#$2044#$0034), // VULGAR FRACTION THREE QUARTERS
    (Unicode:#$00C0; Attr:daNone; Str:#$0041#$0300),    // LATIN CAPITAL LETTER A WITH GRAVE
    (Unicode:#$00C1; Attr:daNone; Str:#$0041#$0301),    // LATIN CAPITAL LETTER A WITH ACUTE
    (Unicode:#$00C2; Attr:daNone; Str:#$0041#$0302),    // LATIN CAPITAL LETTER A WITH CIRCUMFLEX
    (Unicode:#$00C3; Attr:daNone; Str:#$0041#$0303),    // LATIN CAPITAL LETTER A WITH TILDE
    (Unicode:#$00C4; Attr:daNone; Str:#$0041#$0308),    // LATIN CAPITAL LETTER A WITH DIAERESIS
    (Unicode:#$00C5; Attr:daNone; Str:#$0041#$030A),    // LATIN CAPITAL LETTER A WITH RING ABOVE
    (Unicode:#$00C7; Attr:daNone; Str:#$0043#$0327),    // LATIN CAPITAL LETTER C WITH CEDILLA
    (Unicode:#$00C8; Attr:daNone; Str:#$0045#$0300),    // LATIN CAPITAL LETTER E WITH GRAVE
    (Unicode:#$00C9; Attr:daNone; Str:#$0045#$0301),    // LATIN CAPITAL LETTER E WITH ACUTE
    (Unicode:#$00CA; Attr:daNone; Str:#$0045#$0302),    // LATIN CAPITAL LETTER E WITH CIRCUMFLEX
    (Unicode:#$00CB; Attr:daNone; Str:#$0045#$0308),    // LATIN CAPITAL LETTER E WITH DIAERESIS
    (Unicode:#$00CC; Attr:daNone; Str:#$0049#$0300),    // LATIN CAPITAL LETTER I WITH GRAVE
    (Unicode:#$00CD; Attr:daNone; Str:#$0049#$0301),    // LATIN CAPITAL LETTER I WITH ACUTE
    (Unicode:#$00CE; Attr:daNone; Str:#$0049#$0302),    // LATIN CAPITAL LETTER I WITH CIRCUMFLEX
    (Unicode:#$00CF; Attr:daNone; Str:#$0049#$0308),    // LATIN CAPITAL LETTER I WITH DIAERESIS
    (Unicode:#$00D1; Attr:daNone; Str:#$004E#$0303),    // LATIN CAPITAL LETTER N WITH TILDE
    (Unicode:#$00D2; Attr:daNone; Str:#$004F#$0300),    // LATIN CAPITAL LETTER O WITH GRAVE
    (Unicode:#$00D3; Attr:daNone; Str:#$004F#$0301),    // LATIN CAPITAL LETTER O WITH ACUTE
    (Unicode:#$00D4; Attr:daNone; Str:#$004F#$0302),    // LATIN CAPITAL LETTER O WITH CIRCUMFLEX
    (Unicode:#$00D5; Attr:daNone; Str:#$004F#$0303),    // LATIN CAPITAL LETTER O WITH TILDE
    (Unicode:#$00D6; Attr:daNone; Str:#$004F#$0308),    // LATIN CAPITAL LETTER O WITH DIAERESIS
    (Unicode:#$00D9; Attr:daNone; Str:#$0055#$0300),    // LATIN CAPITAL LETTER U WITH GRAVE
    (Unicode:#$00DA; Attr:daNone; Str:#$0055#$0301),    // LATIN CAPITAL LETTER U WITH ACUTE
    (Unicode:#$00DB; Attr:daNone; Str:#$0055#$0302),    // LATIN CAPITAL LETTER U WITH CIRCUMFLEX
    (Unicode:#$00DC; Attr:daNone; Str:#$0055#$0308),    // LATIN CAPITAL LETTER U WITH DIAERESIS
    (Unicode:#$00DD; Attr:daNone; Str:#$0059#$0301),    // LATIN CAPITAL LETTER Y WITH ACUTE
    (Unicode:#$00E0; Attr:daNone; Str:#$0061#$0300),    // LATIN SMALL LETTER A WITH GRAVE
    (Unicode:#$00E1; Attr:daNone; Str:#$0061#$0301),    // LATIN SMALL LETTER A WITH ACUTE
    (Unicode:#$00E2; Attr:daNone; Str:#$0061#$0302),    // LATIN SMALL LETTER A WITH CIRCUMFLEX
    (Unicode:#$00E3; Attr:daNone; Str:#$0061#$0303),    // LATIN SMALL LETTER A WITH TILDE
    (Unicode:#$00E4; Attr:daNone; Str:#$0061#$0308),    // LATIN SMALL LETTER A WITH DIAERESIS
    (Unicode:#$00E5; Attr:daNone; Str:#$0061#$030A),    // LATIN SMALL LETTER A WITH RING ABOVE
    (Unicode:#$00E7; Attr:daNone; Str:#$0063#$0327),    // LATIN SMALL LETTER C WITH CEDILLA
    (Unicode:#$00E8; Attr:daNone; Str:#$0065#$0300),    // LATIN SMALL LETTER E WITH GRAVE
    (Unicode:#$00E9; Attr:daNone; Str:#$0065#$0301),    // LATIN SMALL LETTER E WITH ACUTE
    (Unicode:#$00EA; Attr:daNone; Str:#$0065#$0302),    // LATIN SMALL LETTER E WITH CIRCUMFLEX
    (Unicode:#$00EB; Attr:daNone; Str:#$0065#$0308),    // LATIN SMALL LETTER E WITH DIAERESIS
    (Unicode:#$00EC; Attr:daNone; Str:#$0069#$0300),    // LATIN SMALL LETTER I WITH GRAVE
    (Unicode:#$00ED; Attr:daNone; Str:#$0069#$0301),    // LATIN SMALL LETTER I WITH ACUTE
    (Unicode:#$00EE; Attr:daNone; Str:#$0069#$0302),    // LATIN SMALL LETTER I WITH CIRCUMFLEX
    (Unicode:#$00EF; Attr:daNone; Str:#$0069#$0308),    // LATIN SMALL LETTER I WITH DIAERESIS
    (Unicode:#$00F1; Attr:daNone; Str:#$006E#$0303),    // LATIN SMALL LETTER N WITH TILDE
    (Unicode:#$00F2; Attr:daNone; Str:#$006F#$0300),    // LATIN SMALL LETTER O WITH GRAVE
    (Unicode:#$00F3; Attr:daNone; Str:#$006F#$0301),    // LATIN SMALL LETTER O WITH ACUTE
    (Unicode:#$00F4; Attr:daNone; Str:#$006F#$0302),    // LATIN SMALL LETTER O WITH CIRCUMFLEX
    (Unicode:#$00F5; Attr:daNone; Str:#$006F#$0303),    // LATIN SMALL LETTER O WITH TILDE
    (Unicode:#$00F6; Attr:daNone; Str:#$006F#$0308),    // LATIN SMALL LETTER O WITH DIAERESIS
    (Unicode:#$00F9; Attr:daNone; Str:#$0075#$0300),    // LATIN SMALL LETTER U WITH GRAVE
    (Unicode:#$00FA; Attr:daNone; Str:#$0075#$0301),    // LATIN SMALL LETTER U WITH ACUTE
    (Unicode:#$00FB; Attr:daNone; Str:#$0075#$0302),    // LATIN SMALL LETTER U WITH CIRCUMFLEX
    (Unicode:#$00FC; Attr:daNone; Str:#$0075#$0308),    // LATIN SMALL LETTER U WITH DIAERESIS
    (Unicode:#$00FD; Attr:daNone; Str:#$0079#$0301),    // LATIN SMALL LETTER Y WITH ACUTE
    (Unicode:#$00FF; Attr:daNone; Str:#$0079#$0308),    // LATIN SMALL LETTER Y WITH DIAERESIS
    (Unicode:#$0100; Attr:daNone; Str:#$0041#$0304),    // LATIN CAPITAL LETTER A WITH MACRON
    (Unicode:#$0101; Attr:daNone; Str:#$0061#$0304),    // LATIN SMALL LETTER A WITH MACRON
    (Unicode:#$0102; Attr:daNone; Str:#$0041#$0306),    // LATIN CAPITAL LETTER A WITH BREVE
    (Unicode:#$0103; Attr:daNone; Str:#$0061#$0306),    // LATIN SMALL LETTER A WITH BREVE
    (Unicode:#$0104; Attr:daNone; Str:#$0041#$0328),    // LATIN CAPITAL LETTER A WITH OGONEK
    (Unicode:#$0105; Attr:daNone; Str:#$0061#$0328),    // LATIN SMALL LETTER A WITH OGONEK
    (Unicode:#$0106; Attr:daNone; Str:#$0043#$0301),    // LATIN CAPITAL LETTER C WITH ACUTE
    (Unicode:#$0107; Attr:daNone; Str:#$0063#$0301),    // LATIN SMALL LETTER C WITH ACUTE
    (Unicode:#$0108; Attr:daNone; Str:#$0043#$0302),    // LATIN CAPITAL LETTER C WITH CIRCUMFLEX
    (Unicode:#$0109; Attr:daNone; Str:#$0063#$0302),    // LATIN SMALL LETTER C WITH CIRCUMFLEX
    (Unicode:#$010A; Attr:daNone; Str:#$0043#$0307),    // LATIN CAPITAL LETTER C WITH DOT ABOVE
    (Unicode:#$010B; Attr:daNone; Str:#$0063#$0307),    // LATIN SMALL LETTER C WITH DOT ABOVE
    (Unicode:#$010C; Attr:daNone; Str:#$0043#$030C),    // LATIN CAPITAL LETTER C WITH CARON
    (Unicode:#$010D; Attr:daNone; Str:#$0063#$030C),    // LATIN SMALL LETTER C WITH CARON
    (Unicode:#$010E; Attr:daNone; Str:#$0044#$030C),    // LATIN CAPITAL LETTER D WITH CARON
    (Unicode:#$010F; Attr:daNone; Str:#$0064#$030C),    // LATIN SMALL LETTER D WITH CARON
    (Unicode:#$0112; Attr:daNone; Str:#$0045#$0304),    // LATIN CAPITAL LETTER E WITH MACRON
    (Unicode:#$0113; Attr:daNone; Str:#$0065#$0304),    // LATIN SMALL LETTER E WITH MACRON
    (Unicode:#$0114; Attr:daNone; Str:#$0045#$0306),    // LATIN CAPITAL LETTER E WITH BREVE
    (Unicode:#$0115; Attr:daNone; Str:#$0065#$0306),    // LATIN SMALL LETTER E WITH BREVE
    (Unicode:#$0116; Attr:daNone; Str:#$0045#$0307),    // LATIN CAPITAL LETTER E WITH DOT ABOVE
    (Unicode:#$0117; Attr:daNone; Str:#$0065#$0307),    // LATIN SMALL LETTER E WITH DOT ABOVE
    (Unicode:#$0118; Attr:daNone; Str:#$0045#$0328),    // LATIN CAPITAL LETTER E WITH OGONEK
    (Unicode:#$0119; Attr:daNone; Str:#$0065#$0328),    // LATIN SMALL LETTER E WITH OGONEK
    (Unicode:#$011A; Attr:daNone; Str:#$0045#$030C),    // LATIN CAPITAL LETTER E WITH CARON
    (Unicode:#$011B; Attr:daNone; Str:#$0065#$030C),    // LATIN SMALL LETTER E WITH CARON
    (Unicode:#$011C; Attr:daNone; Str:#$0047#$0302),    // LATIN CAPITAL LETTER G WITH CIRCUMFLEX
    (Unicode:#$011D; Attr:daNone; Str:#$0067#$0302),    // LATIN SMALL LETTER G WITH CIRCUMFLEX
    (Unicode:#$011E; Attr:daNone; Str:#$0047#$0306),    // LATIN CAPITAL LETTER G WITH BREVE
    (Unicode:#$011F; Attr:daNone; Str:#$0067#$0306),    // LATIN SMALL LETTER G WITH BREVE
    (Unicode:#$0120; Attr:daNone; Str:#$0047#$0307),    // LATIN CAPITAL LETTER G WITH DOT ABOVE
    (Unicode:#$0121; Attr:daNone; Str:#$0067#$0307),    // LATIN SMALL LETTER G WITH DOT ABOVE
    (Unicode:#$0122; Attr:daNone; Str:#$0047#$0327),    // LATIN CAPITAL LETTER G WITH CEDILLA
    (Unicode:#$0123; Attr:daNone; Str:#$0067#$0327),    // LATIN SMALL LETTER G WITH CEDILLA
    (Unicode:#$0124; Attr:daNone; Str:#$0048#$0302),    // LATIN CAPITAL LETTER H WITH CIRCUMFLEX
    (Unicode:#$0125; Attr:daNone; Str:#$0068#$0302),    // LATIN SMALL LETTER H WITH CIRCUMFLEX
    (Unicode:#$0128; Attr:daNone; Str:#$0049#$0303),    // LATIN CAPITAL LETTER I WITH TILDE
    (Unicode:#$0129; Attr:daNone; Str:#$0069#$0303),    // LATIN SMALL LETTER I WITH TILDE
    (Unicode:#$012A; Attr:daNone; Str:#$0049#$0304),    // LATIN CAPITAL LETTER I WITH MACRON
    (Unicode:#$012B; Attr:daNone; Str:#$0069#$0304),    // LATIN SMALL LETTER I WITH MACRON
    (Unicode:#$012C; Attr:daNone; Str:#$0049#$0306),    // LATIN CAPITAL LETTER I WITH BREVE
    (Unicode:#$012D; Attr:daNone; Str:#$0069#$0306),    // LATIN SMALL LETTER I WITH BREVE
    (Unicode:#$012E; Attr:daNone; Str:#$0049#$0328),    // LATIN CAPITAL LETTER I WITH OGONEK
    (Unicode:#$012F; Attr:daNone; Str:#$0069#$0328),    // LATIN SMALL LETTER I WITH OGONEK
    (Unicode:#$0130; Attr:daNone; Str:#$0049#$0307),    // LATIN CAPITAL LETTER I WITH DOT ABOVE
    (Unicode:#$0132; Attr:daCompat; Str:#$0049#$004A),  // LATIN CAPITAL LIGATURE IJ
    (Unicode:#$0133; Attr:daCompat; Str:#$0069#$006A),  // LATIN SMALL LIGATURE IJ
    (Unicode:#$0134; Attr:daNone; Str:#$004A#$0302),    // LATIN CAPITAL LETTER J WITH CIRCUMFLEX
    (Unicode:#$0135; Attr:daNone; Str:#$006A#$0302),    // LATIN SMALL LETTER J WITH CIRCUMFLEX
    (Unicode:#$0136; Attr:daNone; Str:#$004B#$0327),    // LATIN CAPITAL LETTER K WITH CEDILLA
    (Unicode:#$0137; Attr:daNone; Str:#$006B#$0327),    // LATIN SMALL LETTER K WITH CEDILLA
    (Unicode:#$0139; Attr:daNone; Str:#$004C#$0301),    // LATIN CAPITAL LETTER L WITH ACUTE
    (Unicode:#$013A; Attr:daNone; Str:#$006C#$0301),    // LATIN SMALL LETTER L WITH ACUTE
    (Unicode:#$013B; Attr:daNone; Str:#$004C#$0327),    // LATIN CAPITAL LETTER L WITH CEDILLA
    (Unicode:#$013C; Attr:daNone; Str:#$006C#$0327),    // LATIN SMALL LETTER L WITH CEDILLA
    (Unicode:#$013D; Attr:daNone; Str:#$004C#$030C),    // LATIN CAPITAL LETTER L WITH CARON
    (Unicode:#$013E; Attr:daNone; Str:#$006C#$030C),    // LATIN SMALL LETTER L WITH CARON
    (Unicode:#$013F; Attr:daCompat; Str:#$004C#$00B7),  // LATIN CAPITAL LETTER L WITH MIDDLE DOT
    (Unicode:#$0140; Attr:daCompat; Str:#$006C#$00B7),  // LATIN SMALL LETTER L WITH MIDDLE DOT
    (Unicode:#$0143; Attr:daNone; Str:#$004E#$0301),    // LATIN CAPITAL LETTER N WITH ACUTE
    (Unicode:#$0144; Attr:daNone; Str:#$006E#$0301),    // LATIN SMALL LETTER N WITH ACUTE
    (Unicode:#$0145; Attr:daNone; Str:#$004E#$0327),    // LATIN CAPITAL LETTER N WITH CEDILLA
    (Unicode:#$0146; Attr:daNone; Str:#$006E#$0327),    // LATIN SMALL LETTER N WITH CEDILLA
    (Unicode:#$0147; Attr:daNone; Str:#$004E#$030C),    // LATIN CAPITAL LETTER N WITH CARON
    (Unicode:#$0148; Attr:daNone; Str:#$006E#$030C),    // LATIN SMALL LETTER N WITH CARON
    (Unicode:#$0149; Attr:daCompat; Str:#$02BC#$006E),  // LATIN SMALL LETTER N PRECEDED BY APOSTROPHE
    (Unicode:#$014C; Attr:daNone; Str:#$004F#$0304),    // LATIN CAPITAL LETTER O WITH MACRON
    (Unicode:#$014D; Attr:daNone; Str:#$006F#$0304),    // LATIN SMALL LETTER O WITH MACRON
    (Unicode:#$014E; Attr:daNone; Str:#$004F#$0306),    // LATIN CAPITAL LETTER O WITH BREVE
    (Unicode:#$014F; Attr:daNone; Str:#$006F#$0306),    // LATIN SMALL LETTER O WITH BREVE
    (Unicode:#$0150; Attr:daNone; Str:#$004F#$030B),    // LATIN CAPITAL LETTER O WITH DOUBLE ACUTE
    (Unicode:#$0151; Attr:daNone; Str:#$006F#$030B),    // LATIN SMALL LETTER O WITH DOUBLE ACUTE
    (Unicode:#$0154; Attr:daNone; Str:#$0052#$0301),    // LATIN CAPITAL LETTER R WITH ACUTE
    (Unicode:#$0155; Attr:daNone; Str:#$0072#$0301),    // LATIN SMALL LETTER R WITH ACUTE
    (Unicode:#$0156; Attr:daNone; Str:#$0052#$0327),    // LATIN CAPITAL LETTER R WITH CEDILLA
    (Unicode:#$0157; Attr:daNone; Str:#$0072#$0327),    // LATIN SMALL LETTER R WITH CEDILLA
    (Unicode:#$0158; Attr:daNone; Str:#$0052#$030C),    // LATIN CAPITAL LETTER R WITH CARON
    (Unicode:#$0159; Attr:daNone; Str:#$0072#$030C),    // LATIN SMALL LETTER R WITH CARON
    (Unicode:#$015A; Attr:daNone; Str:#$0053#$0301),    // LATIN CAPITAL LETTER S WITH ACUTE
    (Unicode:#$015B; Attr:daNone; Str:#$0073#$0301),    // LATIN SMALL LETTER S WITH ACUTE
    (Unicode:#$015C; Attr:daNone; Str:#$0053#$0302),    // LATIN CAPITAL LETTER S WITH CIRCUMFLEX
    (Unicode:#$015D; Attr:daNone; Str:#$0073#$0302),    // LATIN SMALL LETTER S WITH CIRCUMFLEX
    (Unicode:#$015E; Attr:daNone; Str:#$0053#$0327),    // LATIN CAPITAL LETTER S WITH CEDILLA
    (Unicode:#$015F; Attr:daNone; Str:#$0073#$0327),    // LATIN SMALL LETTER S WITH CEDILLA
    (Unicode:#$0160; Attr:daNone; Str:#$0053#$030C),    // LATIN CAPITAL LETTER S WITH CARON
    (Unicode:#$0161; Attr:daNone; Str:#$0073#$030C),    // LATIN SMALL LETTER S WITH CARON
    (Unicode:#$0162; Attr:daNone; Str:#$0054#$0327),    // LATIN CAPITAL LETTER T WITH CEDILLA
    (Unicode:#$0163; Attr:daNone; Str:#$0074#$0327),    // LATIN SMALL LETTER T WITH CEDILLA
    (Unicode:#$0164; Attr:daNone; Str:#$0054#$030C),    // LATIN CAPITAL LETTER T WITH CARON
    (Unicode:#$0165; Attr:daNone; Str:#$0074#$030C),    // LATIN SMALL LETTER T WITH CARON
    (Unicode:#$0168; Attr:daNone; Str:#$0055#$0303),    // LATIN CAPITAL LETTER U WITH TILDE
    (Unicode:#$0169; Attr:daNone; Str:#$0075#$0303),    // LATIN SMALL LETTER U WITH TILDE
    (Unicode:#$016A; Attr:daNone; Str:#$0055#$0304),    // LATIN CAPITAL LETTER U WITH MACRON
    (Unicode:#$016B; Attr:daNone; Str:#$0075#$0304),    // LATIN SMALL LETTER U WITH MACRON
    (Unicode:#$016C; Attr:daNone; Str:#$0055#$0306),    // LATIN CAPITAL LETTER U WITH BREVE
    (Unicode:#$016D; Attr:daNone; Str:#$0075#$0306),    // LATIN SMALL LETTER U WITH BREVE
    (Unicode:#$016E; Attr:daNone; Str:#$0055#$030A),    // LATIN CAPITAL LETTER U WITH RING ABOVE
    (Unicode:#$016F; Attr:daNone; Str:#$0075#$030A),    // LATIN SMALL LETTER U WITH RING ABOVE
    (Unicode:#$0170; Attr:daNone; Str:#$0055#$030B),    // LATIN CAPITAL LETTER U WITH DOUBLE ACUTE
    (Unicode:#$0171; Attr:daNone; Str:#$0075#$030B),    // LATIN SMALL LETTER U WITH DOUBLE ACUTE
    (Unicode:#$0172; Attr:daNone; Str:#$0055#$0328),    // LATIN CAPITAL LETTER U WITH OGONEK
    (Unicode:#$0173; Attr:daNone; Str:#$0075#$0328),    // LATIN SMALL LETTER U WITH OGONEK
    (Unicode:#$0174; Attr:daNone; Str:#$0057#$0302),    // LATIN CAPITAL LETTER W WITH CIRCUMFLEX
    (Unicode:#$0175; Attr:daNone; Str:#$0077#$0302),    // LATIN SMALL LETTER W WITH CIRCUMFLEX
    (Unicode:#$0176; Attr:daNone; Str:#$0059#$0302),    // LATIN CAPITAL LETTER Y WITH CIRCUMFLEX
    (Unicode:#$0177; Attr:daNone; Str:#$0079#$0302),    // LATIN SMALL LETTER Y WITH CIRCUMFLEX
    (Unicode:#$0178; Attr:daNone; Str:#$0059#$0308),    // LATIN CAPITAL LETTER Y WITH DIAERESIS
    (Unicode:#$0179; Attr:daNone; Str:#$005A#$0301),    // LATIN CAPITAL LETTER Z WITH ACUTE
    (Unicode:#$017A; Attr:daNone; Str:#$007A#$0301),    // LATIN SMALL LETTER Z WITH ACUTE
    (Unicode:#$017B; Attr:daNone; Str:#$005A#$0307),    // LATIN CAPITAL LETTER Z WITH DOT ABOVE
    (Unicode:#$017C; Attr:daNone; Str:#$007A#$0307),    // LATIN SMALL LETTER Z WITH DOT ABOVE
    (Unicode:#$017D; Attr:daNone; Str:#$005A#$030C),    // LATIN CAPITAL LETTER Z WITH CARON
    (Unicode:#$017E; Attr:daNone; Str:#$007A#$030C),    // LATIN SMALL LETTER Z WITH CARON
    (Unicode:#$017F; Attr:daCompat; Str:#$0073),        // LATIN SMALL LETTER LONG S
    (Unicode:#$01A0; Attr:daNone; Str:#$004F#$031B),    // LATIN CAPITAL LETTER O WITH HORN
    (Unicode:#$01A1; Attr:daNone; Str:#$006F#$031B),    // LATIN SMALL LETTER O WITH HORN
    (Unicode:#$01AF; Attr:daNone; Str:#$0055#$031B),    // LATIN CAPITAL LETTER U WITH HORN
    (Unicode:#$01B0; Attr:daNone; Str:#$0075#$031B),    // LATIN SMALL LETTER U WITH HORN
    (Unicode:#$01C4; Attr:daCompat; Str:#$0044#$017D),  // LATIN CAPITAL LETTER DZ WITH CARON
    (Unicode:#$01C5; Attr:daCompat; Str:#$0044#$017E),  // LATIN CAPITAL LETTER D WITH SMALL LETTER Z WITH CARON
    (Unicode:#$01C6; Attr:daCompat; Str:#$0064#$017E),  // LATIN SMALL LETTER DZ WITH CARON
    (Unicode:#$01C7; Attr:daCompat; Str:#$004C#$004A),  // LATIN CAPITAL LETTER LJ
    (Unicode:#$01C8; Attr:daCompat; Str:#$004C#$006A),  // LATIN CAPITAL LETTER L WITH SMALL LETTER J
    (Unicode:#$01C9; Attr:daCompat; Str:#$006C#$006A),  // LATIN SMALL LETTER LJ
    (Unicode:#$01CA; Attr:daCompat; Str:#$004E#$004A),  // LATIN CAPITAL LETTER NJ
    (Unicode:#$01CB; Attr:daCompat; Str:#$004E#$006A),  // LATIN CAPITAL LETTER N WITH SMALL LETTER J
    (Unicode:#$01CC; Attr:daCompat; Str:#$006E#$006A),  // LATIN SMALL LETTER NJ
    (Unicode:#$01CD; Attr:daNone; Str:#$0041#$030C),    // LATIN CAPITAL LETTER A WITH CARON
    (Unicode:#$01CE; Attr:daNone; Str:#$0061#$030C),    // LATIN SMALL LETTER A WITH CARON
    (Unicode:#$01CF; Attr:daNone; Str:#$0049#$030C),    // LATIN CAPITAL LETTER I WITH CARON
    (Unicode:#$01D0; Attr:daNone; Str:#$0069#$030C),    // LATIN SMALL LETTER I WITH CARON
    (Unicode:#$01D1; Attr:daNone; Str:#$004F#$030C),    // LATIN CAPITAL LETTER O WITH CARON
    (Unicode:#$01D2; Attr:daNone; Str:#$006F#$030C),    // LATIN SMALL LETTER O WITH CARON
    (Unicode:#$01D3; Attr:daNone; Str:#$0055#$030C),    // LATIN CAPITAL LETTER U WITH CARON
    (Unicode:#$01D4; Attr:daNone; Str:#$0075#$030C),    // LATIN SMALL LETTER U WITH CARON
    (Unicode:#$01D5; Attr:daNone; Str:#$00DC#$0304),    // LATIN CAPITAL LETTER U WITH DIAERESIS AND MACRON
    (Unicode:#$01D6; Attr:daNone; Str:#$00FC#$0304),    // LATIN SMALL LETTER U WITH DIAERESIS AND MACRON
    (Unicode:#$01D7; Attr:daNone; Str:#$00DC#$0301),    // LATIN CAPITAL LETTER U WITH DIAERESIS AND ACUTE
    (Unicode:#$01D8; Attr:daNone; Str:#$00FC#$0301),    // LATIN SMALL LETTER U WITH DIAERESIS AND ACUTE
    (Unicode:#$01D9; Attr:daNone; Str:#$00DC#$030C),    // LATIN CAPITAL LETTER U WITH DIAERESIS AND CARON
    (Unicode:#$01DA; Attr:daNone; Str:#$00FC#$030C),    // LATIN SMALL LETTER U WITH DIAERESIS AND CARON
    (Unicode:#$01DB; Attr:daNone; Str:#$00DC#$0300),    // LATIN CAPITAL LETTER U WITH DIAERESIS AND GRAVE
    (Unicode:#$01DC; Attr:daNone; Str:#$00FC#$0300),    // LATIN SMALL LETTER U WITH DIAERESIS AND GRAVE
    (Unicode:#$01DE; Attr:daNone; Str:#$00C4#$0304),    // LATIN CAPITAL LETTER A WITH DIAERESIS AND MACRON
    (Unicode:#$01DF; Attr:daNone; Str:#$00E4#$0304),    // LATIN SMALL LETTER A WITH DIAERESIS AND MACRON
    (Unicode:#$01E0; Attr:daNone; Str:#$0226#$0304),    // LATIN CAPITAL LETTER A WITH DOT ABOVE AND MACRON
    (Unicode:#$01E1; Attr:daNone; Str:#$0227#$0304),    // LATIN SMALL LETTER A WITH DOT ABOVE AND MACRON
    (Unicode:#$01E2; Attr:daNone; Str:#$00C6#$0304),    // LATIN CAPITAL LETTER AE WITH MACRON
    (Unicode:#$01E3; Attr:daNone; Str:#$00E6#$0304),    // LATIN SMALL LETTER AE WITH MACRON
    (Unicode:#$01E6; Attr:daNone; Str:#$0047#$030C),    // LATIN CAPITAL LETTER G WITH CARON
    (Unicode:#$01E7; Attr:daNone; Str:#$0067#$030C),    // LATIN SMALL LETTER G WITH CARON
    (Unicode:#$01E8; Attr:daNone; Str:#$004B#$030C),    // LATIN CAPITAL LETTER K WITH CARON
    (Unicode:#$01E9; Attr:daNone; Str:#$006B#$030C),    // LATIN SMALL LETTER K WITH CARON
    (Unicode:#$01EA; Attr:daNone; Str:#$004F#$0328),    // LATIN CAPITAL LETTER O WITH OGONEK
    (Unicode:#$01EB; Attr:daNone; Str:#$006F#$0328),    // LATIN SMALL LETTER O WITH OGONEK
    (Unicode:#$01EC; Attr:daNone; Str:#$01EA#$0304),    // LATIN CAPITAL LETTER O WITH OGONEK AND MACRON
    (Unicode:#$01ED; Attr:daNone; Str:#$01EB#$0304),    // LATIN SMALL LETTER O WITH OGONEK AND MACRON
    (Unicode:#$01EE; Attr:daNone; Str:#$01B7#$030C),    // LATIN CAPITAL LETTER EZH WITH CARON
    (Unicode:#$01EF; Attr:daNone; Str:#$0292#$030C),    // LATIN SMALL LETTER EZH WITH CARON
    (Unicode:#$01F0; Attr:daNone; Str:#$006A#$030C),    // LATIN SMALL LETTER J WITH CARON
    (Unicode:#$01F1; Attr:daCompat; Str:#$0044#$005A),  // LATIN CAPITAL LETTER DZ
    (Unicode:#$01F2; Attr:daCompat; Str:#$0044#$007A),  // LATIN CAPITAL LETTER D WITH SMALL LETTER Z
    (Unicode:#$01F3; Attr:daCompat; Str:#$0064#$007A),  // LATIN SMALL LETTER DZ
    (Unicode:#$01F4; Attr:daNone; Str:#$0047#$0301),    // LATIN CAPITAL LETTER G WITH ACUTE
    (Unicode:#$01F5; Attr:daNone; Str:#$0067#$0301),    // LATIN SMALL LETTER G WITH ACUTE
    (Unicode:#$01F8; Attr:daNone; Str:#$004E#$0300),    // LATIN CAPITAL LETTER N WITH GRAVE
    (Unicode:#$01F9; Attr:daNone; Str:#$006E#$0300),    // LATIN SMALL LETTER N WITH GRAVE
    (Unicode:#$01FA; Attr:daNone; Str:#$00C5#$0301),    // LATIN CAPITAL LETTER A WITH RING ABOVE AND ACUTE
    (Unicode:#$01FB; Attr:daNone; Str:#$00E5#$0301),    // LATIN SMALL LETTER A WITH RING ABOVE AND ACUTE
    (Unicode:#$01FC; Attr:daNone; Str:#$00C6#$0301),    // LATIN CAPITAL LETTER AE WITH ACUTE
    (Unicode:#$01FD; Attr:daNone; Str:#$00E6#$0301),    // LATIN SMALL LETTER AE WITH ACUTE
    (Unicode:#$01FE; Attr:daNone; Str:#$00D8#$0301),    // LATIN CAPITAL LETTER O WITH STROKE AND ACUTE
    (Unicode:#$01FF; Attr:daNone; Str:#$00F8#$0301),    // LATIN SMALL LETTER O WITH STROKE AND ACUTE
    (Unicode:#$0200; Attr:daNone; Str:#$0041#$030F),    // LATIN CAPITAL LETTER A WITH DOUBLE GRAVE
    (Unicode:#$0201; Attr:daNone; Str:#$0061#$030F),    // LATIN SMALL LETTER A WITH DOUBLE GRAVE
    (Unicode:#$0202; Attr:daNone; Str:#$0041#$0311),    // LATIN CAPITAL LETTER A WITH INVERTED BREVE
    (Unicode:#$0203; Attr:daNone; Str:#$0061#$0311),    // LATIN SMALL LETTER A WITH INVERTED BREVE
    (Unicode:#$0204; Attr:daNone; Str:#$0045#$030F),    // LATIN CAPITAL LETTER E WITH DOUBLE GRAVE
    (Unicode:#$0205; Attr:daNone; Str:#$0065#$030F),    // LATIN SMALL LETTER E WITH DOUBLE GRAVE
    (Unicode:#$0206; Attr:daNone; Str:#$0045#$0311),    // LATIN CAPITAL LETTER E WITH INVERTED BREVE
    (Unicode:#$0207; Attr:daNone; Str:#$0065#$0311),    // LATIN SMALL LETTER E WITH INVERTED BREVE
    (Unicode:#$0208; Attr:daNone; Str:#$0049#$030F),    // LATIN CAPITAL LETTER I WITH DOUBLE GRAVE
    (Unicode:#$0209; Attr:daNone; Str:#$0069#$030F),    // LATIN SMALL LETTER I WITH DOUBLE GRAVE
    (Unicode:#$020A; Attr:daNone; Str:#$0049#$0311),    // LATIN CAPITAL LETTER I WITH INVERTED BREVE
    (Unicode:#$020B; Attr:daNone; Str:#$0069#$0311),    // LATIN SMALL LETTER I WITH INVERTED BREVE
    (Unicode:#$020C; Attr:daNone; Str:#$004F#$030F),    // LATIN CAPITAL LETTER O WITH DOUBLE GRAVE
    (Unicode:#$020D; Attr:daNone; Str:#$006F#$030F),    // LATIN SMALL LETTER O WITH DOUBLE GRAVE
    (Unicode:#$020E; Attr:daNone; Str:#$004F#$0311),    // LATIN CAPITAL LETTER O WITH INVERTED BREVE
    (Unicode:#$020F; Attr:daNone; Str:#$006F#$0311),    // LATIN SMALL LETTER O WITH INVERTED BREVE
    (Unicode:#$0210; Attr:daNone; Str:#$0052#$030F),    // LATIN CAPITAL LETTER R WITH DOUBLE GRAVE
    (Unicode:#$0211; Attr:daNone; Str:#$0072#$030F),    // LATIN SMALL LETTER R WITH DOUBLE GRAVE
    (Unicode:#$0212; Attr:daNone; Str:#$0052#$0311),    // LATIN CAPITAL LETTER R WITH INVERTED BREVE
    (Unicode:#$0213; Attr:daNone; Str:#$0072#$0311),    // LATIN SMALL LETTER R WITH INVERTED BREVE
    (Unicode:#$0214; Attr:daNone; Str:#$0055#$030F),    // LATIN CAPITAL LETTER U WITH DOUBLE GRAVE
    (Unicode:#$0215; Attr:daNone; Str:#$0075#$030F),    // LATIN SMALL LETTER U WITH DOUBLE GRAVE
    (Unicode:#$0216; Attr:daNone; Str:#$0055#$0311),    // LATIN CAPITAL LETTER U WITH INVERTED BREVE
    (Unicode:#$0217; Attr:daNone; Str:#$0075#$0311),    // LATIN SMALL LETTER U WITH INVERTED BREVE
    (Unicode:#$0218; Attr:daNone; Str:#$0053#$0326),    // LATIN CAPITAL LETTER S WITH COMMA BELOW
    (Unicode:#$0219; Attr:daNone; Str:#$0073#$0326),    // LATIN SMALL LETTER S WITH COMMA BELOW
    (Unicode:#$021A; Attr:daNone; Str:#$0054#$0326),    // LATIN CAPITAL LETTER T WITH COMMA BELOW
    (Unicode:#$021B; Attr:daNone; Str:#$0074#$0326),    // LATIN SMALL LETTER T WITH COMMA BELOW
    (Unicode:#$021E; Attr:daNone; Str:#$0048#$030C),    // LATIN CAPITAL LETTER H WITH CARON
    (Unicode:#$021F; Attr:daNone; Str:#$0068#$030C),    // LATIN SMALL LETTER H WITH CARON
    (Unicode:#$0226; Attr:daNone; Str:#$0041#$0307),    // LATIN CAPITAL LETTER A WITH DOT ABOVE
    (Unicode:#$0227; Attr:daNone; Str:#$0061#$0307),    // LATIN SMALL LETTER A WITH DOT ABOVE
    (Unicode:#$0228; Attr:daNone; Str:#$0045#$0327),    // LATIN CAPITAL LETTER E WITH CEDILLA
    (Unicode:#$0229; Attr:daNone; Str:#$0065#$0327),    // LATIN SMALL LETTER E WITH CEDILLA
    (Unicode:#$022A; Attr:daNone; Str:#$00D6#$0304),    // LATIN CAPITAL LETTER O WITH DIAERESIS AND MACRON
    (Unicode:#$022B; Attr:daNone; Str:#$00F6#$0304),    // LATIN SMALL LETTER O WITH DIAERESIS AND MACRON
    (Unicode:#$022C; Attr:daNone; Str:#$00D5#$0304),    // LATIN CAPITAL LETTER O WITH TILDE AND MACRON
    (Unicode:#$022D; Attr:daNone; Str:#$00F5#$0304),    // LATIN SMALL LETTER O WITH TILDE AND MACRON
    (Unicode:#$022E; Attr:daNone; Str:#$004F#$0307),    // LATIN CAPITAL LETTER O WITH DOT ABOVE
    (Unicode:#$022F; Attr:daNone; Str:#$006F#$0307),    // LATIN SMALL LETTER O WITH DOT ABOVE
    (Unicode:#$0230; Attr:daNone; Str:#$022E#$0304),    // LATIN CAPITAL LETTER O WITH DOT ABOVE AND MACRON
    (Unicode:#$0231; Attr:daNone; Str:#$022F#$0304),    // LATIN SMALL LETTER O WITH DOT ABOVE AND MACRON
    (Unicode:#$0232; Attr:daNone; Str:#$0059#$0304),    // LATIN CAPITAL LETTER Y WITH MACRON
    (Unicode:#$0233; Attr:daNone; Str:#$0079#$0304),    // LATIN SMALL LETTER Y WITH MACRON
    (Unicode:#$02B0; Attr:daSuper; Str:#$0068),         // MODIFIER LETTER SMALL H
    (Unicode:#$02B1; Attr:daSuper; Str:#$0266),         // MODIFIER LETTER SMALL H WITH HOOK
    (Unicode:#$02B2; Attr:daSuper; Str:#$006A),         // MODIFIER LETTER SMALL J
    (Unicode:#$02B3; Attr:daSuper; Str:#$0072),         // MODIFIER LETTER SMALL R
    (Unicode:#$02B4; Attr:daSuper; Str:#$0279),         // MODIFIER LETTER SMALL TURNED R
    (Unicode:#$02B5; Attr:daSuper; Str:#$027B),         // MODIFIER LETTER SMALL TURNED R WITH HOOK
    (Unicode:#$02B6; Attr:daSuper; Str:#$0281),         // MODIFIER LETTER SMALL CAPITAL INVERTED R
    (Unicode:#$02B7; Attr:daSuper; Str:#$0077),         // MODIFIER LETTER SMALL W
    (Unicode:#$02B8; Attr:daSuper; Str:#$0079),         // MODIFIER LETTER SMALL Y
    (Unicode:#$02D8; Attr:daCompat; Str:#$0020#$0306),  // BREVE
    (Unicode:#$02D9; Attr:daCompat; Str:#$0020#$0307),  // DOT ABOVE
    (Unicode:#$02DA; Attr:daCompat; Str:#$0020#$030A),  // RING ABOVE
    (Unicode:#$02DB; Attr:daCompat; Str:#$0020#$0328),  // OGONEK
    (Unicode:#$02DC; Attr:daCompat; Str:#$0020#$0303),  // SMALL TILDE
    (Unicode:#$02DD; Attr:daCompat; Str:#$0020#$030B),  // DOUBLE ACUTE ACCENT
    (Unicode:#$02E0; Attr:daSuper; Str:#$0263),         // MODIFIER LETTER SMALL GAMMA
    (Unicode:#$02E1; Attr:daSuper; Str:#$006C),         // MODIFIER LETTER SMALL L
    (Unicode:#$02E2; Attr:daSuper; Str:#$0073),         // MODIFIER LETTER SMALL S
    (Unicode:#$02E3; Attr:daSuper; Str:#$0078),         // MODIFIER LETTER SMALL X
    (Unicode:#$02E4; Attr:daSuper; Str:#$0295),         // MODIFIER LETTER SMALL REVERSED GLOTTAL STOP
    (Unicode:#$0340; Attr:daNone; Str:#$0300),          // COMBINING GRAVE TONE MARK
    (Unicode:#$0341; Attr:daNone; Str:#$0301),          // COMBINING ACUTE TONE MARK
    (Unicode:#$0343; Attr:daNone; Str:#$0313),          // COMBINING GREEK KORONIS
    (Unicode:#$0344; Attr:daNone; Str:#$0308#$0301),    // COMBINING GREEK DIALYTIKA TONOS
    (Unicode:#$0374; Attr:daNone; Str:#$02B9),          // GREEK NUMERAL SIGN
    (Unicode:#$037A; Attr:daCompat; Str:#$0020#$0345),  // GREEK YPOGEGRAMMENI
    (Unicode:#$037E; Attr:daNone; Str:#$003B),          // GREEK QUESTION MARK
    (Unicode:#$0384; Attr:daCompat; Str:#$0020#$0301),  // GREEK TONOS
    (Unicode:#$0385; Attr:daNone; Str:#$00A8#$0301),    // GREEK DIALYTIKA TONOS
    (Unicode:#$0386; Attr:daNone; Str:#$0391#$0301),    // GREEK CAPITAL LETTER ALPHA WITH TONOS
    (Unicode:#$0387; Attr:daNone; Str:#$00B7),          // GREEK ANO TELEIA
    (Unicode:#$0388; Attr:daNone; Str:#$0395#$0301),    // GREEK CAPITAL LETTER EPSILON WITH TONOS
    (Unicode:#$0389; Attr:daNone; Str:#$0397#$0301),    // GREEK CAPITAL LETTER ETA WITH TONOS
    (Unicode:#$038A; Attr:daNone; Str:#$0399#$0301),    // GREEK CAPITAL LETTER IOTA WITH TONOS
    (Unicode:#$038C; Attr:daNone; Str:#$039F#$0301),    // GREEK CAPITAL LETTER OMICRON WITH TONOS
    (Unicode:#$038E; Attr:daNone; Str:#$03A5#$0301),    // GREEK CAPITAL LETTER UPSILON WITH TONOS
    (Unicode:#$038F; Attr:daNone; Str:#$03A9#$0301),    // GREEK CAPITAL LETTER OMEGA WITH TONOS
    (Unicode:#$0390; Attr:daNone; Str:#$03CA#$0301),    // GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS
    (Unicode:#$03AA; Attr:daNone; Str:#$0399#$0308),    // GREEK CAPITAL LETTER IOTA WITH DIALYTIKA
    (Unicode:#$03AB; Attr:daNone; Str:#$03A5#$0308),    // GREEK CAPITAL LETTER UPSILON WITH DIALYTIKA
    (Unicode:#$03AC; Attr:daNone; Str:#$03B1#$0301),    // GREEK SMALL LETTER ALPHA WITH TONOS
    (Unicode:#$03AD; Attr:daNone; Str:#$03B5#$0301),    // GREEK SMALL LETTER EPSILON WITH TONOS
    (Unicode:#$03AE; Attr:daNone; Str:#$03B7#$0301),    // GREEK SMALL LETTER ETA WITH TONOS
    (Unicode:#$03AF; Attr:daNone; Str:#$03B9#$0301),    // GREEK SMALL LETTER IOTA WITH TONOS
    (Unicode:#$03B0; Attr:daNone; Str:#$03CB#$0301),    // GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS
    (Unicode:#$03CA; Attr:daNone; Str:#$03B9#$0308),    // GREEK SMALL LETTER IOTA WITH DIALYTIKA
    (Unicode:#$03CB; Attr:daNone; Str:#$03C5#$0308),    // GREEK SMALL LETTER UPSILON WITH DIALYTIKA
    (Unicode:#$03CC; Attr:daNone; Str:#$03BF#$0301),    // GREEK SMALL LETTER OMICRON WITH TONOS
    (Unicode:#$03CD; Attr:daNone; Str:#$03C5#$0301),    // GREEK SMALL LETTER UPSILON WITH TONOS
    (Unicode:#$03CE; Attr:daNone; Str:#$03C9#$0301),    // GREEK SMALL LETTER OMEGA WITH TONOS
    (Unicode:#$03D0; Attr:daCompat; Str:#$03B2),        // GREEK BETA SYMBOL
    (Unicode:#$03D1; Attr:daCompat; Str:#$03B8),        // GREEK THETA SYMBOL
    (Unicode:#$03D2; Attr:daCompat; Str:#$03A5),        // GREEK UPSILON WITH HOOK SYMBOL
    (Unicode:#$03D3; Attr:daNone; Str:#$03D2#$0301),    // GREEK UPSILON WITH ACUTE AND HOOK SYMBOL
    (Unicode:#$03D4; Attr:daNone; Str:#$03D2#$0308),    // GREEK UPSILON WITH DIAERESIS AND HOOK SYMBOL
    (Unicode:#$03D5; Attr:daCompat; Str:#$03C6),        // GREEK PHI SYMBOL
    (Unicode:#$03D6; Attr:daCompat; Str:#$03C0),        // GREEK PI SYMBOL
    (Unicode:#$03F0; Attr:daCompat; Str:#$03BA),        // GREEK KAPPA SYMBOL
    (Unicode:#$03F1; Attr:daCompat; Str:#$03C1),        // GREEK RHO SYMBOL
    (Unicode:#$03F2; Attr:daCompat; Str:#$03C2),        // GREEK LUNATE SIGMA SYMBOL
    (Unicode:#$03F4; Attr:daCompat; Str:#$0398),        // GREEK CAPITAL THETA SYMBOL
    (Unicode:#$03F5; Attr:daCompat; Str:#$03B5),        // GREEK LUNATE EPSILON SYMBOL
    (Unicode:#$0400; Attr:daNone; Str:#$0415#$0300),    // CYRILLIC CAPITAL LETTER IE WITH GRAVE
    (Unicode:#$0401; Attr:daNone; Str:#$0415#$0308),    // CYRILLIC CAPITAL LETTER IO
    (Unicode:#$0403; Attr:daNone; Str:#$0413#$0301),    // CYRILLIC CAPITAL LETTER GJE
    (Unicode:#$0407; Attr:daNone; Str:#$0406#$0308),    // CYRILLIC CAPITAL LETTER YI
    (Unicode:#$040C; Attr:daNone; Str:#$041A#$0301),    // CYRILLIC CAPITAL LETTER KJE
    (Unicode:#$040D; Attr:daNone; Str:#$0418#$0300),    // CYRILLIC CAPITAL LETTER I WITH GRAVE
    (Unicode:#$040E; Attr:daNone; Str:#$0423#$0306),    // CYRILLIC CAPITAL LETTER SHORT U
    (Unicode:#$0419; Attr:daNone; Str:#$0418#$0306),    // CYRILLIC CAPITAL LETTER SHORT I
    (Unicode:#$0439; Attr:daNone; Str:#$0438#$0306),    // CYRILLIC SMALL LETTER SHORT I
    (Unicode:#$0450; Attr:daNone; Str:#$0435#$0300),    // CYRILLIC SMALL LETTER IE WITH GRAVE
    (Unicode:#$0451; Attr:daNone; Str:#$0435#$0308),    // CYRILLIC SMALL LETTER IO
    (Unicode:#$0453; Attr:daNone; Str:#$0433#$0301),    // CYRILLIC SMALL LETTER GJE
    (Unicode:#$0457; Attr:daNone; Str:#$0456#$0308),    // CYRILLIC SMALL LETTER YI
    (Unicode:#$045C; Attr:daNone; Str:#$043A#$0301),    // CYRILLIC SMALL LETTER KJE
    (Unicode:#$045D; Attr:daNone; Str:#$0438#$0300),    // CYRILLIC SMALL LETTER I WITH GRAVE
    (Unicode:#$045E; Attr:daNone; Str:#$0443#$0306),    // CYRILLIC SMALL LETTER SHORT U
    (Unicode:#$0476; Attr:daNone; Str:#$0474#$030F),    // CYRILLIC CAPITAL LETTER IZHITSA WITH DOUBLE GRAVE ACCENT
    (Unicode:#$0477; Attr:daNone; Str:#$0475#$030F),    // CYRILLIC SMALL LETTER IZHITSA WITH DOUBLE GRAVE ACCENT
    (Unicode:#$04C1; Attr:daNone; Str:#$0416#$0306),    // CYRILLIC CAPITAL LETTER ZHE WITH BREVE
    (Unicode:#$04C2; Attr:daNone; Str:#$0436#$0306),    // CYRILLIC SMALL LETTER ZHE WITH BREVE
    (Unicode:#$04D0; Attr:daNone; Str:#$0410#$0306),    // CYRILLIC CAPITAL LETTER A WITH BREVE
    (Unicode:#$04D1; Attr:daNone; Str:#$0430#$0306),    // CYRILLIC SMALL LETTER A WITH BREVE
    (Unicode:#$04D2; Attr:daNone; Str:#$0410#$0308),    // CYRILLIC CAPITAL LETTER A WITH DIAERESIS
    (Unicode:#$04D3; Attr:daNone; Str:#$0430#$0308),    // CYRILLIC SMALL LETTER A WITH DIAERESIS
    (Unicode:#$04D6; Attr:daNone; Str:#$0415#$0306),    // CYRILLIC CAPITAL LETTER IE WITH BREVE
    (Unicode:#$04D7; Attr:daNone; Str:#$0435#$0306),    // CYRILLIC SMALL LETTER IE WITH BREVE
    (Unicode:#$04DA; Attr:daNone; Str:#$04D8#$0308),    // CYRILLIC CAPITAL LETTER SCHWA WITH DIAERESIS
    (Unicode:#$04DB; Attr:daNone; Str:#$04D9#$0308),    // CYRILLIC SMALL LETTER SCHWA WITH DIAERESIS
    (Unicode:#$04DC; Attr:daNone; Str:#$0416#$0308),    // CYRILLIC CAPITAL LETTER ZHE WITH DIAERESIS
    (Unicode:#$04DD; Attr:daNone; Str:#$0436#$0308),    // CYRILLIC SMALL LETTER ZHE WITH DIAERESIS
    (Unicode:#$04DE; Attr:daNone; Str:#$0417#$0308),    // CYRILLIC CAPITAL LETTER ZE WITH DIAERESIS
    (Unicode:#$04DF; Attr:daNone; Str:#$0437#$0308),    // CYRILLIC SMALL LETTER ZE WITH DIAERESIS
    (Unicode:#$04E2; Attr:daNone; Str:#$0418#$0304),    // CYRILLIC CAPITAL LETTER I WITH MACRON
    (Unicode:#$04E3; Attr:daNone; Str:#$0438#$0304),    // CYRILLIC SMALL LETTER I WITH MACRON
    (Unicode:#$04E4; Attr:daNone; Str:#$0418#$0308),    // CYRILLIC CAPITAL LETTER I WITH DIAERESIS
    (Unicode:#$04E5; Attr:daNone; Str:#$0438#$0308),    // CYRILLIC SMALL LETTER I WITH DIAERESIS
    (Unicode:#$04E6; Attr:daNone; Str:#$041E#$0308),    // CYRILLIC CAPITAL LETTER O WITH DIAERESIS
    (Unicode:#$04E7; Attr:daNone; Str:#$043E#$0308),    // CYRILLIC SMALL LETTER O WITH DIAERESIS
    (Unicode:#$04EA; Attr:daNone; Str:#$04E8#$0308),    // CYRILLIC CAPITAL LETTER BARRED O WITH DIAERESIS
    (Unicode:#$04EB; Attr:daNone; Str:#$04E9#$0308),    // CYRILLIC SMALL LETTER BARRED O WITH DIAERESIS
    (Unicode:#$04EC; Attr:daNone; Str:#$042D#$0308),    // CYRILLIC CAPITAL LETTER E WITH DIAERESIS
    (Unicode:#$04ED; Attr:daNone; Str:#$044D#$0308),    // CYRILLIC SMALL LETTER E WITH DIAERESIS
    (Unicode:#$04EE; Attr:daNone; Str:#$0423#$0304),    // CYRILLIC CAPITAL LETTER U WITH MACRON
    (Unicode:#$04EF; Attr:daNone; Str:#$0443#$0304),    // CYRILLIC SMALL LETTER U WITH MACRON
    (Unicode:#$04F0; Attr:daNone; Str:#$0423#$0308),    // CYRILLIC CAPITAL LETTER U WITH DIAERESIS
    (Unicode:#$04F1; Attr:daNone; Str:#$0443#$0308),    // CYRILLIC SMALL LETTER U WITH DIAERESIS
    (Unicode:#$04F2; Attr:daNone; Str:#$0423#$030B),    // CYRILLIC CAPITAL LETTER U WITH DOUBLE ACUTE
    (Unicode:#$04F3; Attr:daNone; Str:#$0443#$030B),    // CYRILLIC SMALL LETTER U WITH DOUBLE ACUTE
    (Unicode:#$04F4; Attr:daNone; Str:#$0427#$0308),    // CYRILLIC CAPITAL LETTER CHE WITH DIAERESIS
    (Unicode:#$04F5; Attr:daNone; Str:#$0447#$0308),    // CYRILLIC SMALL LETTER CHE WITH DIAERESIS
    (Unicode:#$04F8; Attr:daNone; Str:#$042B#$0308),    // CYRILLIC CAPITAL LETTER YERU WITH DIAERESIS
    (Unicode:#$04F9; Attr:daNone; Str:#$044B#$0308),    // CYRILLIC SMALL LETTER YERU WITH DIAERESIS
    (Unicode:#$0587; Attr:daCompat; Str:#$0565#$0582),  // ARMENIAN SMALL LIGATURE ECH YIWN
    (Unicode:#$0622; Attr:daNone; Str:#$0627#$0653),    // ARABIC LETTER ALEF WITH MADDA ABOVE
    (Unicode:#$0623; Attr:daNone; Str:#$0627#$0654),    // ARABIC LETTER ALEF WITH HAMZA ABOVE
    (Unicode:#$0624; Attr:daNone; Str:#$0648#$0654),    // ARABIC LETTER WAW WITH HAMZA ABOVE
    (Unicode:#$0625; Attr:daNone; Str:#$0627#$0655),    // ARABIC LETTER ALEF WITH HAMZA BELOW
    (Unicode:#$0626; Attr:daNone; Str:#$064A#$0654),    // ARABIC LETTER YEH WITH HAMZA ABOVE
    (Unicode:#$0675; Attr:daCompat; Str:#$0627#$0674),  // ARABIC LETTER HIGH HAMZA ALEF
    (Unicode:#$0676; Attr:daCompat; Str:#$0648#$0674),  // ARABIC LETTER HIGH HAMZA WAW
    (Unicode:#$0677; Attr:daCompat; Str:#$06C7#$0674),  // ARABIC LETTER U WITH HAMZA ABOVE
    (Unicode:#$0678; Attr:daCompat; Str:#$064A#$0674),  // ARABIC LETTER HIGH HAMZA YEH
    (Unicode:#$06C0; Attr:daNone; Str:#$06D5#$0654),    // ARABIC LETTER HEH WITH YEH ABOVE
    (Unicode:#$06C2; Attr:daNone; Str:#$06C1#$0654),    // ARABIC LETTER HEH GOAL WITH HAMZA ABOVE
    (Unicode:#$06D3; Attr:daNone; Str:#$06D2#$0654),    // ARABIC LETTER YEH BARREE WITH HAMZA ABOVE
    (Unicode:#$0929; Attr:daNone; Str:#$0928#$093C),    // DEVANAGARI LETTER NNNA
    (Unicode:#$0931; Attr:daNone; Str:#$0930#$093C),    // DEVANAGARI LETTER RRA
    (Unicode:#$0934; Attr:daNone; Str:#$0933#$093C),    // DEVANAGARI LETTER LLLA
    (Unicode:#$0958; Attr:daNone; Str:#$0915#$093C),    // DEVANAGARI LETTER QA
    (Unicode:#$0959; Attr:daNone; Str:#$0916#$093C),    // DEVANAGARI LETTER KHHA
    (Unicode:#$095A; Attr:daNone; Str:#$0917#$093C),    // DEVANAGARI LETTER GHHA
    (Unicode:#$095B; Attr:daNone; Str:#$091C#$093C),    // DEVANAGARI LETTER ZA
    (Unicode:#$095C; Attr:daNone; Str:#$0921#$093C),    // DEVANAGARI LETTER DDDHA
    (Unicode:#$095D; Attr:daNone; Str:#$0922#$093C),    // DEVANAGARI LETTER RHA
    (Unicode:#$095E; Attr:daNone; Str:#$092B#$093C),    // DEVANAGARI LETTER FA
    (Unicode:#$095F; Attr:daNone; Str:#$092F#$093C),    // DEVANAGARI LETTER YYA
    (Unicode:#$09CB; Attr:daNone; Str:#$09C7#$09BE),    // BENGALI VOWEL SIGN O
    (Unicode:#$09CC; Attr:daNone; Str:#$09C7#$09D7),    // BENGALI VOWEL SIGN AU
    (Unicode:#$09DC; Attr:daNone; Str:#$09A1#$09BC),    // BENGALI LETTER RRA
    (Unicode:#$09DD; Attr:daNone; Str:#$09A2#$09BC),    // BENGALI LETTER RHA
    (Unicode:#$09DF; Attr:daNone; Str:#$09AF#$09BC),    // BENGALI LETTER YYA
    (Unicode:#$0A33; Attr:daNone; Str:#$0A32#$0A3C),    // GURMUKHI LETTER LLA
    (Unicode:#$0A36; Attr:daNone; Str:#$0A38#$0A3C),    // GURMUKHI LETTER SHA
    (Unicode:#$0A59; Attr:daNone; Str:#$0A16#$0A3C),    // GURMUKHI LETTER KHHA
    (Unicode:#$0A5A; Attr:daNone; Str:#$0A17#$0A3C),    // GURMUKHI LETTER GHHA
    (Unicode:#$0A5B; Attr:daNone; Str:#$0A1C#$0A3C),    // GURMUKHI LETTER ZA
    (Unicode:#$0A5E; Attr:daNone; Str:#$0A2B#$0A3C),    // GURMUKHI LETTER FA
    (Unicode:#$0B48; Attr:daNone; Str:#$0B47#$0B56),    // ORIYA VOWEL SIGN AI
    (Unicode:#$0B4B; Attr:daNone; Str:#$0B47#$0B3E),    // ORIYA VOWEL SIGN O
    (Unicode:#$0B4C; Attr:daNone; Str:#$0B47#$0B57),    // ORIYA VOWEL SIGN AU
    (Unicode:#$0B5C; Attr:daNone; Str:#$0B21#$0B3C),    // ORIYA LETTER RRA
    (Unicode:#$0B5D; Attr:daNone; Str:#$0B22#$0B3C),    // ORIYA LETTER RHA
    (Unicode:#$0B94; Attr:daNone; Str:#$0B92#$0BD7),    // TAMIL LETTER AU
    (Unicode:#$0BCA; Attr:daNone; Str:#$0BC6#$0BBE),    // TAMIL VOWEL SIGN O
    (Unicode:#$0BCB; Attr:daNone; Str:#$0BC7#$0BBE),    // TAMIL VOWEL SIGN OO
    (Unicode:#$0BCC; Attr:daNone; Str:#$0BC6#$0BD7),    // TAMIL VOWEL SIGN AU
    (Unicode:#$0C48; Attr:daNone; Str:#$0C46#$0C56),    // TELUGU VOWEL SIGN AI
    (Unicode:#$0CC0; Attr:daNone; Str:#$0CBF#$0CD5),    // KANNADA VOWEL SIGN II
    (Unicode:#$0CC7; Attr:daNone; Str:#$0CC6#$0CD5),    // KANNADA VOWEL SIGN EE
    (Unicode:#$0CC8; Attr:daNone; Str:#$0CC6#$0CD6),    // KANNADA VOWEL SIGN AI
    (Unicode:#$0CCA; Attr:daNone; Str:#$0CC6#$0CC2),    // KANNADA VOWEL SIGN O
    (Unicode:#$0CCB; Attr:daNone; Str:#$0CCA#$0CD5),    // KANNADA VOWEL SIGN OO
    (Unicode:#$0D4A; Attr:daNone; Str:#$0D46#$0D3E),    // MALAYALAM VOWEL SIGN O
    (Unicode:#$0D4B; Attr:daNone; Str:#$0D47#$0D3E),    // MALAYALAM VOWEL SIGN OO
    (Unicode:#$0D4C; Attr:daNone; Str:#$0D46#$0D57),    // MALAYALAM VOWEL SIGN AU
    (Unicode:#$0DDA; Attr:daNone; Str:#$0DD9#$0DCA),    // SINHALA VOWEL SIGN DIGA KOMBUVA
    (Unicode:#$0DDC; Attr:daNone; Str:#$0DD9#$0DCF),    // SINHALA VOWEL SIGN KOMBUVA HAA AELA-PILLA
    (Unicode:#$0DDD; Attr:daNone; Str:#$0DDC#$0DCA),    // SINHALA VOWEL SIGN KOMBUVA HAA DIGA AELA-PILLA
    (Unicode:#$0DDE; Attr:daNone; Str:#$0DD9#$0DDF),    // SINHALA VOWEL SIGN KOMBUVA HAA GAYANUKITTA
    (Unicode:#$0E33; Attr:daCompat; Str:#$0E4D#$0E32),  // THAI CHARACTER SARA AM
    (Unicode:#$0EB3; Attr:daCompat; Str:#$0ECD#$0EB2),  // LAO VOWEL SIGN AM
    (Unicode:#$0EDC; Attr:daCompat; Str:#$0EAB#$0E99),  // LAO HO NO
    (Unicode:#$0EDD; Attr:daCompat; Str:#$0EAB#$0EA1),  // LAO HO MO
    (Unicode:#$0F0C; Attr:daNoBreak; Str:#$0F0B),       // TIBETAN MARK DELIMITER TSHEG BSTAR
    (Unicode:#$0F43; Attr:daNone; Str:#$0F42#$0FB7),    // TIBETAN LETTER GHA
    (Unicode:#$0F4D; Attr:daNone; Str:#$0F4C#$0FB7),    // TIBETAN LETTER DDHA
    (Unicode:#$0F52; Attr:daNone; Str:#$0F51#$0FB7),    // TIBETAN LETTER DHA
    (Unicode:#$0F57; Attr:daNone; Str:#$0F56#$0FB7),    // TIBETAN LETTER BHA
    (Unicode:#$0F5C; Attr:daNone; Str:#$0F5B#$0FB7),    // TIBETAN LETTER DZHA
    (Unicode:#$0F69; Attr:daNone; Str:#$0F40#$0FB5),    // TIBETAN LETTER KSSA
    (Unicode:#$0F73; Attr:daNone; Str:#$0F71#$0F72),    // TIBETAN VOWEL SIGN II
    (Unicode:#$0F75; Attr:daNone; Str:#$0F71#$0F74),    // TIBETAN VOWEL SIGN UU
    (Unicode:#$0F76; Attr:daNone; Str:#$0FB2#$0F80),    // TIBETAN VOWEL SIGN VOCALIC R
    (Unicode:#$0F77; Attr:daCompat; Str:#$0FB2#$0F81),  // TIBETAN VOWEL SIGN VOCALIC RR
    (Unicode:#$0F78; Attr:daNone; Str:#$0FB3#$0F80),    // TIBETAN VOWEL SIGN VOCALIC L
    (Unicode:#$0F79; Attr:daCompat; Str:#$0FB3#$0F81),  // TIBETAN VOWEL SIGN VOCALIC LL
    (Unicode:#$0F81; Attr:daNone; Str:#$0F71#$0F80),    // TIBETAN VOWEL SIGN REVERSED II
    (Unicode:#$0F93; Attr:daNone; Str:#$0F92#$0FB7),    // TIBETAN SUBJOINED LETTER GHA
    (Unicode:#$0F9D; Attr:daNone; Str:#$0F9C#$0FB7),    // TIBETAN SUBJOINED LETTER DDHA
    (Unicode:#$0FA2; Attr:daNone; Str:#$0FA1#$0FB7),    // TIBETAN SUBJOINED LETTER DHA
    (Unicode:#$0FA7; Attr:daNone; Str:#$0FA6#$0FB7),    // TIBETAN SUBJOINED LETTER BHA
    (Unicode:#$0FAC; Attr:daNone; Str:#$0FAB#$0FB7),    // TIBETAN SUBJOINED LETTER DZHA
    (Unicode:#$0FB9; Attr:daNone; Str:#$0F90#$0FB5),    // TIBETAN SUBJOINED LETTER KSSA
    (Unicode:#$1026; Attr:daNone; Str:#$1025#$102E),    // MYANMAR LETTER UU
    (Unicode:#$1E00; Attr:daNone; Str:#$0041#$0325),    // LATIN CAPITAL LETTER A WITH RING BELOW
    (Unicode:#$1E01; Attr:daNone; Str:#$0061#$0325),    // LATIN SMALL LETTER A WITH RING BELOW
    (Unicode:#$1E02; Attr:daNone; Str:#$0042#$0307),    // LATIN CAPITAL LETTER B WITH DOT ABOVE
    (Unicode:#$1E03; Attr:daNone; Str:#$0062#$0307),    // LATIN SMALL LETTER B WITH DOT ABOVE
    (Unicode:#$1E04; Attr:daNone; Str:#$0042#$0323),    // LATIN CAPITAL LETTER B WITH DOT BELOW
    (Unicode:#$1E05; Attr:daNone; Str:#$0062#$0323),    // LATIN SMALL LETTER B WITH DOT BELOW
    (Unicode:#$1E06; Attr:daNone; Str:#$0042#$0331),    // LATIN CAPITAL LETTER B WITH LINE BELOW
    (Unicode:#$1E07; Attr:daNone; Str:#$0062#$0331),    // LATIN SMALL LETTER B WITH LINE BELOW
    (Unicode:#$1E08; Attr:daNone; Str:#$00C7#$0301),    // LATIN CAPITAL LETTER C WITH CEDILLA AND ACUTE
    (Unicode:#$1E09; Attr:daNone; Str:#$00E7#$0301),    // LATIN SMALL LETTER C WITH CEDILLA AND ACUTE
    (Unicode:#$1E0A; Attr:daNone; Str:#$0044#$0307),    // LATIN CAPITAL LETTER D WITH DOT ABOVE
    (Unicode:#$1E0B; Attr:daNone; Str:#$0064#$0307),    // LATIN SMALL LETTER D WITH DOT ABOVE
    (Unicode:#$1E0C; Attr:daNone; Str:#$0044#$0323),    // LATIN CAPITAL LETTER D WITH DOT BELOW
    (Unicode:#$1E0D; Attr:daNone; Str:#$0064#$0323),    // LATIN SMALL LETTER D WITH DOT BELOW
    (Unicode:#$1E0E; Attr:daNone; Str:#$0044#$0331),    // LATIN CAPITAL LETTER D WITH LINE BELOW
    (Unicode:#$1E0F; Attr:daNone; Str:#$0064#$0331),    // LATIN SMALL LETTER D WITH LINE BELOW
    (Unicode:#$1E10; Attr:daNone; Str:#$0044#$0327),    // LATIN CAPITAL LETTER D WITH CEDILLA
    (Unicode:#$1E11; Attr:daNone; Str:#$0064#$0327),    // LATIN SMALL LETTER D WITH CEDILLA
    (Unicode:#$1E12; Attr:daNone; Str:#$0044#$032D),    // LATIN CAPITAL LETTER D WITH CIRCUMFLEX BELOW
    (Unicode:#$1E13; Attr:daNone; Str:#$0064#$032D),    // LATIN SMALL LETTER D WITH CIRCUMFLEX BELOW
    (Unicode:#$1E14; Attr:daNone; Str:#$0112#$0300),    // LATIN CAPITAL LETTER E WITH MACRON AND GRAVE
    (Unicode:#$1E15; Attr:daNone; Str:#$0113#$0300),    // LATIN SMALL LETTER E WITH MACRON AND GRAVE
    (Unicode:#$1E16; Attr:daNone; Str:#$0112#$0301),    // LATIN CAPITAL LETTER E WITH MACRON AND ACUTE
    (Unicode:#$1E17; Attr:daNone; Str:#$0113#$0301),    // LATIN SMALL LETTER E WITH MACRON AND ACUTE
    (Unicode:#$1E18; Attr:daNone; Str:#$0045#$032D),    // LATIN CAPITAL LETTER E WITH CIRCUMFLEX BELOW
    (Unicode:#$1E19; Attr:daNone; Str:#$0065#$032D),    // LATIN SMALL LETTER E WITH CIRCUMFLEX BELOW
    (Unicode:#$1E1A; Attr:daNone; Str:#$0045#$0330),    // LATIN CAPITAL LETTER E WITH TILDE BELOW
    (Unicode:#$1E1B; Attr:daNone; Str:#$0065#$0330),    // LATIN SMALL LETTER E WITH TILDE BELOW
    (Unicode:#$1E1C; Attr:daNone; Str:#$0228#$0306),    // LATIN CAPITAL LETTER E WITH CEDILLA AND BREVE
    (Unicode:#$1E1D; Attr:daNone; Str:#$0229#$0306),    // LATIN SMALL LETTER E WITH CEDILLA AND BREVE
    (Unicode:#$1E1E; Attr:daNone; Str:#$0046#$0307),    // LATIN CAPITAL LETTER F WITH DOT ABOVE
    (Unicode:#$1E1F; Attr:daNone; Str:#$0066#$0307),    // LATIN SMALL LETTER F WITH DOT ABOVE
    (Unicode:#$1E20; Attr:daNone; Str:#$0047#$0304),    // LATIN CAPITAL LETTER G WITH MACRON
    (Unicode:#$1E21; Attr:daNone; Str:#$0067#$0304),    // LATIN SMALL LETTER G WITH MACRON
    (Unicode:#$1E22; Attr:daNone; Str:#$0048#$0307),    // LATIN CAPITAL LETTER H WITH DOT ABOVE
    (Unicode:#$1E23; Attr:daNone; Str:#$0068#$0307),    // LATIN SMALL LETTER H WITH DOT ABOVE
    (Unicode:#$1E24; Attr:daNone; Str:#$0048#$0323),    // LATIN CAPITAL LETTER H WITH DOT BELOW
    (Unicode:#$1E25; Attr:daNone; Str:#$0068#$0323),    // LATIN SMALL LETTER H WITH DOT BELOW
    (Unicode:#$1E26; Attr:daNone; Str:#$0048#$0308),    // LATIN CAPITAL LETTER H WITH DIAERESIS
    (Unicode:#$1E27; Attr:daNone; Str:#$0068#$0308),    // LATIN SMALL LETTER H WITH DIAERESIS
    (Unicode:#$1E28; Attr:daNone; Str:#$0048#$0327),    // LATIN CAPITAL LETTER H WITH CEDILLA
    (Unicode:#$1E29; Attr:daNone; Str:#$0068#$0327),    // LATIN SMALL LETTER H WITH CEDILLA
    (Unicode:#$1E2A; Attr:daNone; Str:#$0048#$032E),    // LATIN CAPITAL LETTER H WITH BREVE BELOW
    (Unicode:#$1E2B; Attr:daNone; Str:#$0068#$032E),    // LATIN SMALL LETTER H WITH BREVE BELOW
    (Unicode:#$1E2C; Attr:daNone; Str:#$0049#$0330),    // LATIN CAPITAL LETTER I WITH TILDE BELOW
    (Unicode:#$1E2D; Attr:daNone; Str:#$0069#$0330),    // LATIN SMALL LETTER I WITH TILDE BELOW
    (Unicode:#$1E2E; Attr:daNone; Str:#$00CF#$0301),    // LATIN CAPITAL LETTER I WITH DIAERESIS AND ACUTE
    (Unicode:#$1E2F; Attr:daNone; Str:#$00EF#$0301),    // LATIN SMALL LETTER I WITH DIAERESIS AND ACUTE
    (Unicode:#$1E30; Attr:daNone; Str:#$004B#$0301),    // LATIN CAPITAL LETTER K WITH ACUTE
    (Unicode:#$1E31; Attr:daNone; Str:#$006B#$0301),    // LATIN SMALL LETTER K WITH ACUTE
    (Unicode:#$1E32; Attr:daNone; Str:#$004B#$0323),    // LATIN CAPITAL LETTER K WITH DOT BELOW
    (Unicode:#$1E33; Attr:daNone; Str:#$006B#$0323),    // LATIN SMALL LETTER K WITH DOT BELOW
    (Unicode:#$1E34; Attr:daNone; Str:#$004B#$0331),    // LATIN CAPITAL LETTER K WITH LINE BELOW
    (Unicode:#$1E35; Attr:daNone; Str:#$006B#$0331),    // LATIN SMALL LETTER K WITH LINE BELOW
    (Unicode:#$1E36; Attr:daNone; Str:#$004C#$0323),    // LATIN CAPITAL LETTER L WITH DOT BELOW
    (Unicode:#$1E37; Attr:daNone; Str:#$006C#$0323),    // LATIN SMALL LETTER L WITH DOT BELOW
    (Unicode:#$1E38; Attr:daNone; Str:#$1E36#$0304),    // LATIN CAPITAL LETTER L WITH DOT BELOW AND MACRON
    (Unicode:#$1E39; Attr:daNone; Str:#$1E37#$0304),    // LATIN SMALL LETTER L WITH DOT BELOW AND MACRON
    (Unicode:#$1E3A; Attr:daNone; Str:#$004C#$0331),    // LATIN CAPITAL LETTER L WITH LINE BELOW
    (Unicode:#$1E3B; Attr:daNone; Str:#$006C#$0331),    // LATIN SMALL LETTER L WITH LINE BELOW
    (Unicode:#$1E3C; Attr:daNone; Str:#$004C#$032D),    // LATIN CAPITAL LETTER L WITH CIRCUMFLEX BELOW
    (Unicode:#$1E3D; Attr:daNone; Str:#$006C#$032D),    // LATIN SMALL LETTER L WITH CIRCUMFLEX BELOW
    (Unicode:#$1E3E; Attr:daNone; Str:#$004D#$0301),    // LATIN CAPITAL LETTER M WITH ACUTE
    (Unicode:#$1E3F; Attr:daNone; Str:#$006D#$0301),    // LATIN SMALL LETTER M WITH ACUTE
    (Unicode:#$1E40; Attr:daNone; Str:#$004D#$0307),    // LATIN CAPITAL LETTER M WITH DOT ABOVE
    (Unicode:#$1E41; Attr:daNone; Str:#$006D#$0307),    // LATIN SMALL LETTER M WITH DOT ABOVE
    (Unicode:#$1E42; Attr:daNone; Str:#$004D#$0323),    // LATIN CAPITAL LETTER M WITH DOT BELOW
    (Unicode:#$1E43; Attr:daNone; Str:#$006D#$0323),    // LATIN SMALL LETTER M WITH DOT BELOW
    (Unicode:#$1E44; Attr:daNone; Str:#$004E#$0307),    // LATIN CAPITAL LETTER N WITH DOT ABOVE
    (Unicode:#$1E45; Attr:daNone; Str:#$006E#$0307),    // LATIN SMALL LETTER N WITH DOT ABOVE
    (Unicode:#$1E46; Attr:daNone; Str:#$004E#$0323),    // LATIN CAPITAL LETTER N WITH DOT BELOW
    (Unicode:#$1E47; Attr:daNone; Str:#$006E#$0323),    // LATIN SMALL LETTER N WITH DOT BELOW
    (Unicode:#$1E48; Attr:daNone; Str:#$004E#$0331),    // LATIN CAPITAL LETTER N WITH LINE BELOW
    (Unicode:#$1E49; Attr:daNone; Str:#$006E#$0331),    // LATIN SMALL LETTER N WITH LINE BELOW
    (Unicode:#$1E4A; Attr:daNone; Str:#$004E#$032D),    // LATIN CAPITAL LETTER N WITH CIRCUMFLEX BELOW
    (Unicode:#$1E4B; Attr:daNone; Str:#$006E#$032D),    // LATIN SMALL LETTER N WITH CIRCUMFLEX BELOW
    (Unicode:#$1E4C; Attr:daNone; Str:#$00D5#$0301),    // LATIN CAPITAL LETTER O WITH TILDE AND ACUTE
    (Unicode:#$1E4D; Attr:daNone; Str:#$00F5#$0301),    // LATIN SMALL LETTER O WITH TILDE AND ACUTE
    (Unicode:#$1E4E; Attr:daNone; Str:#$00D5#$0308),    // LATIN CAPITAL LETTER O WITH TILDE AND DIAERESIS
    (Unicode:#$1E4F; Attr:daNone; Str:#$00F5#$0308),    // LATIN SMALL LETTER O WITH TILDE AND DIAERESIS
    (Unicode:#$1E50; Attr:daNone; Str:#$014C#$0300),    // LATIN CAPITAL LETTER O WITH MACRON AND GRAVE
    (Unicode:#$1E51; Attr:daNone; Str:#$014D#$0300),    // LATIN SMALL LETTER O WITH MACRON AND GRAVE
    (Unicode:#$1E52; Attr:daNone; Str:#$014C#$0301),    // LATIN CAPITAL LETTER O WITH MACRON AND ACUTE
    (Unicode:#$1E53; Attr:daNone; Str:#$014D#$0301),    // LATIN SMALL LETTER O WITH MACRON AND ACUTE
    (Unicode:#$1E54; Attr:daNone; Str:#$0050#$0301),    // LATIN CAPITAL LETTER P WITH ACUTE
    (Unicode:#$1E55; Attr:daNone; Str:#$0070#$0301),    // LATIN SMALL LETTER P WITH ACUTE
    (Unicode:#$1E56; Attr:daNone; Str:#$0050#$0307),    // LATIN CAPITAL LETTER P WITH DOT ABOVE
    (Unicode:#$1E57; Attr:daNone; Str:#$0070#$0307),    // LATIN SMALL LETTER P WITH DOT ABOVE
    (Unicode:#$1E58; Attr:daNone; Str:#$0052#$0307),    // LATIN CAPITAL LETTER R WITH DOT ABOVE
    (Unicode:#$1E59; Attr:daNone; Str:#$0072#$0307),    // LATIN SMALL LETTER R WITH DOT ABOVE
    (Unicode:#$1E5A; Attr:daNone; Str:#$0052#$0323),    // LATIN CAPITAL LETTER R WITH DOT BELOW
    (Unicode:#$1E5B; Attr:daNone; Str:#$0072#$0323),    // LATIN SMALL LETTER R WITH DOT BELOW
    (Unicode:#$1E5C; Attr:daNone; Str:#$1E5A#$0304),    // LATIN CAPITAL LETTER R WITH DOT BELOW AND MACRON
    (Unicode:#$1E5D; Attr:daNone; Str:#$1E5B#$0304),    // LATIN SMALL LETTER R WITH DOT BELOW AND MACRON
    (Unicode:#$1E5E; Attr:daNone; Str:#$0052#$0331),    // LATIN CAPITAL LETTER R WITH LINE BELOW
    (Unicode:#$1E5F; Attr:daNone; Str:#$0072#$0331),    // LATIN SMALL LETTER R WITH LINE BELOW
    (Unicode:#$1E60; Attr:daNone; Str:#$0053#$0307),    // LATIN CAPITAL LETTER S WITH DOT ABOVE
    (Unicode:#$1E61; Attr:daNone; Str:#$0073#$0307),    // LATIN SMALL LETTER S WITH DOT ABOVE
    (Unicode:#$1E62; Attr:daNone; Str:#$0053#$0323),    // LATIN CAPITAL LETTER S WITH DOT BELOW
    (Unicode:#$1E63; Attr:daNone; Str:#$0073#$0323),    // LATIN SMALL LETTER S WITH DOT BELOW
    (Unicode:#$1E64; Attr:daNone; Str:#$015A#$0307),    // LATIN CAPITAL LETTER S WITH ACUTE AND DOT ABOVE
    (Unicode:#$1E65; Attr:daNone; Str:#$015B#$0307),    // LATIN SMALL LETTER S WITH ACUTE AND DOT ABOVE
    (Unicode:#$1E66; Attr:daNone; Str:#$0160#$0307),    // LATIN CAPITAL LETTER S WITH CARON AND DOT ABOVE
    (Unicode:#$1E67; Attr:daNone; Str:#$0161#$0307),    // LATIN SMALL LETTER S WITH CARON AND DOT ABOVE
    (Unicode:#$1E68; Attr:daNone; Str:#$1E62#$0307),    // LATIN CAPITAL LETTER S WITH DOT BELOW AND DOT ABOVE
    (Unicode:#$1E69; Attr:daNone; Str:#$1E63#$0307),    // LATIN SMALL LETTER S WITH DOT BELOW AND DOT ABOVE
    (Unicode:#$1E6A; Attr:daNone; Str:#$0054#$0307),    // LATIN CAPITAL LETTER T WITH DOT ABOVE
    (Unicode:#$1E6B; Attr:daNone; Str:#$0074#$0307),    // LATIN SMALL LETTER T WITH DOT ABOVE
    (Unicode:#$1E6C; Attr:daNone; Str:#$0054#$0323),    // LATIN CAPITAL LETTER T WITH DOT BELOW
    (Unicode:#$1E6D; Attr:daNone; Str:#$0074#$0323),    // LATIN SMALL LETTER T WITH DOT BELOW
    (Unicode:#$1E6E; Attr:daNone; Str:#$0054#$0331),    // LATIN CAPITAL LETTER T WITH LINE BELOW
    (Unicode:#$1E6F; Attr:daNone; Str:#$0074#$0331),    // LATIN SMALL LETTER T WITH LINE BELOW
    (Unicode:#$1E70; Attr:daNone; Str:#$0054#$032D),    // LATIN CAPITAL LETTER T WITH CIRCUMFLEX BELOW
    (Unicode:#$1E71; Attr:daNone; Str:#$0074#$032D),    // LATIN SMALL LETTER T WITH CIRCUMFLEX BELOW
    (Unicode:#$1E72; Attr:daNone; Str:#$0055#$0324),    // LATIN CAPITAL LETTER U WITH DIAERESIS BELOW
    (Unicode:#$1E73; Attr:daNone; Str:#$0075#$0324),    // LATIN SMALL LETTER U WITH DIAERESIS BELOW
    (Unicode:#$1E74; Attr:daNone; Str:#$0055#$0330),    // LATIN CAPITAL LETTER U WITH TILDE BELOW
    (Unicode:#$1E75; Attr:daNone; Str:#$0075#$0330),    // LATIN SMALL LETTER U WITH TILDE BELOW
    (Unicode:#$1E76; Attr:daNone; Str:#$0055#$032D),    // LATIN CAPITAL LETTER U WITH CIRCUMFLEX BELOW
    (Unicode:#$1E77; Attr:daNone; Str:#$0075#$032D),    // LATIN SMALL LETTER U WITH CIRCUMFLEX BELOW
    (Unicode:#$1E78; Attr:daNone; Str:#$0168#$0301),    // LATIN CAPITAL LETTER U WITH TILDE AND ACUTE
    (Unicode:#$1E79; Attr:daNone; Str:#$0169#$0301),    // LATIN SMALL LETTER U WITH TILDE AND ACUTE
    (Unicode:#$1E7A; Attr:daNone; Str:#$016A#$0308),    // LATIN CAPITAL LETTER U WITH MACRON AND DIAERESIS
    (Unicode:#$1E7B; Attr:daNone; Str:#$016B#$0308),    // LATIN SMALL LETTER U WITH MACRON AND DIAERESIS
    (Unicode:#$1E7C; Attr:daNone; Str:#$0056#$0303),    // LATIN CAPITAL LETTER V WITH TILDE
    (Unicode:#$1E7D; Attr:daNone; Str:#$0076#$0303),    // LATIN SMALL LETTER V WITH TILDE
    (Unicode:#$1E7E; Attr:daNone; Str:#$0056#$0323),    // LATIN CAPITAL LETTER V WITH DOT BELOW
    (Unicode:#$1E7F; Attr:daNone; Str:#$0076#$0323),    // LATIN SMALL LETTER V WITH DOT BELOW
    (Unicode:#$1E80; Attr:daNone; Str:#$0057#$0300),    // LATIN CAPITAL LETTER W WITH GRAVE
    (Unicode:#$1E81; Attr:daNone; Str:#$0077#$0300),    // LATIN SMALL LETTER W WITH GRAVE
    (Unicode:#$1E82; Attr:daNone; Str:#$0057#$0301),    // LATIN CAPITAL LETTER W WITH ACUTE
    (Unicode:#$1E83; Attr:daNone; Str:#$0077#$0301),    // LATIN SMALL LETTER W WITH ACUTE
    (Unicode:#$1E84; Attr:daNone; Str:#$0057#$0308),    // LATIN CAPITAL LETTER W WITH DIAERESIS
    (Unicode:#$1E85; Attr:daNone; Str:#$0077#$0308),    // LATIN SMALL LETTER W WITH DIAERESIS
    (Unicode:#$1E86; Attr:daNone; Str:#$0057#$0307),    // LATIN CAPITAL LETTER W WITH DOT ABOVE
    (Unicode:#$1E87; Attr:daNone; Str:#$0077#$0307),    // LATIN SMALL LETTER W WITH DOT ABOVE
    (Unicode:#$1E88; Attr:daNone; Str:#$0057#$0323),    // LATIN CAPITAL LETTER W WITH DOT BELOW
    (Unicode:#$1E89; Attr:daNone; Str:#$0077#$0323),    // LATIN SMALL LETTER W WITH DOT BELOW
    (Unicode:#$1E8A; Attr:daNone; Str:#$0058#$0307),    // LATIN CAPITAL LETTER X WITH DOT ABOVE
    (Unicode:#$1E8B; Attr:daNone; Str:#$0078#$0307),    // LATIN SMALL LETTER X WITH DOT ABOVE
    (Unicode:#$1E8C; Attr:daNone; Str:#$0058#$0308),    // LATIN CAPITAL LETTER X WITH DIAERESIS
    (Unicode:#$1E8D; Attr:daNone; Str:#$0078#$0308),    // LATIN SMALL LETTER X WITH DIAERESIS
    (Unicode:#$1E8E; Attr:daNone; Str:#$0059#$0307),    // LATIN CAPITAL LETTER Y WITH DOT ABOVE
    (Unicode:#$1E8F; Attr:daNone; Str:#$0079#$0307),    // LATIN SMALL LETTER Y WITH DOT ABOVE
    (Unicode:#$1E90; Attr:daNone; Str:#$005A#$0302),    // LATIN CAPITAL LETTER Z WITH CIRCUMFLEX
    (Unicode:#$1E91; Attr:daNone; Str:#$007A#$0302),    // LATIN SMALL LETTER Z WITH CIRCUMFLEX
    (Unicode:#$1E92; Attr:daNone; Str:#$005A#$0323),    // LATIN CAPITAL LETTER Z WITH DOT BELOW
    (Unicode:#$1E93; Attr:daNone; Str:#$007A#$0323),    // LATIN SMALL LETTER Z WITH DOT BELOW
    (Unicode:#$1E94; Attr:daNone; Str:#$005A#$0331),    // LATIN CAPITAL LETTER Z WITH LINE BELOW
    (Unicode:#$1E95; Attr:daNone; Str:#$007A#$0331),    // LATIN SMALL LETTER Z WITH LINE BELOW
    (Unicode:#$1E96; Attr:daNone; Str:#$0068#$0331),    // LATIN SMALL LETTER H WITH LINE BELOW
    (Unicode:#$1E97; Attr:daNone; Str:#$0074#$0308),    // LATIN SMALL LETTER T WITH DIAERESIS
    (Unicode:#$1E98; Attr:daNone; Str:#$0077#$030A),    // LATIN SMALL LETTER W WITH RING ABOVE
    (Unicode:#$1E99; Attr:daNone; Str:#$0079#$030A),    // LATIN SMALL LETTER Y WITH RING ABOVE
    (Unicode:#$1E9A; Attr:daCompat; Str:#$0061#$02BE),  // LATIN SMALL LETTER A WITH RIGHT HALF RING
    (Unicode:#$1E9B; Attr:daNone; Str:#$017F#$0307),    // LATIN SMALL LETTER LONG S WITH DOT ABOVE
    (Unicode:#$1EA0; Attr:daNone; Str:#$0041#$0323),    // LATIN CAPITAL LETTER A WITH DOT BELOW
    (Unicode:#$1EA1; Attr:daNone; Str:#$0061#$0323),    // LATIN SMALL LETTER A WITH DOT BELOW
    (Unicode:#$1EA2; Attr:daNone; Str:#$0041#$0309),    // LATIN CAPITAL LETTER A WITH HOOK ABOVE
    (Unicode:#$1EA3; Attr:daNone; Str:#$0061#$0309),    // LATIN SMALL LETTER A WITH HOOK ABOVE
    (Unicode:#$1EA4; Attr:daNone; Str:#$00C2#$0301),    // LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND ACUTE
    (Unicode:#$1EA5; Attr:daNone; Str:#$00E2#$0301),    // LATIN SMALL LETTER A WITH CIRCUMFLEX AND ACUTE
    (Unicode:#$1EA6; Attr:daNone; Str:#$00C2#$0300),    // LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND GRAVE
    (Unicode:#$1EA7; Attr:daNone; Str:#$00E2#$0300),    // LATIN SMALL LETTER A WITH CIRCUMFLEX AND GRAVE
    (Unicode:#$1EA8; Attr:daNone; Str:#$00C2#$0309),    // LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE
    (Unicode:#$1EA9; Attr:daNone; Str:#$00E2#$0309),    // LATIN SMALL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE
    (Unicode:#$1EAA; Attr:daNone; Str:#$00C2#$0303),    // LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND TILDE
    (Unicode:#$1EAB; Attr:daNone; Str:#$00E2#$0303),    // LATIN SMALL LETTER A WITH CIRCUMFLEX AND TILDE
    (Unicode:#$1EAC; Attr:daNone; Str:#$1EA0#$0302),    // LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND DOT BELOW
    (Unicode:#$1EAD; Attr:daNone; Str:#$1EA1#$0302),    // LATIN SMALL LETTER A WITH CIRCUMFLEX AND DOT BELOW
    (Unicode:#$1EAE; Attr:daNone; Str:#$0102#$0301),    // LATIN CAPITAL LETTER A WITH BREVE AND ACUTE
    (Unicode:#$1EAF; Attr:daNone; Str:#$0103#$0301),    // LATIN SMALL LETTER A WITH BREVE AND ACUTE
    (Unicode:#$1EB0; Attr:daNone; Str:#$0102#$0300),    // LATIN CAPITAL LETTER A WITH BREVE AND GRAVE
    (Unicode:#$1EB1; Attr:daNone; Str:#$0103#$0300),    // LATIN SMALL LETTER A WITH BREVE AND GRAVE
    (Unicode:#$1EB2; Attr:daNone; Str:#$0102#$0309),    // LATIN CAPITAL LETTER A WITH BREVE AND HOOK ABOVE
    (Unicode:#$1EB3; Attr:daNone; Str:#$0103#$0309),    // LATIN SMALL LETTER A WITH BREVE AND HOOK ABOVE
    (Unicode:#$1EB4; Attr:daNone; Str:#$0102#$0303),    // LATIN CAPITAL LETTER A WITH BREVE AND TILDE
    (Unicode:#$1EB5; Attr:daNone; Str:#$0103#$0303),    // LATIN SMALL LETTER A WITH BREVE AND TILDE
    (Unicode:#$1EB6; Attr:daNone; Str:#$1EA0#$0306),    // LATIN CAPITAL LETTER A WITH BREVE AND DOT BELOW
    (Unicode:#$1EB7; Attr:daNone; Str:#$1EA1#$0306),    // LATIN SMALL LETTER A WITH BREVE AND DOT BELOW
    (Unicode:#$1EB8; Attr:daNone; Str:#$0045#$0323),    // LATIN CAPITAL LETTER E WITH DOT BELOW
    (Unicode:#$1EB9; Attr:daNone; Str:#$0065#$0323),    // LATIN SMALL LETTER E WITH DOT BELOW
    (Unicode:#$1EBA; Attr:daNone; Str:#$0045#$0309),    // LATIN CAPITAL LETTER E WITH HOOK ABOVE
    (Unicode:#$1EBB; Attr:daNone; Str:#$0065#$0309),    // LATIN SMALL LETTER E WITH HOOK ABOVE
    (Unicode:#$1EBC; Attr:daNone; Str:#$0045#$0303),    // LATIN CAPITAL LETTER E WITH TILDE
    (Unicode:#$1EBD; Attr:daNone; Str:#$0065#$0303),    // LATIN SMALL LETTER E WITH TILDE
    (Unicode:#$1EBE; Attr:daNone; Str:#$00CA#$0301),    // LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND ACUTE
    (Unicode:#$1EBF; Attr:daNone; Str:#$00EA#$0301),    // LATIN SMALL LETTER E WITH CIRCUMFLEX AND ACUTE
    (Unicode:#$1EC0; Attr:daNone; Str:#$00CA#$0300),    // LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND GRAVE
    (Unicode:#$1EC1; Attr:daNone; Str:#$00EA#$0300),    // LATIN SMALL LETTER E WITH CIRCUMFLEX AND GRAVE
    (Unicode:#$1EC2; Attr:daNone; Str:#$00CA#$0309),    // LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE
    (Unicode:#$1EC3; Attr:daNone; Str:#$00EA#$0309),    // LATIN SMALL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE
    (Unicode:#$1EC4; Attr:daNone; Str:#$00CA#$0303),    // LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND TILDE
    (Unicode:#$1EC5; Attr:daNone; Str:#$00EA#$0303),    // LATIN SMALL LETTER E WITH CIRCUMFLEX AND TILDE
    (Unicode:#$1EC6; Attr:daNone; Str:#$1EB8#$0302),    // LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND DOT BELOW
    (Unicode:#$1EC7; Attr:daNone; Str:#$1EB9#$0302),    // LATIN SMALL LETTER E WITH CIRCUMFLEX AND DOT BELOW
    (Unicode:#$1EC8; Attr:daNone; Str:#$0049#$0309),    // LATIN CAPITAL LETTER I WITH HOOK ABOVE
    (Unicode:#$1EC9; Attr:daNone; Str:#$0069#$0309),    // LATIN SMALL LETTER I WITH HOOK ABOVE
    (Unicode:#$1ECA; Attr:daNone; Str:#$0049#$0323),    // LATIN CAPITAL LETTER I WITH DOT BELOW
    (Unicode:#$1ECB; Attr:daNone; Str:#$0069#$0323),    // LATIN SMALL LETTER I WITH DOT BELOW
    (Unicode:#$1ECC; Attr:daNone; Str:#$004F#$0323),    // LATIN CAPITAL LETTER O WITH DOT BELOW
    (Unicode:#$1ECD; Attr:daNone; Str:#$006F#$0323),    // LATIN SMALL LETTER O WITH DOT BELOW
    (Unicode:#$1ECE; Attr:daNone; Str:#$004F#$0309),    // LATIN CAPITAL LETTER O WITH HOOK ABOVE
    (Unicode:#$1ECF; Attr:daNone; Str:#$006F#$0309),    // LATIN SMALL LETTER O WITH HOOK ABOVE
    (Unicode:#$1ED0; Attr:daNone; Str:#$00D4#$0301),    // LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND ACUTE
    (Unicode:#$1ED1; Attr:daNone; Str:#$00F4#$0301),    // LATIN SMALL LETTER O WITH CIRCUMFLEX AND ACUTE
    (Unicode:#$1ED2; Attr:daNone; Str:#$00D4#$0300),    // LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND GRAVE
    (Unicode:#$1ED3; Attr:daNone; Str:#$00F4#$0300),    // LATIN SMALL LETTER O WITH CIRCUMFLEX AND GRAVE
    (Unicode:#$1ED4; Attr:daNone; Str:#$00D4#$0309),    // LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE
    (Unicode:#$1ED5; Attr:daNone; Str:#$00F4#$0309),    // LATIN SMALL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE
    (Unicode:#$1ED6; Attr:daNone; Str:#$00D4#$0303),    // LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND TILDE
    (Unicode:#$1ED7; Attr:daNone; Str:#$00F4#$0303),    // LATIN SMALL LETTER O WITH CIRCUMFLEX AND TILDE
    (Unicode:#$1ED8; Attr:daNone; Str:#$1ECC#$0302),    // LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND DOT BELOW
    (Unicode:#$1ED9; Attr:daNone; Str:#$1ECD#$0302),    // LATIN SMALL LETTER O WITH CIRCUMFLEX AND DOT BELOW
    (Unicode:#$1EDA; Attr:daNone; Str:#$01A0#$0301),    // LATIN CAPITAL LETTER O WITH HORN AND ACUTE
    (Unicode:#$1EDB; Attr:daNone; Str:#$01A1#$0301),    // LATIN SMALL LETTER O WITH HORN AND ACUTE
    (Unicode:#$1EDC; Attr:daNone; Str:#$01A0#$0300),    // LATIN CAPITAL LETTER O WITH HORN AND GRAVE
    (Unicode:#$1EDD; Attr:daNone; Str:#$01A1#$0300),    // LATIN SMALL LETTER O WITH HORN AND GRAVE
    (Unicode:#$1EDE; Attr:daNone; Str:#$01A0#$0309),    // LATIN CAPITAL LETTER O WITH HORN AND HOOK ABOVE
    (Unicode:#$1EDF; Attr:daNone; Str:#$01A1#$0309),    // LATIN SMALL LETTER O WITH HORN AND HOOK ABOVE
    (Unicode:#$1EE0; Attr:daNone; Str:#$01A0#$0303),    // LATIN CAPITAL LETTER O WITH HORN AND TILDE
    (Unicode:#$1EE1; Attr:daNone; Str:#$01A1#$0303),    // LATIN SMALL LETTER O WITH HORN AND TILDE
    (Unicode:#$1EE2; Attr:daNone; Str:#$01A0#$0323),    // LATIN CAPITAL LETTER O WITH HORN AND DOT BELOW
    (Unicode:#$1EE3; Attr:daNone; Str:#$01A1#$0323),    // LATIN SMALL LETTER O WITH HORN AND DOT BELOW
    (Unicode:#$1EE4; Attr:daNone; Str:#$0055#$0323),    // LATIN CAPITAL LETTER U WITH DOT BELOW
    (Unicode:#$1EE5; Attr:daNone; Str:#$0075#$0323),    // LATIN SMALL LETTER U WITH DOT BELOW
    (Unicode:#$1EE6; Attr:daNone; Str:#$0055#$0309),    // LATIN CAPITAL LETTER U WITH HOOK ABOVE
    (Unicode:#$1EE7; Attr:daNone; Str:#$0075#$0309),    // LATIN SMALL LETTER U WITH HOOK ABOVE
    (Unicode:#$1EE8; Attr:daNone; Str:#$01AF#$0301),    // LATIN CAPITAL LETTER U WITH HORN AND ACUTE
    (Unicode:#$1EE9; Attr:daNone; Str:#$01B0#$0301),    // LATIN SMALL LETTER U WITH HORN AND ACUTE
    (Unicode:#$1EEA; Attr:daNone; Str:#$01AF#$0300),    // LATIN CAPITAL LETTER U WITH HORN AND GRAVE
    (Unicode:#$1EEB; Attr:daNone; Str:#$01B0#$0300),    // LATIN SMALL LETTER U WITH HORN AND GRAVE
    (Unicode:#$1EEC; Attr:daNone; Str:#$01AF#$0309),    // LATIN CAPITAL LETTER U WITH HORN AND HOOK ABOVE
    (Unicode:#$1EED; Attr:daNone; Str:#$01B0#$0309),    // LATIN SMALL LETTER U WITH HORN AND HOOK ABOVE
    (Unicode:#$1EEE; Attr:daNone; Str:#$01AF#$0303),    // LATIN CAPITAL LETTER U WITH HORN AND TILDE
    (Unicode:#$1EEF; Attr:daNone; Str:#$01B0#$0303),    // LATIN SMALL LETTER U WITH HORN AND TILDE
    (Unicode:#$1EF0; Attr:daNone; Str:#$01AF#$0323),    // LATIN CAPITAL LETTER U WITH HORN AND DOT BELOW
    (Unicode:#$1EF1; Attr:daNone; Str:#$01B0#$0323),    // LATIN SMALL LETTER U WITH HORN AND DOT BELOW
    (Unicode:#$1EF2; Attr:daNone; Str:#$0059#$0300),    // LATIN CAPITAL LETTER Y WITH GRAVE
    (Unicode:#$1EF3; Attr:daNone; Str:#$0079#$0300),    // LATIN SMALL LETTER Y WITH GRAVE
    (Unicode:#$1EF4; Attr:daNone; Str:#$0059#$0323),    // LATIN CAPITAL LETTER Y WITH DOT BELOW
    (Unicode:#$1EF5; Attr:daNone; Str:#$0079#$0323),    // LATIN SMALL LETTER Y WITH DOT BELOW
    (Unicode:#$1EF6; Attr:daNone; Str:#$0059#$0309),    // LATIN CAPITAL LETTER Y WITH HOOK ABOVE
    (Unicode:#$1EF7; Attr:daNone; Str:#$0079#$0309),    // LATIN SMALL LETTER Y WITH HOOK ABOVE
    (Unicode:#$1EF8; Attr:daNone; Str:#$0059#$0303),    // LATIN CAPITAL LETTER Y WITH TILDE
    (Unicode:#$1EF9; Attr:daNone; Str:#$0079#$0303),    // LATIN SMALL LETTER Y WITH TILDE
    (Unicode:#$1F00; Attr:daNone; Str:#$03B1#$0313),    // GREEK SMALL LETTER ALPHA WITH PSILI
    (Unicode:#$1F01; Attr:daNone; Str:#$03B1#$0314),    // GREEK SMALL LETTER ALPHA WITH DASIA
    (Unicode:#$1F02; Attr:daNone; Str:#$1F00#$0300),    // GREEK SMALL LETTER ALPHA WITH PSILI AND VARIA
    (Unicode:#$1F03; Attr:daNone; Str:#$1F01#$0300),    // GREEK SMALL LETTER ALPHA WITH DASIA AND VARIA
    (Unicode:#$1F04; Attr:daNone; Str:#$1F00#$0301),    // GREEK SMALL LETTER ALPHA WITH PSILI AND OXIA
    (Unicode:#$1F05; Attr:daNone; Str:#$1F01#$0301),    // GREEK SMALL LETTER ALPHA WITH DASIA AND OXIA
    (Unicode:#$1F06; Attr:daNone; Str:#$1F00#$0342),    // GREEK SMALL LETTER ALPHA WITH PSILI AND PERISPOMENI
    (Unicode:#$1F07; Attr:daNone; Str:#$1F01#$0342),    // GREEK SMALL LETTER ALPHA WITH DASIA AND PERISPOMENI
    (Unicode:#$1F08; Attr:daNone; Str:#$0391#$0313),    // GREEK CAPITAL LETTER ALPHA WITH PSILI
    (Unicode:#$1F09; Attr:daNone; Str:#$0391#$0314),    // GREEK CAPITAL LETTER ALPHA WITH DASIA
    (Unicode:#$1F0A; Attr:daNone; Str:#$1F08#$0300),    // GREEK CAPITAL LETTER ALPHA WITH PSILI AND VARIA
    (Unicode:#$1F0B; Attr:daNone; Str:#$1F09#$0300),    // GREEK CAPITAL LETTER ALPHA WITH DASIA AND VARIA
    (Unicode:#$1F0C; Attr:daNone; Str:#$1F08#$0301),    // GREEK CAPITAL LETTER ALPHA WITH PSILI AND OXIA
    (Unicode:#$1F0D; Attr:daNone; Str:#$1F09#$0301),    // GREEK CAPITAL LETTER ALPHA WITH DASIA AND OXIA
    (Unicode:#$1F0E; Attr:daNone; Str:#$1F08#$0342),    // GREEK CAPITAL LETTER ALPHA WITH PSILI AND PERISPOMENI
    (Unicode:#$1F0F; Attr:daNone; Str:#$1F09#$0342),    // GREEK CAPITAL LETTER ALPHA WITH DASIA AND PERISPOMENI
    (Unicode:#$1F10; Attr:daNone; Str:#$03B5#$0313),    // GREEK SMALL LETTER EPSILON WITH PSILI
    (Unicode:#$1F11; Attr:daNone; Str:#$03B5#$0314),    // GREEK SMALL LETTER EPSILON WITH DASIA
    (Unicode:#$1F12; Attr:daNone; Str:#$1F10#$0300),    // GREEK SMALL LETTER EPSILON WITH PSILI AND VARIA
    (Unicode:#$1F13; Attr:daNone; Str:#$1F11#$0300),    // GREEK SMALL LETTER EPSILON WITH DASIA AND VARIA
    (Unicode:#$1F14; Attr:daNone; Str:#$1F10#$0301),    // GREEK SMALL LETTER EPSILON WITH PSILI AND OXIA
    (Unicode:#$1F15; Attr:daNone; Str:#$1F11#$0301),    // GREEK SMALL LETTER EPSILON WITH DASIA AND OXIA
    (Unicode:#$1F18; Attr:daNone; Str:#$0395#$0313),    // GREEK CAPITAL LETTER EPSILON WITH PSILI
    (Unicode:#$1F19; Attr:daNone; Str:#$0395#$0314),    // GREEK CAPITAL LETTER EPSILON WITH DASIA
    (Unicode:#$1F1A; Attr:daNone; Str:#$1F18#$0300),    // GREEK CAPITAL LETTER EPSILON WITH PSILI AND VARIA
    (Unicode:#$1F1B; Attr:daNone; Str:#$1F19#$0300),    // GREEK CAPITAL LETTER EPSILON WITH DASIA AND VARIA
    (Unicode:#$1F1C; Attr:daNone; Str:#$1F18#$0301),    // GREEK CAPITAL LETTER EPSILON WITH PSILI AND OXIA
    (Unicode:#$1F1D; Attr:daNone; Str:#$1F19#$0301),    // GREEK CAPITAL LETTER EPSILON WITH DASIA AND OXIA
    (Unicode:#$1F20; Attr:daNone; Str:#$03B7#$0313),    // GREEK SMALL LETTER ETA WITH PSILI
    (Unicode:#$1F21; Attr:daNone; Str:#$03B7#$0314),    // GREEK SMALL LETTER ETA WITH DASIA
    (Unicode:#$1F22; Attr:daNone; Str:#$1F20#$0300),    // GREEK SMALL LETTER ETA WITH PSILI AND VARIA
    (Unicode:#$1F23; Attr:daNone; Str:#$1F21#$0300),    // GREEK SMALL LETTER ETA WITH DASIA AND VARIA
    (Unicode:#$1F24; Attr:daNone; Str:#$1F20#$0301),    // GREEK SMALL LETTER ETA WITH PSILI AND OXIA
    (Unicode:#$1F25; Attr:daNone; Str:#$1F21#$0301),    // GREEK SMALL LETTER ETA WITH DASIA AND OXIA
    (Unicode:#$1F26; Attr:daNone; Str:#$1F20#$0342),    // GREEK SMALL LETTER ETA WITH PSILI AND PERISPOMENI
    (Unicode:#$1F27; Attr:daNone; Str:#$1F21#$0342),    // GREEK SMALL LETTER ETA WITH DASIA AND PERISPOMENI
    (Unicode:#$1F28; Attr:daNone; Str:#$0397#$0313),    // GREEK CAPITAL LETTER ETA WITH PSILI
    (Unicode:#$1F29; Attr:daNone; Str:#$0397#$0314),    // GREEK CAPITAL LETTER ETA WITH DASIA
    (Unicode:#$1F2A; Attr:daNone; Str:#$1F28#$0300),    // GREEK CAPITAL LETTER ETA WITH PSILI AND VARIA
    (Unicode:#$1F2B; Attr:daNone; Str:#$1F29#$0300),    // GREEK CAPITAL LETTER ETA WITH DASIA AND VARIA
    (Unicode:#$1F2C; Attr:daNone; Str:#$1F28#$0301),    // GREEK CAPITAL LETTER ETA WITH PSILI AND OXIA
    (Unicode:#$1F2D; Attr:daNone; Str:#$1F29#$0301),    // GREEK CAPITAL LETTER ETA WITH DASIA AND OXIA
    (Unicode:#$1F2E; Attr:daNone; Str:#$1F28#$0342),    // GREEK CAPITAL LETTER ETA WITH PSILI AND PERISPOMENI
    (Unicode:#$1F2F; Attr:daNone; Str:#$1F29#$0342),    // GREEK CAPITAL LETTER ETA WITH DASIA AND PERISPOMENI
    (Unicode:#$1F30; Attr:daNone; Str:#$03B9#$0313),    // GREEK SMALL LETTER IOTA WITH PSILI
    (Unicode:#$1F31; Attr:daNone; Str:#$03B9#$0314),    // GREEK SMALL LETTER IOTA WITH DASIA
    (Unicode:#$1F32; Attr:daNone; Str:#$1F30#$0300),    // GREEK SMALL LETTER IOTA WITH PSILI AND VARIA
    (Unicode:#$1F33; Attr:daNone; Str:#$1F31#$0300),    // GREEK SMALL LETTER IOTA WITH DASIA AND VARIA
    (Unicode:#$1F34; Attr:daNone; Str:#$1F30#$0301),    // GREEK SMALL LETTER IOTA WITH PSILI AND OXIA
    (Unicode:#$1F35; Attr:daNone; Str:#$1F31#$0301),    // GREEK SMALL LETTER IOTA WITH DASIA AND OXIA
    (Unicode:#$1F36; Attr:daNone; Str:#$1F30#$0342),    // GREEK SMALL LETTER IOTA WITH PSILI AND PERISPOMENI
    (Unicode:#$1F37; Attr:daNone; Str:#$1F31#$0342),    // GREEK SMALL LETTER IOTA WITH DASIA AND PERISPOMENI
    (Unicode:#$1F38; Attr:daNone; Str:#$0399#$0313),    // GREEK CAPITAL LETTER IOTA WITH PSILI
    (Unicode:#$1F39; Attr:daNone; Str:#$0399#$0314),    // GREEK CAPITAL LETTER IOTA WITH DASIA
    (Unicode:#$1F3A; Attr:daNone; Str:#$1F38#$0300),    // GREEK CAPITAL LETTER IOTA WITH PSILI AND VARIA
    (Unicode:#$1F3B; Attr:daNone; Str:#$1F39#$0300),    // GREEK CAPITAL LETTER IOTA WITH DASIA AND VARIA
    (Unicode:#$1F3C; Attr:daNone; Str:#$1F38#$0301),    // GREEK CAPITAL LETTER IOTA WITH PSILI AND OXIA
    (Unicode:#$1F3D; Attr:daNone; Str:#$1F39#$0301),    // GREEK CAPITAL LETTER IOTA WITH DASIA AND OXIA
    (Unicode:#$1F3E; Attr:daNone; Str:#$1F38#$0342),    // GREEK CAPITAL LETTER IOTA WITH PSILI AND PERISPOMENI
    (Unicode:#$1F3F; Attr:daNone; Str:#$1F39#$0342),    // GREEK CAPITAL LETTER IOTA WITH DASIA AND PERISPOMENI
    (Unicode:#$1F40; Attr:daNone; Str:#$03BF#$0313),    // GREEK SMALL LETTER OMICRON WITH PSILI
    (Unicode:#$1F41; Attr:daNone; Str:#$03BF#$0314),    // GREEK SMALL LETTER OMICRON WITH DASIA
    (Unicode:#$1F42; Attr:daNone; Str:#$1F40#$0300),    // GREEK SMALL LETTER OMICRON WITH PSILI AND VARIA
    (Unicode:#$1F43; Attr:daNone; Str:#$1F41#$0300),    // GREEK SMALL LETTER OMICRON WITH DASIA AND VARIA
    (Unicode:#$1F44; Attr:daNone; Str:#$1F40#$0301),    // GREEK SMALL LETTER OMICRON WITH PSILI AND OXIA
    (Unicode:#$1F45; Attr:daNone; Str:#$1F41#$0301),    // GREEK SMALL LETTER OMICRON WITH DASIA AND OXIA
    (Unicode:#$1F48; Attr:daNone; Str:#$039F#$0313),    // GREEK CAPITAL LETTER OMICRON WITH PSILI
    (Unicode:#$1F49; Attr:daNone; Str:#$039F#$0314),    // GREEK CAPITAL LETTER OMICRON WITH DASIA
    (Unicode:#$1F4A; Attr:daNone; Str:#$1F48#$0300),    // GREEK CAPITAL LETTER OMICRON WITH PSILI AND VARIA
    (Unicode:#$1F4B; Attr:daNone; Str:#$1F49#$0300),    // GREEK CAPITAL LETTER OMICRON WITH DASIA AND VARIA
    (Unicode:#$1F4C; Attr:daNone; Str:#$1F48#$0301),    // GREEK CAPITAL LETTER OMICRON WITH PSILI AND OXIA
    (Unicode:#$1F4D; Attr:daNone; Str:#$1F49#$0301),    // GREEK CAPITAL LETTER OMICRON WITH DASIA AND OXIA
    (Unicode:#$1F50; Attr:daNone; Str:#$03C5#$0313),    // GREEK SMALL LETTER UPSILON WITH PSILI
    (Unicode:#$1F51; Attr:daNone; Str:#$03C5#$0314),    // GREEK SMALL LETTER UPSILON WITH DASIA
    (Unicode:#$1F52; Attr:daNone; Str:#$1F50#$0300),    // GREEK SMALL LETTER UPSILON WITH PSILI AND VARIA
    (Unicode:#$1F53; Attr:daNone; Str:#$1F51#$0300),    // GREEK SMALL LETTER UPSILON WITH DASIA AND VARIA
    (Unicode:#$1F54; Attr:daNone; Str:#$1F50#$0301),    // GREEK SMALL LETTER UPSILON WITH PSILI AND OXIA
    (Unicode:#$1F55; Attr:daNone; Str:#$1F51#$0301),    // GREEK SMALL LETTER UPSILON WITH DASIA AND OXIA
    (Unicode:#$1F56; Attr:daNone; Str:#$1F50#$0342),    // GREEK SMALL LETTER UPSILON WITH PSILI AND PERISPOMENI
    (Unicode:#$1F57; Attr:daNone; Str:#$1F51#$0342),    // GREEK SMALL LETTER UPSILON WITH DASIA AND PERISPOMENI
    (Unicode:#$1F59; Attr:daNone; Str:#$03A5#$0314),    // GREEK CAPITAL LETTER UPSILON WITH DASIA
    (Unicode:#$1F5B; Attr:daNone; Str:#$1F59#$0300),    // GREEK CAPITAL LETTER UPSILON WITH DASIA AND VARIA
    (Unicode:#$1F5D; Attr:daNone; Str:#$1F59#$0301),    // GREEK CAPITAL LETTER UPSILON WITH DASIA AND OXIA
    (Unicode:#$1F5F; Attr:daNone; Str:#$1F59#$0342),    // GREEK CAPITAL LETTER UPSILON WITH DASIA AND PERISPOMENI
    (Unicode:#$1F60; Attr:daNone; Str:#$03C9#$0313),    // GREEK SMALL LETTER OMEGA WITH PSILI
    (Unicode:#$1F61; Attr:daNone; Str:#$03C9#$0314),    // GREEK SMALL LETTER OMEGA WITH DASIA
    (Unicode:#$1F62; Attr:daNone; Str:#$1F60#$0300),    // GREEK SMALL LETTER OMEGA WITH PSILI AND VARIA
    (Unicode:#$1F63; Attr:daNone; Str:#$1F61#$0300),    // GREEK SMALL LETTER OMEGA WITH DASIA AND VARIA
    (Unicode:#$1F64; Attr:daNone; Str:#$1F60#$0301),    // GREEK SMALL LETTER OMEGA WITH PSILI AND OXIA
    (Unicode:#$1F65; Attr:daNone; Str:#$1F61#$0301),    // GREEK SMALL LETTER OMEGA WITH DASIA AND OXIA
    (Unicode:#$1F66; Attr:daNone; Str:#$1F60#$0342),    // GREEK SMALL LETTER OMEGA WITH PSILI AND PERISPOMENI
    (Unicode:#$1F67; Attr:daNone; Str:#$1F61#$0342),    // GREEK SMALL LETTER OMEGA WITH DASIA AND PERISPOMENI
    (Unicode:#$1F68; Attr:daNone; Str:#$03A9#$0313),    // GREEK CAPITAL LETTER OMEGA WITH PSILI
    (Unicode:#$1F69; Attr:daNone; Str:#$03A9#$0314),    // GREEK CAPITAL LETTER OMEGA WITH DASIA
    (Unicode:#$1F6A; Attr:daNone; Str:#$1F68#$0300),    // GREEK CAPITAL LETTER OMEGA WITH PSILI AND VARIA
    (Unicode:#$1F6B; Attr:daNone; Str:#$1F69#$0300),    // GREEK CAPITAL LETTER OMEGA WITH DASIA AND VARIA
    (Unicode:#$1F6C; Attr:daNone; Str:#$1F68#$0301),    // GREEK CAPITAL LETTER OMEGA WITH PSILI AND OXIA
    (Unicode:#$1F6D; Attr:daNone; Str:#$1F69#$0301),    // GREEK CAPITAL LETTER OMEGA WITH DASIA AND OXIA
    (Unicode:#$1F6E; Attr:daNone; Str:#$1F68#$0342),    // GREEK CAPITAL LETTER OMEGA WITH PSILI AND PERISPOMENI
    (Unicode:#$1F6F; Attr:daNone; Str:#$1F69#$0342),    // GREEK CAPITAL LETTER OMEGA WITH DASIA AND PERISPOMENI
    (Unicode:#$1F70; Attr:daNone; Str:#$03B1#$0300),    // GREEK SMALL LETTER ALPHA WITH VARIA
    (Unicode:#$1F71; Attr:daNone; Str:#$03AC),          // GREEK SMALL LETTER ALPHA WITH OXIA
    (Unicode:#$1F72; Attr:daNone; Str:#$03B5#$0300),    // GREEK SMALL LETTER EPSILON WITH VARIA
    (Unicode:#$1F73; Attr:daNone; Str:#$03AD),          // GREEK SMALL LETTER EPSILON WITH OXIA
    (Unicode:#$1F74; Attr:daNone; Str:#$03B7#$0300),    // GREEK SMALL LETTER ETA WITH VARIA
    (Unicode:#$1F75; Attr:daNone; Str:#$03AE),          // GREEK SMALL LETTER ETA WITH OXIA
    (Unicode:#$1F76; Attr:daNone; Str:#$03B9#$0300),    // GREEK SMALL LETTER IOTA WITH VARIA
    (Unicode:#$1F77; Attr:daNone; Str:#$03AF),          // GREEK SMALL LETTER IOTA WITH OXIA
    (Unicode:#$1F78; Attr:daNone; Str:#$03BF#$0300),    // GREEK SMALL LETTER OMICRON WITH VARIA
    (Unicode:#$1F79; Attr:daNone; Str:#$03CC),          // GREEK SMALL LETTER OMICRON WITH OXIA
    (Unicode:#$1F7A; Attr:daNone; Str:#$03C5#$0300),    // GREEK SMALL LETTER UPSILON WITH VARIA
    (Unicode:#$1F7B; Attr:daNone; Str:#$03CD),          // GREEK SMALL LETTER UPSILON WITH OXIA
    (Unicode:#$1F7C; Attr:daNone; Str:#$03C9#$0300),    // GREEK SMALL LETTER OMEGA WITH VARIA
    (Unicode:#$1F7D; Attr:daNone; Str:#$03CE),          // GREEK SMALL LETTER OMEGA WITH OXIA
    (Unicode:#$1F80; Attr:daNone; Str:#$1F00#$0345),    // GREEK SMALL LETTER ALPHA WITH PSILI AND YPOGEGRAMMENI
    (Unicode:#$1F81; Attr:daNone; Str:#$1F01#$0345),    // GREEK SMALL LETTER ALPHA WITH DASIA AND YPOGEGRAMMENI
    (Unicode:#$1F82; Attr:daNone; Str:#$1F02#$0345),    // GREEK SMALL LETTER ALPHA WITH PSILI AND VARIA AND YPOGEGRAMMENI
    (Unicode:#$1F83; Attr:daNone; Str:#$1F03#$0345),    // GREEK SMALL LETTER ALPHA WITH DASIA AND VARIA AND YPOGEGRAMMENI
    (Unicode:#$1F84; Attr:daNone; Str:#$1F04#$0345),    // GREEK SMALL LETTER ALPHA WITH PSILI AND OXIA AND YPOGEGRAMMENI
    (Unicode:#$1F85; Attr:daNone; Str:#$1F05#$0345),    // GREEK SMALL LETTER ALPHA WITH DASIA AND OXIA AND YPOGEGRAMMENI
    (Unicode:#$1F86; Attr:daNone; Str:#$1F06#$0345),    // GREEK SMALL LETTER ALPHA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
    (Unicode:#$1F87; Attr:daNone; Str:#$1F07#$0345),    // GREEK SMALL LETTER ALPHA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
    (Unicode:#$1F88; Attr:daNone; Str:#$1F08#$0345),    // GREEK CAPITAL LETTER ALPHA WITH PSILI AND PROSGEGRAMMENI
    (Unicode:#$1F89; Attr:daNone; Str:#$1F09#$0345),    // GREEK CAPITAL LETTER ALPHA WITH DASIA AND PROSGEGRAMMENI
    (Unicode:#$1F8A; Attr:daNone; Str:#$1F0A#$0345),    // GREEK CAPITAL LETTER ALPHA WITH PSILI AND VARIA AND PROSGEGRAMMENI
    (Unicode:#$1F8B; Attr:daNone; Str:#$1F0B#$0345),    // GREEK CAPITAL LETTER ALPHA WITH DASIA AND VARIA AND PROSGEGRAMMENI
    (Unicode:#$1F8C; Attr:daNone; Str:#$1F0C#$0345),    // GREEK CAPITAL LETTER ALPHA WITH PSILI AND OXIA AND PROSGEGRAMMENI
    (Unicode:#$1F8D; Attr:daNone; Str:#$1F0D#$0345),    // GREEK CAPITAL LETTER ALPHA WITH DASIA AND OXIA AND PROSGEGRAMMENI
    (Unicode:#$1F8E; Attr:daNone; Str:#$1F0E#$0345),    // GREEK CAPITAL LETTER ALPHA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
    (Unicode:#$1F8F; Attr:daNone; Str:#$1F0F#$0345),    // GREEK CAPITAL LETTER ALPHA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
    (Unicode:#$1F90; Attr:daNone; Str:#$1F20#$0345),    // GREEK SMALL LETTER ETA WITH PSILI AND YPOGEGRAMMENI
    (Unicode:#$1F91; Attr:daNone; Str:#$1F21#$0345),    // GREEK SMALL LETTER ETA WITH DASIA AND YPOGEGRAMMENI
    (Unicode:#$1F92; Attr:daNone; Str:#$1F22#$0345),    // GREEK SMALL LETTER ETA WITH PSILI AND VARIA AND YPOGEGRAMMENI
    (Unicode:#$1F93; Attr:daNone; Str:#$1F23#$0345),    // GREEK SMALL LETTER ETA WITH DASIA AND VARIA AND YPOGEGRAMMENI
    (Unicode:#$1F94; Attr:daNone; Str:#$1F24#$0345),    // GREEK SMALL LETTER ETA WITH PSILI AND OXIA AND YPOGEGRAMMENI
    (Unicode:#$1F95; Attr:daNone; Str:#$1F25#$0345),    // GREEK SMALL LETTER ETA WITH DASIA AND OXIA AND YPOGEGRAMMENI
    (Unicode:#$1F96; Attr:daNone; Str:#$1F26#$0345),    // GREEK SMALL LETTER ETA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
    (Unicode:#$1F97; Attr:daNone; Str:#$1F27#$0345),    // GREEK SMALL LETTER ETA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
    (Unicode:#$1F98; Attr:daNone; Str:#$1F28#$0345),    // GREEK CAPITAL LETTER ETA WITH PSILI AND PROSGEGRAMMENI
    (Unicode:#$1F99; Attr:daNone; Str:#$1F29#$0345),    // GREEK CAPITAL LETTER ETA WITH DASIA AND PROSGEGRAMMENI
    (Unicode:#$1F9A; Attr:daNone; Str:#$1F2A#$0345),    // GREEK CAPITAL LETTER ETA WITH PSILI AND VARIA AND PROSGEGRAMMENI
    (Unicode:#$1F9B; Attr:daNone; Str:#$1F2B#$0345),    // GREEK CAPITAL LETTER ETA WITH DASIA AND VARIA AND PROSGEGRAMMENI
    (Unicode:#$1F9C; Attr:daNone; Str:#$1F2C#$0345),    // GREEK CAPITAL LETTER ETA WITH PSILI AND OXIA AND PROSGEGRAMMENI
    (Unicode:#$1F9D; Attr:daNone; Str:#$1F2D#$0345),    // GREEK CAPITAL LETTER ETA WITH DASIA AND OXIA AND PROSGEGRAMMENI
    (Unicode:#$1F9E; Attr:daNone; Str:#$1F2E#$0345),    // GREEK CAPITAL LETTER ETA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
    (Unicode:#$1F9F; Attr:daNone; Str:#$1F2F#$0345),    // GREEK CAPITAL LETTER ETA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
    (Unicode:#$1FA0; Attr:daNone; Str:#$1F60#$0345),    // GREEK SMALL LETTER OMEGA WITH PSILI AND YPOGEGRAMMENI
    (Unicode:#$1FA1; Attr:daNone; Str:#$1F61#$0345),    // GREEK SMALL LETTER OMEGA WITH DASIA AND YPOGEGRAMMENI
    (Unicode:#$1FA2; Attr:daNone; Str:#$1F62#$0345),    // GREEK SMALL LETTER OMEGA WITH PSILI AND VARIA AND YPOGEGRAMMENI
    (Unicode:#$1FA3; Attr:daNone; Str:#$1F63#$0345),    // GREEK SMALL LETTER OMEGA WITH DASIA AND VARIA AND YPOGEGRAMMENI
    (Unicode:#$1FA4; Attr:daNone; Str:#$1F64#$0345),    // GREEK SMALL LETTER OMEGA WITH PSILI AND OXIA AND YPOGEGRAMMENI
    (Unicode:#$1FA5; Attr:daNone; Str:#$1F65#$0345),    // GREEK SMALL LETTER OMEGA WITH DASIA AND OXIA AND YPOGEGRAMMENI
    (Unicode:#$1FA6; Attr:daNone; Str:#$1F66#$0345),    // GREEK SMALL LETTER OMEGA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
    (Unicode:#$1FA7; Attr:daNone; Str:#$1F67#$0345),    // GREEK SMALL LETTER OMEGA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
    (Unicode:#$1FA8; Attr:daNone; Str:#$1F68#$0345),    // GREEK CAPITAL LETTER OMEGA WITH PSILI AND PROSGEGRAMMENI
    (Unicode:#$1FA9; Attr:daNone; Str:#$1F69#$0345),    // GREEK CAPITAL LETTER OMEGA WITH DASIA AND PROSGEGRAMMENI
    (Unicode:#$1FAA; Attr:daNone; Str:#$1F6A#$0345),    // GREEK CAPITAL LETTER OMEGA WITH PSILI AND VARIA AND PROSGEGRAMMENI
    (Unicode:#$1FAB; Attr:daNone; Str:#$1F6B#$0345),    // GREEK CAPITAL LETTER OMEGA WITH DASIA AND VARIA AND PROSGEGRAMMENI
    (Unicode:#$1FAC; Attr:daNone; Str:#$1F6C#$0345),    // GREEK CAPITAL LETTER OMEGA WITH PSILI AND OXIA AND PROSGEGRAMMENI
    (Unicode:#$1FAD; Attr:daNone; Str:#$1F6D#$0345),    // GREEK CAPITAL LETTER OMEGA WITH DASIA AND OXIA AND PROSGEGRAMMENI
    (Unicode:#$1FAE; Attr:daNone; Str:#$1F6E#$0345),    // GREEK CAPITAL LETTER OMEGA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
    (Unicode:#$1FAF; Attr:daNone; Str:#$1F6F#$0345),    // GREEK CAPITAL LETTER OMEGA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
    (Unicode:#$1FB0; Attr:daNone; Str:#$03B1#$0306),    // GREEK SMALL LETTER ALPHA WITH VRACHY
    (Unicode:#$1FB1; Attr:daNone; Str:#$03B1#$0304),    // GREEK SMALL LETTER ALPHA WITH MACRON
    (Unicode:#$1FB2; Attr:daNone; Str:#$1F70#$0345),    // GREEK SMALL LETTER ALPHA WITH VARIA AND YPOGEGRAMMENI
    (Unicode:#$1FB3; Attr:daNone; Str:#$03B1#$0345),    // GREEK SMALL LETTER ALPHA WITH YPOGEGRAMMENI
    (Unicode:#$1FB4; Attr:daNone; Str:#$03AC#$0345),    // GREEK SMALL LETTER ALPHA WITH OXIA AND YPOGEGRAMMENI
    (Unicode:#$1FB6; Attr:daNone; Str:#$03B1#$0342),    // GREEK SMALL LETTER ALPHA WITH PERISPOMENI
    (Unicode:#$1FB7; Attr:daNone; Str:#$1FB6#$0345),    // GREEK SMALL LETTER ALPHA WITH PERISPOMENI AND YPOGEGRAMMENI
    (Unicode:#$1FB8; Attr:daNone; Str:#$0391#$0306),    // GREEK CAPITAL LETTER ALPHA WITH VRACHY
    (Unicode:#$1FB9; Attr:daNone; Str:#$0391#$0304),    // GREEK CAPITAL LETTER ALPHA WITH MACRON
    (Unicode:#$1FBA; Attr:daNone; Str:#$0391#$0300),    // GREEK CAPITAL LETTER ALPHA WITH VARIA
    (Unicode:#$1FBB; Attr:daNone; Str:#$0386),          // GREEK CAPITAL LETTER ALPHA WITH OXIA
    (Unicode:#$1FBC; Attr:daNone; Str:#$0391#$0345),    // GREEK CAPITAL LETTER ALPHA WITH PROSGEGRAMMENI
    (Unicode:#$1FBD; Attr:daCompat; Str:#$0020#$0313),  // GREEK KORONIS
    (Unicode:#$1FBE; Attr:daNone; Str:#$03B9),          // GREEK PROSGEGRAMMENI
    (Unicode:#$1FBF; Attr:daCompat; Str:#$0020#$0313),  // GREEK PSILI
    (Unicode:#$1FC0; Attr:daCompat; Str:#$0020#$0342),  // GREEK PERISPOMENI
    (Unicode:#$1FC1; Attr:daNone; Str:#$00A8#$0342),    // GREEK DIALYTIKA AND PERISPOMENI
    (Unicode:#$1FC2; Attr:daNone; Str:#$1F74#$0345),    // GREEK SMALL LETTER ETA WITH VARIA AND YPOGEGRAMMENI
    (Unicode:#$1FC3; Attr:daNone; Str:#$03B7#$0345),    // GREEK SMALL LETTER ETA WITH YPOGEGRAMMENI
    (Unicode:#$1FC4; Attr:daNone; Str:#$03AE#$0345),    // GREEK SMALL LETTER ETA WITH OXIA AND YPOGEGRAMMENI
    (Unicode:#$1FC6; Attr:daNone; Str:#$03B7#$0342),    // GREEK SMALL LETTER ETA WITH PERISPOMENI
    (Unicode:#$1FC7; Attr:daNone; Str:#$1FC6#$0345),    // GREEK SMALL LETTER ETA WITH PERISPOMENI AND YPOGEGRAMMENI
    (Unicode:#$1FC8; Attr:daNone; Str:#$0395#$0300),    // GREEK CAPITAL LETTER EPSILON WITH VARIA
    (Unicode:#$1FC9; Attr:daNone; Str:#$0388),          // GREEK CAPITAL LETTER EPSILON WITH OXIA
    (Unicode:#$1FCA; Attr:daNone; Str:#$0397#$0300),    // GREEK CAPITAL LETTER ETA WITH VARIA
    (Unicode:#$1FCB; Attr:daNone; Str:#$0389),          // GREEK CAPITAL LETTER ETA WITH OXIA
    (Unicode:#$1FCC; Attr:daNone; Str:#$0397#$0345),    // GREEK CAPITAL LETTER ETA WITH PROSGEGRAMMENI
    (Unicode:#$1FCD; Attr:daNone; Str:#$1FBF#$0300),    // GREEK PSILI AND VARIA
    (Unicode:#$1FCE; Attr:daNone; Str:#$1FBF#$0301),    // GREEK PSILI AND OXIA
    (Unicode:#$1FCF; Attr:daNone; Str:#$1FBF#$0342),    // GREEK PSILI AND PERISPOMENI
    (Unicode:#$1FD0; Attr:daNone; Str:#$03B9#$0306),    // GREEK SMALL LETTER IOTA WITH VRACHY
    (Unicode:#$1FD1; Attr:daNone; Str:#$03B9#$0304),    // GREEK SMALL LETTER IOTA WITH MACRON
    (Unicode:#$1FD2; Attr:daNone; Str:#$03CA#$0300),    // GREEK SMALL LETTER IOTA WITH DIALYTIKA AND VARIA
    (Unicode:#$1FD3; Attr:daNone; Str:#$0390),          // GREEK SMALL LETTER IOTA WITH DIALYTIKA AND OXIA
    (Unicode:#$1FD6; Attr:daNone; Str:#$03B9#$0342),    // GREEK SMALL LETTER IOTA WITH PERISPOMENI
    (Unicode:#$1FD7; Attr:daNone; Str:#$03CA#$0342),    // GREEK SMALL LETTER IOTA WITH DIALYTIKA AND PERISPOMENI
    (Unicode:#$1FD8; Attr:daNone; Str:#$0399#$0306),    // GREEK CAPITAL LETTER IOTA WITH VRACHY
    (Unicode:#$1FD9; Attr:daNone; Str:#$0399#$0304),    // GREEK CAPITAL LETTER IOTA WITH MACRON
    (Unicode:#$1FDA; Attr:daNone; Str:#$0399#$0300),    // GREEK CAPITAL LETTER IOTA WITH VARIA
    (Unicode:#$1FDB; Attr:daNone; Str:#$038A),          // GREEK CAPITAL LETTER IOTA WITH OXIA
    (Unicode:#$1FDD; Attr:daNone; Str:#$1FFE#$0300),    // GREEK DASIA AND VARIA
    (Unicode:#$1FDE; Attr:daNone; Str:#$1FFE#$0301),    // GREEK DASIA AND OXIA
    (Unicode:#$1FDF; Attr:daNone; Str:#$1FFE#$0342),    // GREEK DASIA AND PERISPOMENI
    (Unicode:#$1FE0; Attr:daNone; Str:#$03C5#$0306),    // GREEK SMALL LETTER UPSILON WITH VRACHY
    (Unicode:#$1FE1; Attr:daNone; Str:#$03C5#$0304),    // GREEK SMALL LETTER UPSILON WITH MACRON
    (Unicode:#$1FE2; Attr:daNone; Str:#$03CB#$0300),    // GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND VARIA
    (Unicode:#$1FE3; Attr:daNone; Str:#$03B0),          // GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND OXIA
    (Unicode:#$1FE4; Attr:daNone; Str:#$03C1#$0313),    // GREEK SMALL LETTER RHO WITH PSILI
    (Unicode:#$1FE5; Attr:daNone; Str:#$03C1#$0314),    // GREEK SMALL LETTER RHO WITH DASIA
    (Unicode:#$1FE6; Attr:daNone; Str:#$03C5#$0342),    // GREEK SMALL LETTER UPSILON WITH PERISPOMENI
    (Unicode:#$1FE7; Attr:daNone; Str:#$03CB#$0342),    // GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND PERISPOMENI
    (Unicode:#$1FE8; Attr:daNone; Str:#$03A5#$0306),    // GREEK CAPITAL LETTER UPSILON WITH VRACHY
    (Unicode:#$1FE9; Attr:daNone; Str:#$03A5#$0304),    // GREEK CAPITAL LETTER UPSILON WITH MACRON
    (Unicode:#$1FEA; Attr:daNone; Str:#$03A5#$0300),    // GREEK CAPITAL LETTER UPSILON WITH VARIA
    (Unicode:#$1FEB; Attr:daNone; Str:#$038E),          // GREEK CAPITAL LETTER UPSILON WITH OXIA
    (Unicode:#$1FEC; Attr:daNone; Str:#$03A1#$0314),    // GREEK CAPITAL LETTER RHO WITH DASIA
    (Unicode:#$1FED; Attr:daNone; Str:#$00A8#$0300),    // GREEK DIALYTIKA AND VARIA
    (Unicode:#$1FEE; Attr:daNone; Str:#$0385),          // GREEK DIALYTIKA AND OXIA
    (Unicode:#$1FEF; Attr:daNone; Str:#$0060),          // GREEK VARIA
    (Unicode:#$1FF2; Attr:daNone; Str:#$1F7C#$0345),    // GREEK SMALL LETTER OMEGA WITH VARIA AND YPOGEGRAMMENI
    (Unicode:#$1FF3; Attr:daNone; Str:#$03C9#$0345),    // GREEK SMALL LETTER OMEGA WITH YPOGEGRAMMENI
    (Unicode:#$1FF4; Attr:daNone; Str:#$03CE#$0345),    // GREEK SMALL LETTER OMEGA WITH OXIA AND YPOGEGRAMMENI
    (Unicode:#$1FF6; Attr:daNone; Str:#$03C9#$0342),    // GREEK SMALL LETTER OMEGA WITH PERISPOMENI
    (Unicode:#$1FF7; Attr:daNone; Str:#$1FF6#$0345),    // GREEK SMALL LETTER OMEGA WITH PERISPOMENI AND YPOGEGRAMMENI
    (Unicode:#$1FF8; Attr:daNone; Str:#$039F#$0300),    // GREEK CAPITAL LETTER OMICRON WITH VARIA
    (Unicode:#$1FF9; Attr:daNone; Str:#$038C),          // GREEK CAPITAL LETTER OMICRON WITH OXIA
    (Unicode:#$1FFA; Attr:daNone; Str:#$03A9#$0300),    // GREEK CAPITAL LETTER OMEGA WITH VARIA
    (Unicode:#$1FFB; Attr:daNone; Str:#$038F),          // GREEK CAPITAL LETTER OMEGA WITH OXIA
    (Unicode:#$1FFC; Attr:daNone; Str:#$03A9#$0345),    // GREEK CAPITAL LETTER OMEGA WITH PROSGEGRAMMENI
    (Unicode:#$1FFD; Attr:daNone; Str:#$00B4),          // GREEK OXIA
    (Unicode:#$1FFE; Attr:daCompat; Str:#$0020#$0314),  // GREEK DASIA
    (Unicode:#$2000; Attr:daNone; Str:#$2002),          // EN QUAD
    (Unicode:#$2001; Attr:daNone; Str:#$2003),          // EM QUAD
    (Unicode:#$2002; Attr:daCompat; Str:#$0020),        // EN SPACE
    (Unicode:#$2003; Attr:daCompat; Str:#$0020),        // EM SPACE
    (Unicode:#$2004; Attr:daCompat; Str:#$0020),        // THREE-PER-EM SPACE
    (Unicode:#$2005; Attr:daCompat; Str:#$0020),        // FOUR-PER-EM SPACE
    (Unicode:#$2006; Attr:daCompat; Str:#$0020),        // SIX-PER-EM SPACE
    (Unicode:#$2007; Attr:daNoBreak; Str:#$0020),       // FIGURE SPACE
    (Unicode:#$2008; Attr:daCompat; Str:#$0020),        // PUNCTUATION SPACE
    (Unicode:#$2009; Attr:daCompat; Str:#$0020),        // THIN SPACE
    (Unicode:#$200A; Attr:daCompat; Str:#$0020),        // HAIR SPACE
    (Unicode:#$2011; Attr:daNoBreak; Str:#$2010),       // NON-BREAKING HYPHEN
    (Unicode:#$2017; Attr:daCompat; Str:#$0020#$0333),  // DOUBLE LOW LINE
    (Unicode:#$2024; Attr:daCompat; Str:#$002E),        // ONE DOT LEADER
    (Unicode:#$2025; Attr:daCompat; Str:#$002E#$002E),  // TWO DOT LEADER
    (Unicode:#$2026; Attr:daCompat; Str:#$002E#$002E#$002E),   // HORIZONTAL ELLIPSIS
    (Unicode:#$202F; Attr:daNoBreak; Str:#$0020),       // NARROW NO-BREAK SPACE
    (Unicode:#$2033; Attr:daCompat; Str:#$2032#$2032),  // DOUBLE PRIME
    (Unicode:#$2034; Attr:daCompat; Str:#$2032#$2032#$2032),   // TRIPLE PRIME
    (Unicode:#$2036; Attr:daCompat; Str:#$2035#$2035),  // REVERSED DOUBLE PRIME
    (Unicode:#$2037; Attr:daCompat; Str:#$2035#$2035#$2035),   // REVERSED TRIPLE PRIME
    (Unicode:#$203C; Attr:daCompat; Str:#$0021#$0021),  // DOUBLE EXCLAMATION MARK
    (Unicode:#$203E; Attr:daCompat; Str:#$0020#$0305),  // OVERLINE
    (Unicode:#$2048; Attr:daCompat; Str:#$003F#$0021),  // QUESTION EXCLAMATION MARK
    (Unicode:#$2049; Attr:daCompat; Str:#$0021#$003F),  // EXCLAMATION QUESTION MARK
    (Unicode:#$2070; Attr:daSuper; Str:#$0030),         // SUPERSCRIPT ZERO
    (Unicode:#$2074; Attr:daSuper; Str:#$0034),         // SUPERSCRIPT FOUR
    (Unicode:#$2075; Attr:daSuper; Str:#$0035),         // SUPERSCRIPT FIVE
    (Unicode:#$2076; Attr:daSuper; Str:#$0036),         // SUPERSCRIPT SIX
    (Unicode:#$2077; Attr:daSuper; Str:#$0037),         // SUPERSCRIPT SEVEN
    (Unicode:#$2078; Attr:daSuper; Str:#$0038),         // SUPERSCRIPT EIGHT
    (Unicode:#$2079; Attr:daSuper; Str:#$0039),         // SUPERSCRIPT NINE
    (Unicode:#$207A; Attr:daSuper; Str:#$002B),         // SUPERSCRIPT PLUS SIGN
    (Unicode:#$207B; Attr:daSuper; Str:#$2212),         // SUPERSCRIPT MINUS
    (Unicode:#$207C; Attr:daSuper; Str:#$003D),         // SUPERSCRIPT EQUALS SIGN
    (Unicode:#$207D; Attr:daSuper; Str:#$0028),         // SUPERSCRIPT LEFT PARENTHESIS
    (Unicode:#$207E; Attr:daSuper; Str:#$0029),         // SUPERSCRIPT RIGHT PARENTHESIS
    (Unicode:#$207F; Attr:daSuper; Str:#$006E),         // SUPERSCRIPT LATIN SMALL LETTER N
    (Unicode:#$2080; Attr:daSub; Str:#$0030),           // SUBSCRIPT ZERO
    (Unicode:#$2081; Attr:daSub; Str:#$0031),           // SUBSCRIPT ONE
    (Unicode:#$2082; Attr:daSub; Str:#$0032),           // SUBSCRIPT TWO
    (Unicode:#$2083; Attr:daSub; Str:#$0033),           // SUBSCRIPT THREE
    (Unicode:#$2084; Attr:daSub; Str:#$0034),           // SUBSCRIPT FOUR
    (Unicode:#$2085; Attr:daSub; Str:#$0035),           // SUBSCRIPT FIVE
    (Unicode:#$2086; Attr:daSub; Str:#$0036),           // SUBSCRIPT SIX
    (Unicode:#$2087; Attr:daSub; Str:#$0037),           // SUBSCRIPT SEVEN
    (Unicode:#$2088; Attr:daSub; Str:#$0038),           // SUBSCRIPT EIGHT
    (Unicode:#$2089; Attr:daSub; Str:#$0039),           // SUBSCRIPT NINE
    (Unicode:#$208A; Attr:daSub; Str:#$002B),           // SUBSCRIPT PLUS SIGN
    (Unicode:#$208B; Attr:daSub; Str:#$2212),           // SUBSCRIPT MINUS
    (Unicode:#$208C; Attr:daSub; Str:#$003D),           // SUBSCRIPT EQUALS SIGN
    (Unicode:#$208D; Attr:daSub; Str:#$0028),           // SUBSCRIPT LEFT PARENTHESIS
    (Unicode:#$208E; Attr:daSub; Str:#$0029),           // SUBSCRIPT RIGHT PARENTHESIS
    (Unicode:#$20A8; Attr:daCompat; Str:#$0052#$0073),  // RUPEE SIGN
    (Unicode:#$2100; Attr:daCompat; Str:#$0061#$002F#$0063),   // ACCOUNT OF
    (Unicode:#$2101; Attr:daCompat; Str:#$0061#$002F#$0073),   // ADDRESSED TO THE SUBJECT
    (Unicode:#$2102; Attr:daFont; Str:#$0043),          // DOUBLE-STRUCK CAPITAL C
    (Unicode:#$2103; Attr:daCompat; Str:#$00B0#$0043),  // DEGREE CELSIUS
    (Unicode:#$2105; Attr:daCompat; Str:#$0063#$002F#$006F),   // CARE OF
    (Unicode:#$2106; Attr:daCompat; Str:#$0063#$002F#$0075),   // CADA UNA
    (Unicode:#$2107; Attr:daCompat; Str:#$0190),        // EULER CONSTANT
    (Unicode:#$2109; Attr:daCompat; Str:#$00B0#$0046),  // DEGREE FAHRENHEIT
    (Unicode:#$210A; Attr:daFont; Str:#$0067),          // SCRIPT SMALL G
    (Unicode:#$210B; Attr:daFont; Str:#$0048),          // SCRIPT CAPITAL H
    (Unicode:#$210C; Attr:daFont; Str:#$0048),          // BLACK-LETTER CAPITAL H
    (Unicode:#$210D; Attr:daFont; Str:#$0048),          // DOUBLE-STRUCK CAPITAL H
    (Unicode:#$210E; Attr:daFont; Str:#$0068),          // PLANCK CONSTANT
    (Unicode:#$210F; Attr:daFont; Str:#$0127),          // PLANCK CONSTANT OVER TWO PI
    (Unicode:#$2110; Attr:daFont; Str:#$0049),          // SCRIPT CAPITAL I
    (Unicode:#$2111; Attr:daFont; Str:#$0049),          // BLACK-LETTER CAPITAL I
    (Unicode:#$2112; Attr:daFont; Str:#$004C),          // SCRIPT CAPITAL L
    (Unicode:#$2113; Attr:daFont; Str:#$006C),          // SCRIPT SMALL L
    (Unicode:#$2115; Attr:daFont; Str:#$004E),          // DOUBLE-STRUCK CAPITAL N
    (Unicode:#$2116; Attr:daCompat; Str:#$004E#$006F),  // NUMERO SIGN
    (Unicode:#$2119; Attr:daFont; Str:#$0050),          // DOUBLE-STRUCK CAPITAL P
    (Unicode:#$211A; Attr:daFont; Str:#$0051),          // DOUBLE-STRUCK CAPITAL Q
    (Unicode:#$211B; Attr:daFont; Str:#$0052),          // SCRIPT CAPITAL R
    (Unicode:#$211C; Attr:daFont; Str:#$0052),          // BLACK-LETTER CAPITAL R
    (Unicode:#$211D; Attr:daFont; Str:#$0052),          // DOUBLE-STRUCK CAPITAL R
    (Unicode:#$2120; Attr:daSuper; Str:#$0053#$004D),   // SERVICE MARK
    (Unicode:#$2121; Attr:daCompat; Str:#$0054#$0045#$004C),   // TELEPHONE SIGN
    (Unicode:#$2122; Attr:daSuper; Str:#$0054#$004D),   // TRADE MARK SIGN
    (Unicode:#$2124; Attr:daFont; Str:#$005A),          // DOUBLE-STRUCK CAPITAL Z
    (Unicode:#$2126; Attr:daNone; Str:#$03A9),          // OHM SIGN
    (Unicode:#$2128; Attr:daFont; Str:#$005A),          // BLACK-LETTER CAPITAL Z
    (Unicode:#$212A; Attr:daNone; Str:#$004B),          // KELVIN SIGN
    (Unicode:#$212B; Attr:daNone; Str:#$00C5),          // ANGSTROM SIGN
    (Unicode:#$212C; Attr:daFont; Str:#$0042),          // SCRIPT CAPITAL B
    (Unicode:#$212D; Attr:daFont; Str:#$0043),          // BLACK-LETTER CAPITAL C
    (Unicode:#$212F; Attr:daFont; Str:#$0065),          // SCRIPT SMALL E
    (Unicode:#$2130; Attr:daFont; Str:#$0045),          // SCRIPT CAPITAL E
    (Unicode:#$2131; Attr:daFont; Str:#$0046),          // SCRIPT CAPITAL F
    (Unicode:#$2133; Attr:daFont; Str:#$004D),          // SCRIPT CAPITAL M
    (Unicode:#$2134; Attr:daFont; Str:#$006F),          // SCRIPT SMALL O
    (Unicode:#$2135; Attr:daCompat; Str:#$05D0),        // ALEF SYMBOL
    (Unicode:#$2136; Attr:daCompat; Str:#$05D1),        // BET SYMBOL
    (Unicode:#$2137; Attr:daCompat; Str:#$05D2),        // GIMEL SYMBOL
    (Unicode:#$2138; Attr:daCompat; Str:#$05D3),        // DALET SYMBOL
    (Unicode:#$2139; Attr:daFont; Str:#$0069),          // INFORMATION SOURCE
    (Unicode:#$2153; Attr:daFraction; Str:#$0031#$2044#$0033), // VULGAR FRACTION ONE THIRD
    (Unicode:#$2154; Attr:daFraction; Str:#$0032#$2044#$0033), // VULGAR FRACTION TWO THIRDS
    (Unicode:#$2155; Attr:daFraction; Str:#$0031#$2044#$0035), // VULGAR FRACTION ONE FIFTH
    (Unicode:#$2156; Attr:daFraction; Str:#$0032#$2044#$0035), // VULGAR FRACTION TWO FIFTHS
    (Unicode:#$2157; Attr:daFraction; Str:#$0033#$2044#$0035), // VULGAR FRACTION THREE FIFTHS
    (Unicode:#$2158; Attr:daFraction; Str:#$0034#$2044#$0035), // VULGAR FRACTION FOUR FIFTHS
    (Unicode:#$2159; Attr:daFraction; Str:#$0031#$2044#$0036), // VULGAR FRACTION ONE SIXTH
    (Unicode:#$215A; Attr:daFraction; Str:#$0035#$2044#$0036), // VULGAR FRACTION FIVE SIXTHS
    (Unicode:#$215B; Attr:daFraction; Str:#$0031#$2044#$0038), // VULGAR FRACTION ONE EIGHTH
    (Unicode:#$215C; Attr:daFraction; Str:#$0033#$2044#$0038), // VULGAR FRACTION THREE EIGHTHS
    (Unicode:#$215D; Attr:daFraction; Str:#$0035#$2044#$0038), // VULGAR FRACTION FIVE EIGHTHS
    (Unicode:#$215E; Attr:daFraction; Str:#$0037#$2044#$0038), // VULGAR FRACTION SEVEN EIGHTHS
    (Unicode:#$215F; Attr:daFraction; Str:#$0031#$2044),// FRACTION NUMERATOR ONE
    (Unicode:#$2160; Attr:daCompat; Str:#$0049),        // ROMAN NUMERAL ONE
    (Unicode:#$2161; Attr:daCompat; Str:#$0049#$0049),  // ROMAN NUMERAL TWO
    (Unicode:#$2162; Attr:daCompat; Str:#$0049#$0049#$0049),   // ROMAN NUMERAL THREE
    (Unicode:#$2163; Attr:daCompat; Str:#$0049#$0056),  // ROMAN NUMERAL FOUR
    (Unicode:#$2164; Attr:daCompat; Str:#$0056),        // ROMAN NUMERAL FIVE
    (Unicode:#$2165; Attr:daCompat; Str:#$0056#$0049),  // ROMAN NUMERAL SIX
    (Unicode:#$2166; Attr:daCompat; Str:#$0056#$0049#$0049),   // ROMAN NUMERAL SEVEN
    (Unicode:#$2167; Attr:daCompat; Str:#$0056#$0049#$0049#$0049),  // ROMAN NUMERAL EIGHT
    (Unicode:#$2168; Attr:daCompat; Str:#$0049#$0058),  // ROMAN NUMERAL NINE
    (Unicode:#$2169; Attr:daCompat; Str:#$0058),        // ROMAN NUMERAL TEN
    (Unicode:#$216A; Attr:daCompat; Str:#$0058#$0049),  // ROMAN NUMERAL ELEVEN
    (Unicode:#$216B; Attr:daCompat; Str:#$0058#$0049#$0049),   // ROMAN NUMERAL TWELVE
    (Unicode:#$216C; Attr:daCompat; Str:#$004C),        // ROMAN NUMERAL FIFTY
    (Unicode:#$216D; Attr:daCompat; Str:#$0043),        // ROMAN NUMERAL ONE HUNDRED
    (Unicode:#$216E; Attr:daCompat; Str:#$0044),        // ROMAN NUMERAL FIVE HUNDRED
    (Unicode:#$216F; Attr:daCompat; Str:#$004D),        // ROMAN NUMERAL ONE THOUSAND
    (Unicode:#$2170; Attr:daCompat; Str:#$0069),        // SMALL ROMAN NUMERAL ONE
    (Unicode:#$2171; Attr:daCompat; Str:#$0069#$0069),  // SMALL ROMAN NUMERAL TWO
    (Unicode:#$2172; Attr:daCompat; Str:#$0069#$0069#$0069),   // SMALL ROMAN NUMERAL THREE
    (Unicode:#$2173; Attr:daCompat; Str:#$0069#$0076),  // SMALL ROMAN NUMERAL FOUR
    (Unicode:#$2174; Attr:daCompat; Str:#$0076),        // SMALL ROMAN NUMERAL FIVE
    (Unicode:#$2175; Attr:daCompat; Str:#$0076#$0069),  // SMALL ROMAN NUMERAL SIX
    (Unicode:#$2176; Attr:daCompat; Str:#$0076#$0069#$0069),   // SMALL ROMAN NUMERAL SEVEN
    (Unicode:#$2177; Attr:daCompat; Str:#$0076#$0069#$0069#$0069),  // SMALL ROMAN NUMERAL EIGHT
    (Unicode:#$2178; Attr:daCompat; Str:#$0069#$0078),  // SMALL ROMAN NUMERAL NINE
    (Unicode:#$2179; Attr:daCompat; Str:#$0078),        // SMALL ROMAN NUMERAL TEN
    (Unicode:#$217A; Attr:daCompat; Str:#$0078#$0069),  // SMALL ROMAN NUMERAL ELEVEN
    (Unicode:#$217B; Attr:daCompat; Str:#$0078#$0069#$0069),   // SMALL ROMAN NUMERAL TWELVE
    (Unicode:#$217C; Attr:daCompat; Str:#$006C),        // SMALL ROMAN NUMERAL FIFTY
    (Unicode:#$217D; Attr:daCompat; Str:#$0063),        // SMALL ROMAN NUMERAL ONE HUNDRED
    (Unicode:#$217E; Attr:daCompat; Str:#$0064),        // SMALL ROMAN NUMERAL FIVE HUNDRED
    (Unicode:#$217F; Attr:daCompat; Str:#$006D),        // SMALL ROMAN NUMERAL ONE THOUSAND
    (Unicode:#$219A; Attr:daNone; Str:#$2190#$0338),    // LEFTWARDS ARROW WITH STROKE
    (Unicode:#$219B; Attr:daNone; Str:#$2192#$0338),    // RIGHTWARDS ARROW WITH STROKE
    (Unicode:#$21AE; Attr:daNone; Str:#$2194#$0338),    // LEFT RIGHT ARROW WITH STROKE
    (Unicode:#$21CD; Attr:daNone; Str:#$21D0#$0338),    // LEFTWARDS DOUBLE ARROW WITH STROKE
    (Unicode:#$21CE; Attr:daNone; Str:#$21D4#$0338),    // LEFT RIGHT DOUBLE ARROW WITH STROKE
    (Unicode:#$21CF; Attr:daNone; Str:#$21D2#$0338),    // RIGHTWARDS DOUBLE ARROW WITH STROKE
    (Unicode:#$2204; Attr:daNone; Str:#$2203#$0338),    // THERE DOES NOT EXIST
    (Unicode:#$2209; Attr:daNone; Str:#$2208#$0338),    // NOT AN ELEMENT OF
    (Unicode:#$220C; Attr:daNone; Str:#$220B#$0338),    // DOES NOT CONTAIN AS MEMBER
    (Unicode:#$2224; Attr:daNone; Str:#$2223#$0338),    // DOES NOT DIVIDE
    (Unicode:#$2226; Attr:daNone; Str:#$2225#$0338),    // NOT PARALLEL TO
    (Unicode:#$222C; Attr:daCompat; Str:#$222B#$222B),  // DOUBLE INTEGRAL
    (Unicode:#$222D; Attr:daCompat; Str:#$222B#$222B#$222B),   // TRIPLE INTEGRAL
    (Unicode:#$222F; Attr:daCompat; Str:#$222E#$222E),  // SURFACE INTEGRAL
    (Unicode:#$2230; Attr:daCompat; Str:#$222E#$222E#$222E),   // VOLUME INTEGRAL
    (Unicode:#$2241; Attr:daNone; Str:#$223C#$0338),    // NOT TILDE
    (Unicode:#$2244; Attr:daNone; Str:#$2243#$0338),    // NOT ASYMPTOTICALLY EQUAL TO
    (Unicode:#$2247; Attr:daNone; Str:#$2245#$0338),    // NEITHER APPROXIMATELY NOR ACTUALLY EQUAL TO
    (Unicode:#$2249; Attr:daNone; Str:#$2248#$0338),    // NOT ALMOST EQUAL TO
    (Unicode:#$2260; Attr:daNone; Str:#$003D#$0338),    // NOT EQUAL TO
    (Unicode:#$2262; Attr:daNone; Str:#$2261#$0338),    // NOT IDENTICAL TO
    (Unicode:#$226D; Attr:daNone; Str:#$224D#$0338),    // NOT EQUIVALENT TO
    (Unicode:#$226E; Attr:daNone; Str:#$003C#$0338),    // NOT LESS-THAN
    (Unicode:#$226F; Attr:daNone; Str:#$003E#$0338),    // NOT GREATER-THAN
    (Unicode:#$2270; Attr:daNone; Str:#$2264#$0338),    // NEITHER LESS-THAN NOR EQUAL TO
    (Unicode:#$2271; Attr:daNone; Str:#$2265#$0338),    // NEITHER GREATER-THAN NOR EQUAL TO
    (Unicode:#$2274; Attr:daNone; Str:#$2272#$0338),    // NEITHER LESS-THAN NOR EQUIVALENT TO
    (Unicode:#$2275; Attr:daNone; Str:#$2273#$0338),    // NEITHER GREATER-THAN NOR EQUIVALENT TO
    (Unicode:#$2278; Attr:daNone; Str:#$2276#$0338),    // NEITHER LESS-THAN NOR GREATER-THAN
    (Unicode:#$2279; Attr:daNone; Str:#$2277#$0338),    // NEITHER GREATER-THAN NOR LESS-THAN
    (Unicode:#$2280; Attr:daNone; Str:#$227A#$0338),    // DOES NOT PRECEDE
    (Unicode:#$2281; Attr:daNone; Str:#$227B#$0338),    // DOES NOT SUCCEED
    (Unicode:#$2284; Attr:daNone; Str:#$2282#$0338),    // NOT A SUBSET OF
    (Unicode:#$2285; Attr:daNone; Str:#$2283#$0338),    // NOT A SUPERSET OF
    (Unicode:#$2288; Attr:daNone; Str:#$2286#$0338),    // NEITHER A SUBSET OF NOR EQUAL TO
    (Unicode:#$2289; Attr:daNone; Str:#$2287#$0338),    // NEITHER A SUPERSET OF NOR EQUAL TO
    (Unicode:#$22AC; Attr:daNone; Str:#$22A2#$0338),    // DOES NOT PROVE
    (Unicode:#$22AD; Attr:daNone; Str:#$22A8#$0338),    // NOT TRUE
    (Unicode:#$22AE; Attr:daNone; Str:#$22A9#$0338),    // DOES NOT FORCE
    (Unicode:#$22AF; Attr:daNone; Str:#$22AB#$0338),    // NEGATED DOUBLE VERTICAL BAR DOUBLE RIGHT TURNSTILE
    (Unicode:#$22E0; Attr:daNone; Str:#$227C#$0338),    // DOES NOT PRECEDE OR EQUAL
    (Unicode:#$22E1; Attr:daNone; Str:#$227D#$0338),    // DOES NOT SUCCEED OR EQUAL
    (Unicode:#$22E2; Attr:daNone; Str:#$2291#$0338),    // NOT SQUARE IMAGE OF OR EQUAL TO
    (Unicode:#$22E3; Attr:daNone; Str:#$2292#$0338),    // NOT SQUARE ORIGINAL OF OR EQUAL TO
    (Unicode:#$22EA; Attr:daNone; Str:#$22B2#$0338),    // NOT NORMAL SUBGROUP OF
    (Unicode:#$22EB; Attr:daNone; Str:#$22B3#$0338),    // DOES NOT CONTAIN AS NORMAL SUBGROUP
    (Unicode:#$22EC; Attr:daNone; Str:#$22B4#$0338),    // NOT NORMAL SUBGROUP OF OR EQUAL TO
    (Unicode:#$22ED; Attr:daNone; Str:#$22B5#$0338),    // DOES NOT CONTAIN AS NORMAL SUBGROUP OR EQUAL
    (Unicode:#$2329; Attr:daNone; Str:#$3008),          // LEFT-POINTING ANGLE BRACKET
    (Unicode:#$232A; Attr:daNone; Str:#$3009),          // RIGHT-POINTING ANGLE BRACKET
    (Unicode:#$2460; Attr:daCircle; Str:#$0031),        // CIRCLED DIGIT ONE
    (Unicode:#$2461; Attr:daCircle; Str:#$0032),        // CIRCLED DIGIT TWO
    (Unicode:#$2462; Attr:daCircle; Str:#$0033),        // CIRCLED DIGIT THREE
    (Unicode:#$2463; Attr:daCircle; Str:#$0034),        // CIRCLED DIGIT FOUR
    (Unicode:#$2464; Attr:daCircle; Str:#$0035),        // CIRCLED DIGIT FIVE
    (Unicode:#$2465; Attr:daCircle; Str:#$0036),        // CIRCLED DIGIT SIX
    (Unicode:#$2466; Attr:daCircle; Str:#$0037),        // CIRCLED DIGIT SEVEN
    (Unicode:#$2467; Attr:daCircle; Str:#$0038),        // CIRCLED DIGIT EIGHT
    (Unicode:#$2468; Attr:daCircle; Str:#$0039),        // CIRCLED DIGIT NINE
    (Unicode:#$2469; Attr:daCircle; Str:#$0031#$0030),  // CIRCLED NUMBER TEN
    (Unicode:#$246A; Attr:daCircle; Str:#$0031#$0031),  // CIRCLED NUMBER ELEVEN
    (Unicode:#$246B; Attr:daCircle; Str:#$0031#$0032),  // CIRCLED NUMBER TWELVE
    (Unicode:#$246C; Attr:daCircle; Str:#$0031#$0033),  // CIRCLED NUMBER THIRTEEN
    (Unicode:#$246D; Attr:daCircle; Str:#$0031#$0034),  // CIRCLED NUMBER FOURTEEN
    (Unicode:#$246E; Attr:daCircle; Str:#$0031#$0035),  // CIRCLED NUMBER FIFTEEN
    (Unicode:#$246F; Attr:daCircle; Str:#$0031#$0036),  // CIRCLED NUMBER SIXTEEN
    (Unicode:#$2470; Attr:daCircle; Str:#$0031#$0037),  // CIRCLED NUMBER SEVENTEEN
    (Unicode:#$2471; Attr:daCircle; Str:#$0031#$0038),  // CIRCLED NUMBER EIGHTEEN
    (Unicode:#$2472; Attr:daCircle; Str:#$0031#$0039),  // CIRCLED NUMBER NINETEEN
    (Unicode:#$2473; Attr:daCircle; Str:#$0032#$0030),  // CIRCLED NUMBER TWENTY
    (Unicode:#$2474; Attr:daCompat; Str:#$0028#$0031#$0029),   // PARENTHESIZED DIGIT ONE
    (Unicode:#$2475; Attr:daCompat; Str:#$0028#$0032#$0029),   // PARENTHESIZED DIGIT TWO
    (Unicode:#$2476; Attr:daCompat; Str:#$0028#$0033#$0029),   // PARENTHESIZED DIGIT THREE
    (Unicode:#$2477; Attr:daCompat; Str:#$0028#$0034#$0029),   // PARENTHESIZED DIGIT FOUR
    (Unicode:#$2478; Attr:daCompat; Str:#$0028#$0035#$0029),   // PARENTHESIZED DIGIT FIVE
    (Unicode:#$2479; Attr:daCompat; Str:#$0028#$0036#$0029),   // PARENTHESIZED DIGIT SIX
    (Unicode:#$247A; Attr:daCompat; Str:#$0028#$0037#$0029),   // PARENTHESIZED DIGIT SEVEN
    (Unicode:#$247B; Attr:daCompat; Str:#$0028#$0038#$0029),   // PARENTHESIZED DIGIT EIGHT
    (Unicode:#$247C; Attr:daCompat; Str:#$0028#$0039#$0029),   // PARENTHESIZED DIGIT NINE
    (Unicode:#$247D; Attr:daCompat; Str:#$0028#$0031#$0030#$0029),  // PARENTHESIZED NUMBER TEN
    (Unicode:#$247E; Attr:daCompat; Str:#$0028#$0031#$0031#$0029),  // PARENTHESIZED NUMBER ELEVEN
    (Unicode:#$247F; Attr:daCompat; Str:#$0028#$0031#$0032#$0029),  // PARENTHESIZED NUMBER TWELVE
    (Unicode:#$2480; Attr:daCompat; Str:#$0028#$0031#$0033#$0029),  // PARENTHESIZED NUMBER THIRTEEN
    (Unicode:#$2481; Attr:daCompat; Str:#$0028#$0031#$0034#$0029),  // PARENTHESIZED NUMBER FOURTEEN
    (Unicode:#$2482; Attr:daCompat; Str:#$0028#$0031#$0035#$0029),  // PARENTHESIZED NUMBER FIFTEEN
    (Unicode:#$2483; Attr:daCompat; Str:#$0028#$0031#$0036#$0029),  // PARENTHESIZED NUMBER SIXTEEN
    (Unicode:#$2484; Attr:daCompat; Str:#$0028#$0031#$0037#$0029),  // PARENTHESIZED NUMBER SEVENTEEN
    (Unicode:#$2485; Attr:daCompat; Str:#$0028#$0031#$0038#$0029),  // PARENTHESIZED NUMBER EIGHTEEN
    (Unicode:#$2486; Attr:daCompat; Str:#$0028#$0031#$0039#$0029),  // PARENTHESIZED NUMBER NINETEEN
    (Unicode:#$2487; Attr:daCompat; Str:#$0028#$0032#$0030#$0029),  // PARENTHESIZED NUMBER TWENTY
    (Unicode:#$2488; Attr:daCompat; Str:#$0031#$002E),  // DIGIT ONE FULL STOP
    (Unicode:#$2489; Attr:daCompat; Str:#$0032#$002E),  // DIGIT TWO FULL STOP
    (Unicode:#$248A; Attr:daCompat; Str:#$0033#$002E),  // DIGIT THREE FULL STOP
    (Unicode:#$248B; Attr:daCompat; Str:#$0034#$002E),  // DIGIT FOUR FULL STOP
    (Unicode:#$248C; Attr:daCompat; Str:#$0035#$002E),  // DIGIT FIVE FULL STOP
    (Unicode:#$248D; Attr:daCompat; Str:#$0036#$002E),  // DIGIT SIX FULL STOP
    (Unicode:#$248E; Attr:daCompat; Str:#$0037#$002E),  // DIGIT SEVEN FULL STOP
    (Unicode:#$248F; Attr:daCompat; Str:#$0038#$002E),  // DIGIT EIGHT FULL STOP
    (Unicode:#$2490; Attr:daCompat; Str:#$0039#$002E),  // DIGIT NINE FULL STOP
    (Unicode:#$2491; Attr:daCompat; Str:#$0031#$0030#$002E),   // NUMBER TEN FULL STOP
    (Unicode:#$2492; Attr:daCompat; Str:#$0031#$0031#$002E),   // NUMBER ELEVEN FULL STOP
    (Unicode:#$2493; Attr:daCompat; Str:#$0031#$0032#$002E),   // NUMBER TWELVE FULL STOP
    (Unicode:#$2494; Attr:daCompat; Str:#$0031#$0033#$002E),   // NUMBER THIRTEEN FULL STOP
    (Unicode:#$2495; Attr:daCompat; Str:#$0031#$0034#$002E),   // NUMBER FOURTEEN FULL STOP
    (Unicode:#$2496; Attr:daCompat; Str:#$0031#$0035#$002E),   // NUMBER FIFTEEN FULL STOP
    (Unicode:#$2497; Attr:daCompat; Str:#$0031#$0036#$002E),   // NUMBER SIXTEEN FULL STOP
    (Unicode:#$2498; Attr:daCompat; Str:#$0031#$0037#$002E),   // NUMBER SEVENTEEN FULL STOP
    (Unicode:#$2499; Attr:daCompat; Str:#$0031#$0038#$002E),   // NUMBER EIGHTEEN FULL STOP
    (Unicode:#$249A; Attr:daCompat; Str:#$0031#$0039#$002E),   // NUMBER NINETEEN FULL STOP
    (Unicode:#$249B; Attr:daCompat; Str:#$0032#$0030#$002E),   // NUMBER TWENTY FULL STOP
    (Unicode:#$249C; Attr:daCompat; Str:#$0028#$0061#$0029),   // PARENTHESIZED LATIN SMALL LETTER A
    (Unicode:#$249D; Attr:daCompat; Str:#$0028#$0062#$0029),   // PARENTHESIZED LATIN SMALL LETTER B
    (Unicode:#$249E; Attr:daCompat; Str:#$0028#$0063#$0029),   // PARENTHESIZED LATIN SMALL LETTER C
    (Unicode:#$249F; Attr:daCompat; Str:#$0028#$0064#$0029),   // PARENTHESIZED LATIN SMALL LETTER D
    (Unicode:#$24A0; Attr:daCompat; Str:#$0028#$0065#$0029),   // PARENTHESIZED LATIN SMALL LETTER E
    (Unicode:#$24A1; Attr:daCompat; Str:#$0028#$0066#$0029),   // PARENTHESIZED LATIN SMALL LETTER F
    (Unicode:#$24A2; Attr:daCompat; Str:#$0028#$0067#$0029),   // PARENTHESIZED LATIN SMALL LETTER G
    (Unicode:#$24A3; Attr:daCompat; Str:#$0028#$0068#$0029),   // PARENTHESIZED LATIN SMALL LETTER H
    (Unicode:#$24A4; Attr:daCompat; Str:#$0028#$0069#$0029),   // PARENTHESIZED LATIN SMALL LETTER I
    (Unicode:#$24A5; Attr:daCompat; Str:#$0028#$006A#$0029),   // PARENTHESIZED LATIN SMALL LETTER J
    (Unicode:#$24A6; Attr:daCompat; Str:#$0028#$006B#$0029),   // PARENTHESIZED LATIN SMALL LETTER K
    (Unicode:#$24A7; Attr:daCompat; Str:#$0028#$006C#$0029),   // PARENTHESIZED LATIN SMALL LETTER L
    (Unicode:#$24A8; Attr:daCompat; Str:#$0028#$006D#$0029),   // PARENTHESIZED LATIN SMALL LETTER M
    (Unicode:#$24A9; Attr:daCompat; Str:#$0028#$006E#$0029),   // PARENTHESIZED LATIN SMALL LETTER N
    (Unicode:#$24AA; Attr:daCompat; Str:#$0028#$006F#$0029),   // PARENTHESIZED LATIN SMALL LETTER O
    (Unicode:#$24AB; Attr:daCompat; Str:#$0028#$0070#$0029),   // PARENTHESIZED LATIN SMALL LETTER P
    (Unicode:#$24AC; Attr:daCompat; Str:#$0028#$0071#$0029),   // PARENTHESIZED LATIN SMALL LETTER Q
    (Unicode:#$24AD; Attr:daCompat; Str:#$0028#$0072#$0029),   // PARENTHESIZED LATIN SMALL LETTER R
    (Unicode:#$24AE; Attr:daCompat; Str:#$0028#$0073#$0029),   // PARENTHESIZED LATIN SMALL LETTER S
    (Unicode:#$24AF; Attr:daCompat; Str:#$0028#$0074#$0029),   // PARENTHESIZED LATIN SMALL LETTER T
    (Unicode:#$24B0; Attr:daCompat; Str:#$0028#$0075#$0029),   // PARENTHESIZED LATIN SMALL LETTER U
    (Unicode:#$24B1; Attr:daCompat; Str:#$0028#$0076#$0029),   // PARENTHESIZED LATIN SMALL LETTER V
    (Unicode:#$24B2; Attr:daCompat; Str:#$0028#$0077#$0029),   // PARENTHESIZED LATIN SMALL LETTER W
    (Unicode:#$24B3; Attr:daCompat; Str:#$0028#$0078#$0029),   // PARENTHESIZED LATIN SMALL LETTER X
    (Unicode:#$24B4; Attr:daCompat; Str:#$0028#$0079#$0029),   // PARENTHESIZED LATIN SMALL LETTER Y
    (Unicode:#$24B5; Attr:daCompat; Str:#$0028#$007A#$0029),   // PARENTHESIZED LATIN SMALL LETTER Z
    (Unicode:#$24B6; Attr:daCircle; Str:#$0041),        // CIRCLED LATIN CAPITAL LETTER A
    (Unicode:#$24B7; Attr:daCircle; Str:#$0042),        // CIRCLED LATIN CAPITAL LETTER B
    (Unicode:#$24B8; Attr:daCircle; Str:#$0043),        // CIRCLED LATIN CAPITAL LETTER C
    (Unicode:#$24B9; Attr:daCircle; Str:#$0044),        // CIRCLED LATIN CAPITAL LETTER D
    (Unicode:#$24BA; Attr:daCircle; Str:#$0045),        // CIRCLED LATIN CAPITAL LETTER E
    (Unicode:#$24BB; Attr:daCircle; Str:#$0046),        // CIRCLED LATIN CAPITAL LETTER F
    (Unicode:#$24BC; Attr:daCircle; Str:#$0047),        // CIRCLED LATIN CAPITAL LETTER G
    (Unicode:#$24BD; Attr:daCircle; Str:#$0048),        // CIRCLED LATIN CAPITAL LETTER H
    (Unicode:#$24BE; Attr:daCircle; Str:#$0049),        // CIRCLED LATIN CAPITAL LETTER I
    (Unicode:#$24BF; Attr:daCircle; Str:#$004A),        // CIRCLED LATIN CAPITAL LETTER J
    (Unicode:#$24C0; Attr:daCircle; Str:#$004B),        // CIRCLED LATIN CAPITAL LETTER K
    (Unicode:#$24C1; Attr:daCircle; Str:#$004C),        // CIRCLED LATIN CAPITAL LETTER L
    (Unicode:#$24C2; Attr:daCircle; Str:#$004D),        // CIRCLED LATIN CAPITAL LETTER M
    (Unicode:#$24C3; Attr:daCircle; Str:#$004E),        // CIRCLED LATIN CAPITAL LETTER N
    (Unicode:#$24C4; Attr:daCircle; Str:#$004F),        // CIRCLED LATIN CAPITAL LETTER O
    (Unicode:#$24C5; Attr:daCircle; Str:#$0050),        // CIRCLED LATIN CAPITAL LETTER P
    (Unicode:#$24C6; Attr:daCircle; Str:#$0051),        // CIRCLED LATIN CAPITAL LETTER Q
    (Unicode:#$24C7; Attr:daCircle; Str:#$0052),        // CIRCLED LATIN CAPITAL LETTER R
    (Unicode:#$24C8; Attr:daCircle; Str:#$0053),        // CIRCLED LATIN CAPITAL LETTER S
    (Unicode:#$24C9; Attr:daCircle; Str:#$0054),        // CIRCLED LATIN CAPITAL LETTER T
    (Unicode:#$24CA; Attr:daCircle; Str:#$0055),        // CIRCLED LATIN CAPITAL LETTER U
    (Unicode:#$24CB; Attr:daCircle; Str:#$0056),        // CIRCLED LATIN CAPITAL LETTER V
    (Unicode:#$24CC; Attr:daCircle; Str:#$0057),        // CIRCLED LATIN CAPITAL LETTER W
    (Unicode:#$24CD; Attr:daCircle; Str:#$0058),        // CIRCLED LATIN CAPITAL LETTER X
    (Unicode:#$24CE; Attr:daCircle; Str:#$0059),        // CIRCLED LATIN CAPITAL LETTER Y
    (Unicode:#$24CF; Attr:daCircle; Str:#$005A),        // CIRCLED LATIN CAPITAL LETTER Z
    (Unicode:#$24D0; Attr:daCircle; Str:#$0061),        // CIRCLED LATIN SMALL LETTER A
    (Unicode:#$24D1; Attr:daCircle; Str:#$0062),        // CIRCLED LATIN SMALL LETTER B
    (Unicode:#$24D2; Attr:daCircle; Str:#$0063),        // CIRCLED LATIN SMALL LETTER C
    (Unicode:#$24D3; Attr:daCircle; Str:#$0064),        // CIRCLED LATIN SMALL LETTER D
    (Unicode:#$24D4; Attr:daCircle; Str:#$0065),        // CIRCLED LATIN SMALL LETTER E
    (Unicode:#$24D5; Attr:daCircle; Str:#$0066),        // CIRCLED LATIN SMALL LETTER F
    (Unicode:#$24D6; Attr:daCircle; Str:#$0067),        // CIRCLED LATIN SMALL LETTER G
    (Unicode:#$24D7; Attr:daCircle; Str:#$0068),        // CIRCLED LATIN SMALL LETTER H
    (Unicode:#$24D8; Attr:daCircle; Str:#$0069),        // CIRCLED LATIN SMALL LETTER I
    (Unicode:#$24D9; Attr:daCircle; Str:#$006A),        // CIRCLED LATIN SMALL LETTER J
    (Unicode:#$24DA; Attr:daCircle; Str:#$006B),        // CIRCLED LATIN SMALL LETTER K
    (Unicode:#$24DB; Attr:daCircle; Str:#$006C),        // CIRCLED LATIN SMALL LETTER L
    (Unicode:#$24DC; Attr:daCircle; Str:#$006D),        // CIRCLED LATIN SMALL LETTER M
    (Unicode:#$24DD; Attr:daCircle; Str:#$006E),        // CIRCLED LATIN SMALL LETTER N
    (Unicode:#$24DE; Attr:daCircle; Str:#$006F),        // CIRCLED LATIN SMALL LETTER O
    (Unicode:#$24DF; Attr:daCircle; Str:#$0070),        // CIRCLED LATIN SMALL LETTER P
    (Unicode:#$24E0; Attr:daCircle; Str:#$0071),        // CIRCLED LATIN SMALL LETTER Q
    (Unicode:#$24E1; Attr:daCircle; Str:#$0072),        // CIRCLED LATIN SMALL LETTER R
    (Unicode:#$24E2; Attr:daCircle; Str:#$0073),        // CIRCLED LATIN SMALL LETTER S
    (Unicode:#$24E3; Attr:daCircle; Str:#$0074),        // CIRCLED LATIN SMALL LETTER T
    (Unicode:#$24E4; Attr:daCircle; Str:#$0075),        // CIRCLED LATIN SMALL LETTER U
    (Unicode:#$24E5; Attr:daCircle; Str:#$0076),        // CIRCLED LATIN SMALL LETTER V
    (Unicode:#$24E6; Attr:daCircle; Str:#$0077),        // CIRCLED LATIN SMALL LETTER W
    (Unicode:#$24E7; Attr:daCircle; Str:#$0078),        // CIRCLED LATIN SMALL LETTER X
    (Unicode:#$24E8; Attr:daCircle; Str:#$0079),        // CIRCLED LATIN SMALL LETTER Y
    (Unicode:#$24E9; Attr:daCircle; Str:#$007A),        // CIRCLED LATIN SMALL LETTER Z
    (Unicode:#$24EA; Attr:daCircle; Str:#$0030),        // CIRCLED DIGIT ZERO
    (Unicode:#$2E9F; Attr:daCompat; Str:#$6BCD),        // CJK RADICAL MOTHER
    (Unicode:#$2EF3; Attr:daCompat; Str:#$9F9F),        // CJK RADICAL C-SIMPLIFIED TURTLE
    (Unicode:#$2F00; Attr:daCompat; Str:#$4E00),        // KANGXI RADICAL ONE
    (Unicode:#$2F01; Attr:daCompat; Str:#$4E28),        // KANGXI RADICAL LINE
    (Unicode:#$2F02; Attr:daCompat; Str:#$4E36),        // KANGXI RADICAL DOT
    (Unicode:#$2F03; Attr:daCompat; Str:#$4E3F),        // KANGXI RADICAL SLASH
    (Unicode:#$2F04; Attr:daCompat; Str:#$4E59),        // KANGXI RADICAL SECOND
    (Unicode:#$2F05; Attr:daCompat; Str:#$4E85),        // KANGXI RADICAL HOOK
    (Unicode:#$2F06; Attr:daCompat; Str:#$4E8C),        // KANGXI RADICAL TWO
    (Unicode:#$2F07; Attr:daCompat; Str:#$4EA0),        // KANGXI RADICAL LID
    (Unicode:#$2F08; Attr:daCompat; Str:#$4EBA),        // KANGXI RADICAL MAN
    (Unicode:#$2F09; Attr:daCompat; Str:#$513F),        // KANGXI RADICAL LEGS
    (Unicode:#$2F0A; Attr:daCompat; Str:#$5165),        // KANGXI RADICAL ENTER
    (Unicode:#$2F0B; Attr:daCompat; Str:#$516B),        // KANGXI RADICAL EIGHT
    (Unicode:#$2F0C; Attr:daCompat; Str:#$5182),        // KANGXI RADICAL DOWN BOX
    (Unicode:#$2F0D; Attr:daCompat; Str:#$5196),        // KANGXI RADICAL COVER
    (Unicode:#$2F0E; Attr:daCompat; Str:#$51AB),        // KANGXI RADICAL ICE
    (Unicode:#$2F0F; Attr:daCompat; Str:#$51E0),        // KANGXI RADICAL TABLE
    (Unicode:#$2F10; Attr:daCompat; Str:#$51F5),        // KANGXI RADICAL OPEN BOX
    (Unicode:#$2F11; Attr:daCompat; Str:#$5200),        // KANGXI RADICAL KNIFE
    (Unicode:#$2F12; Attr:daCompat; Str:#$529B),        // KANGXI RADICAL POWER
    (Unicode:#$2F13; Attr:daCompat; Str:#$52F9),        // KANGXI RADICAL WRAP
    (Unicode:#$2F14; Attr:daCompat; Str:#$5315),        // KANGXI RADICAL SPOON
    (Unicode:#$2F15; Attr:daCompat; Str:#$531A),        // KANGXI RADICAL RIGHT OPEN BOX
    (Unicode:#$2F16; Attr:daCompat; Str:#$5338),        // KANGXI RADICAL HIDING ENCLOSURE
    (Unicode:#$2F17; Attr:daCompat; Str:#$5341),        // KANGXI RADICAL TEN
    (Unicode:#$2F18; Attr:daCompat; Str:#$535C),        // KANGXI RADICAL DIVINATION
    (Unicode:#$2F19; Attr:daCompat; Str:#$5369),        // KANGXI RADICAL SEAL
    (Unicode:#$2F1A; Attr:daCompat; Str:#$5382),        // KANGXI RADICAL CLIFF
    (Unicode:#$2F1B; Attr:daCompat; Str:#$53B6),        // KANGXI RADICAL PRIVATE
    (Unicode:#$2F1C; Attr:daCompat; Str:#$53C8),        // KANGXI RADICAL AGAIN
    (Unicode:#$2F1D; Attr:daCompat; Str:#$53E3),        // KANGXI RADICAL MOUTH
    (Unicode:#$2F1E; Attr:daCompat; Str:#$56D7),        // KANGXI RADICAL ENCLOSURE
    (Unicode:#$2F1F; Attr:daCompat; Str:#$571F),        // KANGXI RADICAL EARTH
    (Unicode:#$2F20; Attr:daCompat; Str:#$58EB),        // KANGXI RADICAL SCHOLAR
    (Unicode:#$2F21; Attr:daCompat; Str:#$5902),        // KANGXI RADICAL GO
    (Unicode:#$2F22; Attr:daCompat; Str:#$590A),        // KANGXI RADICAL GO SLOWLY
    (Unicode:#$2F23; Attr:daCompat; Str:#$5915),        // KANGXI RADICAL EVENING
    (Unicode:#$2F24; Attr:daCompat; Str:#$5927),        // KANGXI RADICAL BIG
    (Unicode:#$2F25; Attr:daCompat; Str:#$5973),        // KANGXI RADICAL WOMAN
    (Unicode:#$2F26; Attr:daCompat; Str:#$5B50),        // KANGXI RADICAL CHILD
    (Unicode:#$2F27; Attr:daCompat; Str:#$5B80),        // KANGXI RADICAL ROOF
    (Unicode:#$2F28; Attr:daCompat; Str:#$5BF8),        // KANGXI RADICAL INCH
    (Unicode:#$2F29; Attr:daCompat; Str:#$5C0F),        // KANGXI RADICAL SMALL
    (Unicode:#$2F2A; Attr:daCompat; Str:#$5C22),        // KANGXI RADICAL LAME
    (Unicode:#$2F2B; Attr:daCompat; Str:#$5C38),        // KANGXI RADICAL CORPSE
    (Unicode:#$2F2C; Attr:daCompat; Str:#$5C6E),        // KANGXI RADICAL SPROUT
    (Unicode:#$2F2D; Attr:daCompat; Str:#$5C71),        // KANGXI RADICAL MOUNTAIN
    (Unicode:#$2F2E; Attr:daCompat; Str:#$5DDB),        // KANGXI RADICAL RIVER
    (Unicode:#$2F2F; Attr:daCompat; Str:#$5DE5),        // KANGXI RADICAL WORK
    (Unicode:#$2F30; Attr:daCompat; Str:#$5DF1),        // KANGXI RADICAL ONESELF
    (Unicode:#$2F31; Attr:daCompat; Str:#$5DFE),        // KANGXI RADICAL TURBAN
    (Unicode:#$2F32; Attr:daCompat; Str:#$5E72),        // KANGXI RADICAL DRY
    (Unicode:#$2F33; Attr:daCompat; Str:#$5E7A),        // KANGXI RADICAL SHORT THREAD
    (Unicode:#$2F34; Attr:daCompat; Str:#$5E7F),        // KANGXI RADICAL DOTTED CLIFF
    (Unicode:#$2F35; Attr:daCompat; Str:#$5EF4),        // KANGXI RADICAL LONG STRIDE
    (Unicode:#$2F36; Attr:daCompat; Str:#$5EFE),        // KANGXI RADICAL TWO HANDS
    (Unicode:#$2F37; Attr:daCompat; Str:#$5F0B),        // KANGXI RADICAL SHOOT
    (Unicode:#$2F38; Attr:daCompat; Str:#$5F13),        // KANGXI RADICAL BOW
    (Unicode:#$2F39; Attr:daCompat; Str:#$5F50),        // KANGXI RADICAL SNOUT
    (Unicode:#$2F3A; Attr:daCompat; Str:#$5F61),        // KANGXI RADICAL BRISTLE
    (Unicode:#$2F3B; Attr:daCompat; Str:#$5F73),        // KANGXI RADICAL STEP
    (Unicode:#$2F3C; Attr:daCompat; Str:#$5FC3),        // KANGXI RADICAL HEART
    (Unicode:#$2F3D; Attr:daCompat; Str:#$6208),        // KANGXI RADICAL HALBERD
    (Unicode:#$2F3E; Attr:daCompat; Str:#$6236),        // KANGXI RADICAL DOOR
    (Unicode:#$2F3F; Attr:daCompat; Str:#$624B),        // KANGXI RADICAL HAND
    (Unicode:#$2F40; Attr:daCompat; Str:#$652F),        // KANGXI RADICAL BRANCH
    (Unicode:#$2F41; Attr:daCompat; Str:#$6534),        // KANGXI RADICAL RAP
    (Unicode:#$2F42; Attr:daCompat; Str:#$6587),        // KANGXI RADICAL SCRIPT
    (Unicode:#$2F43; Attr:daCompat; Str:#$6597),        // KANGXI RADICAL DIPPER
    (Unicode:#$2F44; Attr:daCompat; Str:#$65A4),        // KANGXI RADICAL AXE
    (Unicode:#$2F45; Attr:daCompat; Str:#$65B9),        // KANGXI RADICAL SQUARE
    (Unicode:#$2F46; Attr:daCompat; Str:#$65E0),        // KANGXI RADICAL NOT
    (Unicode:#$2F47; Attr:daCompat; Str:#$65E5),        // KANGXI RADICAL SUN
    (Unicode:#$2F48; Attr:daCompat; Str:#$66F0),        // KANGXI RADICAL SAY
    (Unicode:#$2F49; Attr:daCompat; Str:#$6708),        // KANGXI RADICAL MOON
    (Unicode:#$2F4A; Attr:daCompat; Str:#$6728),        // KANGXI RADICAL TREE
    (Unicode:#$2F4B; Attr:daCompat; Str:#$6B20),        // KANGXI RADICAL LACK
    (Unicode:#$2F4C; Attr:daCompat; Str:#$6B62),        // KANGXI RADICAL STOP
    (Unicode:#$2F4D; Attr:daCompat; Str:#$6B79),        // KANGXI RADICAL DEATH
    (Unicode:#$2F4E; Attr:daCompat; Str:#$6BB3),        // KANGXI RADICAL WEAPON
    (Unicode:#$2F4F; Attr:daCompat; Str:#$6BCB),        // KANGXI RADICAL DO NOT
    (Unicode:#$2F50; Attr:daCompat; Str:#$6BD4),        // KANGXI RADICAL COMPARE
    (Unicode:#$2F51; Attr:daCompat; Str:#$6BDB),        // KANGXI RADICAL FUR
    (Unicode:#$2F52; Attr:daCompat; Str:#$6C0F),        // KANGXI RADICAL CLAN
    (Unicode:#$2F53; Attr:daCompat; Str:#$6C14),        // KANGXI RADICAL STEAM
    (Unicode:#$2F54; Attr:daCompat; Str:#$6C34),        // KANGXI RADICAL WATER
    (Unicode:#$2F55; Attr:daCompat; Str:#$706B),        // KANGXI RADICAL FIRE
    (Unicode:#$2F56; Attr:daCompat; Str:#$722A),        // KANGXI RADICAL CLAW
    (Unicode:#$2F57; Attr:daCompat; Str:#$7236),        // KANGXI RADICAL FATHER
    (Unicode:#$2F58; Attr:daCompat; Str:#$723B),        // KANGXI RADICAL DOUBLE X
    (Unicode:#$2F59; Attr:daCompat; Str:#$723F),        // KANGXI RADICAL HALF TREE TRUNK
    (Unicode:#$2F5A; Attr:daCompat; Str:#$7247),        // KANGXI RADICAL SLICE
    (Unicode:#$2F5B; Attr:daCompat; Str:#$7259),        // KANGXI RADICAL FANG
    (Unicode:#$2F5C; Attr:daCompat; Str:#$725B),        // KANGXI RADICAL COW
    (Unicode:#$2F5D; Attr:daCompat; Str:#$72AC),        // KANGXI RADICAL DOG
    (Unicode:#$2F5E; Attr:daCompat; Str:#$7384),        // KANGXI RADICAL PROFOUND
    (Unicode:#$2F5F; Attr:daCompat; Str:#$7389),        // KANGXI RADICAL JADE
    (Unicode:#$2F60; Attr:daCompat; Str:#$74DC),        // KANGXI RADICAL MELON
    (Unicode:#$2F61; Attr:daCompat; Str:#$74E6),        // KANGXI RADICAL TILE
    (Unicode:#$2F62; Attr:daCompat; Str:#$7518),        // KANGXI RADICAL SWEET
    (Unicode:#$2F63; Attr:daCompat; Str:#$751F),        // KANGXI RADICAL LIFE
    (Unicode:#$2F64; Attr:daCompat; Str:#$7528),        // KANGXI RADICAL USE
    (Unicode:#$2F65; Attr:daCompat; Str:#$7530),        // KANGXI RADICAL FIELD
    (Unicode:#$2F66; Attr:daCompat; Str:#$758B),        // KANGXI RADICAL BOLT OF CLOTH
    (Unicode:#$2F67; Attr:daCompat; Str:#$7592),        // KANGXI RADICAL SICKNESS
    (Unicode:#$2F68; Attr:daCompat; Str:#$7676),        // KANGXI RADICAL DOTTED TENT
    (Unicode:#$2F69; Attr:daCompat; Str:#$767D),        // KANGXI RADICAL WHITE
    (Unicode:#$2F6A; Attr:daCompat; Str:#$76AE),        // KANGXI RADICAL SKIN
    (Unicode:#$2F6B; Attr:daCompat; Str:#$76BF),        // KANGXI RADICAL DISH
    (Unicode:#$2F6C; Attr:daCompat; Str:#$76EE),        // KANGXI RADICAL EYE
    (Unicode:#$2F6D; Attr:daCompat; Str:#$77DB),        // KANGXI RADICAL SPEAR
    (Unicode:#$2F6E; Attr:daCompat; Str:#$77E2),        // KANGXI RADICAL ARROW
    (Unicode:#$2F6F; Attr:daCompat; Str:#$77F3),        // KANGXI RADICAL STONE
    (Unicode:#$2F70; Attr:daCompat; Str:#$793A),        // KANGXI RADICAL SPIRIT
    (Unicode:#$2F71; Attr:daCompat; Str:#$79B8),        // KANGXI RADICAL TRACK
    (Unicode:#$2F72; Attr:daCompat; Str:#$79BE),        // KANGXI RADICAL GRAIN
    (Unicode:#$2F73; Attr:daCompat; Str:#$7A74),        // KANGXI RADICAL CAVE
    (Unicode:#$2F74; Attr:daCompat; Str:#$7ACB),        // KANGXI RADICAL STAND
    (Unicode:#$2F75; Attr:daCompat; Str:#$7AF9),        // KANGXI RADICAL BAMBOO
    (Unicode:#$2F76; Attr:daCompat; Str:#$7C73),        // KANGXI RADICAL RICE
    (Unicode:#$2F77; Attr:daCompat; Str:#$7CF8),        // KANGXI RADICAL SILK
    (Unicode:#$2F78; Attr:daCompat; Str:#$7F36),        // KANGXI RADICAL JAR
    (Unicode:#$2F79; Attr:daCompat; Str:#$7F51),        // KANGXI RADICAL NET
    (Unicode:#$2F7A; Attr:daCompat; Str:#$7F8A),        // KANGXI RADICAL SHEEP
    (Unicode:#$2F7B; Attr:daCompat; Str:#$7FBD),        // KANGXI RADICAL FEATHER
    (Unicode:#$2F7C; Attr:daCompat; Str:#$8001),        // KANGXI RADICAL OLD
    (Unicode:#$2F7D; Attr:daCompat; Str:#$800C),        // KANGXI RADICAL AND
    (Unicode:#$2F7E; Attr:daCompat; Str:#$8012),        // KANGXI RADICAL PLOW
    (Unicode:#$2F7F; Attr:daCompat; Str:#$8033),        // KANGXI RADICAL EAR
    (Unicode:#$2F80; Attr:daCompat; Str:#$807F),        // KANGXI RADICAL BRUSH
    (Unicode:#$2F81; Attr:daCompat; Str:#$8089),        // KANGXI RADICAL MEAT
    (Unicode:#$2F82; Attr:daCompat; Str:#$81E3),        // KANGXI RADICAL MINISTER
    (Unicode:#$2F83; Attr:daCompat; Str:#$81EA),        // KANGXI RADICAL SELF
    (Unicode:#$2F84; Attr:daCompat; Str:#$81F3),        // KANGXI RADICAL ARRIVE
    (Unicode:#$2F85; Attr:daCompat; Str:#$81FC),        // KANGXI RADICAL MORTAR
    (Unicode:#$2F86; Attr:daCompat; Str:#$820C),        // KANGXI RADICAL TONGUE
    (Unicode:#$2F87; Attr:daCompat; Str:#$821B),        // KANGXI RADICAL OPPOSE
    (Unicode:#$2F88; Attr:daCompat; Str:#$821F),        // KANGXI RADICAL BOAT
    (Unicode:#$2F89; Attr:daCompat; Str:#$826E),        // KANGXI RADICAL STOPPING
    (Unicode:#$2F8A; Attr:daCompat; Str:#$8272),        // KANGXI RADICAL COLOR
    (Unicode:#$2F8B; Attr:daCompat; Str:#$8278),        // KANGXI RADICAL GRASS
    (Unicode:#$2F8C; Attr:daCompat; Str:#$864D),        // KANGXI RADICAL TIGER
    (Unicode:#$2F8D; Attr:daCompat; Str:#$866B),        // KANGXI RADICAL INSECT
    (Unicode:#$2F8E; Attr:daCompat; Str:#$8840),        // KANGXI RADICAL BLOOD
    (Unicode:#$2F8F; Attr:daCompat; Str:#$884C),        // KANGXI RADICAL WALK ENCLOSURE
    (Unicode:#$2F90; Attr:daCompat; Str:#$8863),        // KANGXI RADICAL CLOTHES
    (Unicode:#$2F91; Attr:daCompat; Str:#$897E),        // KANGXI RADICAL WEST
    (Unicode:#$2F92; Attr:daCompat; Str:#$898B),        // KANGXI RADICAL SEE
    (Unicode:#$2F93; Attr:daCompat; Str:#$89D2),        // KANGXI RADICAL HORN
    (Unicode:#$2F94; Attr:daCompat; Str:#$8A00),        // KANGXI RADICAL SPEECH
    (Unicode:#$2F95; Attr:daCompat; Str:#$8C37),        // KANGXI RADICAL VALLEY
    (Unicode:#$2F96; Attr:daCompat; Str:#$8C46),        // KANGXI RADICAL BEAN
    (Unicode:#$2F97; Attr:daCompat; Str:#$8C55),        // KANGXI RADICAL PIG
    (Unicode:#$2F98; Attr:daCompat; Str:#$8C78),        // KANGXI RADICAL BADGER
    (Unicode:#$2F99; Attr:daCompat; Str:#$8C9D),        // KANGXI RADICAL SHELL
    (Unicode:#$2F9A; Attr:daCompat; Str:#$8D64),        // KANGXI RADICAL RED
    (Unicode:#$2F9B; Attr:daCompat; Str:#$8D70),        // KANGXI RADICAL RUN
    (Unicode:#$2F9C; Attr:daCompat; Str:#$8DB3),        // KANGXI RADICAL FOOT
    (Unicode:#$2F9D; Attr:daCompat; Str:#$8EAB),        // KANGXI RADICAL BODY
    (Unicode:#$2F9E; Attr:daCompat; Str:#$8ECA),        // KANGXI RADICAL CART
    (Unicode:#$2F9F; Attr:daCompat; Str:#$8F9B),        // KANGXI RADICAL BITTER
    (Unicode:#$2FA0; Attr:daCompat; Str:#$8FB0),        // KANGXI RADICAL MORNING
    (Unicode:#$2FA1; Attr:daCompat; Str:#$8FB5),        // KANGXI RADICAL WALK
    (Unicode:#$2FA2; Attr:daCompat; Str:#$9091),        // KANGXI RADICAL CITY
    (Unicode:#$2FA3; Attr:daCompat; Str:#$9149),        // KANGXI RADICAL WINE
    (Unicode:#$2FA4; Attr:daCompat; Str:#$91C6),        // KANGXI RADICAL DISTINGUISH
    (Unicode:#$2FA5; Attr:daCompat; Str:#$91CC),        // KANGXI RADICAL VILLAGE
    (Unicode:#$2FA6; Attr:daCompat; Str:#$91D1),        // KANGXI RADICAL GOLD
    (Unicode:#$2FA7; Attr:daCompat; Str:#$9577),        // KANGXI RADICAL LONG
    (Unicode:#$2FA8; Attr:daCompat; Str:#$9580),        // KANGXI RADICAL GATE
    (Unicode:#$2FA9; Attr:daCompat; Str:#$961C),        // KANGXI RADICAL MOUND
    (Unicode:#$2FAA; Attr:daCompat; Str:#$96B6),        // KANGXI RADICAL SLAVE
    (Unicode:#$2FAB; Attr:daCompat; Str:#$96B9),        // KANGXI RADICAL SHORT TAILED BIRD
    (Unicode:#$2FAC; Attr:daCompat; Str:#$96E8),        // KANGXI RADICAL RAIN
    (Unicode:#$2FAD; Attr:daCompat; Str:#$9751),        // KANGXI RADICAL BLUE
    (Unicode:#$2FAE; Attr:daCompat; Str:#$975E),        // KANGXI RADICAL WRONG
    (Unicode:#$2FAF; Attr:daCompat; Str:#$9762),        // KANGXI RADICAL FACE
    (Unicode:#$2FB0; Attr:daCompat; Str:#$9769),        // KANGXI RADICAL LEATHER
    (Unicode:#$2FB1; Attr:daCompat; Str:#$97CB),        // KANGXI RADICAL TANNED LEATHER
    (Unicode:#$2FB2; Attr:daCompat; Str:#$97ED),        // KANGXI RADICAL LEEK
    (Unicode:#$2FB3; Attr:daCompat; Str:#$97F3),        // KANGXI RADICAL SOUND
    (Unicode:#$2FB4; Attr:daCompat; Str:#$9801),        // KANGXI RADICAL LEAF
    (Unicode:#$2FB5; Attr:daCompat; Str:#$98A8),        // KANGXI RADICAL WIND
    (Unicode:#$2FB6; Attr:daCompat; Str:#$98DB),        // KANGXI RADICAL FLY
    (Unicode:#$2FB7; Attr:daCompat; Str:#$98DF),        // KANGXI RADICAL EAT
    (Unicode:#$2FB8; Attr:daCompat; Str:#$9996),        // KANGXI RADICAL HEAD
    (Unicode:#$2FB9; Attr:daCompat; Str:#$9999),        // KANGXI RADICAL FRAGRANT
    (Unicode:#$2FBA; Attr:daCompat; Str:#$99AC),        // KANGXI RADICAL HORSE
    (Unicode:#$2FBB; Attr:daCompat; Str:#$9AA8),        // KANGXI RADICAL BONE
    (Unicode:#$2FBC; Attr:daCompat; Str:#$9AD8),        // KANGXI RADICAL TALL
    (Unicode:#$2FBD; Attr:daCompat; Str:#$9ADF),        // KANGXI RADICAL HAIR
    (Unicode:#$2FBE; Attr:daCompat; Str:#$9B25),        // KANGXI RADICAL FIGHT
    (Unicode:#$2FBF; Attr:daCompat; Str:#$9B2F),        // KANGXI RADICAL SACRIFICIAL WINE
    (Unicode:#$2FC0; Attr:daCompat; Str:#$9B32),        // KANGXI RADICAL CAULDRON
    (Unicode:#$2FC1; Attr:daCompat; Str:#$9B3C),        // KANGXI RADICAL GHOST
    (Unicode:#$2FC2; Attr:daCompat; Str:#$9B5A),        // KANGXI RADICAL FISH
    (Unicode:#$2FC3; Attr:daCompat; Str:#$9CE5),        // KANGXI RADICAL BIRD
    (Unicode:#$2FC4; Attr:daCompat; Str:#$9E75),        // KANGXI RADICAL SALT
    (Unicode:#$2FC5; Attr:daCompat; Str:#$9E7F),        // KANGXI RADICAL DEER
    (Unicode:#$2FC6; Attr:daCompat; Str:#$9EA5),        // KANGXI RADICAL WHEAT
    (Unicode:#$2FC7; Attr:daCompat; Str:#$9EBB),        // KANGXI RADICAL HEMP
    (Unicode:#$2FC8; Attr:daCompat; Str:#$9EC3),        // KANGXI RADICAL YELLOW
    (Unicode:#$2FC9; Attr:daCompat; Str:#$9ECD),        // KANGXI RADICAL MILLET
    (Unicode:#$2FCA; Attr:daCompat; Str:#$9ED1),        // KANGXI RADICAL BLACK
    (Unicode:#$2FCB; Attr:daCompat; Str:#$9EF9),        // KANGXI RADICAL EMBROIDERY
    (Unicode:#$2FCC; Attr:daCompat; Str:#$9EFD),        // KANGXI RADICAL FROG
    (Unicode:#$2FCD; Attr:daCompat; Str:#$9F0E),        // KANGXI RADICAL TRIPOD
    (Unicode:#$2FCE; Attr:daCompat; Str:#$9F13),        // KANGXI RADICAL DRUM
    (Unicode:#$2FCF; Attr:daCompat; Str:#$9F20),        // KANGXI RADICAL RAT
    (Unicode:#$2FD0; Attr:daCompat; Str:#$9F3B),        // KANGXI RADICAL NOSE
    (Unicode:#$2FD1; Attr:daCompat; Str:#$9F4A),        // KANGXI RADICAL EVEN
    (Unicode:#$2FD2; Attr:daCompat; Str:#$9F52),        // KANGXI RADICAL TOOTH
    (Unicode:#$2FD3; Attr:daCompat; Str:#$9F8D),        // KANGXI RADICAL DRAGON
    (Unicode:#$2FD4; Attr:daCompat; Str:#$9F9C),        // KANGXI RADICAL TURTLE
    (Unicode:#$2FD5; Attr:daCompat; Str:#$9FA0),        // KANGXI RADICAL FLUTE
    (Unicode:#$3000; Attr:daWide; Str:#$0020),          // IDEOGRAPHIC SPACE
    (Unicode:#$3036; Attr:daCompat; Str:#$3012),        // CIRCLED POSTAL MARK
    (Unicode:#$3038; Attr:daCompat; Str:#$5341),        // HANGZHOU NUMERAL TEN
    (Unicode:#$3039; Attr:daCompat; Str:#$5344),        // HANGZHOU NUMERAL TWENTY
    (Unicode:#$303A; Attr:daCompat; Str:#$5345),        // HANGZHOU NUMERAL THIRTY
    (Unicode:#$304C; Attr:daNone; Str:#$304B#$3099),    // HIRAGANA LETTER GA
    (Unicode:#$304E; Attr:daNone; Str:#$304D#$3099),    // HIRAGANA LETTER GI
    (Unicode:#$3050; Attr:daNone; Str:#$304F#$3099),    // HIRAGANA LETTER GU
    (Unicode:#$3052; Attr:daNone; Str:#$3051#$3099),    // HIRAGANA LETTER GE
    (Unicode:#$3054; Attr:daNone; Str:#$3053#$3099),    // HIRAGANA LETTER GO
    (Unicode:#$3056; Attr:daNone; Str:#$3055#$3099),    // HIRAGANA LETTER ZA
    (Unicode:#$3058; Attr:daNone; Str:#$3057#$3099),    // HIRAGANA LETTER ZI
    (Unicode:#$305A; Attr:daNone; Str:#$3059#$3099),    // HIRAGANA LETTER ZU
    (Unicode:#$305C; Attr:daNone; Str:#$305B#$3099),    // HIRAGANA LETTER ZE
    (Unicode:#$305E; Attr:daNone; Str:#$305D#$3099),    // HIRAGANA LETTER ZO
    (Unicode:#$3060; Attr:daNone; Str:#$305F#$3099),    // HIRAGANA LETTER DA
    (Unicode:#$3062; Attr:daNone; Str:#$3061#$3099),    // HIRAGANA LETTER DI
    (Unicode:#$3065; Attr:daNone; Str:#$3064#$3099),    // HIRAGANA LETTER DU
    (Unicode:#$3067; Attr:daNone; Str:#$3066#$3099),    // HIRAGANA LETTER DE
    (Unicode:#$3069; Attr:daNone; Str:#$3068#$3099),    // HIRAGANA LETTER DO
    (Unicode:#$3070; Attr:daNone; Str:#$306F#$3099),    // HIRAGANA LETTER BA
    (Unicode:#$3071; Attr:daNone; Str:#$306F#$309A),    // HIRAGANA LETTER PA
    (Unicode:#$3073; Attr:daNone; Str:#$3072#$3099),    // HIRAGANA LETTER BI
    (Unicode:#$3074; Attr:daNone; Str:#$3072#$309A),    // HIRAGANA LETTER PI
    (Unicode:#$3076; Attr:daNone; Str:#$3075#$3099),    // HIRAGANA LETTER BU
    (Unicode:#$3077; Attr:daNone; Str:#$3075#$309A),    // HIRAGANA LETTER PU
    (Unicode:#$3079; Attr:daNone; Str:#$3078#$3099),    // HIRAGANA LETTER BE
    (Unicode:#$307A; Attr:daNone; Str:#$3078#$309A),    // HIRAGANA LETTER PE
    (Unicode:#$307C; Attr:daNone; Str:#$307B#$3099),    // HIRAGANA LETTER BO
    (Unicode:#$307D; Attr:daNone; Str:#$307B#$309A),    // HIRAGANA LETTER PO
    (Unicode:#$3094; Attr:daNone; Str:#$3046#$3099),    // HIRAGANA LETTER VU
    (Unicode:#$309B; Attr:daCompat; Str:#$0020#$3099),  // KATAKANA-HIRAGANA VOICED SOUND MARK
    (Unicode:#$309C; Attr:daCompat; Str:#$0020#$309A),  // KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK
    (Unicode:#$309E; Attr:daNone; Str:#$309D#$3099),    // HIRAGANA VOICED ITERATION MARK
    (Unicode:#$30AC; Attr:daNone; Str:#$30AB#$3099),    // KATAKANA LETTER GA
    (Unicode:#$30AE; Attr:daNone; Str:#$30AD#$3099),    // KATAKANA LETTER GI
    (Unicode:#$30B0; Attr:daNone; Str:#$30AF#$3099),    // KATAKANA LETTER GU
    (Unicode:#$30B2; Attr:daNone; Str:#$30B1#$3099),    // KATAKANA LETTER GE
    (Unicode:#$30B4; Attr:daNone; Str:#$30B3#$3099),    // KATAKANA LETTER GO
    (Unicode:#$30B6; Attr:daNone; Str:#$30B5#$3099),    // KATAKANA LETTER ZA
    (Unicode:#$30B8; Attr:daNone; Str:#$30B7#$3099),    // KATAKANA LETTER ZI
    (Unicode:#$30BA; Attr:daNone; Str:#$30B9#$3099),    // KATAKANA LETTER ZU
    (Unicode:#$30BC; Attr:daNone; Str:#$30BB#$3099),    // KATAKANA LETTER ZE
    (Unicode:#$30BE; Attr:daNone; Str:#$30BD#$3099),    // KATAKANA LETTER ZO
    (Unicode:#$30C0; Attr:daNone; Str:#$30BF#$3099),    // KATAKANA LETTER DA
    (Unicode:#$30C2; Attr:daNone; Str:#$30C1#$3099),    // KATAKANA LETTER DI
    (Unicode:#$30C5; Attr:daNone; Str:#$30C4#$3099),    // KATAKANA LETTER DU
    (Unicode:#$30C7; Attr:daNone; Str:#$30C6#$3099),    // KATAKANA LETTER DE
    (Unicode:#$30C9; Attr:daNone; Str:#$30C8#$3099),    // KATAKANA LETTER DO
    (Unicode:#$30D0; Attr:daNone; Str:#$30CF#$3099),    // KATAKANA LETTER BA
    (Unicode:#$30D1; Attr:daNone; Str:#$30CF#$309A),    // KATAKANA LETTER PA
    (Unicode:#$30D3; Attr:daNone; Str:#$30D2#$3099),    // KATAKANA LETTER BI
    (Unicode:#$30D4; Attr:daNone; Str:#$30D2#$309A),    // KATAKANA LETTER PI
    (Unicode:#$30D6; Attr:daNone; Str:#$30D5#$3099),    // KATAKANA LETTER BU
    (Unicode:#$30D7; Attr:daNone; Str:#$30D5#$309A),    // KATAKANA LETTER PU
    (Unicode:#$30D9; Attr:daNone; Str:#$30D8#$3099),    // KATAKANA LETTER BE
    (Unicode:#$30DA; Attr:daNone; Str:#$30D8#$309A),    // KATAKANA LETTER PE
    (Unicode:#$30DC; Attr:daNone; Str:#$30DB#$3099),    // KATAKANA LETTER BO
    (Unicode:#$30DD; Attr:daNone; Str:#$30DB#$309A),    // KATAKANA LETTER PO
    (Unicode:#$30F4; Attr:daNone; Str:#$30A6#$3099),    // KATAKANA LETTER VU
    (Unicode:#$30F7; Attr:daNone; Str:#$30EF#$3099),    // KATAKANA LETTER VA
    (Unicode:#$30F8; Attr:daNone; Str:#$30F0#$3099),    // KATAKANA LETTER VI
    (Unicode:#$30F9; Attr:daNone; Str:#$30F1#$3099),    // KATAKANA LETTER VE
    (Unicode:#$30FA; Attr:daNone; Str:#$30F2#$3099),    // KATAKANA LETTER VO
    (Unicode:#$30FE; Attr:daNone; Str:#$30FD#$3099),    // KATAKANA VOICED ITERATION MARK
    (Unicode:#$3131; Attr:daCompat; Str:#$1100),        // HANGUL LETTER KIYEOK
    (Unicode:#$3132; Attr:daCompat; Str:#$1101),        // HANGUL LETTER SSANGKIYEOK
    (Unicode:#$3133; Attr:daCompat; Str:#$11AA),        // HANGUL LETTER KIYEOK-SIOS
    (Unicode:#$3134; Attr:daCompat; Str:#$1102),        // HANGUL LETTER NIEUN
    (Unicode:#$3135; Attr:daCompat; Str:#$11AC),        // HANGUL LETTER NIEUN-CIEUC
    (Unicode:#$3136; Attr:daCompat; Str:#$11AD),        // HANGUL LETTER NIEUN-HIEUH
    (Unicode:#$3137; Attr:daCompat; Str:#$1103),        // HANGUL LETTER TIKEUT
    (Unicode:#$3138; Attr:daCompat; Str:#$1104),        // HANGUL LETTER SSANGTIKEUT
    (Unicode:#$3139; Attr:daCompat; Str:#$1105),        // HANGUL LETTER RIEUL
    (Unicode:#$313A; Attr:daCompat; Str:#$11B0),        // HANGUL LETTER RIEUL-KIYEOK
    (Unicode:#$313B; Attr:daCompat; Str:#$11B1),        // HANGUL LETTER RIEUL-MIEUM
    (Unicode:#$313C; Attr:daCompat; Str:#$11B2),        // HANGUL LETTER RIEUL-PIEUP
    (Unicode:#$313D; Attr:daCompat; Str:#$11B3),        // HANGUL LETTER RIEUL-SIOS
    (Unicode:#$313E; Attr:daCompat; Str:#$11B4),        // HANGUL LETTER RIEUL-THIEUTH
    (Unicode:#$313F; Attr:daCompat; Str:#$11B5),        // HANGUL LETTER RIEUL-PHIEUPH
    (Unicode:#$3140; Attr:daCompat; Str:#$111A),        // HANGUL LETTER RIEUL-HIEUH
    (Unicode:#$3141; Attr:daCompat; Str:#$1106),        // HANGUL LETTER MIEUM
    (Unicode:#$3142; Attr:daCompat; Str:#$1107),        // HANGUL LETTER PIEUP
    (Unicode:#$3143; Attr:daCompat; Str:#$1108),        // HANGUL LETTER SSANGPIEUP
    (Unicode:#$3144; Attr:daCompat; Str:#$1121),        // HANGUL LETTER PIEUP-SIOS
    (Unicode:#$3145; Attr:daCompat; Str:#$1109),        // HANGUL LETTER SIOS
    (Unicode:#$3146; Attr:daCompat; Str:#$110A),        // HANGUL LETTER SSANGSIOS
    (Unicode:#$3147; Attr:daCompat; Str:#$110B),        // HANGUL LETTER IEUNG
    (Unicode:#$3148; Attr:daCompat; Str:#$110C),        // HANGUL LETTER CIEUC
    (Unicode:#$3149; Attr:daCompat; Str:#$110D),        // HANGUL LETTER SSANGCIEUC
    (Unicode:#$314A; Attr:daCompat; Str:#$110E),        // HANGUL LETTER CHIEUCH
    (Unicode:#$314B; Attr:daCompat; Str:#$110F),        // HANGUL LETTER KHIEUKH
    (Unicode:#$314C; Attr:daCompat; Str:#$1110),        // HANGUL LETTER THIEUTH
    (Unicode:#$314D; Attr:daCompat; Str:#$1111),        // HANGUL LETTER PHIEUPH
    (Unicode:#$314E; Attr:daCompat; Str:#$1112),        // HANGUL LETTER HIEUH
    (Unicode:#$314F; Attr:daCompat; Str:#$1161),        // HANGUL LETTER A
    (Unicode:#$3150; Attr:daCompat; Str:#$1162),        // HANGUL LETTER AE
    (Unicode:#$3151; Attr:daCompat; Str:#$1163),        // HANGUL LETTER YA
    (Unicode:#$3152; Attr:daCompat; Str:#$1164),        // HANGUL LETTER YAE
    (Unicode:#$3153; Attr:daCompat; Str:#$1165),        // HANGUL LETTER EO
    (Unicode:#$3154; Attr:daCompat; Str:#$1166),        // HANGUL LETTER E
    (Unicode:#$3155; Attr:daCompat; Str:#$1167),        // HANGUL LETTER YEO
    (Unicode:#$3156; Attr:daCompat; Str:#$1168),        // HANGUL LETTER YE
    (Unicode:#$3157; Attr:daCompat; Str:#$1169),        // HANGUL LETTER O
    (Unicode:#$3158; Attr:daCompat; Str:#$116A),        // HANGUL LETTER WA
    (Unicode:#$3159; Attr:daCompat; Str:#$116B),        // HANGUL LETTER WAE
    (Unicode:#$315A; Attr:daCompat; Str:#$116C),        // HANGUL LETTER OE
    (Unicode:#$315B; Attr:daCompat; Str:#$116D),        // HANGUL LETTER YO
    (Unicode:#$315C; Attr:daCompat; Str:#$116E),        // HANGUL LETTER U
    (Unicode:#$315D; Attr:daCompat; Str:#$116F),        // HANGUL LETTER WEO
    (Unicode:#$315E; Attr:daCompat; Str:#$1170),        // HANGUL LETTER WE
    (Unicode:#$315F; Attr:daCompat; Str:#$1171),        // HANGUL LETTER WI
    (Unicode:#$3160; Attr:daCompat; Str:#$1172),        // HANGUL LETTER YU
    (Unicode:#$3161; Attr:daCompat; Str:#$1173),        // HANGUL LETTER EU
    (Unicode:#$3162; Attr:daCompat; Str:#$1174),        // HANGUL LETTER YI
    (Unicode:#$3163; Attr:daCompat; Str:#$1175),        // HANGUL LETTER I
    (Unicode:#$3164; Attr:daCompat; Str:#$1160),        // HANGUL FILLER
    (Unicode:#$3165; Attr:daCompat; Str:#$1114),        // HANGUL LETTER SSANGNIEUN
    (Unicode:#$3166; Attr:daCompat; Str:#$1115),        // HANGUL LETTER NIEUN-TIKEUT
    (Unicode:#$3167; Attr:daCompat; Str:#$11C7),        // HANGUL LETTER NIEUN-SIOS
    (Unicode:#$3168; Attr:daCompat; Str:#$11C8),        // HANGUL LETTER NIEUN-PANSIOS
    (Unicode:#$3169; Attr:daCompat; Str:#$11CC),        // HANGUL LETTER RIEUL-KIYEOK-SIOS
    (Unicode:#$316A; Attr:daCompat; Str:#$11CE),        // HANGUL LETTER RIEUL-TIKEUT
    (Unicode:#$316B; Attr:daCompat; Str:#$11D3),        // HANGUL LETTER RIEUL-PIEUP-SIOS
    (Unicode:#$316C; Attr:daCompat; Str:#$11D7),        // HANGUL LETTER RIEUL-PANSIOS
    (Unicode:#$316D; Attr:daCompat; Str:#$11D9),        // HANGUL LETTER RIEUL-YEORINHIEUH
    (Unicode:#$316E; Attr:daCompat; Str:#$111C),        // HANGUL LETTER MIEUM-PIEUP
    (Unicode:#$316F; Attr:daCompat; Str:#$11DD),        // HANGUL LETTER MIEUM-SIOS
    (Unicode:#$3170; Attr:daCompat; Str:#$11DF),        // HANGUL LETTER MIEUM-PANSIOS
    (Unicode:#$3171; Attr:daCompat; Str:#$111D),        // HANGUL LETTER KAPYEOUNMIEUM
    (Unicode:#$3172; Attr:daCompat; Str:#$111E),        // HANGUL LETTER PIEUP-KIYEOK
    (Unicode:#$3173; Attr:daCompat; Str:#$1120),        // HANGUL LETTER PIEUP-TIKEUT
    (Unicode:#$3174; Attr:daCompat; Str:#$1122),        // HANGUL LETTER PIEUP-SIOS-KIYEOK
    (Unicode:#$3175; Attr:daCompat; Str:#$1123),        // HANGUL LETTER PIEUP-SIOS-TIKEUT
    (Unicode:#$3176; Attr:daCompat; Str:#$1127),        // HANGUL LETTER PIEUP-CIEUC
    (Unicode:#$3177; Attr:daCompat; Str:#$1129),        // HANGUL LETTER PIEUP-THIEUTH
    (Unicode:#$3178; Attr:daCompat; Str:#$112B),        // HANGUL LETTER KAPYEOUNPIEUP
    (Unicode:#$3179; Attr:daCompat; Str:#$112C),        // HANGUL LETTER KAPYEOUNSSANGPIEUP
    (Unicode:#$317A; Attr:daCompat; Str:#$112D),        // HANGUL LETTER SIOS-KIYEOK
    (Unicode:#$317B; Attr:daCompat; Str:#$112E),        // HANGUL LETTER SIOS-NIEUN
    (Unicode:#$317C; Attr:daCompat; Str:#$112F),        // HANGUL LETTER SIOS-TIKEUT
    (Unicode:#$317D; Attr:daCompat; Str:#$1132),        // HANGUL LETTER SIOS-PIEUP
    (Unicode:#$317E; Attr:daCompat; Str:#$1136),        // HANGUL LETTER SIOS-CIEUC
    (Unicode:#$317F; Attr:daCompat; Str:#$1140),        // HANGUL LETTER PANSIOS
    (Unicode:#$3180; Attr:daCompat; Str:#$1147),        // HANGUL LETTER SSANGIEUNG
    (Unicode:#$3181; Attr:daCompat; Str:#$114C),        // HANGUL LETTER YESIEUNG
    (Unicode:#$3182; Attr:daCompat; Str:#$11F1),        // HANGUL LETTER YESIEUNG-SIOS
    (Unicode:#$3183; Attr:daCompat; Str:#$11F2),        // HANGUL LETTER YESIEUNG-PANSIOS
    (Unicode:#$3184; Attr:daCompat; Str:#$1157),        // HANGUL LETTER KAPYEOUNPHIEUPH
    (Unicode:#$3185; Attr:daCompat; Str:#$1158),        // HANGUL LETTER SSANGHIEUH
    (Unicode:#$3186; Attr:daCompat; Str:#$1159),        // HANGUL LETTER YEORINHIEUH
    (Unicode:#$3187; Attr:daCompat; Str:#$1184),        // HANGUL LETTER YO-YA
    (Unicode:#$3188; Attr:daCompat; Str:#$1185),        // HANGUL LETTER YO-YAE
    (Unicode:#$3189; Attr:daCompat; Str:#$1188),        // HANGUL LETTER YO-I
    (Unicode:#$318A; Attr:daCompat; Str:#$1191),        // HANGUL LETTER YU-YEO
    (Unicode:#$318B; Attr:daCompat; Str:#$1192),        // HANGUL LETTER YU-YE
    (Unicode:#$318C; Attr:daCompat; Str:#$1194),        // HANGUL LETTER YU-I
    (Unicode:#$318D; Attr:daCompat; Str:#$119E),        // HANGUL LETTER ARAEA
    (Unicode:#$318E; Attr:daCompat; Str:#$11A1),        // HANGUL LETTER ARAEAE
    (Unicode:#$3192; Attr:daSuper; Str:#$4E00),         // IDEOGRAPHIC ANNOTATION ONE MARK
    (Unicode:#$3193; Attr:daSuper; Str:#$4E8C),         // IDEOGRAPHIC ANNOTATION TWO MARK
    (Unicode:#$3194; Attr:daSuper; Str:#$4E09),         // IDEOGRAPHIC ANNOTATION THREE MARK
    (Unicode:#$3195; Attr:daSuper; Str:#$56DB),         // IDEOGRAPHIC ANNOTATION FOUR MARK
    (Unicode:#$3196; Attr:daSuper; Str:#$4E0A),         // IDEOGRAPHIC ANNOTATION TOP MARK
    (Unicode:#$3197; Attr:daSuper; Str:#$4E2D),         // IDEOGRAPHIC ANNOTATION MIDDLE MARK
    (Unicode:#$3198; Attr:daSuper; Str:#$4E0B),         // IDEOGRAPHIC ANNOTATION BOTTOM MARK
    (Unicode:#$3199; Attr:daSuper; Str:#$7532),         // IDEOGRAPHIC ANNOTATION FIRST MARK
    (Unicode:#$319A; Attr:daSuper; Str:#$4E59),         // IDEOGRAPHIC ANNOTATION SECOND MARK
    (Unicode:#$319B; Attr:daSuper; Str:#$4E19),         // IDEOGRAPHIC ANNOTATION THIRD MARK
    (Unicode:#$319C; Attr:daSuper; Str:#$4E01),         // IDEOGRAPHIC ANNOTATION FOURTH MARK
    (Unicode:#$319D; Attr:daSuper; Str:#$5929),         // IDEOGRAPHIC ANNOTATION HEAVEN MARK
    (Unicode:#$319E; Attr:daSuper; Str:#$5730),         // IDEOGRAPHIC ANNOTATION EARTH MARK
    (Unicode:#$319F; Attr:daSuper; Str:#$4EBA),         // IDEOGRAPHIC ANNOTATION MAN MARK
    (Unicode:#$3200; Attr:daCompat; Str:#$0028#$1100#$0029),   // PARENTHESIZED HANGUL KIYEOK
    (Unicode:#$3201; Attr:daCompat; Str:#$0028#$1102#$0029),   // PARENTHESIZED HANGUL NIEUN
    (Unicode:#$3202; Attr:daCompat; Str:#$0028#$1103#$0029),   // PARENTHESIZED HANGUL TIKEUT
    (Unicode:#$3203; Attr:daCompat; Str:#$0028#$1105#$0029),   // PARENTHESIZED HANGUL RIEUL
    (Unicode:#$3204; Attr:daCompat; Str:#$0028#$1106#$0029),   // PARENTHESIZED HANGUL MIEUM
    (Unicode:#$3205; Attr:daCompat; Str:#$0028#$1107#$0029),   // PARENTHESIZED HANGUL PIEUP
    (Unicode:#$3206; Attr:daCompat; Str:#$0028#$1109#$0029),   // PARENTHESIZED HANGUL SIOS
    (Unicode:#$3207; Attr:daCompat; Str:#$0028#$110B#$0029),   // PARENTHESIZED HANGUL IEUNG
    (Unicode:#$3208; Attr:daCompat; Str:#$0028#$110C#$0029),   // PARENTHESIZED HANGUL CIEUC
    (Unicode:#$3209; Attr:daCompat; Str:#$0028#$110E#$0029),   // PARENTHESIZED HANGUL CHIEUCH
    (Unicode:#$320A; Attr:daCompat; Str:#$0028#$110F#$0029),   // PARENTHESIZED HANGUL KHIEUKH
    (Unicode:#$320B; Attr:daCompat; Str:#$0028#$1110#$0029),   // PARENTHESIZED HANGUL THIEUTH
    (Unicode:#$320C; Attr:daCompat; Str:#$0028#$1111#$0029),   // PARENTHESIZED HANGUL PHIEUPH
    (Unicode:#$320D; Attr:daCompat; Str:#$0028#$1112#$0029),   // PARENTHESIZED HANGUL HIEUH
    (Unicode:#$320E; Attr:daCompat; Str:#$0028#$1100#$1161#$0029),  // PARENTHESIZED HANGUL KIYEOK A
    (Unicode:#$320F; Attr:daCompat; Str:#$0028#$1102#$1161#$0029),  // PARENTHESIZED HANGUL NIEUN A
    (Unicode:#$3210; Attr:daCompat; Str:#$0028#$1103#$1161#$0029),  // PARENTHESIZED HANGUL TIKEUT A
    (Unicode:#$3211; Attr:daCompat; Str:#$0028#$1105#$1161#$0029),  // PARENTHESIZED HANGUL RIEUL A
    (Unicode:#$3212; Attr:daCompat; Str:#$0028#$1106#$1161#$0029),  // PARENTHESIZED HANGUL MIEUM A
    (Unicode:#$3213; Attr:daCompat; Str:#$0028#$1107#$1161#$0029),  // PARENTHESIZED HANGUL PIEUP A
    (Unicode:#$3214; Attr:daCompat; Str:#$0028#$1109#$1161#$0029),  // PARENTHESIZED HANGUL SIOS A
    (Unicode:#$3215; Attr:daCompat; Str:#$0028#$110B#$1161#$0029),  // PARENTHESIZED HANGUL IEUNG A
    (Unicode:#$3216; Attr:daCompat; Str:#$0028#$110C#$1161#$0029),  // PARENTHESIZED HANGUL CIEUC A
    (Unicode:#$3217; Attr:daCompat; Str:#$0028#$110E#$1161#$0029),  // PARENTHESIZED HANGUL CHIEUCH A
    (Unicode:#$3218; Attr:daCompat; Str:#$0028#$110F#$1161#$0029),  // PARENTHESIZED HANGUL KHIEUKH A
    (Unicode:#$3219; Attr:daCompat; Str:#$0028#$1110#$1161#$0029),  // PARENTHESIZED HANGUL THIEUTH A
    (Unicode:#$321A; Attr:daCompat; Str:#$0028#$1111#$1161#$0029),  // PARENTHESIZED HANGUL PHIEUPH A
    (Unicode:#$321B; Attr:daCompat; Str:#$0028#$1112#$1161#$0029),  // PARENTHESIZED HANGUL HIEUH A
    (Unicode:#$321C; Attr:daCompat; Str:#$0028#$110C#$116E#$0029),  // PARENTHESIZED HANGUL CIEUC U
    (Unicode:#$3220; Attr:daCompat; Str:#$0028#$4E00#$0029),   // PARENTHESIZED IDEOGRAPH ONE
    (Unicode:#$3221; Attr:daCompat; Str:#$0028#$4E8C#$0029),   // PARENTHESIZED IDEOGRAPH TWO
    (Unicode:#$3222; Attr:daCompat; Str:#$0028#$4E09#$0029),   // PARENTHESIZED IDEOGRAPH THREE
    (Unicode:#$3223; Attr:daCompat; Str:#$0028#$56DB#$0029),   // PARENTHESIZED IDEOGRAPH FOUR
    (Unicode:#$3224; Attr:daCompat; Str:#$0028#$4E94#$0029),   // PARENTHESIZED IDEOGRAPH FIVE
    (Unicode:#$3225; Attr:daCompat; Str:#$0028#$516D#$0029),   // PARENTHESIZED IDEOGRAPH SIX
    (Unicode:#$3226; Attr:daCompat; Str:#$0028#$4E03#$0029),   // PARENTHESIZED IDEOGRAPH SEVEN
    (Unicode:#$3227; Attr:daCompat; Str:#$0028#$516B#$0029),   // PARENTHESIZED IDEOGRAPH EIGHT
    (Unicode:#$3228; Attr:daCompat; Str:#$0028#$4E5D#$0029),   // PARENTHESIZED IDEOGRAPH NINE
    (Unicode:#$3229; Attr:daCompat; Str:#$0028#$5341#$0029),   // PARENTHESIZED IDEOGRAPH TEN
    (Unicode:#$322A; Attr:daCompat; Str:#$0028#$6708#$0029),   // PARENTHESIZED IDEOGRAPH MOON
    (Unicode:#$322B; Attr:daCompat; Str:#$0028#$706B#$0029),   // PARENTHESIZED IDEOGRAPH FIRE
    (Unicode:#$322C; Attr:daCompat; Str:#$0028#$6C34#$0029),   // PARENTHESIZED IDEOGRAPH WATER
    (Unicode:#$322D; Attr:daCompat; Str:#$0028#$6728#$0029),   // PARENTHESIZED IDEOGRAPH WOOD
    (Unicode:#$322E; Attr:daCompat; Str:#$0028#$91D1#$0029),   // PARENTHESIZED IDEOGRAPH METAL
    (Unicode:#$322F; Attr:daCompat; Str:#$0028#$571F#$0029),   // PARENTHESIZED IDEOGRAPH EARTH
    (Unicode:#$3230; Attr:daCompat; Str:#$0028#$65E5#$0029),   // PARENTHESIZED IDEOGRAPH SUN
    (Unicode:#$3231; Attr:daCompat; Str:#$0028#$682A#$0029),   // PARENTHESIZED IDEOGRAPH STOCK
    (Unicode:#$3232; Attr:daCompat; Str:#$0028#$6709#$0029),   // PARENTHESIZED IDEOGRAPH HAVE
    (Unicode:#$3233; Attr:daCompat; Str:#$0028#$793E#$0029),   // PARENTHESIZED IDEOGRAPH SOCIETY
    (Unicode:#$3234; Attr:daCompat; Str:#$0028#$540D#$0029),   // PARENTHESIZED IDEOGRAPH NAME
    (Unicode:#$3235; Attr:daCompat; Str:#$0028#$7279#$0029),   // PARENTHESIZED IDEOGRAPH SPECIAL
    (Unicode:#$3236; Attr:daCompat; Str:#$0028#$8CA1#$0029),   // PARENTHESIZED IDEOGRAPH FINANCIAL
    (Unicode:#$3237; Attr:daCompat; Str:#$0028#$795D#$0029),   // PARENTHESIZED IDEOGRAPH CONGRATULATION
    (Unicode:#$3238; Attr:daCompat; Str:#$0028#$52B4#$0029),   // PARENTHESIZED IDEOGRAPH LABOR
    (Unicode:#$3239; Attr:daCompat; Str:#$0028#$4EE3#$0029),   // PARENTHESIZED IDEOGRAPH REPRESENT
    (Unicode:#$323A; Attr:daCompat; Str:#$0028#$547C#$0029),   // PARENTHESIZED IDEOGRAPH CALL
    (Unicode:#$323B; Attr:daCompat; Str:#$0028#$5B66#$0029),   // PARENTHESIZED IDEOGRAPH STUDY
    (Unicode:#$323C; Attr:daCompat; Str:#$0028#$76E3#$0029),   // PARENTHESIZED IDEOGRAPH SUPERVISE
    (Unicode:#$323D; Attr:daCompat; Str:#$0028#$4F01#$0029),   // PARENTHESIZED IDEOGRAPH ENTERPRISE
    (Unicode:#$323E; Attr:daCompat; Str:#$0028#$8CC7#$0029),   // PARENTHESIZED IDEOGRAPH RESOURCE
    (Unicode:#$323F; Attr:daCompat; Str:#$0028#$5354#$0029),   // PARENTHESIZED IDEOGRAPH ALLIANCE
    (Unicode:#$3240; Attr:daCompat; Str:#$0028#$796D#$0029),   // PARENTHESIZED IDEOGRAPH FESTIVAL
    (Unicode:#$3241; Attr:daCompat; Str:#$0028#$4F11#$0029),   // PARENTHESIZED IDEOGRAPH REST
    (Unicode:#$3242; Attr:daCompat; Str:#$0028#$81EA#$0029),   // PARENTHESIZED IDEOGRAPH SELF
    (Unicode:#$3243; Attr:daCompat; Str:#$0028#$81F3#$0029),   // PARENTHESIZED IDEOGRAPH REACH
    (Unicode:#$3260; Attr:daCircle; Str:#$1100),        // CIRCLED HANGUL KIYEOK
    (Unicode:#$3261; Attr:daCircle; Str:#$1102),        // CIRCLED HANGUL NIEUN
    (Unicode:#$3262; Attr:daCircle; Str:#$1103),        // CIRCLED HANGUL TIKEUT
    (Unicode:#$3263; Attr:daCircle; Str:#$1105),        // CIRCLED HANGUL RIEUL
    (Unicode:#$3264; Attr:daCircle; Str:#$1106),        // CIRCLED HANGUL MIEUM
    (Unicode:#$3265; Attr:daCircle; Str:#$1107),        // CIRCLED HANGUL PIEUP
    (Unicode:#$3266; Attr:daCircle; Str:#$1109),        // CIRCLED HANGUL SIOS
    (Unicode:#$3267; Attr:daCircle; Str:#$110B),        // CIRCLED HANGUL IEUNG
    (Unicode:#$3268; Attr:daCircle; Str:#$110C),        // CIRCLED HANGUL CIEUC
    (Unicode:#$3269; Attr:daCircle; Str:#$110E),        // CIRCLED HANGUL CHIEUCH
    (Unicode:#$326A; Attr:daCircle; Str:#$110F),        // CIRCLED HANGUL KHIEUKH
    (Unicode:#$326B; Attr:daCircle; Str:#$1110),        // CIRCLED HANGUL THIEUTH
    (Unicode:#$326C; Attr:daCircle; Str:#$1111),        // CIRCLED HANGUL PHIEUPH
    (Unicode:#$326D; Attr:daCircle; Str:#$1112),        // CIRCLED HANGUL HIEUH
    (Unicode:#$326E; Attr:daCircle; Str:#$1100#$1161),  // CIRCLED HANGUL KIYEOK A
    (Unicode:#$326F; Attr:daCircle; Str:#$1102#$1161),  // CIRCLED HANGUL NIEUN A
    (Unicode:#$3270; Attr:daCircle; Str:#$1103#$1161),  // CIRCLED HANGUL TIKEUT A
    (Unicode:#$3271; Attr:daCircle; Str:#$1105#$1161),  // CIRCLED HANGUL RIEUL A
    (Unicode:#$3272; Attr:daCircle; Str:#$1106#$1161),  // CIRCLED HANGUL MIEUM A
    (Unicode:#$3273; Attr:daCircle; Str:#$1107#$1161),  // CIRCLED HANGUL PIEUP A
    (Unicode:#$3274; Attr:daCircle; Str:#$1109#$1161),  // CIRCLED HANGUL SIOS A
    (Unicode:#$3275; Attr:daCircle; Str:#$110B#$1161),  // CIRCLED HANGUL IEUNG A
    (Unicode:#$3276; Attr:daCircle; Str:#$110C#$1161),  // CIRCLED HANGUL CIEUC A
    (Unicode:#$3277; Attr:daCircle; Str:#$110E#$1161),  // CIRCLED HANGUL CHIEUCH A
    (Unicode:#$3278; Attr:daCircle; Str:#$110F#$1161),  // CIRCLED HANGUL KHIEUKH A
    (Unicode:#$3279; Attr:daCircle; Str:#$1110#$1161),  // CIRCLED HANGUL THIEUTH A
    (Unicode:#$327A; Attr:daCircle; Str:#$1111#$1161),  // CIRCLED HANGUL PHIEUPH A
    (Unicode:#$327B; Attr:daCircle; Str:#$1112#$1161),  // CIRCLED HANGUL HIEUH A
    (Unicode:#$3280; Attr:daCircle; Str:#$4E00),        // CIRCLED IDEOGRAPH ONE
    (Unicode:#$3281; Attr:daCircle; Str:#$4E8C),        // CIRCLED IDEOGRAPH TWO
    (Unicode:#$3282; Attr:daCircle; Str:#$4E09),        // CIRCLED IDEOGRAPH THREE
    (Unicode:#$3283; Attr:daCircle; Str:#$56DB),        // CIRCLED IDEOGRAPH FOUR
    (Unicode:#$3284; Attr:daCircle; Str:#$4E94),        // CIRCLED IDEOGRAPH FIVE
    (Unicode:#$3285; Attr:daCircle; Str:#$516D),        // CIRCLED IDEOGRAPH SIX
    (Unicode:#$3286; Attr:daCircle; Str:#$4E03),        // CIRCLED IDEOGRAPH SEVEN
    (Unicode:#$3287; Attr:daCircle; Str:#$516B),        // CIRCLED IDEOGRAPH EIGHT
    (Unicode:#$3288; Attr:daCircle; Str:#$4E5D),        // CIRCLED IDEOGRAPH NINE
    (Unicode:#$3289; Attr:daCircle; Str:#$5341),        // CIRCLED IDEOGRAPH TEN
    (Unicode:#$328A; Attr:daCircle; Str:#$6708),        // CIRCLED IDEOGRAPH MOON
    (Unicode:#$328B; Attr:daCircle; Str:#$706B),        // CIRCLED IDEOGRAPH FIRE
    (Unicode:#$328C; Attr:daCircle; Str:#$6C34),        // CIRCLED IDEOGRAPH WATER
    (Unicode:#$328D; Attr:daCircle; Str:#$6728),        // CIRCLED IDEOGRAPH WOOD
    (Unicode:#$328E; Attr:daCircle; Str:#$91D1),        // CIRCLED IDEOGRAPH METAL
    (Unicode:#$328F; Attr:daCircle; Str:#$571F),        // CIRCLED IDEOGRAPH EARTH
    (Unicode:#$3290; Attr:daCircle; Str:#$65E5),        // CIRCLED IDEOGRAPH SUN
    (Unicode:#$3291; Attr:daCircle; Str:#$682A),        // CIRCLED IDEOGRAPH STOCK
    (Unicode:#$3292; Attr:daCircle; Str:#$6709),        // CIRCLED IDEOGRAPH HAVE
    (Unicode:#$3293; Attr:daCircle; Str:#$793E),        // CIRCLED IDEOGRAPH SOCIETY
    (Unicode:#$3294; Attr:daCircle; Str:#$540D),        // CIRCLED IDEOGRAPH NAME
    (Unicode:#$3295; Attr:daCircle; Str:#$7279),        // CIRCLED IDEOGRAPH SPECIAL
    (Unicode:#$3296; Attr:daCircle; Str:#$8CA1),        // CIRCLED IDEOGRAPH FINANCIAL
    (Unicode:#$3297; Attr:daCircle; Str:#$795D),        // CIRCLED IDEOGRAPH CONGRATULATION
    (Unicode:#$3298; Attr:daCircle; Str:#$52B4),        // CIRCLED IDEOGRAPH LABOR
    (Unicode:#$3299; Attr:daCircle; Str:#$79D8),        // CIRCLED IDEOGRAPH SECRET
    (Unicode:#$329A; Attr:daCircle; Str:#$7537),        // CIRCLED IDEOGRAPH MALE
    (Unicode:#$329B; Attr:daCircle; Str:#$5973),        // CIRCLED IDEOGRAPH FEMALE
    (Unicode:#$329C; Attr:daCircle; Str:#$9069),        // CIRCLED IDEOGRAPH SUITABLE
    (Unicode:#$329D; Attr:daCircle; Str:#$512A),        // CIRCLED IDEOGRAPH EXCELLENT
    (Unicode:#$329E; Attr:daCircle; Str:#$5370),        // CIRCLED IDEOGRAPH PRINT
    (Unicode:#$329F; Attr:daCircle; Str:#$6CE8),        // CIRCLED IDEOGRAPH ATTENTION
    (Unicode:#$32A0; Attr:daCircle; Str:#$9805),        // CIRCLED IDEOGRAPH ITEM
    (Unicode:#$32A1; Attr:daCircle; Str:#$4F11),        // CIRCLED IDEOGRAPH REST
    (Unicode:#$32A2; Attr:daCircle; Str:#$5199),        // CIRCLED IDEOGRAPH COPY
    (Unicode:#$32A3; Attr:daCircle; Str:#$6B63),        // CIRCLED IDEOGRAPH CORRECT
    (Unicode:#$32A4; Attr:daCircle; Str:#$4E0A),        // CIRCLED IDEOGRAPH HIGH
    (Unicode:#$32A5; Attr:daCircle; Str:#$4E2D),        // CIRCLED IDEOGRAPH CENTRE
    (Unicode:#$32A6; Attr:daCircle; Str:#$4E0B),        // CIRCLED IDEOGRAPH LOW
    (Unicode:#$32A7; Attr:daCircle; Str:#$5DE6),        // CIRCLED IDEOGRAPH LEFT
    (Unicode:#$32A8; Attr:daCircle; Str:#$53F3),        // CIRCLED IDEOGRAPH RIGHT
    (Unicode:#$32A9; Attr:daCircle; Str:#$533B),        // CIRCLED IDEOGRAPH MEDICINE
    (Unicode:#$32AA; Attr:daCircle; Str:#$5B97),        // CIRCLED IDEOGRAPH RELIGION
    (Unicode:#$32AB; Attr:daCircle; Str:#$5B66),        // CIRCLED IDEOGRAPH STUDY
    (Unicode:#$32AC; Attr:daCircle; Str:#$76E3),        // CIRCLED IDEOGRAPH SUPERVISE
    (Unicode:#$32AD; Attr:daCircle; Str:#$4F01),        // CIRCLED IDEOGRAPH ENTERPRISE
    (Unicode:#$32AE; Attr:daCircle; Str:#$8CC7),        // CIRCLED IDEOGRAPH RESOURCE
    (Unicode:#$32AF; Attr:daCircle; Str:#$5354),        // CIRCLED IDEOGRAPH ALLIANCE
    (Unicode:#$32B0; Attr:daCircle; Str:#$591C),        // CIRCLED IDEOGRAPH NIGHT
    (Unicode:#$32C0; Attr:daCompat; Str:#$0031#$6708),  // IDEOGRAPHIC TELEGRAPH SYMBOL FOR JANUARY
    (Unicode:#$32C1; Attr:daCompat; Str:#$0032#$6708),  // IDEOGRAPHIC TELEGRAPH SYMBOL FOR FEBRUARY
    (Unicode:#$32C2; Attr:daCompat; Str:#$0033#$6708),  // IDEOGRAPHIC TELEGRAPH SYMBOL FOR MARCH
    (Unicode:#$32C3; Attr:daCompat; Str:#$0034#$6708),  // IDEOGRAPHIC TELEGRAPH SYMBOL FOR APRIL
    (Unicode:#$32C4; Attr:daCompat; Str:#$0035#$6708),  // IDEOGRAPHIC TELEGRAPH SYMBOL FOR MAY
    (Unicode:#$32C5; Attr:daCompat; Str:#$0036#$6708),  // IDEOGRAPHIC TELEGRAPH SYMBOL FOR JUNE
    (Unicode:#$32C6; Attr:daCompat; Str:#$0037#$6708),  // IDEOGRAPHIC TELEGRAPH SYMBOL FOR JULY
    (Unicode:#$32C7; Attr:daCompat; Str:#$0038#$6708),  // IDEOGRAPHIC TELEGRAPH SYMBOL FOR AUGUST
    (Unicode:#$32C8; Attr:daCompat; Str:#$0039#$6708),  // IDEOGRAPHIC TELEGRAPH SYMBOL FOR SEPTEMBER
    (Unicode:#$32C9; Attr:daCompat; Str:#$0031#$0030#$6708),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR OCTOBER
    (Unicode:#$32CA; Attr:daCompat; Str:#$0031#$0031#$6708),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR NOVEMBER
    (Unicode:#$32CB; Attr:daCompat; Str:#$0031#$0032#$6708),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DECEMBER
    (Unicode:#$32D0; Attr:daCircle; Str:#$30A2),        // CIRCLED KATAKANA A
    (Unicode:#$32D1; Attr:daCircle; Str:#$30A4),        // CIRCLED KATAKANA I
    (Unicode:#$32D2; Attr:daCircle; Str:#$30A6),        // CIRCLED KATAKANA U
    (Unicode:#$32D3; Attr:daCircle; Str:#$30A8),        // CIRCLED KATAKANA E
    (Unicode:#$32D4; Attr:daCircle; Str:#$30AA),        // CIRCLED KATAKANA O
    (Unicode:#$32D5; Attr:daCircle; Str:#$30AB),        // CIRCLED KATAKANA KA
    (Unicode:#$32D6; Attr:daCircle; Str:#$30AD),        // CIRCLED KATAKANA KI
    (Unicode:#$32D7; Attr:daCircle; Str:#$30AF),        // CIRCLED KATAKANA KU
    (Unicode:#$32D8; Attr:daCircle; Str:#$30B1),        // CIRCLED KATAKANA KE
    (Unicode:#$32D9; Attr:daCircle; Str:#$30B3),        // CIRCLED KATAKANA KO
    (Unicode:#$32DA; Attr:daCircle; Str:#$30B5),        // CIRCLED KATAKANA SA
    (Unicode:#$32DB; Attr:daCircle; Str:#$30B7),        // CIRCLED KATAKANA SI
    (Unicode:#$32DC; Attr:daCircle; Str:#$30B9),        // CIRCLED KATAKANA SU
    (Unicode:#$32DD; Attr:daCircle; Str:#$30BB),        // CIRCLED KATAKANA SE
    (Unicode:#$32DE; Attr:daCircle; Str:#$30BD),        // CIRCLED KATAKANA SO
    (Unicode:#$32DF; Attr:daCircle; Str:#$30BF),        // CIRCLED KATAKANA TA
    (Unicode:#$32E0; Attr:daCircle; Str:#$30C1),        // CIRCLED KATAKANA TI
    (Unicode:#$32E1; Attr:daCircle; Str:#$30C4),        // CIRCLED KATAKANA TU
    (Unicode:#$32E2; Attr:daCircle; Str:#$30C6),        // CIRCLED KATAKANA TE
    (Unicode:#$32E3; Attr:daCircle; Str:#$30C8),        // CIRCLED KATAKANA TO
    (Unicode:#$32E4; Attr:daCircle; Str:#$30CA),        // CIRCLED KATAKANA NA
    (Unicode:#$32E5; Attr:daCircle; Str:#$30CB),        // CIRCLED KATAKANA NI
    (Unicode:#$32E6; Attr:daCircle; Str:#$30CC),        // CIRCLED KATAKANA NU
    (Unicode:#$32E7; Attr:daCircle; Str:#$30CD),        // CIRCLED KATAKANA NE
    (Unicode:#$32E8; Attr:daCircle; Str:#$30CE),        // CIRCLED KATAKANA NO
    (Unicode:#$32E9; Attr:daCircle; Str:#$30CF),        // CIRCLED KATAKANA HA
    (Unicode:#$32EA; Attr:daCircle; Str:#$30D2),        // CIRCLED KATAKANA HI
    (Unicode:#$32EB; Attr:daCircle; Str:#$30D5),        // CIRCLED KATAKANA HU
    (Unicode:#$32EC; Attr:daCircle; Str:#$30D8),        // CIRCLED KATAKANA HE
    (Unicode:#$32ED; Attr:daCircle; Str:#$30DB),        // CIRCLED KATAKANA HO
    (Unicode:#$32EE; Attr:daCircle; Str:#$30DE),        // CIRCLED KATAKANA MA
    (Unicode:#$32EF; Attr:daCircle; Str:#$30DF),        // CIRCLED KATAKANA MI
    (Unicode:#$32F0; Attr:daCircle; Str:#$30E0),        // CIRCLED KATAKANA MU
    (Unicode:#$32F1; Attr:daCircle; Str:#$30E1),        // CIRCLED KATAKANA ME
    (Unicode:#$32F2; Attr:daCircle; Str:#$30E2),        // CIRCLED KATAKANA MO
    (Unicode:#$32F3; Attr:daCircle; Str:#$30E4),        // CIRCLED KATAKANA YA
    (Unicode:#$32F4; Attr:daCircle; Str:#$30E6),        // CIRCLED KATAKANA YU
    (Unicode:#$32F5; Attr:daCircle; Str:#$30E8),        // CIRCLED KATAKANA YO
    (Unicode:#$32F6; Attr:daCircle; Str:#$30E9),        // CIRCLED KATAKANA RA
    (Unicode:#$32F7; Attr:daCircle; Str:#$30EA),        // CIRCLED KATAKANA RI
    (Unicode:#$32F8; Attr:daCircle; Str:#$30EB),        // CIRCLED KATAKANA RU
    (Unicode:#$32F9; Attr:daCircle; Str:#$30EC),        // CIRCLED KATAKANA RE
    (Unicode:#$32FA; Attr:daCircle; Str:#$30ED),        // CIRCLED KATAKANA RO
    (Unicode:#$32FB; Attr:daCircle; Str:#$30EF),        // CIRCLED KATAKANA WA
    (Unicode:#$32FC; Attr:daCircle; Str:#$30F0),        // CIRCLED KATAKANA WI
    (Unicode:#$32FD; Attr:daCircle; Str:#$30F1),        // CIRCLED KATAKANA WE
    (Unicode:#$32FE; Attr:daCircle; Str:#$30F2),        // CIRCLED KATAKANA WO
    (Unicode:#$3300; Attr:daSquare; Str:#$30A2#$30D1#$30FC#$30C8),  // SQUARE APAATO
    (Unicode:#$3301; Attr:daSquare; Str:#$30A2#$30EB#$30D5#$30A1),  // SQUARE ARUHUA
    (Unicode:#$3302; Attr:daSquare; Str:#$30A2#$30F3#$30DA#$30A2),  // SQUARE ANPEA
    (Unicode:#$3303; Attr:daSquare; Str:#$30A2#$30FC#$30EB),   // SQUARE AARU
    (Unicode:#$3304; Attr:daSquare; Str:#$30A4#$30CB#$30F3#$30B0),  // SQUARE ININGU
    (Unicode:#$3305; Attr:daSquare; Str:#$30A4#$30F3#$30C1),   // SQUARE INTI
    (Unicode:#$3306; Attr:daSquare; Str:#$30A6#$30A9#$30F3),   // SQUARE UON
    (Unicode:#$3307; Attr:daSquare; Str:#$30A8#$30B9#$30AF#$30FC#$30C9),   // SQUARE ESUKUUDO
    (Unicode:#$3308; Attr:daSquare; Str:#$30A8#$30FC#$30AB#$30FC),  // SQUARE EEKAA
    (Unicode:#$3309; Attr:daSquare; Str:#$30AA#$30F3#$30B9),   // SQUARE ONSU
    (Unicode:#$330A; Attr:daSquare; Str:#$30AA#$30FC#$30E0),   // SQUARE OOMU
    (Unicode:#$330B; Attr:daSquare; Str:#$30AB#$30A4#$30EA),   // SQUARE KAIRI
    (Unicode:#$330C; Attr:daSquare; Str:#$30AB#$30E9#$30C3#$30C8),  // SQUARE KARATTO
    (Unicode:#$330D; Attr:daSquare; Str:#$30AB#$30ED#$30EA#$30FC),  // SQUARE KARORII
    (Unicode:#$330E; Attr:daSquare; Str:#$30AC#$30ED#$30F3),   // SQUARE GARON
    (Unicode:#$330F; Attr:daSquare; Str:#$30AC#$30F3#$30DE),   // SQUARE GANMA
    (Unicode:#$3310; Attr:daSquare; Str:#$30AE#$30AC),  // SQUARE GIGA
    (Unicode:#$3311; Attr:daSquare; Str:#$30AE#$30CB#$30FC),   // SQUARE GINII
    (Unicode:#$3312; Attr:daSquare; Str:#$30AD#$30E5#$30EA#$30FC),  // SQUARE KYURII
    (Unicode:#$3313; Attr:daSquare; Str:#$30AE#$30EB#$30C0#$30FC),  // SQUARE GIRUDAA
    (Unicode:#$3314; Attr:daSquare; Str:#$30AD#$30ED),  // SQUARE KIRO
    (Unicode:#$3315; Attr:daSquare; Str:#$30AD#$30ED#$30B0#$30E9#$30E0),   // SQUARE KIROGURAMU
    (Unicode:#$3316; Attr:daSquare; Str:#$30AD#$30ED#$30E1#$30FC#$30C8#$30EB),// SQUARE KIROMEETORU
    (Unicode:#$3317; Attr:daSquare; Str:#$30AD#$30ED#$30EF#$30C3#$30C8),   // SQUARE KIROWATTO
    (Unicode:#$3318; Attr:daSquare; Str:#$30B0#$30E9#$30E0),   // SQUARE GURAMU
    (Unicode:#$3319; Attr:daSquare; Str:#$30B0#$30E9#$30E0#$30C8#$30F3),   // SQUARE GURAMUTON
    (Unicode:#$331A; Attr:daSquare; Str:#$30AF#$30EB#$30BC#$30A4#$30ED),   // SQUARE KURUZEIRO
    (Unicode:#$331B; Attr:daSquare; Str:#$30AF#$30ED#$30FC#$30CD),  // SQUARE KUROONE
    (Unicode:#$331C; Attr:daSquare; Str:#$30B1#$30FC#$30B9),   // SQUARE KEESU
    (Unicode:#$331D; Attr:daSquare; Str:#$30B3#$30EB#$30CA),   // SQUARE KORUNA
    (Unicode:#$331E; Attr:daSquare; Str:#$30B3#$30FC#$30DD),   // SQUARE KOOPO
    (Unicode:#$331F; Attr:daSquare; Str:#$30B5#$30A4#$30AF#$30EB),  // SQUARE SAIKURU
    (Unicode:#$3320; Attr:daSquare; Str:#$30B5#$30F3#$30C1#$30FC#$30E0),   // SQUARE SANTIIMU
    (Unicode:#$3321; Attr:daSquare; Str:#$30B7#$30EA#$30F3#$30B0),  // SQUARE SIRINGU
    (Unicode:#$3322; Attr:daSquare; Str:#$30BB#$30F3#$30C1),   // SQUARE SENTI
    (Unicode:#$3323; Attr:daSquare; Str:#$30BB#$30F3#$30C8),   // SQUARE SENTO
    (Unicode:#$3324; Attr:daSquare; Str:#$30C0#$30FC#$30B9),   // SQUARE DAASU
    (Unicode:#$3325; Attr:daSquare; Str:#$30C7#$30B7),  // SQUARE DESI
    (Unicode:#$3326; Attr:daSquare; Str:#$30C9#$30EB),  // SQUARE DORU
    (Unicode:#$3327; Attr:daSquare; Str:#$30C8#$30F3),  // SQUARE TON
    (Unicode:#$3328; Attr:daSquare; Str:#$30CA#$30CE),  // SQUARE NANO
    (Unicode:#$3329; Attr:daSquare; Str:#$30CE#$30C3#$30C8),   // SQUARE NOTTO
    (Unicode:#$332A; Attr:daSquare; Str:#$30CF#$30A4#$30C4),   // SQUARE HAITU
    (Unicode:#$332B; Attr:daSquare; Str:#$30D1#$30FC#$30BB#$30F3#$30C8),   // SQUARE PAASENTO
    (Unicode:#$332C; Attr:daSquare; Str:#$30D1#$30FC#$30C4),   // SQUARE PAATU
    (Unicode:#$332D; Attr:daSquare; Str:#$30D0#$30FC#$30EC#$30EB),  // SQUARE BAARERU
    (Unicode:#$332E; Attr:daSquare; Str:#$30D4#$30A2#$30B9#$30C8#$30EB),   // SQUARE PIASUTORU
    (Unicode:#$332F; Attr:daSquare; Str:#$30D4#$30AF#$30EB),   // SQUARE PIKURU
    (Unicode:#$3330; Attr:daSquare; Str:#$30D4#$30B3),  // SQUARE PIKO
    (Unicode:#$3331; Attr:daSquare; Str:#$30D3#$30EB),  // SQUARE BIRU
    (Unicode:#$3332; Attr:daSquare; Str:#$30D5#$30A1#$30E9#$30C3#$30C9),   // SQUARE HUARADDO
    (Unicode:#$3333; Attr:daSquare; Str:#$30D5#$30A3#$30FC#$30C8),  // SQUARE HUIITO
    (Unicode:#$3334; Attr:daSquare; Str:#$30D6#$30C3#$30B7#$30A7#$30EB),   // SQUARE BUSSYERU
    (Unicode:#$3335; Attr:daSquare; Str:#$30D5#$30E9#$30F3),   // SQUARE HURAN
    (Unicode:#$3336; Attr:daSquare; Str:#$30D8#$30AF#$30BF#$30FC#$30EB),   // SQUARE HEKUTAARU
    (Unicode:#$3337; Attr:daSquare; Str:#$30DA#$30BD),  // SQUARE PESO
    (Unicode:#$3338; Attr:daSquare; Str:#$30DA#$30CB#$30D2),   // SQUARE PENIHI
    (Unicode:#$3339; Attr:daSquare; Str:#$30D8#$30EB#$30C4),   // SQUARE HERUTU
    (Unicode:#$333A; Attr:daSquare; Str:#$30DA#$30F3#$30B9),   // SQUARE PENSU
    (Unicode:#$333B; Attr:daSquare; Str:#$30DA#$30FC#$30B8),   // SQUARE PEEZI
    (Unicode:#$333C; Attr:daSquare; Str:#$30D9#$30FC#$30BF),   // SQUARE BEETA
    (Unicode:#$333D; Attr:daSquare; Str:#$30DD#$30A4#$30F3#$30C8),  // SQUARE POINTO
    (Unicode:#$333E; Attr:daSquare; Str:#$30DC#$30EB#$30C8),   // SQUARE BORUTO
    (Unicode:#$333F; Attr:daSquare; Str:#$30DB#$30F3),  // SQUARE HON
    (Unicode:#$3340; Attr:daSquare; Str:#$30DD#$30F3#$30C9),   // SQUARE PONDO
    (Unicode:#$3341; Attr:daSquare; Str:#$30DB#$30FC#$30EB),   // SQUARE HOORU
    (Unicode:#$3342; Attr:daSquare; Str:#$30DB#$30FC#$30F3),   // SQUARE HOON
    (Unicode:#$3343; Attr:daSquare; Str:#$30DE#$30A4#$30AF#$30ED),  // SQUARE MAIKURO
    (Unicode:#$3344; Attr:daSquare; Str:#$30DE#$30A4#$30EB),   // SQUARE MAIRU
    (Unicode:#$3345; Attr:daSquare; Str:#$30DE#$30C3#$30CF),   // SQUARE MAHHA
    (Unicode:#$3346; Attr:daSquare; Str:#$30DE#$30EB#$30AF),   // SQUARE MARUKU
    (Unicode:#$3347; Attr:daSquare; Str:#$30DE#$30F3#$30B7#$30E7#$30F3),   // SQUARE MANSYON
    (Unicode:#$3348; Attr:daSquare; Str:#$30DF#$30AF#$30ED#$30F3),  // SQUARE MIKURON
    (Unicode:#$3349; Attr:daSquare; Str:#$30DF#$30EA),  // SQUARE MIRI
    (Unicode:#$334A; Attr:daSquare; Str:#$30DF#$30EA#$30D0#$30FC#$30EB),   // SQUARE MIRIBAARU
    (Unicode:#$334B; Attr:daSquare; Str:#$30E1#$30AC),  // SQUARE MEGA
    (Unicode:#$334C; Attr:daSquare; Str:#$30E1#$30AC#$30C8#$30F3),  // SQUARE MEGATON
    (Unicode:#$334D; Attr:daSquare; Str:#$30E1#$30FC#$30C8#$30EB),  // SQUARE MEETORU
    (Unicode:#$334E; Attr:daSquare; Str:#$30E4#$30FC#$30C9),   // SQUARE YAADO
    (Unicode:#$334F; Attr:daSquare; Str:#$30E4#$30FC#$30EB),   // SQUARE YAARU
    (Unicode:#$3350; Attr:daSquare; Str:#$30E6#$30A2#$30F3),   // SQUARE YUAN
    (Unicode:#$3351; Attr:daSquare; Str:#$30EA#$30C3#$30C8#$30EB),  // SQUARE RITTORU
    (Unicode:#$3352; Attr:daSquare; Str:#$30EA#$30E9),  // SQUARE RIRA
    (Unicode:#$3353; Attr:daSquare; Str:#$30EB#$30D4#$30FC),   // SQUARE RUPII
    (Unicode:#$3354; Attr:daSquare; Str:#$30EB#$30FC#$30D6#$30EB),  // SQUARE RUUBURU
    (Unicode:#$3355; Attr:daSquare; Str:#$30EC#$30E0),  // SQUARE REMU
    (Unicode:#$3356; Attr:daSquare; Str:#$30EC#$30F3#$30C8#$30B2#$30F3),   // SQUARE RENTOGEN
    (Unicode:#$3357; Attr:daSquare; Str:#$30EF#$30C3#$30C8),   // SQUARE WATTO
    (Unicode:#$3358; Attr:daCompat; Str:#$0030#$70B9),  // IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR ZERO
    (Unicode:#$3359; Attr:daCompat; Str:#$0031#$70B9),  // IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR ONE
    (Unicode:#$335A; Attr:daCompat; Str:#$0032#$70B9),  // IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR TWO
    (Unicode:#$335B; Attr:daCompat; Str:#$0033#$70B9),  // IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR THREE
    (Unicode:#$335C; Attr:daCompat; Str:#$0034#$70B9),  // IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR FOUR
    (Unicode:#$335D; Attr:daCompat; Str:#$0035#$70B9),  // IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR FIVE
    (Unicode:#$335E; Attr:daCompat; Str:#$0036#$70B9),  // IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR SIX
    (Unicode:#$335F; Attr:daCompat; Str:#$0037#$70B9),  // IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR SEVEN
    (Unicode:#$3360; Attr:daCompat; Str:#$0038#$70B9),  // IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR EIGHT
    (Unicode:#$3361; Attr:daCompat; Str:#$0039#$70B9),  // IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR NINE
    (Unicode:#$3362; Attr:daCompat; Str:#$0031#$0030#$70B9),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR TEN
    (Unicode:#$3363; Attr:daCompat; Str:#$0031#$0031#$70B9),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR ELEVEN
    (Unicode:#$3364; Attr:daCompat; Str:#$0031#$0032#$70B9),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR TWELVE
    (Unicode:#$3365; Attr:daCompat; Str:#$0031#$0033#$70B9),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR THIRTEEN
    (Unicode:#$3366; Attr:daCompat; Str:#$0031#$0034#$70B9),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR FOURTEEN
    (Unicode:#$3367; Attr:daCompat; Str:#$0031#$0035#$70B9),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR FIFTEEN
    (Unicode:#$3368; Attr:daCompat; Str:#$0031#$0036#$70B9),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR SIXTEEN
    (Unicode:#$3369; Attr:daCompat; Str:#$0031#$0037#$70B9),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR SEVENTEEN
    (Unicode:#$336A; Attr:daCompat; Str:#$0031#$0038#$70B9),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR EIGHTEEN
    (Unicode:#$336B; Attr:daCompat; Str:#$0031#$0039#$70B9),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR NINETEEN
    (Unicode:#$336C; Attr:daCompat; Str:#$0032#$0030#$70B9),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR TWENTY
    (Unicode:#$336D; Attr:daCompat; Str:#$0032#$0031#$70B9),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR TWENTY-ONE
    (Unicode:#$336E; Attr:daCompat; Str:#$0032#$0032#$70B9),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR TWENTY-TWO
    (Unicode:#$336F; Attr:daCompat; Str:#$0032#$0033#$70B9),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR TWENTY-THREE
    (Unicode:#$3370; Attr:daCompat; Str:#$0032#$0034#$70B9),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR TWENTY-FOUR
    (Unicode:#$3371; Attr:daSquare; Str:#$0068#$0050#$0061),   // SQUARE HPA
    (Unicode:#$3372; Attr:daSquare; Str:#$0064#$0061),  // SQUARE DA
    (Unicode:#$3373; Attr:daSquare; Str:#$0041#$0055),  // SQUARE AU
    (Unicode:#$3374; Attr:daSquare; Str:#$0062#$0061#$0072),   // SQUARE BAR
    (Unicode:#$3375; Attr:daSquare; Str:#$006F#$0056),  // SQUARE OV
    (Unicode:#$3376; Attr:daSquare; Str:#$0070#$0063),  // SQUARE PC
    (Unicode:#$337B; Attr:daSquare; Str:#$5E73#$6210),  // SQUARE ERA NAME HEISEI
    (Unicode:#$337C; Attr:daSquare; Str:#$662D#$548C),  // SQUARE ERA NAME SYOUWA
    (Unicode:#$337D; Attr:daSquare; Str:#$5927#$6B63),  // SQUARE ERA NAME TAISYOU
    (Unicode:#$337E; Attr:daSquare; Str:#$660E#$6CBB),  // SQUARE ERA NAME MEIZI
    (Unicode:#$337F; Attr:daSquare; Str:#$682A#$5F0F#$4F1A#$793E),  // SQUARE CORPORATION
    (Unicode:#$3380; Attr:daSquare; Str:#$0070#$0041),  // SQUARE PA AMPS
    (Unicode:#$3381; Attr:daSquare; Str:#$006E#$0041),  // SQUARE NA
    (Unicode:#$3382; Attr:daSquare; Str:#$03BC#$0041),  // SQUARE MU A
    (Unicode:#$3383; Attr:daSquare; Str:#$006D#$0041),  // SQUARE MA
    (Unicode:#$3384; Attr:daSquare; Str:#$006B#$0041),  // SQUARE KA
    (Unicode:#$3385; Attr:daSquare; Str:#$004B#$0042),  // SQUARE KB
    (Unicode:#$3386; Attr:daSquare; Str:#$004D#$0042),  // SQUARE MB
    (Unicode:#$3387; Attr:daSquare; Str:#$0047#$0042),  // SQUARE GB
    (Unicode:#$3388; Attr:daSquare; Str:#$0063#$0061#$006C),   // SQUARE CAL
    (Unicode:#$3389; Attr:daSquare; Str:#$006B#$0063#$0061#$006C),  // SQUARE KCAL
    (Unicode:#$338A; Attr:daSquare; Str:#$0070#$0046),  // SQUARE PF
    (Unicode:#$338B; Attr:daSquare; Str:#$006E#$0046),  // SQUARE NF
    (Unicode:#$338C; Attr:daSquare; Str:#$03BC#$0046),  // SQUARE MU F
    (Unicode:#$338D; Attr:daSquare; Str:#$03BC#$0067),  // SQUARE MU G
    (Unicode:#$338E; Attr:daSquare; Str:#$006D#$0067),  // SQUARE MG
    (Unicode:#$338F; Attr:daSquare; Str:#$006B#$0067),  // SQUARE KG
    (Unicode:#$3390; Attr:daSquare; Str:#$0048#$007A),  // SQUARE HZ
    (Unicode:#$3391; Attr:daSquare; Str:#$006B#$0048#$007A),   // SQUARE KHZ
    (Unicode:#$3392; Attr:daSquare; Str:#$004D#$0048#$007A),   // SQUARE MHZ
    (Unicode:#$3393; Attr:daSquare; Str:#$0047#$0048#$007A),   // SQUARE GHZ
    (Unicode:#$3394; Attr:daSquare; Str:#$0054#$0048#$007A),   // SQUARE THZ
    (Unicode:#$3395; Attr:daSquare; Str:#$03BC#$2113),  // SQUARE MU L
    (Unicode:#$3396; Attr:daSquare; Str:#$006D#$2113),  // SQUARE ML
    (Unicode:#$3397; Attr:daSquare; Str:#$0064#$2113),  // SQUARE DL
    (Unicode:#$3398; Attr:daSquare; Str:#$006B#$2113),  // SQUARE KL
    (Unicode:#$3399; Attr:daSquare; Str:#$0066#$006D),  // SQUARE FM
    (Unicode:#$339A; Attr:daSquare; Str:#$006E#$006D),  // SQUARE NM
    (Unicode:#$339B; Attr:daSquare; Str:#$03BC#$006D),  // SQUARE MU M
    (Unicode:#$339C; Attr:daSquare; Str:#$006D#$006D),  // SQUARE MM
    (Unicode:#$339D; Attr:daSquare; Str:#$0063#$006D),  // SQUARE CM
    (Unicode:#$339E; Attr:daSquare; Str:#$006B#$006D),  // SQUARE KM
    (Unicode:#$339F; Attr:daSquare; Str:#$006D#$006D#$00B2),   // SQUARE MM SQUARED
    (Unicode:#$33A0; Attr:daSquare; Str:#$0063#$006D#$00B2),   // SQUARE CM SQUARED
    (Unicode:#$33A1; Attr:daSquare; Str:#$006D#$00B2),  // SQUARE M SQUARED
    (Unicode:#$33A2; Attr:daSquare; Str:#$006B#$006D#$00B2),   // SQUARE KM SQUARED
    (Unicode:#$33A3; Attr:daSquare; Str:#$006D#$006D#$00B3),   // SQUARE MM CUBED
    (Unicode:#$33A4; Attr:daSquare; Str:#$0063#$006D#$00B3),   // SQUARE CM CUBED
    (Unicode:#$33A5; Attr:daSquare; Str:#$006D#$00B3),  // SQUARE M CUBED
    (Unicode:#$33A6; Attr:daSquare; Str:#$006B#$006D#$00B3),   // SQUARE KM CUBED
    (Unicode:#$33A7; Attr:daSquare; Str:#$006D#$2215#$0073),   // SQUARE M OVER S
    (Unicode:#$33A8; Attr:daSquare; Str:#$006D#$2215#$0073#$00B2),  // SQUARE M OVER S SQUARED
    (Unicode:#$33A9; Attr:daSquare; Str:#$0050#$0061),  // SQUARE PA
    (Unicode:#$33AA; Attr:daSquare; Str:#$006B#$0050#$0061),   // SQUARE KPA
    (Unicode:#$33AB; Attr:daSquare; Str:#$004D#$0050#$0061),   // SQUARE MPA
    (Unicode:#$33AC; Attr:daSquare; Str:#$0047#$0050#$0061),   // SQUARE GPA
    (Unicode:#$33AD; Attr:daSquare; Str:#$0072#$0061#$0064),   // SQUARE RAD
    (Unicode:#$33AE; Attr:daSquare; Str:#$0072#$0061#$0064#$2215#$0073),   // SQUARE RAD OVER S
    (Unicode:#$33AF; Attr:daSquare; Str:#$0072#$0061#$0064#$2215#$0073#$00B2),// SQUARE RAD OVER S SQUARED
    (Unicode:#$33B0; Attr:daSquare; Str:#$0070#$0073),  // SQUARE PS
    (Unicode:#$33B1; Attr:daSquare; Str:#$006E#$0073),  // SQUARE NS
    (Unicode:#$33B2; Attr:daSquare; Str:#$03BC#$0073),  // SQUARE MU S
    (Unicode:#$33B3; Attr:daSquare; Str:#$006D#$0073),  // SQUARE MS
    (Unicode:#$33B4; Attr:daSquare; Str:#$0070#$0056),  // SQUARE PV
    (Unicode:#$33B5; Attr:daSquare; Str:#$006E#$0056),  // SQUARE NV
    (Unicode:#$33B6; Attr:daSquare; Str:#$03BC#$0056),  // SQUARE MU V
    (Unicode:#$33B7; Attr:daSquare; Str:#$006D#$0056),  // SQUARE MV
    (Unicode:#$33B8; Attr:daSquare; Str:#$006B#$0056),  // SQUARE KV
    (Unicode:#$33B9; Attr:daSquare; Str:#$004D#$0056),  // SQUARE MV MEGA
    (Unicode:#$33BA; Attr:daSquare; Str:#$0070#$0057),  // SQUARE PW
    (Unicode:#$33BB; Attr:daSquare; Str:#$006E#$0057),  // SQUARE NW
    (Unicode:#$33BC; Attr:daSquare; Str:#$03BC#$0057),  // SQUARE MU W
    (Unicode:#$33BD; Attr:daSquare; Str:#$006D#$0057),  // SQUARE MW
    (Unicode:#$33BE; Attr:daSquare; Str:#$006B#$0057),  // SQUARE KW
    (Unicode:#$33BF; Attr:daSquare; Str:#$004D#$0057),  // SQUARE MW MEGA
    (Unicode:#$33C0; Attr:daSquare; Str:#$006B#$03A9),  // SQUARE K OHM
    (Unicode:#$33C1; Attr:daSquare; Str:#$004D#$03A9),  // SQUARE M OHM
    (Unicode:#$33C2; Attr:daSquare; Str:#$0061#$002E#$006D#$002E),  // SQUARE AM
    (Unicode:#$33C3; Attr:daSquare; Str:#$0042#$0071),  // SQUARE BQ
    (Unicode:#$33C4; Attr:daSquare; Str:#$0063#$0063),  // SQUARE CC
    (Unicode:#$33C5; Attr:daSquare; Str:#$0063#$0064),  // SQUARE CD
    (Unicode:#$33C6; Attr:daSquare; Str:#$0043#$2215#$006B#$0067),  // SQUARE C OVER KG
    (Unicode:#$33C7; Attr:daSquare; Str:#$0043#$006F#$002E),   // SQUARE CO
    (Unicode:#$33C8; Attr:daSquare; Str:#$0064#$0042),  // SQUARE DB
    (Unicode:#$33C9; Attr:daSquare; Str:#$0047#$0079),  // SQUARE GY
    (Unicode:#$33CA; Attr:daSquare; Str:#$0068#$0061),  // SQUARE HA
    (Unicode:#$33CB; Attr:daSquare; Str:#$0048#$0050),  // SQUARE HP
    (Unicode:#$33CC; Attr:daSquare; Str:#$0069#$006E),  // SQUARE IN
    (Unicode:#$33CD; Attr:daSquare; Str:#$004B#$004B),  // SQUARE KK
    (Unicode:#$33CE; Attr:daSquare; Str:#$004B#$004D),  // SQUARE KM CAPITAL
    (Unicode:#$33CF; Attr:daSquare; Str:#$006B#$0074),  // SQUARE KT
    (Unicode:#$33D0; Attr:daSquare; Str:#$006C#$006D),  // SQUARE LM
    (Unicode:#$33D1; Attr:daSquare; Str:#$006C#$006E),  // SQUARE LN
    (Unicode:#$33D2; Attr:daSquare; Str:#$006C#$006F#$0067),   // SQUARE LOG
    (Unicode:#$33D3; Attr:daSquare; Str:#$006C#$0078),  // SQUARE LX
    (Unicode:#$33D4; Attr:daSquare; Str:#$006D#$0062),  // SQUARE MB SMALL
    (Unicode:#$33D5; Attr:daSquare; Str:#$006D#$0069#$006C),   // SQUARE MIL
    (Unicode:#$33D6; Attr:daSquare; Str:#$006D#$006F#$006C),   // SQUARE MOL
    (Unicode:#$33D7; Attr:daSquare; Str:#$0050#$0048),  // SQUARE PH
    (Unicode:#$33D8; Attr:daSquare; Str:#$0070#$002E#$006D#$002E),  // SQUARE PM
    (Unicode:#$33D9; Attr:daSquare; Str:#$0050#$0050#$004D),   // SQUARE PPM
    (Unicode:#$33DA; Attr:daSquare; Str:#$0050#$0052),  // SQUARE PR
    (Unicode:#$33DB; Attr:daSquare; Str:#$0073#$0072),  // SQUARE SR
    (Unicode:#$33DC; Attr:daSquare; Str:#$0053#$0076),  // SQUARE SV
    (Unicode:#$33DD; Attr:daSquare; Str:#$0057#$0062),  // SQUARE WB
    (Unicode:#$33E0; Attr:daCompat; Str:#$0031#$65E5),  // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY ONE
    (Unicode:#$33E1; Attr:daCompat; Str:#$0032#$65E5),  // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY TWO
    (Unicode:#$33E2; Attr:daCompat; Str:#$0033#$65E5),  // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY THREE
    (Unicode:#$33E3; Attr:daCompat; Str:#$0034#$65E5),  // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY FOUR
    (Unicode:#$33E4; Attr:daCompat; Str:#$0035#$65E5),  // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY FIVE
    (Unicode:#$33E5; Attr:daCompat; Str:#$0036#$65E5),  // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY SIX
    (Unicode:#$33E6; Attr:daCompat; Str:#$0037#$65E5),  // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY SEVEN
    (Unicode:#$33E7; Attr:daCompat; Str:#$0038#$65E5),  // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY EIGHT
    (Unicode:#$33E8; Attr:daCompat; Str:#$0039#$65E5),  // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY NINE
    (Unicode:#$33E9; Attr:daCompat; Str:#$0031#$0030#$65E5),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY TEN
    (Unicode:#$33EA; Attr:daCompat; Str:#$0031#$0031#$65E5),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY ELEVEN
    (Unicode:#$33EB; Attr:daCompat; Str:#$0031#$0032#$65E5),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY TWELVE
    (Unicode:#$33EC; Attr:daCompat; Str:#$0031#$0033#$65E5),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY THIRTEEN
    (Unicode:#$33ED; Attr:daCompat; Str:#$0031#$0034#$65E5),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY FOURTEEN
    (Unicode:#$33EE; Attr:daCompat; Str:#$0031#$0035#$65E5),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY FIFTEEN
    (Unicode:#$33EF; Attr:daCompat; Str:#$0031#$0036#$65E5),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY SIXTEEN
    (Unicode:#$33F0; Attr:daCompat; Str:#$0031#$0037#$65E5),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY SEVENTEEN
    (Unicode:#$33F1; Attr:daCompat; Str:#$0031#$0038#$65E5),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY EIGHTEEN
    (Unicode:#$33F2; Attr:daCompat; Str:#$0031#$0039#$65E5),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY NINETEEN
    (Unicode:#$33F3; Attr:daCompat; Str:#$0032#$0030#$65E5),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY TWENTY
    (Unicode:#$33F4; Attr:daCompat; Str:#$0032#$0031#$65E5),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY TWENTY-ONE
    (Unicode:#$33F5; Attr:daCompat; Str:#$0032#$0032#$65E5),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY TWENTY-TWO
    (Unicode:#$33F6; Attr:daCompat; Str:#$0032#$0033#$65E5),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY TWENTY-THREE
    (Unicode:#$33F7; Attr:daCompat; Str:#$0032#$0034#$65E5),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY TWENTY-FOUR
    (Unicode:#$33F8; Attr:daCompat; Str:#$0032#$0035#$65E5),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY TWENTY-FIVE
    (Unicode:#$33F9; Attr:daCompat; Str:#$0032#$0036#$65E5),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY TWENTY-SIX
    (Unicode:#$33FA; Attr:daCompat; Str:#$0032#$0037#$65E5),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY TWENTY-SEVEN
    (Unicode:#$33FB; Attr:daCompat; Str:#$0032#$0038#$65E5),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY TWENTY-EIGHT
    (Unicode:#$33FC; Attr:daCompat; Str:#$0032#$0039#$65E5),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY TWENTY-NINE
    (Unicode:#$33FD; Attr:daCompat; Str:#$0033#$0030#$65E5),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY THIRTY
    (Unicode:#$33FE; Attr:daCompat; Str:#$0033#$0031#$65E5),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY THIRTY-ONE
    (Unicode:#$F900; Attr:daNone; Str:#$8C48),          // CJK COMPATIBILITY IDEOGRAPH-F900
    (Unicode:#$F901; Attr:daNone; Str:#$66F4),          // CJK COMPATIBILITY IDEOGRAPH-F901
    (Unicode:#$F902; Attr:daNone; Str:#$8ECA),          // CJK COMPATIBILITY IDEOGRAPH-F902
    (Unicode:#$F903; Attr:daNone; Str:#$8CC8),          // CJK COMPATIBILITY IDEOGRAPH-F903
    (Unicode:#$F904; Attr:daNone; Str:#$6ED1),          // CJK COMPATIBILITY IDEOGRAPH-F904
    (Unicode:#$F905; Attr:daNone; Str:#$4E32),          // CJK COMPATIBILITY IDEOGRAPH-F905
    (Unicode:#$F906; Attr:daNone; Str:#$53E5),          // CJK COMPATIBILITY IDEOGRAPH-F906
    (Unicode:#$F907; Attr:daNone; Str:#$9F9C),          // CJK COMPATIBILITY IDEOGRAPH-F907
    (Unicode:#$F908; Attr:daNone; Str:#$9F9C),          // CJK COMPATIBILITY IDEOGRAPH-F908
    (Unicode:#$F909; Attr:daNone; Str:#$5951),          // CJK COMPATIBILITY IDEOGRAPH-F909
    (Unicode:#$F90A; Attr:daNone; Str:#$91D1),          // CJK COMPATIBILITY IDEOGRAPH-F90A
    (Unicode:#$F90B; Attr:daNone; Str:#$5587),          // CJK COMPATIBILITY IDEOGRAPH-F90B
    (Unicode:#$F90C; Attr:daNone; Str:#$5948),          // CJK COMPATIBILITY IDEOGRAPH-F90C
    (Unicode:#$F90D; Attr:daNone; Str:#$61F6),          // CJK COMPATIBILITY IDEOGRAPH-F90D
    (Unicode:#$F90E; Attr:daNone; Str:#$7669),          // CJK COMPATIBILITY IDEOGRAPH-F90E
    (Unicode:#$F90F; Attr:daNone; Str:#$7F85),          // CJK COMPATIBILITY IDEOGRAPH-F90F
    (Unicode:#$F910; Attr:daNone; Str:#$863F),          // CJK COMPATIBILITY IDEOGRAPH-F910
    (Unicode:#$F911; Attr:daNone; Str:#$87BA),          // CJK COMPATIBILITY IDEOGRAPH-F911
    (Unicode:#$F912; Attr:daNone; Str:#$88F8),          // CJK COMPATIBILITY IDEOGRAPH-F912
    (Unicode:#$F913; Attr:daNone; Str:#$908F),          // CJK COMPATIBILITY IDEOGRAPH-F913
    (Unicode:#$F914; Attr:daNone; Str:#$6A02),          // CJK COMPATIBILITY IDEOGRAPH-F914
    (Unicode:#$F915; Attr:daNone; Str:#$6D1B),          // CJK COMPATIBILITY IDEOGRAPH-F915
    (Unicode:#$F916; Attr:daNone; Str:#$70D9),          // CJK COMPATIBILITY IDEOGRAPH-F916
    (Unicode:#$F917; Attr:daNone; Str:#$73DE),          // CJK COMPATIBILITY IDEOGRAPH-F917
    (Unicode:#$F918; Attr:daNone; Str:#$843D),          // CJK COMPATIBILITY IDEOGRAPH-F918
    (Unicode:#$F919; Attr:daNone; Str:#$916A),          // CJK COMPATIBILITY IDEOGRAPH-F919
    (Unicode:#$F91A; Attr:daNone; Str:#$99F1),          // CJK COMPATIBILITY IDEOGRAPH-F91A
    (Unicode:#$F91B; Attr:daNone; Str:#$4E82),          // CJK COMPATIBILITY IDEOGRAPH-F91B
    (Unicode:#$F91C; Attr:daNone; Str:#$5375),          // CJK COMPATIBILITY IDEOGRAPH-F91C
    (Unicode:#$F91D; Attr:daNone; Str:#$6B04),          // CJK COMPATIBILITY IDEOGRAPH-F91D
    (Unicode:#$F91E; Attr:daNone; Str:#$721B),          // CJK COMPATIBILITY IDEOGRAPH-F91E
    (Unicode:#$F91F; Attr:daNone; Str:#$862D),          // CJK COMPATIBILITY IDEOGRAPH-F91F
    (Unicode:#$F920; Attr:daNone; Str:#$9E1E),          // CJK COMPATIBILITY IDEOGRAPH-F920
    (Unicode:#$F921; Attr:daNone; Str:#$5D50),          // CJK COMPATIBILITY IDEOGRAPH-F921
    (Unicode:#$F922; Attr:daNone; Str:#$6FEB),          // CJK COMPATIBILITY IDEOGRAPH-F922
    (Unicode:#$F923; Attr:daNone; Str:#$85CD),          // CJK COMPATIBILITY IDEOGRAPH-F923
    (Unicode:#$F924; Attr:daNone; Str:#$8964),          // CJK COMPATIBILITY IDEOGRAPH-F924
    (Unicode:#$F925; Attr:daNone; Str:#$62C9),          // CJK COMPATIBILITY IDEOGRAPH-F925
    (Unicode:#$F926; Attr:daNone; Str:#$81D8),          // CJK COMPATIBILITY IDEOGRAPH-F926
    (Unicode:#$F927; Attr:daNone; Str:#$881F),          // CJK COMPATIBILITY IDEOGRAPH-F927
    (Unicode:#$F928; Attr:daNone; Str:#$5ECA),          // CJK COMPATIBILITY IDEOGRAPH-F928
    (Unicode:#$F929; Attr:daNone; Str:#$6717),          // CJK COMPATIBILITY IDEOGRAPH-F929
    (Unicode:#$F92A; Attr:daNone; Str:#$6D6A),          // CJK COMPATIBILITY IDEOGRAPH-F92A
    (Unicode:#$F92B; Attr:daNone; Str:#$72FC),          // CJK COMPATIBILITY IDEOGRAPH-F92B
    (Unicode:#$F92C; Attr:daNone; Str:#$90CE),          // CJK COMPATIBILITY IDEOGRAPH-F92C
    (Unicode:#$F92D; Attr:daNone; Str:#$4F86),          // CJK COMPATIBILITY IDEOGRAPH-F92D
    (Unicode:#$F92E; Attr:daNone; Str:#$51B7),          // CJK COMPATIBILITY IDEOGRAPH-F92E
    (Unicode:#$F92F; Attr:daNone; Str:#$52DE),          // CJK COMPATIBILITY IDEOGRAPH-F92F
    (Unicode:#$F930; Attr:daNone; Str:#$64C4),          // CJK COMPATIBILITY IDEOGRAPH-F930
    (Unicode:#$F931; Attr:daNone; Str:#$6AD3),          // CJK COMPATIBILITY IDEOGRAPH-F931
    (Unicode:#$F932; Attr:daNone; Str:#$7210),          // CJK COMPATIBILITY IDEOGRAPH-F932
    (Unicode:#$F933; Attr:daNone; Str:#$76E7),          // CJK COMPATIBILITY IDEOGRAPH-F933
    (Unicode:#$F934; Attr:daNone; Str:#$8001),          // CJK COMPATIBILITY IDEOGRAPH-F934
    (Unicode:#$F935; Attr:daNone; Str:#$8606),          // CJK COMPATIBILITY IDEOGRAPH-F935
    (Unicode:#$F936; Attr:daNone; Str:#$865C),          // CJK COMPATIBILITY IDEOGRAPH-F936
    (Unicode:#$F937; Attr:daNone; Str:#$8DEF),          // CJK COMPATIBILITY IDEOGRAPH-F937
    (Unicode:#$F938; Attr:daNone; Str:#$9732),          // CJK COMPATIBILITY IDEOGRAPH-F938
    (Unicode:#$F939; Attr:daNone; Str:#$9B6F),          // CJK COMPATIBILITY IDEOGRAPH-F939
    (Unicode:#$F93A; Attr:daNone; Str:#$9DFA),          // CJK COMPATIBILITY IDEOGRAPH-F93A
    (Unicode:#$F93B; Attr:daNone; Str:#$788C),          // CJK COMPATIBILITY IDEOGRAPH-F93B
    (Unicode:#$F93C; Attr:daNone; Str:#$797F),          // CJK COMPATIBILITY IDEOGRAPH-F93C
    (Unicode:#$F93D; Attr:daNone; Str:#$7DA0),          // CJK COMPATIBILITY IDEOGRAPH-F93D
    (Unicode:#$F93E; Attr:daNone; Str:#$83C9),          // CJK COMPATIBILITY IDEOGRAPH-F93E
    (Unicode:#$F93F; Attr:daNone; Str:#$9304),          // CJK COMPATIBILITY IDEOGRAPH-F93F
    (Unicode:#$F940; Attr:daNone; Str:#$9E7F),          // CJK COMPATIBILITY IDEOGRAPH-F940
    (Unicode:#$F941; Attr:daNone; Str:#$8AD6),          // CJK COMPATIBILITY IDEOGRAPH-F941
    (Unicode:#$F942; Attr:daNone; Str:#$58DF),          // CJK COMPATIBILITY IDEOGRAPH-F942
    (Unicode:#$F943; Attr:daNone; Str:#$5F04),          // CJK COMPATIBILITY IDEOGRAPH-F943
    (Unicode:#$F944; Attr:daNone; Str:#$7C60),          // CJK COMPATIBILITY IDEOGRAPH-F944
    (Unicode:#$F945; Attr:daNone; Str:#$807E),          // CJK COMPATIBILITY IDEOGRAPH-F945
    (Unicode:#$F946; Attr:daNone; Str:#$7262),          // CJK COMPATIBILITY IDEOGRAPH-F946
    (Unicode:#$F947; Attr:daNone; Str:#$78CA),          // CJK COMPATIBILITY IDEOGRAPH-F947
    (Unicode:#$F948; Attr:daNone; Str:#$8CC2),          // CJK COMPATIBILITY IDEOGRAPH-F948
    (Unicode:#$F949; Attr:daNone; Str:#$96F7),          // CJK COMPATIBILITY IDEOGRAPH-F949
    (Unicode:#$F94A; Attr:daNone; Str:#$58D8),          // CJK COMPATIBILITY IDEOGRAPH-F94A
    (Unicode:#$F94B; Attr:daNone; Str:#$5C62),          // CJK COMPATIBILITY IDEOGRAPH-F94B
    (Unicode:#$F94C; Attr:daNone; Str:#$6A13),          // CJK COMPATIBILITY IDEOGRAPH-F94C
    (Unicode:#$F94D; Attr:daNone; Str:#$6DDA),          // CJK COMPATIBILITY IDEOGRAPH-F94D
    (Unicode:#$F94E; Attr:daNone; Str:#$6F0F),          // CJK COMPATIBILITY IDEOGRAPH-F94E
    (Unicode:#$F94F; Attr:daNone; Str:#$7D2F),          // CJK COMPATIBILITY IDEOGRAPH-F94F
    (Unicode:#$F950; Attr:daNone; Str:#$7E37),          // CJK COMPATIBILITY IDEOGRAPH-F950
    (Unicode:#$F951; Attr:daNone; Str:#$96FB),          // CJK COMPATIBILITY IDEOGRAPH-F951
    (Unicode:#$F952; Attr:daNone; Str:#$52D2),          // CJK COMPATIBILITY IDEOGRAPH-F952
    (Unicode:#$F953; Attr:daNone; Str:#$808B),          // CJK COMPATIBILITY IDEOGRAPH-F953
    (Unicode:#$F954; Attr:daNone; Str:#$51DC),          // CJK COMPATIBILITY IDEOGRAPH-F954
    (Unicode:#$F955; Attr:daNone; Str:#$51CC),          // CJK COMPATIBILITY IDEOGRAPH-F955
    (Unicode:#$F956; Attr:daNone; Str:#$7A1C),          // CJK COMPATIBILITY IDEOGRAPH-F956
    (Unicode:#$F957; Attr:daNone; Str:#$7DBE),          // CJK COMPATIBILITY IDEOGRAPH-F957
    (Unicode:#$F958; Attr:daNone; Str:#$83F1),          // CJK COMPATIBILITY IDEOGRAPH-F958
    (Unicode:#$F959; Attr:daNone; Str:#$9675),          // CJK COMPATIBILITY IDEOGRAPH-F959
    (Unicode:#$F95A; Attr:daNone; Str:#$8B80),          // CJK COMPATIBILITY IDEOGRAPH-F95A
    (Unicode:#$F95B; Attr:daNone; Str:#$62CF),          // CJK COMPATIBILITY IDEOGRAPH-F95B
    (Unicode:#$F95C; Attr:daNone; Str:#$6A02),          // CJK COMPATIBILITY IDEOGRAPH-F95C
    (Unicode:#$F95D; Attr:daNone; Str:#$8AFE),          // CJK COMPATIBILITY IDEOGRAPH-F95D
    (Unicode:#$F95E; Attr:daNone; Str:#$4E39),          // CJK COMPATIBILITY IDEOGRAPH-F95E
    (Unicode:#$F95F; Attr:daNone; Str:#$5BE7),          // CJK COMPATIBILITY IDEOGRAPH-F95F
    (Unicode:#$F960; Attr:daNone; Str:#$6012),          // CJK COMPATIBILITY IDEOGRAPH-F960
    (Unicode:#$F961; Attr:daNone; Str:#$7387),          // CJK COMPATIBILITY IDEOGRAPH-F961
    (Unicode:#$F962; Attr:daNone; Str:#$7570),          // CJK COMPATIBILITY IDEOGRAPH-F962
    (Unicode:#$F963; Attr:daNone; Str:#$5317),          // CJK COMPATIBILITY IDEOGRAPH-F963
    (Unicode:#$F964; Attr:daNone; Str:#$78FB),          // CJK COMPATIBILITY IDEOGRAPH-F964
    (Unicode:#$F965; Attr:daNone; Str:#$4FBF),          // CJK COMPATIBILITY IDEOGRAPH-F965
    (Unicode:#$F966; Attr:daNone; Str:#$5FA9),          // CJK COMPATIBILITY IDEOGRAPH-F966
    (Unicode:#$F967; Attr:daNone; Str:#$4E0D),          // CJK COMPATIBILITY IDEOGRAPH-F967
    (Unicode:#$F968; Attr:daNone; Str:#$6CCC),          // CJK COMPATIBILITY IDEOGRAPH-F968
    (Unicode:#$F969; Attr:daNone; Str:#$6578),          // CJK COMPATIBILITY IDEOGRAPH-F969
    (Unicode:#$F96A; Attr:daNone; Str:#$7D22),          // CJK COMPATIBILITY IDEOGRAPH-F96A
    (Unicode:#$F96B; Attr:daNone; Str:#$53C3),          // CJK COMPATIBILITY IDEOGRAPH-F96B
    (Unicode:#$F96C; Attr:daNone; Str:#$585E),          // CJK COMPATIBILITY IDEOGRAPH-F96C
    (Unicode:#$F96D; Attr:daNone; Str:#$7701),          // CJK COMPATIBILITY IDEOGRAPH-F96D
    (Unicode:#$F96E; Attr:daNone; Str:#$8449),          // CJK COMPATIBILITY IDEOGRAPH-F96E
    (Unicode:#$F96F; Attr:daNone; Str:#$8AAA),          // CJK COMPATIBILITY IDEOGRAPH-F96F
    (Unicode:#$F970; Attr:daNone; Str:#$6BBA),          // CJK COMPATIBILITY IDEOGRAPH-F970
    (Unicode:#$F971; Attr:daNone; Str:#$8FB0),          // CJK COMPATIBILITY IDEOGRAPH-F971
    (Unicode:#$F972; Attr:daNone; Str:#$6C88),          // CJK COMPATIBILITY IDEOGRAPH-F972
    (Unicode:#$F973; Attr:daNone; Str:#$62FE),          // CJK COMPATIBILITY IDEOGRAPH-F973
    (Unicode:#$F974; Attr:daNone; Str:#$82E5),          // CJK COMPATIBILITY IDEOGRAPH-F974
    (Unicode:#$F975; Attr:daNone; Str:#$63A0),          // CJK COMPATIBILITY IDEOGRAPH-F975
    (Unicode:#$F976; Attr:daNone; Str:#$7565),          // CJK COMPATIBILITY IDEOGRAPH-F976
    (Unicode:#$F977; Attr:daNone; Str:#$4EAE),          // CJK COMPATIBILITY IDEOGRAPH-F977
    (Unicode:#$F978; Attr:daNone; Str:#$5169),          // CJK COMPATIBILITY IDEOGRAPH-F978
    (Unicode:#$F979; Attr:daNone; Str:#$51C9),          // CJK COMPATIBILITY IDEOGRAPH-F979
    (Unicode:#$F97A; Attr:daNone; Str:#$6881),          // CJK COMPATIBILITY IDEOGRAPH-F97A
    (Unicode:#$F97B; Attr:daNone; Str:#$7CE7),          // CJK COMPATIBILITY IDEOGRAPH-F97B
    (Unicode:#$F97C; Attr:daNone; Str:#$826F),          // CJK COMPATIBILITY IDEOGRAPH-F97C
    (Unicode:#$F97D; Attr:daNone; Str:#$8AD2),          // CJK COMPATIBILITY IDEOGRAPH-F97D
    (Unicode:#$F97E; Attr:daNone; Str:#$91CF),          // CJK COMPATIBILITY IDEOGRAPH-F97E
    (Unicode:#$F97F; Attr:daNone; Str:#$52F5),          // CJK COMPATIBILITY IDEOGRAPH-F97F
    (Unicode:#$F980; Attr:daNone; Str:#$5442),          // CJK COMPATIBILITY IDEOGRAPH-F980
    (Unicode:#$F981; Attr:daNone; Str:#$5973),          // CJK COMPATIBILITY IDEOGRAPH-F981
    (Unicode:#$F982; Attr:daNone; Str:#$5EEC),          // CJK COMPATIBILITY IDEOGRAPH-F982
    (Unicode:#$F983; Attr:daNone; Str:#$65C5),          // CJK COMPATIBILITY IDEOGRAPH-F983
    (Unicode:#$F984; Attr:daNone; Str:#$6FFE),          // CJK COMPATIBILITY IDEOGRAPH-F984
    (Unicode:#$F985; Attr:daNone; Str:#$792A),          // CJK COMPATIBILITY IDEOGRAPH-F985
    (Unicode:#$F986; Attr:daNone; Str:#$95AD),          // CJK COMPATIBILITY IDEOGRAPH-F986
    (Unicode:#$F987; Attr:daNone; Str:#$9A6A),          // CJK COMPATIBILITY IDEOGRAPH-F987
    (Unicode:#$F988; Attr:daNone; Str:#$9E97),          // CJK COMPATIBILITY IDEOGRAPH-F988
    (Unicode:#$F989; Attr:daNone; Str:#$9ECE),          // CJK COMPATIBILITY IDEOGRAPH-F989
    (Unicode:#$F98A; Attr:daNone; Str:#$529B),          // CJK COMPATIBILITY IDEOGRAPH-F98A
    (Unicode:#$F98B; Attr:daNone; Str:#$66C6),          // CJK COMPATIBILITY IDEOGRAPH-F98B
    (Unicode:#$F98C; Attr:daNone; Str:#$6B77),          // CJK COMPATIBILITY IDEOGRAPH-F98C
    (Unicode:#$F98D; Attr:daNone; Str:#$8F62),          // CJK COMPATIBILITY IDEOGRAPH-F98D
    (Unicode:#$F98E; Attr:daNone; Str:#$5E74),          // CJK COMPATIBILITY IDEOGRAPH-F98E
    (Unicode:#$F98F; Attr:daNone; Str:#$6190),          // CJK COMPATIBILITY IDEOGRAPH-F98F
    (Unicode:#$F990; Attr:daNone; Str:#$6200),          // CJK COMPATIBILITY IDEOGRAPH-F990
    (Unicode:#$F991; Attr:daNone; Str:#$649A),          // CJK COMPATIBILITY IDEOGRAPH-F991
    (Unicode:#$F992; Attr:daNone; Str:#$6F23),          // CJK COMPATIBILITY IDEOGRAPH-F992
    (Unicode:#$F993; Attr:daNone; Str:#$7149),          // CJK COMPATIBILITY IDEOGRAPH-F993
    (Unicode:#$F994; Attr:daNone; Str:#$7489),          // CJK COMPATIBILITY IDEOGRAPH-F994
    (Unicode:#$F995; Attr:daNone; Str:#$79CA),          // CJK COMPATIBILITY IDEOGRAPH-F995
    (Unicode:#$F996; Attr:daNone; Str:#$7DF4),          // CJK COMPATIBILITY IDEOGRAPH-F996
    (Unicode:#$F997; Attr:daNone; Str:#$806F),          // CJK COMPATIBILITY IDEOGRAPH-F997
    (Unicode:#$F998; Attr:daNone; Str:#$8F26),          // CJK COMPATIBILITY IDEOGRAPH-F998
    (Unicode:#$F999; Attr:daNone; Str:#$84EE),          // CJK COMPATIBILITY IDEOGRAPH-F999
    (Unicode:#$F99A; Attr:daNone; Str:#$9023),          // CJK COMPATIBILITY IDEOGRAPH-F99A
    (Unicode:#$F99B; Attr:daNone; Str:#$934A),          // CJK COMPATIBILITY IDEOGRAPH-F99B
    (Unicode:#$F99C; Attr:daNone; Str:#$5217),          // CJK COMPATIBILITY IDEOGRAPH-F99C
    (Unicode:#$F99D; Attr:daNone; Str:#$52A3),          // CJK COMPATIBILITY IDEOGRAPH-F99D
    (Unicode:#$F99E; Attr:daNone; Str:#$54BD),          // CJK COMPATIBILITY IDEOGRAPH-F99E
    (Unicode:#$F99F; Attr:daNone; Str:#$70C8),          // CJK COMPATIBILITY IDEOGRAPH-F99F
    (Unicode:#$F9A0; Attr:daNone; Str:#$88C2),          // CJK COMPATIBILITY IDEOGRAPH-F9A0
    (Unicode:#$F9A1; Attr:daNone; Str:#$8AAA),          // CJK COMPATIBILITY IDEOGRAPH-F9A1
    (Unicode:#$F9A2; Attr:daNone; Str:#$5EC9),          // CJK COMPATIBILITY IDEOGRAPH-F9A2
    (Unicode:#$F9A3; Attr:daNone; Str:#$5FF5),          // CJK COMPATIBILITY IDEOGRAPH-F9A3
    (Unicode:#$F9A4; Attr:daNone; Str:#$637B),          // CJK COMPATIBILITY IDEOGRAPH-F9A4
    (Unicode:#$F9A5; Attr:daNone; Str:#$6BAE),          // CJK COMPATIBILITY IDEOGRAPH-F9A5
    (Unicode:#$F9A6; Attr:daNone; Str:#$7C3E),          // CJK COMPATIBILITY IDEOGRAPH-F9A6
    (Unicode:#$F9A7; Attr:daNone; Str:#$7375),          // CJK COMPATIBILITY IDEOGRAPH-F9A7
    (Unicode:#$F9A8; Attr:daNone; Str:#$4EE4),          // CJK COMPATIBILITY IDEOGRAPH-F9A8
    (Unicode:#$F9A9; Attr:daNone; Str:#$56F9),          // CJK COMPATIBILITY IDEOGRAPH-F9A9
    (Unicode:#$F9AA; Attr:daNone; Str:#$5BE7),          // CJK COMPATIBILITY IDEOGRAPH-F9AA
    (Unicode:#$F9AB; Attr:daNone; Str:#$5DBA),          // CJK COMPATIBILITY IDEOGRAPH-F9AB
    (Unicode:#$F9AC; Attr:daNone; Str:#$601C),          // CJK COMPATIBILITY IDEOGRAPH-F9AC
    (Unicode:#$F9AD; Attr:daNone; Str:#$73B2),          // CJK COMPATIBILITY IDEOGRAPH-F9AD
    (Unicode:#$F9AE; Attr:daNone; Str:#$7469),          // CJK COMPATIBILITY IDEOGRAPH-F9AE
    (Unicode:#$F9AF; Attr:daNone; Str:#$7F9A),          // CJK COMPATIBILITY IDEOGRAPH-F9AF
    (Unicode:#$F9B0; Attr:daNone; Str:#$8046),          // CJK COMPATIBILITY IDEOGRAPH-F9B0
    (Unicode:#$F9B1; Attr:daNone; Str:#$9234),          // CJK COMPATIBILITY IDEOGRAPH-F9B1
    (Unicode:#$F9B2; Attr:daNone; Str:#$96F6),          // CJK COMPATIBILITY IDEOGRAPH-F9B2
    (Unicode:#$F9B3; Attr:daNone; Str:#$9748),          // CJK COMPATIBILITY IDEOGRAPH-F9B3
    (Unicode:#$F9B4; Attr:daNone; Str:#$9818),          // CJK COMPATIBILITY IDEOGRAPH-F9B4
    (Unicode:#$F9B5; Attr:daNone; Str:#$4F8B),          // CJK COMPATIBILITY IDEOGRAPH-F9B5
    (Unicode:#$F9B6; Attr:daNone; Str:#$79AE),          // CJK COMPATIBILITY IDEOGRAPH-F9B6
    (Unicode:#$F9B7; Attr:daNone; Str:#$91B4),          // CJK COMPATIBILITY IDEOGRAPH-F9B7
    (Unicode:#$F9B8; Attr:daNone; Str:#$96B8),          // CJK COMPATIBILITY IDEOGRAPH-F9B8
    (Unicode:#$F9B9; Attr:daNone; Str:#$60E1),          // CJK COMPATIBILITY IDEOGRAPH-F9B9
    (Unicode:#$F9BA; Attr:daNone; Str:#$4E86),          // CJK COMPATIBILITY IDEOGRAPH-F9BA
    (Unicode:#$F9BB; Attr:daNone; Str:#$50DA),          // CJK COMPATIBILITY IDEOGRAPH-F9BB
    (Unicode:#$F9BC; Attr:daNone; Str:#$5BEE),          // CJK COMPATIBILITY IDEOGRAPH-F9BC
    (Unicode:#$F9BD; Attr:daNone; Str:#$5C3F),          // CJK COMPATIBILITY IDEOGRAPH-F9BD
    (Unicode:#$F9BE; Attr:daNone; Str:#$6599),          // CJK COMPATIBILITY IDEOGRAPH-F9BE
    (Unicode:#$F9BF; Attr:daNone; Str:#$6A02),          // CJK COMPATIBILITY IDEOGRAPH-F9BF
    (Unicode:#$F9C0; Attr:daNone; Str:#$71CE),          // CJK COMPATIBILITY IDEOGRAPH-F9C0
    (Unicode:#$F9C1; Attr:daNone; Str:#$7642),          // CJK COMPATIBILITY IDEOGRAPH-F9C1
    (Unicode:#$F9C2; Attr:daNone; Str:#$84FC),          // CJK COMPATIBILITY IDEOGRAPH-F9C2
    (Unicode:#$F9C3; Attr:daNone; Str:#$907C),          // CJK COMPATIBILITY IDEOGRAPH-F9C3
    (Unicode:#$F9C4; Attr:daNone; Str:#$9F8D),          // CJK COMPATIBILITY IDEOGRAPH-F9C4
    (Unicode:#$F9C5; Attr:daNone; Str:#$6688),          // CJK COMPATIBILITY IDEOGRAPH-F9C5
    (Unicode:#$F9C6; Attr:daNone; Str:#$962E),          // CJK COMPATIBILITY IDEOGRAPH-F9C6
    (Unicode:#$F9C7; Attr:daNone; Str:#$5289),          // CJK COMPATIBILITY IDEOGRAPH-F9C7
    (Unicode:#$F9C8; Attr:daNone; Str:#$677B),          // CJK COMPATIBILITY IDEOGRAPH-F9C8
    (Unicode:#$F9C9; Attr:daNone; Str:#$67F3),          // CJK COMPATIBILITY IDEOGRAPH-F9C9
    (Unicode:#$F9CA; Attr:daNone; Str:#$6D41),          // CJK COMPATIBILITY IDEOGRAPH-F9CA
    (Unicode:#$F9CB; Attr:daNone; Str:#$6E9C),          // CJK COMPATIBILITY IDEOGRAPH-F9CB
    (Unicode:#$F9CC; Attr:daNone; Str:#$7409),          // CJK COMPATIBILITY IDEOGRAPH-F9CC
    (Unicode:#$F9CD; Attr:daNone; Str:#$7559),          // CJK COMPATIBILITY IDEOGRAPH-F9CD
    (Unicode:#$F9CE; Attr:daNone; Str:#$786B),          // CJK COMPATIBILITY IDEOGRAPH-F9CE
    (Unicode:#$F9CF; Attr:daNone; Str:#$7D10),          // CJK COMPATIBILITY IDEOGRAPH-F9CF
    (Unicode:#$F9D0; Attr:daNone; Str:#$985E),          // CJK COMPATIBILITY IDEOGRAPH-F9D0
    (Unicode:#$F9D1; Attr:daNone; Str:#$516D),          // CJK COMPATIBILITY IDEOGRAPH-F9D1
    (Unicode:#$F9D2; Attr:daNone; Str:#$622E),          // CJK COMPATIBILITY IDEOGRAPH-F9D2
    (Unicode:#$F9D3; Attr:daNone; Str:#$9678),          // CJK COMPATIBILITY IDEOGRAPH-F9D3
    (Unicode:#$F9D4; Attr:daNone; Str:#$502B),          // CJK COMPATIBILITY IDEOGRAPH-F9D4
    (Unicode:#$F9D5; Attr:daNone; Str:#$5D19),          // CJK COMPATIBILITY IDEOGRAPH-F9D5
    (Unicode:#$F9D6; Attr:daNone; Str:#$6DEA),          // CJK COMPATIBILITY IDEOGRAPH-F9D6
    (Unicode:#$F9D7; Attr:daNone; Str:#$8F2A),          // CJK COMPATIBILITY IDEOGRAPH-F9D7
    (Unicode:#$F9D8; Attr:daNone; Str:#$5F8B),          // CJK COMPATIBILITY IDEOGRAPH-F9D8
    (Unicode:#$F9D9; Attr:daNone; Str:#$6144),          // CJK COMPATIBILITY IDEOGRAPH-F9D9
    (Unicode:#$F9DA; Attr:daNone; Str:#$6817),          // CJK COMPATIBILITY IDEOGRAPH-F9DA
    (Unicode:#$F9DB; Attr:daNone; Str:#$7387),          // CJK COMPATIBILITY IDEOGRAPH-F9DB
    (Unicode:#$F9DC; Attr:daNone; Str:#$9686),          // CJK COMPATIBILITY IDEOGRAPH-F9DC
    (Unicode:#$F9DD; Attr:daNone; Str:#$5229),          // CJK COMPATIBILITY IDEOGRAPH-F9DD
    (Unicode:#$F9DE; Attr:daNone; Str:#$540F),          // CJK COMPATIBILITY IDEOGRAPH-F9DE
    (Unicode:#$F9DF; Attr:daNone; Str:#$5C65),          // CJK COMPATIBILITY IDEOGRAPH-F9DF
    (Unicode:#$F9E0; Attr:daNone; Str:#$6613),          // CJK COMPATIBILITY IDEOGRAPH-F9E0
    (Unicode:#$F9E1; Attr:daNone; Str:#$674E),          // CJK COMPATIBILITY IDEOGRAPH-F9E1
    (Unicode:#$F9E2; Attr:daNone; Str:#$68A8),          // CJK COMPATIBILITY IDEOGRAPH-F9E2
    (Unicode:#$F9E3; Attr:daNone; Str:#$6CE5),          // CJK COMPATIBILITY IDEOGRAPH-F9E3
    (Unicode:#$F9E4; Attr:daNone; Str:#$7406),          // CJK COMPATIBILITY IDEOGRAPH-F9E4
    (Unicode:#$F9E5; Attr:daNone; Str:#$75E2),          // CJK COMPATIBILITY IDEOGRAPH-F9E5
    (Unicode:#$F9E6; Attr:daNone; Str:#$7F79),          // CJK COMPATIBILITY IDEOGRAPH-F9E6
    (Unicode:#$F9E7; Attr:daNone; Str:#$88CF),          // CJK COMPATIBILITY IDEOGRAPH-F9E7
    (Unicode:#$F9E8; Attr:daNone; Str:#$88E1),          // CJK COMPATIBILITY IDEOGRAPH-F9E8
    (Unicode:#$F9E9; Attr:daNone; Str:#$91CC),          // CJK COMPATIBILITY IDEOGRAPH-F9E9
    (Unicode:#$F9EA; Attr:daNone; Str:#$96E2),          // CJK COMPATIBILITY IDEOGRAPH-F9EA
    (Unicode:#$F9EB; Attr:daNone; Str:#$533F),          // CJK COMPATIBILITY IDEOGRAPH-F9EB
    (Unicode:#$F9EC; Attr:daNone; Str:#$6EBA),          // CJK COMPATIBILITY IDEOGRAPH-F9EC
    (Unicode:#$F9ED; Attr:daNone; Str:#$541D),          // CJK COMPATIBILITY IDEOGRAPH-F9ED
    (Unicode:#$F9EE; Attr:daNone; Str:#$71D0),          // CJK COMPATIBILITY IDEOGRAPH-F9EE
    (Unicode:#$F9EF; Attr:daNone; Str:#$7498),          // CJK COMPATIBILITY IDEOGRAPH-F9EF
    (Unicode:#$F9F0; Attr:daNone; Str:#$85FA),          // CJK COMPATIBILITY IDEOGRAPH-F9F0
    (Unicode:#$F9F1; Attr:daNone; Str:#$96A3),          // CJK COMPATIBILITY IDEOGRAPH-F9F1
    (Unicode:#$F9F2; Attr:daNone; Str:#$9C57),          // CJK COMPATIBILITY IDEOGRAPH-F9F2
    (Unicode:#$F9F3; Attr:daNone; Str:#$9E9F),          // CJK COMPATIBILITY IDEOGRAPH-F9F3
    (Unicode:#$F9F4; Attr:daNone; Str:#$6797),          // CJK COMPATIBILITY IDEOGRAPH-F9F4
    (Unicode:#$F9F5; Attr:daNone; Str:#$6DCB),          // CJK COMPATIBILITY IDEOGRAPH-F9F5
    (Unicode:#$F9F6; Attr:daNone; Str:#$81E8),          // CJK COMPATIBILITY IDEOGRAPH-F9F6
    (Unicode:#$F9F7; Attr:daNone; Str:#$7ACB),          // CJK COMPATIBILITY IDEOGRAPH-F9F7
    (Unicode:#$F9F8; Attr:daNone; Str:#$7B20),          // CJK COMPATIBILITY IDEOGRAPH-F9F8
    (Unicode:#$F9F9; Attr:daNone; Str:#$7C92),          // CJK COMPATIBILITY IDEOGRAPH-F9F9
    (Unicode:#$F9FA; Attr:daNone; Str:#$72C0),          // CJK COMPATIBILITY IDEOGRAPH-F9FA
    (Unicode:#$F9FB; Attr:daNone; Str:#$7099),          // CJK COMPATIBILITY IDEOGRAPH-F9FB
    (Unicode:#$F9FC; Attr:daNone; Str:#$8B58),          // CJK COMPATIBILITY IDEOGRAPH-F9FC
    (Unicode:#$F9FD; Attr:daNone; Str:#$4EC0),          // CJK COMPATIBILITY IDEOGRAPH-F9FD
    (Unicode:#$F9FE; Attr:daNone; Str:#$8336),          // CJK COMPATIBILITY IDEOGRAPH-F9FE
    (Unicode:#$F9FF; Attr:daNone; Str:#$523A),          // CJK COMPATIBILITY IDEOGRAPH-F9FF
    (Unicode:#$FA00; Attr:daNone; Str:#$5207),          // CJK COMPATIBILITY IDEOGRAPH-FA00
    (Unicode:#$FA01; Attr:daNone; Str:#$5EA6),          // CJK COMPATIBILITY IDEOGRAPH-FA01
    (Unicode:#$FA02; Attr:daNone; Str:#$62D3),          // CJK COMPATIBILITY IDEOGRAPH-FA02
    (Unicode:#$FA03; Attr:daNone; Str:#$7CD6),          // CJK COMPATIBILITY IDEOGRAPH-FA03
    (Unicode:#$FA04; Attr:daNone; Str:#$5B85),          // CJK COMPATIBILITY IDEOGRAPH-FA04
    (Unicode:#$FA05; Attr:daNone; Str:#$6D1E),          // CJK COMPATIBILITY IDEOGRAPH-FA05
    (Unicode:#$FA06; Attr:daNone; Str:#$66B4),          // CJK COMPATIBILITY IDEOGRAPH-FA06
    (Unicode:#$FA07; Attr:daNone; Str:#$8F3B),          // CJK COMPATIBILITY IDEOGRAPH-FA07
    (Unicode:#$FA08; Attr:daNone; Str:#$884C),          // CJK COMPATIBILITY IDEOGRAPH-FA08
    (Unicode:#$FA09; Attr:daNone; Str:#$964D),          // CJK COMPATIBILITY IDEOGRAPH-FA09
    (Unicode:#$FA0A; Attr:daNone; Str:#$898B),          // CJK COMPATIBILITY IDEOGRAPH-FA0A
    (Unicode:#$FA0B; Attr:daNone; Str:#$5ED3),          // CJK COMPATIBILITY IDEOGRAPH-FA0B
    (Unicode:#$FA0C; Attr:daNone; Str:#$5140),          // CJK COMPATIBILITY IDEOGRAPH-FA0C
    (Unicode:#$FA0D; Attr:daNone; Str:#$55C0),          // CJK COMPATIBILITY IDEOGRAPH-FA0D
    (Unicode:#$FA10; Attr:daNone; Str:#$585A),          // CJK COMPATIBILITY IDEOGRAPH-FA10
    (Unicode:#$FA12; Attr:daNone; Str:#$6674),          // CJK COMPATIBILITY IDEOGRAPH-FA12
    (Unicode:#$FA15; Attr:daNone; Str:#$51DE),          // CJK COMPATIBILITY IDEOGRAPH-FA15
    (Unicode:#$FA16; Attr:daNone; Str:#$732A),          // CJK COMPATIBILITY IDEOGRAPH-FA16
    (Unicode:#$FA17; Attr:daNone; Str:#$76CA),          // CJK COMPATIBILITY IDEOGRAPH-FA17
    (Unicode:#$FA18; Attr:daNone; Str:#$793C),          // CJK COMPATIBILITY IDEOGRAPH-FA18
    (Unicode:#$FA19; Attr:daNone; Str:#$795E),          // CJK COMPATIBILITY IDEOGRAPH-FA19
    (Unicode:#$FA1A; Attr:daNone; Str:#$7965),          // CJK COMPATIBILITY IDEOGRAPH-FA1A
    (Unicode:#$FA1B; Attr:daNone; Str:#$798F),          // CJK COMPATIBILITY IDEOGRAPH-FA1B
    (Unicode:#$FA1C; Attr:daNone; Str:#$9756),          // CJK COMPATIBILITY IDEOGRAPH-FA1C
    (Unicode:#$FA1D; Attr:daNone; Str:#$7CBE),          // CJK COMPATIBILITY IDEOGRAPH-FA1D
    (Unicode:#$FA1E; Attr:daNone; Str:#$7FBD),          // CJK COMPATIBILITY IDEOGRAPH-FA1E
    (Unicode:#$FA20; Attr:daNone; Str:#$8612),          // CJK COMPATIBILITY IDEOGRAPH-FA20
    (Unicode:#$FA22; Attr:daNone; Str:#$8AF8),          // CJK COMPATIBILITY IDEOGRAPH-FA22
    (Unicode:#$FA25; Attr:daNone; Str:#$9038),          // CJK COMPATIBILITY IDEOGRAPH-FA25
    (Unicode:#$FA26; Attr:daNone; Str:#$90FD),          // CJK COMPATIBILITY IDEOGRAPH-FA26
    (Unicode:#$FA2A; Attr:daNone; Str:#$98EF),          // CJK COMPATIBILITY IDEOGRAPH-FA2A
    (Unicode:#$FA2B; Attr:daNone; Str:#$98FC),          // CJK COMPATIBILITY IDEOGRAPH-FA2B
    (Unicode:#$FA2C; Attr:daNone; Str:#$9928),          // CJK COMPATIBILITY IDEOGRAPH-FA2C
    (Unicode:#$FA2D; Attr:daNone; Str:#$9DB4),          // CJK COMPATIBILITY IDEOGRAPH-FA2D
    (Unicode:#$FB00; Attr:daCompat; Str:#$0066#$0066),  // LATIN SMALL LIGATURE FF
    (Unicode:#$FB01; Attr:daCompat; Str:#$0066#$0069),  // LATIN SMALL LIGATURE FI
    (Unicode:#$FB02; Attr:daCompat; Str:#$0066#$006C),  // LATIN SMALL LIGATURE FL
    (Unicode:#$FB03; Attr:daCompat; Str:#$0066#$0066#$0069),   // LATIN SMALL LIGATURE FFI
    (Unicode:#$FB04; Attr:daCompat; Str:#$0066#$0066#$006C),   // LATIN SMALL LIGATURE FFL
    (Unicode:#$FB05; Attr:daCompat; Str:#$017F#$0074),  // LATIN SMALL LIGATURE LONG S T
    (Unicode:#$FB06; Attr:daCompat; Str:#$0073#$0074),  // LATIN SMALL LIGATURE ST
    (Unicode:#$FB13; Attr:daCompat; Str:#$0574#$0576),  // ARMENIAN SMALL LIGATURE MEN NOW
    (Unicode:#$FB14; Attr:daCompat; Str:#$0574#$0565),  // ARMENIAN SMALL LIGATURE MEN ECH
    (Unicode:#$FB15; Attr:daCompat; Str:#$0574#$056B),  // ARMENIAN SMALL LIGATURE MEN INI
    (Unicode:#$FB16; Attr:daCompat; Str:#$057E#$0576),  // ARMENIAN SMALL LIGATURE VEW NOW
    (Unicode:#$FB17; Attr:daCompat; Str:#$0574#$056D),  // ARMENIAN SMALL LIGATURE MEN XEH
    (Unicode:#$FB1D; Attr:daNone; Str:#$05D9#$05B4),    // HEBREW LETTER YOD WITH HIRIQ
    (Unicode:#$FB1F; Attr:daNone; Str:#$05F2#$05B7),    // HEBREW LIGATURE YIDDISH YOD YOD PATAH
    (Unicode:#$FB20; Attr:daFont; Str:#$05E2),          // HEBREW LETTER ALTERNATIVE AYIN
    (Unicode:#$FB21; Attr:daFont; Str:#$05D0),          // HEBREW LETTER WIDE ALEF
    (Unicode:#$FB22; Attr:daFont; Str:#$05D3),          // HEBREW LETTER WIDE DALET
    (Unicode:#$FB23; Attr:daFont; Str:#$05D4),          // HEBREW LETTER WIDE HE
    (Unicode:#$FB24; Attr:daFont; Str:#$05DB),          // HEBREW LETTER WIDE KAF
    (Unicode:#$FB25; Attr:daFont; Str:#$05DC),          // HEBREW LETTER WIDE LAMED
    (Unicode:#$FB26; Attr:daFont; Str:#$05DD),          // HEBREW LETTER WIDE FINAL MEM
    (Unicode:#$FB27; Attr:daFont; Str:#$05E8),          // HEBREW LETTER WIDE RESH
    (Unicode:#$FB28; Attr:daFont; Str:#$05EA),          // HEBREW LETTER WIDE TAV
    (Unicode:#$FB29; Attr:daFont; Str:#$002B),          // HEBREW LETTER ALTERNATIVE PLUS SIGN
    (Unicode:#$FB2A; Attr:daNone; Str:#$05E9#$05C1),    // HEBREW LETTER SHIN WITH SHIN DOT
    (Unicode:#$FB2B; Attr:daNone; Str:#$05E9#$05C2),    // HEBREW LETTER SHIN WITH SIN DOT
    (Unicode:#$FB2C; Attr:daNone; Str:#$FB49#$05C1),    // HEBREW LETTER SHIN WITH DAGESH AND SHIN DOT
    (Unicode:#$FB2D; Attr:daNone; Str:#$FB49#$05C2),    // HEBREW LETTER SHIN WITH DAGESH AND SIN DOT
    (Unicode:#$FB2E; Attr:daNone; Str:#$05D0#$05B7),    // HEBREW LETTER ALEF WITH PATAH
    (Unicode:#$FB2F; Attr:daNone; Str:#$05D0#$05B8),    // HEBREW LETTER ALEF WITH QAMATS
    (Unicode:#$FB30; Attr:daNone; Str:#$05D0#$05BC),    // HEBREW LETTER ALEF WITH MAPIQ
    (Unicode:#$FB31; Attr:daNone; Str:#$05D1#$05BC),    // HEBREW LETTER BET WITH DAGESH
    (Unicode:#$FB32; Attr:daNone; Str:#$05D2#$05BC),    // HEBREW LETTER GIMEL WITH DAGESH
    (Unicode:#$FB33; Attr:daNone; Str:#$05D3#$05BC),    // HEBREW LETTER DALET WITH DAGESH
    (Unicode:#$FB34; Attr:daNone; Str:#$05D4#$05BC),    // HEBREW LETTER HE WITH MAPIQ
    (Unicode:#$FB35; Attr:daNone; Str:#$05D5#$05BC),    // HEBREW LETTER VAV WITH DAGESH
    (Unicode:#$FB36; Attr:daNone; Str:#$05D6#$05BC),    // HEBREW LETTER ZAYIN WITH DAGESH
    (Unicode:#$FB38; Attr:daNone; Str:#$05D8#$05BC),    // HEBREW LETTER TET WITH DAGESH
    (Unicode:#$FB39; Attr:daNone; Str:#$05D9#$05BC),    // HEBREW LETTER YOD WITH DAGESH
    (Unicode:#$FB3A; Attr:daNone; Str:#$05DA#$05BC),    // HEBREW LETTER FINAL KAF WITH DAGESH
    (Unicode:#$FB3B; Attr:daNone; Str:#$05DB#$05BC),    // HEBREW LETTER KAF WITH DAGESH
    (Unicode:#$FB3C; Attr:daNone; Str:#$05DC#$05BC),    // HEBREW LETTER LAMED WITH DAGESH
    (Unicode:#$FB3E; Attr:daNone; Str:#$05DE#$05BC),    // HEBREW LETTER MEM WITH DAGESH
    (Unicode:#$FB40; Attr:daNone; Str:#$05E0#$05BC),    // HEBREW LETTER NUN WITH DAGESH
    (Unicode:#$FB41; Attr:daNone; Str:#$05E1#$05BC),    // HEBREW LETTER SAMEKH WITH DAGESH
    (Unicode:#$FB43; Attr:daNone; Str:#$05E3#$05BC),    // HEBREW LETTER FINAL PE WITH DAGESH
    (Unicode:#$FB44; Attr:daNone; Str:#$05E4#$05BC),    // HEBREW LETTER PE WITH DAGESH
    (Unicode:#$FB46; Attr:daNone; Str:#$05E6#$05BC),    // HEBREW LETTER TSADI WITH DAGESH
    (Unicode:#$FB47; Attr:daNone; Str:#$05E7#$05BC),    // HEBREW LETTER QOF WITH DAGESH
    (Unicode:#$FB48; Attr:daNone; Str:#$05E8#$05BC),    // HEBREW LETTER RESH WITH DAGESH
    (Unicode:#$FB49; Attr:daNone; Str:#$05E9#$05BC),    // HEBREW LETTER SHIN WITH DAGESH
    (Unicode:#$FB4A; Attr:daNone; Str:#$05EA#$05BC),    // HEBREW LETTER TAV WITH DAGESH
    (Unicode:#$FB4B; Attr:daNone; Str:#$05D5#$05B9),    // HEBREW LETTER VAV WITH HOLAM
    (Unicode:#$FB4C; Attr:daNone; Str:#$05D1#$05BF),    // HEBREW LETTER BET WITH RAFE
    (Unicode:#$FB4D; Attr:daNone; Str:#$05DB#$05BF),    // HEBREW LETTER KAF WITH RAFE
    (Unicode:#$FB4E; Attr:daNone; Str:#$05E4#$05BF),    // HEBREW LETTER PE WITH RAFE
    (Unicode:#$FB4F; Attr:daCompat; Str:#$05D0#$05DC),  // HEBREW LIGATURE ALEF LAMED
    (Unicode:#$FB50; Attr:daIsolated; Str:#$0671),      // ARABIC LETTER ALEF WASLA ISOLATED FORM
    (Unicode:#$FB51; Attr:daFinal; Str:#$0671),         // ARABIC LETTER ALEF WASLA FINAL FORM
    (Unicode:#$FB52; Attr:daIsolated; Str:#$067B),      // ARABIC LETTER BEEH ISOLATED FORM
    (Unicode:#$FB53; Attr:daFinal; Str:#$067B),         // ARABIC LETTER BEEH FINAL FORM
    (Unicode:#$FB54; Attr:daInitial; Str:#$067B),       // ARABIC LETTER BEEH INITIAL FORM
    (Unicode:#$FB55; Attr:daMedial; Str:#$067B),        // ARABIC LETTER BEEH MEDIAL FORM
    (Unicode:#$FB56; Attr:daIsolated; Str:#$067E),      // ARABIC LETTER PEH ISOLATED FORM
    (Unicode:#$FB57; Attr:daFinal; Str:#$067E),         // ARABIC LETTER PEH FINAL FORM
    (Unicode:#$FB58; Attr:daInitial; Str:#$067E),       // ARABIC LETTER PEH INITIAL FORM
    (Unicode:#$FB59; Attr:daMedial; Str:#$067E),        // ARABIC LETTER PEH MEDIAL FORM
    (Unicode:#$FB5A; Attr:daIsolated; Str:#$0680),      // ARABIC LETTER BEHEH ISOLATED FORM
    (Unicode:#$FB5B; Attr:daFinal; Str:#$0680),         // ARABIC LETTER BEHEH FINAL FORM
    (Unicode:#$FB5C; Attr:daInitial; Str:#$0680),       // ARABIC LETTER BEHEH INITIAL FORM
    (Unicode:#$FB5D; Attr:daMedial; Str:#$0680),        // ARABIC LETTER BEHEH MEDIAL FORM
    (Unicode:#$FB5E; Attr:daIsolated; Str:#$067A),      // ARABIC LETTER TTEHEH ISOLATED FORM
    (Unicode:#$FB5F; Attr:daFinal; Str:#$067A),         // ARABIC LETTER TTEHEH FINAL FORM
    (Unicode:#$FB60; Attr:daInitial; Str:#$067A),       // ARABIC LETTER TTEHEH INITIAL FORM
    (Unicode:#$FB61; Attr:daMedial; Str:#$067A),        // ARABIC LETTER TTEHEH MEDIAL FORM
    (Unicode:#$FB62; Attr:daIsolated; Str:#$067F),      // ARABIC LETTER TEHEH ISOLATED FORM
    (Unicode:#$FB63; Attr:daFinal; Str:#$067F),         // ARABIC LETTER TEHEH FINAL FORM
    (Unicode:#$FB64; Attr:daInitial; Str:#$067F),       // ARABIC LETTER TEHEH INITIAL FORM
    (Unicode:#$FB65; Attr:daMedial; Str:#$067F),        // ARABIC LETTER TEHEH MEDIAL FORM
    (Unicode:#$FB66; Attr:daIsolated; Str:#$0679),      // ARABIC LETTER TTEH ISOLATED FORM
    (Unicode:#$FB67; Attr:daFinal; Str:#$0679),         // ARABIC LETTER TTEH FINAL FORM
    (Unicode:#$FB68; Attr:daInitial; Str:#$0679),       // ARABIC LETTER TTEH INITIAL FORM
    (Unicode:#$FB69; Attr:daMedial; Str:#$0679),        // ARABIC LETTER TTEH MEDIAL FORM
    (Unicode:#$FB6A; Attr:daIsolated; Str:#$06A4),      // ARABIC LETTER VEH ISOLATED FORM
    (Unicode:#$FB6B; Attr:daFinal; Str:#$06A4),         // ARABIC LETTER VEH FINAL FORM
    (Unicode:#$FB6C; Attr:daInitial; Str:#$06A4),       // ARABIC LETTER VEH INITIAL FORM
    (Unicode:#$FB6D; Attr:daMedial; Str:#$06A4),        // ARABIC LETTER VEH MEDIAL FORM
    (Unicode:#$FB6E; Attr:daIsolated; Str:#$06A6),      // ARABIC LETTER PEHEH ISOLATED FORM
    (Unicode:#$FB6F; Attr:daFinal; Str:#$06A6),         // ARABIC LETTER PEHEH FINAL FORM
    (Unicode:#$FB70; Attr:daInitial; Str:#$06A6),       // ARABIC LETTER PEHEH INITIAL FORM
    (Unicode:#$FB71; Attr:daMedial; Str:#$06A6),        // ARABIC LETTER PEHEH MEDIAL FORM
    (Unicode:#$FB72; Attr:daIsolated; Str:#$0684),      // ARABIC LETTER DYEH ISOLATED FORM
    (Unicode:#$FB73; Attr:daFinal; Str:#$0684),         // ARABIC LETTER DYEH FINAL FORM
    (Unicode:#$FB74; Attr:daInitial; Str:#$0684),       // ARABIC LETTER DYEH INITIAL FORM
    (Unicode:#$FB75; Attr:daMedial; Str:#$0684),        // ARABIC LETTER DYEH MEDIAL FORM
    (Unicode:#$FB76; Attr:daIsolated; Str:#$0683),      // ARABIC LETTER NYEH ISOLATED FORM
    (Unicode:#$FB77; Attr:daFinal; Str:#$0683),         // ARABIC LETTER NYEH FINAL FORM
    (Unicode:#$FB78; Attr:daInitial; Str:#$0683),       // ARABIC LETTER NYEH INITIAL FORM
    (Unicode:#$FB79; Attr:daMedial; Str:#$0683),        // ARABIC LETTER NYEH MEDIAL FORM
    (Unicode:#$FB7A; Attr:daIsolated; Str:#$0686),      // ARABIC LETTER TCHEH ISOLATED FORM
    (Unicode:#$FB7B; Attr:daFinal; Str:#$0686),         // ARABIC LETTER TCHEH FINAL FORM
    (Unicode:#$FB7C; Attr:daInitial; Str:#$0686),       // ARABIC LETTER TCHEH INITIAL FORM
    (Unicode:#$FB7D; Attr:daMedial; Str:#$0686),        // ARABIC LETTER TCHEH MEDIAL FORM
    (Unicode:#$FB7E; Attr:daIsolated; Str:#$0687),      // ARABIC LETTER TCHEHEH ISOLATED FORM
    (Unicode:#$FB7F; Attr:daFinal; Str:#$0687),         // ARABIC LETTER TCHEHEH FINAL FORM
    (Unicode:#$FB80; Attr:daInitial; Str:#$0687),       // ARABIC LETTER TCHEHEH INITIAL FORM
    (Unicode:#$FB81; Attr:daMedial; Str:#$0687),        // ARABIC LETTER TCHEHEH MEDIAL FORM
    (Unicode:#$FB82; Attr:daIsolated; Str:#$068D),      // ARABIC LETTER DDAHAL ISOLATED FORM
    (Unicode:#$FB83; Attr:daFinal; Str:#$068D),         // ARABIC LETTER DDAHAL FINAL FORM
    (Unicode:#$FB84; Attr:daIsolated; Str:#$068C),      // ARABIC LETTER DAHAL ISOLATED FORM
    (Unicode:#$FB85; Attr:daFinal; Str:#$068C),         // ARABIC LETTER DAHAL FINAL FORM
    (Unicode:#$FB86; Attr:daIsolated; Str:#$068E),      // ARABIC LETTER DUL ISOLATED FORM
    (Unicode:#$FB87; Attr:daFinal; Str:#$068E),         // ARABIC LETTER DUL FINAL FORM
    (Unicode:#$FB88; Attr:daIsolated; Str:#$0688),      // ARABIC LETTER DDAL ISOLATED FORM
    (Unicode:#$FB89; Attr:daFinal; Str:#$0688),         // ARABIC LETTER DDAL FINAL FORM
    (Unicode:#$FB8A; Attr:daIsolated; Str:#$0698),      // ARABIC LETTER JEH ISOLATED FORM
    (Unicode:#$FB8B; Attr:daFinal; Str:#$0698),         // ARABIC LETTER JEH FINAL FORM
    (Unicode:#$FB8C; Attr:daIsolated; Str:#$0691),      // ARABIC LETTER RREH ISOLATED FORM
    (Unicode:#$FB8D; Attr:daFinal; Str:#$0691),         // ARABIC LETTER RREH FINAL FORM
    (Unicode:#$FB8E; Attr:daIsolated; Str:#$06A9),      // ARABIC LETTER KEHEH ISOLATED FORM
    (Unicode:#$FB8F; Attr:daFinal; Str:#$06A9),         // ARABIC LETTER KEHEH FINAL FORM
    (Unicode:#$FB90; Attr:daInitial; Str:#$06A9),       // ARABIC LETTER KEHEH INITIAL FORM
    (Unicode:#$FB91; Attr:daMedial; Str:#$06A9),        // ARABIC LETTER KEHEH MEDIAL FORM
    (Unicode:#$FB92; Attr:daIsolated; Str:#$06AF),      // ARABIC LETTER GAF ISOLATED FORM
    (Unicode:#$FB93; Attr:daFinal; Str:#$06AF),         // ARABIC LETTER GAF FINAL FORM
    (Unicode:#$FB94; Attr:daInitial; Str:#$06AF),       // ARABIC LETTER GAF INITIAL FORM
    (Unicode:#$FB95; Attr:daMedial; Str:#$06AF),        // ARABIC LETTER GAF MEDIAL FORM
    (Unicode:#$FB96; Attr:daIsolated; Str:#$06B3),      // ARABIC LETTER GUEH ISOLATED FORM
    (Unicode:#$FB97; Attr:daFinal; Str:#$06B3),         // ARABIC LETTER GUEH FINAL FORM
    (Unicode:#$FB98; Attr:daInitial; Str:#$06B3),       // ARABIC LETTER GUEH INITIAL FORM
    (Unicode:#$FB99; Attr:daMedial; Str:#$06B3),        // ARABIC LETTER GUEH MEDIAL FORM
    (Unicode:#$FB9A; Attr:daIsolated; Str:#$06B1),      // ARABIC LETTER NGOEH ISOLATED FORM
    (Unicode:#$FB9B; Attr:daFinal; Str:#$06B1),         // ARABIC LETTER NGOEH FINAL FORM
    (Unicode:#$FB9C; Attr:daInitial; Str:#$06B1),       // ARABIC LETTER NGOEH INITIAL FORM
    (Unicode:#$FB9D; Attr:daMedial; Str:#$06B1),        // ARABIC LETTER NGOEH MEDIAL FORM
    (Unicode:#$FB9E; Attr:daIsolated; Str:#$06BA),      // ARABIC LETTER NOON GHUNNA ISOLATED FORM
    (Unicode:#$FB9F; Attr:daFinal; Str:#$06BA),         // ARABIC LETTER NOON GHUNNA FINAL FORM
    (Unicode:#$FBA0; Attr:daIsolated; Str:#$06BB),      // ARABIC LETTER RNOON ISOLATED FORM
    (Unicode:#$FBA1; Attr:daFinal; Str:#$06BB),         // ARABIC LETTER RNOON FINAL FORM
    (Unicode:#$FBA2; Attr:daInitial; Str:#$06BB),       // ARABIC LETTER RNOON INITIAL FORM
    (Unicode:#$FBA3; Attr:daMedial; Str:#$06BB),        // ARABIC LETTER RNOON MEDIAL FORM
    (Unicode:#$FBA4; Attr:daIsolated; Str:#$06C0),      // ARABIC LETTER HEH WITH YEH ABOVE ISOLATED FORM
    (Unicode:#$FBA5; Attr:daFinal; Str:#$06C0),         // ARABIC LETTER HEH WITH YEH ABOVE FINAL FORM
    (Unicode:#$FBA6; Attr:daIsolated; Str:#$06C1),      // ARABIC LETTER HEH GOAL ISOLATED FORM
    (Unicode:#$FBA7; Attr:daFinal; Str:#$06C1),         // ARABIC LETTER HEH GOAL FINAL FORM
    (Unicode:#$FBA8; Attr:daInitial; Str:#$06C1),       // ARABIC LETTER HEH GOAL INITIAL FORM
    (Unicode:#$FBA9; Attr:daMedial; Str:#$06C1),        // ARABIC LETTER HEH GOAL MEDIAL FORM
    (Unicode:#$FBAA; Attr:daIsolated; Str:#$06BE),      // ARABIC LETTER HEH DOACHASHMEE ISOLATED FORM
    (Unicode:#$FBAB; Attr:daFinal; Str:#$06BE),         // ARABIC LETTER HEH DOACHASHMEE FINAL FORM
    (Unicode:#$FBAC; Attr:daInitial; Str:#$06BE),       // ARABIC LETTER HEH DOACHASHMEE INITIAL FORM
    (Unicode:#$FBAD; Attr:daMedial; Str:#$06BE),        // ARABIC LETTER HEH DOACHASHMEE MEDIAL FORM
    (Unicode:#$FBAE; Attr:daIsolated; Str:#$06D2),      // ARABIC LETTER YEH BARREE ISOLATED FORM
    (Unicode:#$FBAF; Attr:daFinal; Str:#$06D2),         // ARABIC LETTER YEH BARREE FINAL FORM
    (Unicode:#$FBB0; Attr:daIsolated; Str:#$06D3),      // ARABIC LETTER YEH BARREE WITH HAMZA ABOVE ISOLATED FORM
    (Unicode:#$FBB1; Attr:daFinal; Str:#$06D3),         // ARABIC LETTER YEH BARREE WITH HAMZA ABOVE FINAL FORM
    (Unicode:#$FBD3; Attr:daIsolated; Str:#$06AD),      // ARABIC LETTER NG ISOLATED FORM
    (Unicode:#$FBD4; Attr:daFinal; Str:#$06AD),         // ARABIC LETTER NG FINAL FORM
    (Unicode:#$FBD5; Attr:daInitial; Str:#$06AD),       // ARABIC LETTER NG INITIAL FORM
    (Unicode:#$FBD6; Attr:daMedial; Str:#$06AD),        // ARABIC LETTER NG MEDIAL FORM
    (Unicode:#$FBD7; Attr:daIsolated; Str:#$06C7),      // ARABIC LETTER U ISOLATED FORM
    (Unicode:#$FBD8; Attr:daFinal; Str:#$06C7),         // ARABIC LETTER U FINAL FORM
    (Unicode:#$FBD9; Attr:daIsolated; Str:#$06C6),      // ARABIC LETTER OE ISOLATED FORM
    (Unicode:#$FBDA; Attr:daFinal; Str:#$06C6),         // ARABIC LETTER OE FINAL FORM
    (Unicode:#$FBDB; Attr:daIsolated; Str:#$06C8),      // ARABIC LETTER YU ISOLATED FORM
    (Unicode:#$FBDC; Attr:daFinal; Str:#$06C8),         // ARABIC LETTER YU FINAL FORM
    (Unicode:#$FBDD; Attr:daIsolated; Str:#$0677),      // ARABIC LETTER U WITH HAMZA ABOVE ISOLATED FORM
    (Unicode:#$FBDE; Attr:daIsolated; Str:#$06CB),      // ARABIC LETTER VE ISOLATED FORM
    (Unicode:#$FBDF; Attr:daFinal; Str:#$06CB),         // ARABIC LETTER VE FINAL FORM
    (Unicode:#$FBE0; Attr:daIsolated; Str:#$06C5),      // ARABIC LETTER KIRGHIZ OE ISOLATED FORM
    (Unicode:#$FBE1; Attr:daFinal; Str:#$06C5),         // ARABIC LETTER KIRGHIZ OE FINAL FORM
    (Unicode:#$FBE2; Attr:daIsolated; Str:#$06C9),      // ARABIC LETTER KIRGHIZ YU ISOLATED FORM
    (Unicode:#$FBE3; Attr:daFinal; Str:#$06C9),         // ARABIC LETTER KIRGHIZ YU FINAL FORM
    (Unicode:#$FBE4; Attr:daIsolated; Str:#$06D0),      // ARABIC LETTER E ISOLATED FORM
    (Unicode:#$FBE5; Attr:daFinal; Str:#$06D0),         // ARABIC LETTER E FINAL FORM
    (Unicode:#$FBE6; Attr:daInitial; Str:#$06D0),       // ARABIC LETTER E INITIAL FORM
    (Unicode:#$FBE7; Attr:daMedial; Str:#$06D0),        // ARABIC LETTER E MEDIAL FORM
    (Unicode:#$FBE8; Attr:daInitial; Str:#$0649),       // ARABIC LETTER UIGHUR KAZAKH KIRGHIZ ALEF MAKSURA INITIAL FORM
    (Unicode:#$FBE9; Attr:daMedial; Str:#$0649),        // ARABIC LETTER UIGHUR KAZAKH KIRGHIZ ALEF MAKSURA MEDIAL FORM
    (Unicode:#$FBEA; Attr:daIsolated; Str:#$0626#$0627),// ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH ALEF ISOLATED FORM
    (Unicode:#$FBEB; Attr:daFinal; Str:#$0626#$0627),   // ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH ALEF FINAL FORM
    (Unicode:#$FBEC; Attr:daIsolated; Str:#$0626#$06D5),// ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH AE ISOLATED FORM
    (Unicode:#$FBED; Attr:daFinal; Str:#$0626#$06D5),   // ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH AE FINAL FORM
    (Unicode:#$FBEE; Attr:daIsolated; Str:#$0626#$0648),// ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH WAW ISOLATED FORM
    (Unicode:#$FBEF; Attr:daFinal; Str:#$0626#$0648),   // ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH WAW FINAL FORM
    (Unicode:#$FBF0; Attr:daIsolated; Str:#$0626#$06C7),// ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH U ISOLATED FORM
    (Unicode:#$FBF1; Attr:daFinal; Str:#$0626#$06C7),   // ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH U FINAL FORM
    (Unicode:#$FBF2; Attr:daIsolated; Str:#$0626#$06C6),// ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH OE ISOLATED FORM
    (Unicode:#$FBF3; Attr:daFinal; Str:#$0626#$06C6),   // ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH OE FINAL FORM
    (Unicode:#$FBF4; Attr:daIsolated; Str:#$0626#$06C8),// ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH YU ISOLATED FORM
    (Unicode:#$FBF5; Attr:daFinal; Str:#$0626#$06C8),   // ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH YU FINAL FORM
    (Unicode:#$FBF6; Attr:daIsolated; Str:#$0626#$06D0),// ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH E ISOLATED FORM
    (Unicode:#$FBF7; Attr:daFinal; Str:#$0626#$06D0),   // ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH E FINAL FORM
    (Unicode:#$FBF8; Attr:daInitial; Str:#$0626#$06D0), // ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH E INITIAL FORM
    (Unicode:#$FBF9; Attr:daIsolated; Str:#$0626#$0649),// ARABIC LIGATURE UIGHUR KIRGHIZ YEH WITH HAMZA ABOVE WITH ALEF MAKSURA ISOLATED FORM
    (Unicode:#$FBFA; Attr:daFinal; Str:#$0626#$0649),   // ARABIC LIGATURE UIGHUR KIRGHIZ YEH WITH HAMZA ABOVE WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FBFB; Attr:daInitial; Str:#$0626#$0649), // ARABIC LIGATURE UIGHUR KIRGHIZ YEH WITH HAMZA ABOVE WITH ALEF MAKSURA INITIAL FORM
    (Unicode:#$FBFC; Attr:daIsolated; Str:#$06CC),      // ARABIC LETTER FARSI YEH ISOLATED FORM
    (Unicode:#$FBFD; Attr:daFinal; Str:#$06CC),         // ARABIC LETTER FARSI YEH FINAL FORM
    (Unicode:#$FBFE; Attr:daInitial; Str:#$06CC),       // ARABIC LETTER FARSI YEH INITIAL FORM
    (Unicode:#$FBFF; Attr:daMedial; Str:#$06CC),        // ARABIC LETTER FARSI YEH MEDIAL FORM
    (Unicode:#$FC00; Attr:daIsolated; Str:#$0626#$062C),// ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH JEEM ISOLATED FORM
    (Unicode:#$FC01; Attr:daIsolated; Str:#$0626#$062D),// ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH HAH ISOLATED FORM
    (Unicode:#$FC02; Attr:daIsolated; Str:#$0626#$0645),// ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH MEEM ISOLATED FORM
    (Unicode:#$FC03; Attr:daIsolated; Str:#$0626#$0649),// ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH ALEF MAKSURA ISOLATED FORM
    (Unicode:#$FC04; Attr:daIsolated; Str:#$0626#$064A),// ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH YEH ISOLATED FORM
    (Unicode:#$FC05; Attr:daIsolated; Str:#$0628#$062C),// ARABIC LIGATURE BEH WITH JEEM ISOLATED FORM
    (Unicode:#$FC06; Attr:daIsolated; Str:#$0628#$062D),// ARABIC LIGATURE BEH WITH HAH ISOLATED FORM
    (Unicode:#$FC07; Attr:daIsolated; Str:#$0628#$062E),// ARABIC LIGATURE BEH WITH KHAH ISOLATED FORM
    (Unicode:#$FC08; Attr:daIsolated; Str:#$0628#$0645),// ARABIC LIGATURE BEH WITH MEEM ISOLATED FORM
    (Unicode:#$FC09; Attr:daIsolated; Str:#$0628#$0649),// ARABIC LIGATURE BEH WITH ALEF MAKSURA ISOLATED FORM
    (Unicode:#$FC0A; Attr:daIsolated; Str:#$0628#$064A),// ARABIC LIGATURE BEH WITH YEH ISOLATED FORM
    (Unicode:#$FC0B; Attr:daIsolated; Str:#$062A#$062C),// ARABIC LIGATURE TEH WITH JEEM ISOLATED FORM
    (Unicode:#$FC0C; Attr:daIsolated; Str:#$062A#$062D),// ARABIC LIGATURE TEH WITH HAH ISOLATED FORM
    (Unicode:#$FC0D; Attr:daIsolated; Str:#$062A#$062E),// ARABIC LIGATURE TEH WITH KHAH ISOLATED FORM
    (Unicode:#$FC0E; Attr:daIsolated; Str:#$062A#$0645),// ARABIC LIGATURE TEH WITH MEEM ISOLATED FORM
    (Unicode:#$FC0F; Attr:daIsolated; Str:#$062A#$0649),// ARABIC LIGATURE TEH WITH ALEF MAKSURA ISOLATED FORM
    (Unicode:#$FC10; Attr:daIsolated; Str:#$062A#$064A),// ARABIC LIGATURE TEH WITH YEH ISOLATED FORM
    (Unicode:#$FC11; Attr:daIsolated; Str:#$062B#$062C),// ARABIC LIGATURE THEH WITH JEEM ISOLATED FORM
    (Unicode:#$FC12; Attr:daIsolated; Str:#$062B#$0645),// ARABIC LIGATURE THEH WITH MEEM ISOLATED FORM
    (Unicode:#$FC13; Attr:daIsolated; Str:#$062B#$0649),// ARABIC LIGATURE THEH WITH ALEF MAKSURA ISOLATED FORM
    (Unicode:#$FC14; Attr:daIsolated; Str:#$062B#$064A),// ARABIC LIGATURE THEH WITH YEH ISOLATED FORM
    (Unicode:#$FC15; Attr:daIsolated; Str:#$062C#$062D),// ARABIC LIGATURE JEEM WITH HAH ISOLATED FORM
    (Unicode:#$FC16; Attr:daIsolated; Str:#$062C#$0645),// ARABIC LIGATURE JEEM WITH MEEM ISOLATED FORM
    (Unicode:#$FC17; Attr:daIsolated; Str:#$062D#$062C),// ARABIC LIGATURE HAH WITH JEEM ISOLATED FORM
    (Unicode:#$FC18; Attr:daIsolated; Str:#$062D#$0645),// ARABIC LIGATURE HAH WITH MEEM ISOLATED FORM
    (Unicode:#$FC19; Attr:daIsolated; Str:#$062E#$062C),// ARABIC LIGATURE KHAH WITH JEEM ISOLATED FORM
    (Unicode:#$FC1A; Attr:daIsolated; Str:#$062E#$062D),// ARABIC LIGATURE KHAH WITH HAH ISOLATED FORM
    (Unicode:#$FC1B; Attr:daIsolated; Str:#$062E#$0645),// ARABIC LIGATURE KHAH WITH MEEM ISOLATED FORM
    (Unicode:#$FC1C; Attr:daIsolated; Str:#$0633#$062C),// ARABIC LIGATURE SEEN WITH JEEM ISOLATED FORM
    (Unicode:#$FC1D; Attr:daIsolated; Str:#$0633#$062D),// ARABIC LIGATURE SEEN WITH HAH ISOLATED FORM
    (Unicode:#$FC1E; Attr:daIsolated; Str:#$0633#$062E),// ARABIC LIGATURE SEEN WITH KHAH ISOLATED FORM
    (Unicode:#$FC1F; Attr:daIsolated; Str:#$0633#$0645),// ARABIC LIGATURE SEEN WITH MEEM ISOLATED FORM
    (Unicode:#$FC20; Attr:daIsolated; Str:#$0635#$062D),// ARABIC LIGATURE SAD WITH HAH ISOLATED FORM
    (Unicode:#$FC21; Attr:daIsolated; Str:#$0635#$0645),// ARABIC LIGATURE SAD WITH MEEM ISOLATED FORM
    (Unicode:#$FC22; Attr:daIsolated; Str:#$0636#$062C),// ARABIC LIGATURE DAD WITH JEEM ISOLATED FORM
    (Unicode:#$FC23; Attr:daIsolated; Str:#$0636#$062D),// ARABIC LIGATURE DAD WITH HAH ISOLATED FORM
    (Unicode:#$FC24; Attr:daIsolated; Str:#$0636#$062E),// ARABIC LIGATURE DAD WITH KHAH ISOLATED FORM
    (Unicode:#$FC25; Attr:daIsolated; Str:#$0636#$0645),// ARABIC LIGATURE DAD WITH MEEM ISOLATED FORM
    (Unicode:#$FC26; Attr:daIsolated; Str:#$0637#$062D),// ARABIC LIGATURE TAH WITH HAH ISOLATED FORM
    (Unicode:#$FC27; Attr:daIsolated; Str:#$0637#$0645),// ARABIC LIGATURE TAH WITH MEEM ISOLATED FORM
    (Unicode:#$FC28; Attr:daIsolated; Str:#$0638#$0645),// ARABIC LIGATURE ZAH WITH MEEM ISOLATED FORM
    (Unicode:#$FC29; Attr:daIsolated; Str:#$0639#$062C),// ARABIC LIGATURE AIN WITH JEEM ISOLATED FORM
    (Unicode:#$FC2A; Attr:daIsolated; Str:#$0639#$0645),// ARABIC LIGATURE AIN WITH MEEM ISOLATED FORM
    (Unicode:#$FC2B; Attr:daIsolated; Str:#$063A#$062C),// ARABIC LIGATURE GHAIN WITH JEEM ISOLATED FORM
    (Unicode:#$FC2C; Attr:daIsolated; Str:#$063A#$0645),// ARABIC LIGATURE GHAIN WITH MEEM ISOLATED FORM
    (Unicode:#$FC2D; Attr:daIsolated; Str:#$0641#$062C),// ARABIC LIGATURE FEH WITH JEEM ISOLATED FORM
    (Unicode:#$FC2E; Attr:daIsolated; Str:#$0641#$062D),// ARABIC LIGATURE FEH WITH HAH ISOLATED FORM
    (Unicode:#$FC2F; Attr:daIsolated; Str:#$0641#$062E),// ARABIC LIGATURE FEH WITH KHAH ISOLATED FORM
    (Unicode:#$FC30; Attr:daIsolated; Str:#$0641#$0645),// ARABIC LIGATURE FEH WITH MEEM ISOLATED FORM
    (Unicode:#$FC31; Attr:daIsolated; Str:#$0641#$0649),// ARABIC LIGATURE FEH WITH ALEF MAKSURA ISOLATED FORM
    (Unicode:#$FC32; Attr:daIsolated; Str:#$0641#$064A),// ARABIC LIGATURE FEH WITH YEH ISOLATED FORM
    (Unicode:#$FC33; Attr:daIsolated; Str:#$0642#$062D),// ARABIC LIGATURE QAF WITH HAH ISOLATED FORM
    (Unicode:#$FC34; Attr:daIsolated; Str:#$0642#$0645),// ARABIC LIGATURE QAF WITH MEEM ISOLATED FORM
    (Unicode:#$FC35; Attr:daIsolated; Str:#$0642#$0649),// ARABIC LIGATURE QAF WITH ALEF MAKSURA ISOLATED FORM
    (Unicode:#$FC36; Attr:daIsolated; Str:#$0642#$064A),// ARABIC LIGATURE QAF WITH YEH ISOLATED FORM
    (Unicode:#$FC37; Attr:daIsolated; Str:#$0643#$0627),// ARABIC LIGATURE KAF WITH ALEF ISOLATED FORM
    (Unicode:#$FC38; Attr:daIsolated; Str:#$0643#$062C),// ARABIC LIGATURE KAF WITH JEEM ISOLATED FORM
    (Unicode:#$FC39; Attr:daIsolated; Str:#$0643#$062D),// ARABIC LIGATURE KAF WITH HAH ISOLATED FORM
    (Unicode:#$FC3A; Attr:daIsolated; Str:#$0643#$062E),// ARABIC LIGATURE KAF WITH KHAH ISOLATED FORM
    (Unicode:#$FC3B; Attr:daIsolated; Str:#$0643#$0644),// ARABIC LIGATURE KAF WITH LAM ISOLATED FORM
    (Unicode:#$FC3C; Attr:daIsolated; Str:#$0643#$0645),// ARABIC LIGATURE KAF WITH MEEM ISOLATED FORM
    (Unicode:#$FC3D; Attr:daIsolated; Str:#$0643#$0649),// ARABIC LIGATURE KAF WITH ALEF MAKSURA ISOLATED FORM
    (Unicode:#$FC3E; Attr:daIsolated; Str:#$0643#$064A),// ARABIC LIGATURE KAF WITH YEH ISOLATED FORM
    (Unicode:#$FC3F; Attr:daIsolated; Str:#$0644#$062C),// ARABIC LIGATURE LAM WITH JEEM ISOLATED FORM
    (Unicode:#$FC40; Attr:daIsolated; Str:#$0644#$062D),// ARABIC LIGATURE LAM WITH HAH ISOLATED FORM
    (Unicode:#$FC41; Attr:daIsolated; Str:#$0644#$062E),// ARABIC LIGATURE LAM WITH KHAH ISOLATED FORM
    (Unicode:#$FC42; Attr:daIsolated; Str:#$0644#$0645),// ARABIC LIGATURE LAM WITH MEEM ISOLATED FORM
    (Unicode:#$FC43; Attr:daIsolated; Str:#$0644#$0649),// ARABIC LIGATURE LAM WITH ALEF MAKSURA ISOLATED FORM
    (Unicode:#$FC44; Attr:daIsolated; Str:#$0644#$064A),// ARABIC LIGATURE LAM WITH YEH ISOLATED FORM
    (Unicode:#$FC45; Attr:daIsolated; Str:#$0645#$062C),// ARABIC LIGATURE MEEM WITH JEEM ISOLATED FORM
    (Unicode:#$FC46; Attr:daIsolated; Str:#$0645#$062D),// ARABIC LIGATURE MEEM WITH HAH ISOLATED FORM
    (Unicode:#$FC47; Attr:daIsolated; Str:#$0645#$062E),// ARABIC LIGATURE MEEM WITH KHAH ISOLATED FORM
    (Unicode:#$FC48; Attr:daIsolated; Str:#$0645#$0645),// ARABIC LIGATURE MEEM WITH MEEM ISOLATED FORM
    (Unicode:#$FC49; Attr:daIsolated; Str:#$0645#$0649),// ARABIC LIGATURE MEEM WITH ALEF MAKSURA ISOLATED FORM
    (Unicode:#$FC4A; Attr:daIsolated; Str:#$0645#$064A),// ARABIC LIGATURE MEEM WITH YEH ISOLATED FORM
    (Unicode:#$FC4B; Attr:daIsolated; Str:#$0646#$062C),// ARABIC LIGATURE NOON WITH JEEM ISOLATED FORM
    (Unicode:#$FC4C; Attr:daIsolated; Str:#$0646#$062D),// ARABIC LIGATURE NOON WITH HAH ISOLATED FORM
    (Unicode:#$FC4D; Attr:daIsolated; Str:#$0646#$062E),// ARABIC LIGATURE NOON WITH KHAH ISOLATED FORM
    (Unicode:#$FC4E; Attr:daIsolated; Str:#$0646#$0645),// ARABIC LIGATURE NOON WITH MEEM ISOLATED FORM
    (Unicode:#$FC4F; Attr:daIsolated; Str:#$0646#$0649),// ARABIC LIGATURE NOON WITH ALEF MAKSURA ISOLATED FORM
    (Unicode:#$FC50; Attr:daIsolated; Str:#$0646#$064A),// ARABIC LIGATURE NOON WITH YEH ISOLATED FORM
    (Unicode:#$FC51; Attr:daIsolated; Str:#$0647#$062C),// ARABIC LIGATURE HEH WITH JEEM ISOLATED FORM
    (Unicode:#$FC52; Attr:daIsolated; Str:#$0647#$0645),// ARABIC LIGATURE HEH WITH MEEM ISOLATED FORM
    (Unicode:#$FC53; Attr:daIsolated; Str:#$0647#$0649),// ARABIC LIGATURE HEH WITH ALEF MAKSURA ISOLATED FORM
    (Unicode:#$FC54; Attr:daIsolated; Str:#$0647#$064A),// ARABIC LIGATURE HEH WITH YEH ISOLATED FORM
    (Unicode:#$FC55; Attr:daIsolated; Str:#$064A#$062C),// ARABIC LIGATURE YEH WITH JEEM ISOLATED FORM
    (Unicode:#$FC56; Attr:daIsolated; Str:#$064A#$062D),// ARABIC LIGATURE YEH WITH HAH ISOLATED FORM
    (Unicode:#$FC57; Attr:daIsolated; Str:#$064A#$062E),// ARABIC LIGATURE YEH WITH KHAH ISOLATED FORM
    (Unicode:#$FC58; Attr:daIsolated; Str:#$064A#$0645),// ARABIC LIGATURE YEH WITH MEEM ISOLATED FORM
    (Unicode:#$FC59; Attr:daIsolated; Str:#$064A#$0649),// ARABIC LIGATURE YEH WITH ALEF MAKSURA ISOLATED FORM
    (Unicode:#$FC5A; Attr:daIsolated; Str:#$064A#$064A),// ARABIC LIGATURE YEH WITH YEH ISOLATED FORM
    (Unicode:#$FC5B; Attr:daIsolated; Str:#$0630#$0670),// ARABIC LIGATURE THAL WITH SUPERSCRIPT ALEF ISOLATED FORM
    (Unicode:#$FC5C; Attr:daIsolated; Str:#$0631#$0670),// ARABIC LIGATURE REH WITH SUPERSCRIPT ALEF ISOLATED FORM
    (Unicode:#$FC5D; Attr:daIsolated; Str:#$0649#$0670),// ARABIC LIGATURE ALEF MAKSURA WITH SUPERSCRIPT ALEF ISOLATED FORM
    (Unicode:#$FC5E; Attr:daIsolated; Str:#$0020#$064C#$0651), // ARABIC LIGATURE SHADDA WITH DAMMATAN ISOLATED FORM
    (Unicode:#$FC5F; Attr:daIsolated; Str:#$0020#$064D#$0651), // ARABIC LIGATURE SHADDA WITH KASRATAN ISOLATED FORM
    (Unicode:#$FC60; Attr:daIsolated; Str:#$0020#$064E#$0651), // ARABIC LIGATURE SHADDA WITH FATHA ISOLATED FORM
    (Unicode:#$FC61; Attr:daIsolated; Str:#$0020#$064F#$0651), // ARABIC LIGATURE SHADDA WITH DAMMA ISOLATED FORM
    (Unicode:#$FC62; Attr:daIsolated; Str:#$0020#$0650#$0651), // ARABIC LIGATURE SHADDA WITH KASRA ISOLATED FORM
    (Unicode:#$FC63; Attr:daIsolated; Str:#$0020#$0651#$0670), // ARABIC LIGATURE SHADDA WITH SUPERSCRIPT ALEF ISOLATED FORM
    (Unicode:#$FC64; Attr:daFinal; Str:#$0626#$0631),   // ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH REH FINAL FORM
    (Unicode:#$FC65; Attr:daFinal; Str:#$0626#$0632),   // ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH ZAIN FINAL FORM
    (Unicode:#$FC66; Attr:daFinal; Str:#$0626#$0645),   // ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH MEEM FINAL FORM
    (Unicode:#$FC67; Attr:daFinal; Str:#$0626#$0646),   // ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH NOON FINAL FORM
    (Unicode:#$FC68; Attr:daFinal; Str:#$0626#$0649),   // ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FC69; Attr:daFinal; Str:#$0626#$064A),   // ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH YEH FINAL FORM
    (Unicode:#$FC6A; Attr:daFinal; Str:#$0628#$0631),   // ARABIC LIGATURE BEH WITH REH FINAL FORM
    (Unicode:#$FC6B; Attr:daFinal; Str:#$0628#$0632),   // ARABIC LIGATURE BEH WITH ZAIN FINAL FORM
    (Unicode:#$FC6C; Attr:daFinal; Str:#$0628#$0645),   // ARABIC LIGATURE BEH WITH MEEM FINAL FORM
    (Unicode:#$FC6D; Attr:daFinal; Str:#$0628#$0646),   // ARABIC LIGATURE BEH WITH NOON FINAL FORM
    (Unicode:#$FC6E; Attr:daFinal; Str:#$0628#$0649),   // ARABIC LIGATURE BEH WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FC6F; Attr:daFinal; Str:#$0628#$064A),   // ARABIC LIGATURE BEH WITH YEH FINAL FORM
    (Unicode:#$FC70; Attr:daFinal; Str:#$062A#$0631),   // ARABIC LIGATURE TEH WITH REH FINAL FORM
    (Unicode:#$FC71; Attr:daFinal; Str:#$062A#$0632),   // ARABIC LIGATURE TEH WITH ZAIN FINAL FORM
    (Unicode:#$FC72; Attr:daFinal; Str:#$062A#$0645),   // ARABIC LIGATURE TEH WITH MEEM FINAL FORM
    (Unicode:#$FC73; Attr:daFinal; Str:#$062A#$0646),   // ARABIC LIGATURE TEH WITH NOON FINAL FORM
    (Unicode:#$FC74; Attr:daFinal; Str:#$062A#$0649),   // ARABIC LIGATURE TEH WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FC75; Attr:daFinal; Str:#$062A#$064A),   // ARABIC LIGATURE TEH WITH YEH FINAL FORM
    (Unicode:#$FC76; Attr:daFinal; Str:#$062B#$0631),   // ARABIC LIGATURE THEH WITH REH FINAL FORM
    (Unicode:#$FC77; Attr:daFinal; Str:#$062B#$0632),   // ARABIC LIGATURE THEH WITH ZAIN FINAL FORM
    (Unicode:#$FC78; Attr:daFinal; Str:#$062B#$0645),   // ARABIC LIGATURE THEH WITH MEEM FINAL FORM
    (Unicode:#$FC79; Attr:daFinal; Str:#$062B#$0646),   // ARABIC LIGATURE THEH WITH NOON FINAL FORM
    (Unicode:#$FC7A; Attr:daFinal; Str:#$062B#$0649),   // ARABIC LIGATURE THEH WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FC7B; Attr:daFinal; Str:#$062B#$064A),   // ARABIC LIGATURE THEH WITH YEH FINAL FORM
    (Unicode:#$FC7C; Attr:daFinal; Str:#$0641#$0649),   // ARABIC LIGATURE FEH WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FC7D; Attr:daFinal; Str:#$0641#$064A),   // ARABIC LIGATURE FEH WITH YEH FINAL FORM
    (Unicode:#$FC7E; Attr:daFinal; Str:#$0642#$0649),   // ARABIC LIGATURE QAF WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FC7F; Attr:daFinal; Str:#$0642#$064A),   // ARABIC LIGATURE QAF WITH YEH FINAL FORM
    (Unicode:#$FC80; Attr:daFinal; Str:#$0643#$0627),   // ARABIC LIGATURE KAF WITH ALEF FINAL FORM
    (Unicode:#$FC81; Attr:daFinal; Str:#$0643#$0644),   // ARABIC LIGATURE KAF WITH LAM FINAL FORM
    (Unicode:#$FC82; Attr:daFinal; Str:#$0643#$0645),   // ARABIC LIGATURE KAF WITH MEEM FINAL FORM
    (Unicode:#$FC83; Attr:daFinal; Str:#$0643#$0649),   // ARABIC LIGATURE KAF WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FC84; Attr:daFinal; Str:#$0643#$064A),   // ARABIC LIGATURE KAF WITH YEH FINAL FORM
    (Unicode:#$FC85; Attr:daFinal; Str:#$0644#$0645),   // ARABIC LIGATURE LAM WITH MEEM FINAL FORM
    (Unicode:#$FC86; Attr:daFinal; Str:#$0644#$0649),   // ARABIC LIGATURE LAM WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FC87; Attr:daFinal; Str:#$0644#$064A),   // ARABIC LIGATURE LAM WITH YEH FINAL FORM
    (Unicode:#$FC88; Attr:daFinal; Str:#$0645#$0627),   // ARABIC LIGATURE MEEM WITH ALEF FINAL FORM
    (Unicode:#$FC89; Attr:daFinal; Str:#$0645#$0645),   // ARABIC LIGATURE MEEM WITH MEEM FINAL FORM
    (Unicode:#$FC8A; Attr:daFinal; Str:#$0646#$0631),   // ARABIC LIGATURE NOON WITH REH FINAL FORM
    (Unicode:#$FC8B; Attr:daFinal; Str:#$0646#$0632),   // ARABIC LIGATURE NOON WITH ZAIN FINAL FORM
    (Unicode:#$FC8C; Attr:daFinal; Str:#$0646#$0645),   // ARABIC LIGATURE NOON WITH MEEM FINAL FORM
    (Unicode:#$FC8D; Attr:daFinal; Str:#$0646#$0646),   // ARABIC LIGATURE NOON WITH NOON FINAL FORM
    (Unicode:#$FC8E; Attr:daFinal; Str:#$0646#$0649),   // ARABIC LIGATURE NOON WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FC8F; Attr:daFinal; Str:#$0646#$064A),   // ARABIC LIGATURE NOON WITH YEH FINAL FORM
    (Unicode:#$FC90; Attr:daFinal; Str:#$0649#$0670),   // ARABIC LIGATURE ALEF MAKSURA WITH SUPERSCRIPT ALEF FINAL FORM
    (Unicode:#$FC91; Attr:daFinal; Str:#$064A#$0631),   // ARABIC LIGATURE YEH WITH REH FINAL FORM
    (Unicode:#$FC92; Attr:daFinal; Str:#$064A#$0632),   // ARABIC LIGATURE YEH WITH ZAIN FINAL FORM
    (Unicode:#$FC93; Attr:daFinal; Str:#$064A#$0645),   // ARABIC LIGATURE YEH WITH MEEM FINAL FORM
    (Unicode:#$FC94; Attr:daFinal; Str:#$064A#$0646),   // ARABIC LIGATURE YEH WITH NOON FINAL FORM
    (Unicode:#$FC95; Attr:daFinal; Str:#$064A#$0649),   // ARABIC LIGATURE YEH WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FC96; Attr:daFinal; Str:#$064A#$064A),   // ARABIC LIGATURE YEH WITH YEH FINAL FORM
    (Unicode:#$FC97; Attr:daInitial; Str:#$0626#$062C), // ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH JEEM INITIAL FORM
    (Unicode:#$FC98; Attr:daInitial; Str:#$0626#$062D), // ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH HAH INITIAL FORM
    (Unicode:#$FC99; Attr:daInitial; Str:#$0626#$062E), // ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH KHAH INITIAL FORM
    (Unicode:#$FC9A; Attr:daInitial; Str:#$0626#$0645), // ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH MEEM INITIAL FORM
    (Unicode:#$FC9B; Attr:daInitial; Str:#$0626#$0647), // ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH HEH INITIAL FORM
    (Unicode:#$FC9C; Attr:daInitial; Str:#$0628#$062C), // ARABIC LIGATURE BEH WITH JEEM INITIAL FORM
    (Unicode:#$FC9D; Attr:daInitial; Str:#$0628#$062D), // ARABIC LIGATURE BEH WITH HAH INITIAL FORM
    (Unicode:#$FC9E; Attr:daInitial; Str:#$0628#$062E), // ARABIC LIGATURE BEH WITH KHAH INITIAL FORM
    (Unicode:#$FC9F; Attr:daInitial; Str:#$0628#$0645), // ARABIC LIGATURE BEH WITH MEEM INITIAL FORM
    (Unicode:#$FCA0; Attr:daInitial; Str:#$0628#$0647), // ARABIC LIGATURE BEH WITH HEH INITIAL FORM
    (Unicode:#$FCA1; Attr:daInitial; Str:#$062A#$062C), // ARABIC LIGATURE TEH WITH JEEM INITIAL FORM
    (Unicode:#$FCA2; Attr:daInitial; Str:#$062A#$062D), // ARABIC LIGATURE TEH WITH HAH INITIAL FORM
    (Unicode:#$FCA3; Attr:daInitial; Str:#$062A#$062E), // ARABIC LIGATURE TEH WITH KHAH INITIAL FORM
    (Unicode:#$FCA4; Attr:daInitial; Str:#$062A#$0645), // ARABIC LIGATURE TEH WITH MEEM INITIAL FORM
    (Unicode:#$FCA5; Attr:daInitial; Str:#$062A#$0647), // ARABIC LIGATURE TEH WITH HEH INITIAL FORM
    (Unicode:#$FCA6; Attr:daInitial; Str:#$062B#$0645), // ARABIC LIGATURE THEH WITH MEEM INITIAL FORM
    (Unicode:#$FCA7; Attr:daInitial; Str:#$062C#$062D), // ARABIC LIGATURE JEEM WITH HAH INITIAL FORM
    (Unicode:#$FCA8; Attr:daInitial; Str:#$062C#$0645), // ARABIC LIGATURE JEEM WITH MEEM INITIAL FORM
    (Unicode:#$FCA9; Attr:daInitial; Str:#$062D#$062C), // ARABIC LIGATURE HAH WITH JEEM INITIAL FORM
    (Unicode:#$FCAA; Attr:daInitial; Str:#$062D#$0645), // ARABIC LIGATURE HAH WITH MEEM INITIAL FORM
    (Unicode:#$FCAB; Attr:daInitial; Str:#$062E#$062C), // ARABIC LIGATURE KHAH WITH JEEM INITIAL FORM
    (Unicode:#$FCAC; Attr:daInitial; Str:#$062E#$0645), // ARABIC LIGATURE KHAH WITH MEEM INITIAL FORM
    (Unicode:#$FCAD; Attr:daInitial; Str:#$0633#$062C), // ARABIC LIGATURE SEEN WITH JEEM INITIAL FORM
    (Unicode:#$FCAE; Attr:daInitial; Str:#$0633#$062D), // ARABIC LIGATURE SEEN WITH HAH INITIAL FORM
    (Unicode:#$FCAF; Attr:daInitial; Str:#$0633#$062E), // ARABIC LIGATURE SEEN WITH KHAH INITIAL FORM
    (Unicode:#$FCB0; Attr:daInitial; Str:#$0633#$0645), // ARABIC LIGATURE SEEN WITH MEEM INITIAL FORM
    (Unicode:#$FCB1; Attr:daInitial; Str:#$0635#$062D), // ARABIC LIGATURE SAD WITH HAH INITIAL FORM
    (Unicode:#$FCB2; Attr:daInitial; Str:#$0635#$062E), // ARABIC LIGATURE SAD WITH KHAH INITIAL FORM
    (Unicode:#$FCB3; Attr:daInitial; Str:#$0635#$0645), // ARABIC LIGATURE SAD WITH MEEM INITIAL FORM
    (Unicode:#$FCB4; Attr:daInitial; Str:#$0636#$062C), // ARABIC LIGATURE DAD WITH JEEM INITIAL FORM
    (Unicode:#$FCB5; Attr:daInitial; Str:#$0636#$062D), // ARABIC LIGATURE DAD WITH HAH INITIAL FORM
    (Unicode:#$FCB6; Attr:daInitial; Str:#$0636#$062E), // ARABIC LIGATURE DAD WITH KHAH INITIAL FORM
    (Unicode:#$FCB7; Attr:daInitial; Str:#$0636#$0645), // ARABIC LIGATURE DAD WITH MEEM INITIAL FORM
    (Unicode:#$FCB8; Attr:daInitial; Str:#$0637#$062D), // ARABIC LIGATURE TAH WITH HAH INITIAL FORM
    (Unicode:#$FCB9; Attr:daInitial; Str:#$0638#$0645), // ARABIC LIGATURE ZAH WITH MEEM INITIAL FORM
    (Unicode:#$FCBA; Attr:daInitial; Str:#$0639#$062C), // ARABIC LIGATURE AIN WITH JEEM INITIAL FORM
    (Unicode:#$FCBB; Attr:daInitial; Str:#$0639#$0645), // ARABIC LIGATURE AIN WITH MEEM INITIAL FORM
    (Unicode:#$FCBC; Attr:daInitial; Str:#$063A#$062C), // ARABIC LIGATURE GHAIN WITH JEEM INITIAL FORM
    (Unicode:#$FCBD; Attr:daInitial; Str:#$063A#$0645), // ARABIC LIGATURE GHAIN WITH MEEM INITIAL FORM
    (Unicode:#$FCBE; Attr:daInitial; Str:#$0641#$062C), // ARABIC LIGATURE FEH WITH JEEM INITIAL FORM
    (Unicode:#$FCBF; Attr:daInitial; Str:#$0641#$062D), // ARABIC LIGATURE FEH WITH HAH INITIAL FORM
    (Unicode:#$FCC0; Attr:daInitial; Str:#$0641#$062E), // ARABIC LIGATURE FEH WITH KHAH INITIAL FORM
    (Unicode:#$FCC1; Attr:daInitial; Str:#$0641#$0645), // ARABIC LIGATURE FEH WITH MEEM INITIAL FORM
    (Unicode:#$FCC2; Attr:daInitial; Str:#$0642#$062D), // ARABIC LIGATURE QAF WITH HAH INITIAL FORM
    (Unicode:#$FCC3; Attr:daInitial; Str:#$0642#$0645), // ARABIC LIGATURE QAF WITH MEEM INITIAL FORM
    (Unicode:#$FCC4; Attr:daInitial; Str:#$0643#$062C), // ARABIC LIGATURE KAF WITH JEEM INITIAL FORM
    (Unicode:#$FCC5; Attr:daInitial; Str:#$0643#$062D), // ARABIC LIGATURE KAF WITH HAH INITIAL FORM
    (Unicode:#$FCC6; Attr:daInitial; Str:#$0643#$062E), // ARABIC LIGATURE KAF WITH KHAH INITIAL FORM
    (Unicode:#$FCC7; Attr:daInitial; Str:#$0643#$0644), // ARABIC LIGATURE KAF WITH LAM INITIAL FORM
    (Unicode:#$FCC8; Attr:daInitial; Str:#$0643#$0645), // ARABIC LIGATURE KAF WITH MEEM INITIAL FORM
    (Unicode:#$FCC9; Attr:daInitial; Str:#$0644#$062C), // ARABIC LIGATURE LAM WITH JEEM INITIAL FORM
    (Unicode:#$FCCA; Attr:daInitial; Str:#$0644#$062D), // ARABIC LIGATURE LAM WITH HAH INITIAL FORM
    (Unicode:#$FCCB; Attr:daInitial; Str:#$0644#$062E), // ARABIC LIGATURE LAM WITH KHAH INITIAL FORM
    (Unicode:#$FCCC; Attr:daInitial; Str:#$0644#$0645), // ARABIC LIGATURE LAM WITH MEEM INITIAL FORM
    (Unicode:#$FCCD; Attr:daInitial; Str:#$0644#$0647), // ARABIC LIGATURE LAM WITH HEH INITIAL FORM
    (Unicode:#$FCCE; Attr:daInitial; Str:#$0645#$062C), // ARABIC LIGATURE MEEM WITH JEEM INITIAL FORM
    (Unicode:#$FCCF; Attr:daInitial; Str:#$0645#$062D), // ARABIC LIGATURE MEEM WITH HAH INITIAL FORM
    (Unicode:#$FCD0; Attr:daInitial; Str:#$0645#$062E), // ARABIC LIGATURE MEEM WITH KHAH INITIAL FORM
    (Unicode:#$FCD1; Attr:daInitial; Str:#$0645#$0645), // ARABIC LIGATURE MEEM WITH MEEM INITIAL FORM
    (Unicode:#$FCD2; Attr:daInitial; Str:#$0646#$062C), // ARABIC LIGATURE NOON WITH JEEM INITIAL FORM
    (Unicode:#$FCD3; Attr:daInitial; Str:#$0646#$062D), // ARABIC LIGATURE NOON WITH HAH INITIAL FORM
    (Unicode:#$FCD4; Attr:daInitial; Str:#$0646#$062E), // ARABIC LIGATURE NOON WITH KHAH INITIAL FORM
    (Unicode:#$FCD5; Attr:daInitial; Str:#$0646#$0645), // ARABIC LIGATURE NOON WITH MEEM INITIAL FORM
    (Unicode:#$FCD6; Attr:daInitial; Str:#$0646#$0647), // ARABIC LIGATURE NOON WITH HEH INITIAL FORM
    (Unicode:#$FCD7; Attr:daInitial; Str:#$0647#$062C), // ARABIC LIGATURE HEH WITH JEEM INITIAL FORM
    (Unicode:#$FCD8; Attr:daInitial; Str:#$0647#$0645), // ARABIC LIGATURE HEH WITH MEEM INITIAL FORM
    (Unicode:#$FCD9; Attr:daInitial; Str:#$0647#$0670), // ARABIC LIGATURE HEH WITH SUPERSCRIPT ALEF INITIAL FORM
    (Unicode:#$FCDA; Attr:daInitial; Str:#$064A#$062C), // ARABIC LIGATURE YEH WITH JEEM INITIAL FORM
    (Unicode:#$FCDB; Attr:daInitial; Str:#$064A#$062D), // ARABIC LIGATURE YEH WITH HAH INITIAL FORM
    (Unicode:#$FCDC; Attr:daInitial; Str:#$064A#$062E), // ARABIC LIGATURE YEH WITH KHAH INITIAL FORM
    (Unicode:#$FCDD; Attr:daInitial; Str:#$064A#$0645), // ARABIC LIGATURE YEH WITH MEEM INITIAL FORM
    (Unicode:#$FCDE; Attr:daInitial; Str:#$064A#$0647), // ARABIC LIGATURE YEH WITH HEH INITIAL FORM
    (Unicode:#$FCDF; Attr:daMedial; Str:#$0626#$0645),  // ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH MEEM MEDIAL FORM
    (Unicode:#$FCE0; Attr:daMedial; Str:#$0626#$0647),  // ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH HEH MEDIAL FORM
    (Unicode:#$FCE1; Attr:daMedial; Str:#$0628#$0645),  // ARABIC LIGATURE BEH WITH MEEM MEDIAL FORM
    (Unicode:#$FCE2; Attr:daMedial; Str:#$0628#$0647),  // ARABIC LIGATURE BEH WITH HEH MEDIAL FORM
    (Unicode:#$FCE3; Attr:daMedial; Str:#$062A#$0645),  // ARABIC LIGATURE TEH WITH MEEM MEDIAL FORM
    (Unicode:#$FCE4; Attr:daMedial; Str:#$062A#$0647),  // ARABIC LIGATURE TEH WITH HEH MEDIAL FORM
    (Unicode:#$FCE5; Attr:daMedial; Str:#$062B#$0645),  // ARABIC LIGATURE THEH WITH MEEM MEDIAL FORM
    (Unicode:#$FCE6; Attr:daMedial; Str:#$062B#$0647),  // ARABIC LIGATURE THEH WITH HEH MEDIAL FORM
    (Unicode:#$FCE7; Attr:daMedial; Str:#$0633#$0645),  // ARABIC LIGATURE SEEN WITH MEEM MEDIAL FORM
    (Unicode:#$FCE8; Attr:daMedial; Str:#$0633#$0647),  // ARABIC LIGATURE SEEN WITH HEH MEDIAL FORM
    (Unicode:#$FCE9; Attr:daMedial; Str:#$0634#$0645),  // ARABIC LIGATURE SHEEN WITH MEEM MEDIAL FORM
    (Unicode:#$FCEA; Attr:daMedial; Str:#$0634#$0647),  // ARABIC LIGATURE SHEEN WITH HEH MEDIAL FORM
    (Unicode:#$FCEB; Attr:daMedial; Str:#$0643#$0644),  // ARABIC LIGATURE KAF WITH LAM MEDIAL FORM
    (Unicode:#$FCEC; Attr:daMedial; Str:#$0643#$0645),  // ARABIC LIGATURE KAF WITH MEEM MEDIAL FORM
    (Unicode:#$FCED; Attr:daMedial; Str:#$0644#$0645),  // ARABIC LIGATURE LAM WITH MEEM MEDIAL FORM
    (Unicode:#$FCEE; Attr:daMedial; Str:#$0646#$0645),  // ARABIC LIGATURE NOON WITH MEEM MEDIAL FORM
    (Unicode:#$FCEF; Attr:daMedial; Str:#$0646#$0647),  // ARABIC LIGATURE NOON WITH HEH MEDIAL FORM
    (Unicode:#$FCF0; Attr:daMedial; Str:#$064A#$0645),  // ARABIC LIGATURE YEH WITH MEEM MEDIAL FORM
    (Unicode:#$FCF1; Attr:daMedial; Str:#$064A#$0647),  // ARABIC LIGATURE YEH WITH HEH MEDIAL FORM
    (Unicode:#$FCF2; Attr:daMedial; Str:#$0640#$064E#$0651),   // ARABIC LIGATURE SHADDA WITH FATHA MEDIAL FORM
    (Unicode:#$FCF3; Attr:daMedial; Str:#$0640#$064F#$0651),   // ARABIC LIGATURE SHADDA WITH DAMMA MEDIAL FORM
    (Unicode:#$FCF4; Attr:daMedial; Str:#$0640#$0650#$0651),   // ARABIC LIGATURE SHADDA WITH KASRA MEDIAL FORM
    (Unicode:#$FCF5; Attr:daIsolated; Str:#$0637#$0649),// ARABIC LIGATURE TAH WITH ALEF MAKSURA ISOLATED FORM
    (Unicode:#$FCF6; Attr:daIsolated; Str:#$0637#$064A),// ARABIC LIGATURE TAH WITH YEH ISOLATED FORM
    (Unicode:#$FCF7; Attr:daIsolated; Str:#$0639#$0649),// ARABIC LIGATURE AIN WITH ALEF MAKSURA ISOLATED FORM
    (Unicode:#$FCF8; Attr:daIsolated; Str:#$0639#$064A),// ARABIC LIGATURE AIN WITH YEH ISOLATED FORM
    (Unicode:#$FCF9; Attr:daIsolated; Str:#$063A#$0649),// ARABIC LIGATURE GHAIN WITH ALEF MAKSURA ISOLATED FORM
    (Unicode:#$FCFA; Attr:daIsolated; Str:#$063A#$064A),// ARABIC LIGATURE GHAIN WITH YEH ISOLATED FORM
    (Unicode:#$FCFB; Attr:daIsolated; Str:#$0633#$0649),// ARABIC LIGATURE SEEN WITH ALEF MAKSURA ISOLATED FORM
    (Unicode:#$FCFC; Attr:daIsolated; Str:#$0633#$064A),// ARABIC LIGATURE SEEN WITH YEH ISOLATED FORM
    (Unicode:#$FCFD; Attr:daIsolated; Str:#$0634#$0649),// ARABIC LIGATURE SHEEN WITH ALEF MAKSURA ISOLATED FORM
    (Unicode:#$FCFE; Attr:daIsolated; Str:#$0634#$064A),// ARABIC LIGATURE SHEEN WITH YEH ISOLATED FORM
    (Unicode:#$FCFF; Attr:daIsolated; Str:#$062D#$0649),// ARABIC LIGATURE HAH WITH ALEF MAKSURA ISOLATED FORM
    (Unicode:#$FD00; Attr:daIsolated; Str:#$062D#$064A),// ARABIC LIGATURE HAH WITH YEH ISOLATED FORM
    (Unicode:#$FD01; Attr:daIsolated; Str:#$062C#$0649),// ARABIC LIGATURE JEEM WITH ALEF MAKSURA ISOLATED FORM
    (Unicode:#$FD02; Attr:daIsolated; Str:#$062C#$064A),// ARABIC LIGATURE JEEM WITH YEH ISOLATED FORM
    (Unicode:#$FD03; Attr:daIsolated; Str:#$062E#$0649),// ARABIC LIGATURE KHAH WITH ALEF MAKSURA ISOLATED FORM
    (Unicode:#$FD04; Attr:daIsolated; Str:#$062E#$064A),// ARABIC LIGATURE KHAH WITH YEH ISOLATED FORM
    (Unicode:#$FD05; Attr:daIsolated; Str:#$0635#$0649),// ARABIC LIGATURE SAD WITH ALEF MAKSURA ISOLATED FORM
    (Unicode:#$FD06; Attr:daIsolated; Str:#$0635#$064A),// ARABIC LIGATURE SAD WITH YEH ISOLATED FORM
    (Unicode:#$FD07; Attr:daIsolated; Str:#$0636#$0649),// ARABIC LIGATURE DAD WITH ALEF MAKSURA ISOLATED FORM
    (Unicode:#$FD08; Attr:daIsolated; Str:#$0636#$064A),// ARABIC LIGATURE DAD WITH YEH ISOLATED FORM
    (Unicode:#$FD09; Attr:daIsolated; Str:#$0634#$062C),// ARABIC LIGATURE SHEEN WITH JEEM ISOLATED FORM
    (Unicode:#$FD0A; Attr:daIsolated; Str:#$0634#$062D),// ARABIC LIGATURE SHEEN WITH HAH ISOLATED FORM
    (Unicode:#$FD0B; Attr:daIsolated; Str:#$0634#$062E),// ARABIC LIGATURE SHEEN WITH KHAH ISOLATED FORM
    (Unicode:#$FD0C; Attr:daIsolated; Str:#$0634#$0645),// ARABIC LIGATURE SHEEN WITH MEEM ISOLATED FORM
    (Unicode:#$FD0D; Attr:daIsolated; Str:#$0634#$0631),// ARABIC LIGATURE SHEEN WITH REH ISOLATED FORM
    (Unicode:#$FD0E; Attr:daIsolated; Str:#$0633#$0631),// ARABIC LIGATURE SEEN WITH REH ISOLATED FORM
    (Unicode:#$FD0F; Attr:daIsolated; Str:#$0635#$0631),// ARABIC LIGATURE SAD WITH REH ISOLATED FORM
    (Unicode:#$FD10; Attr:daIsolated; Str:#$0636#$0631),// ARABIC LIGATURE DAD WITH REH ISOLATED FORM
    (Unicode:#$FD11; Attr:daFinal; Str:#$0637#$0649),   // ARABIC LIGATURE TAH WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FD12; Attr:daFinal; Str:#$0637#$064A),   // ARABIC LIGATURE TAH WITH YEH FINAL FORM
    (Unicode:#$FD13; Attr:daFinal; Str:#$0639#$0649),   // ARABIC LIGATURE AIN WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FD14; Attr:daFinal; Str:#$0639#$064A),   // ARABIC LIGATURE AIN WITH YEH FINAL FORM
    (Unicode:#$FD15; Attr:daFinal; Str:#$063A#$0649),   // ARABIC LIGATURE GHAIN WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FD16; Attr:daFinal; Str:#$063A#$064A),   // ARABIC LIGATURE GHAIN WITH YEH FINAL FORM
    (Unicode:#$FD17; Attr:daFinal; Str:#$0633#$0649),   // ARABIC LIGATURE SEEN WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FD18; Attr:daFinal; Str:#$0633#$064A),   // ARABIC LIGATURE SEEN WITH YEH FINAL FORM
    (Unicode:#$FD19; Attr:daFinal; Str:#$0634#$0649),   // ARABIC LIGATURE SHEEN WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FD1A; Attr:daFinal; Str:#$0634#$064A),   // ARABIC LIGATURE SHEEN WITH YEH FINAL FORM
    (Unicode:#$FD1B; Attr:daFinal; Str:#$062D#$0649),   // ARABIC LIGATURE HAH WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FD1C; Attr:daFinal; Str:#$062D#$064A),   // ARABIC LIGATURE HAH WITH YEH FINAL FORM
    (Unicode:#$FD1D; Attr:daFinal; Str:#$062C#$0649),   // ARABIC LIGATURE JEEM WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FD1E; Attr:daFinal; Str:#$062C#$064A),   // ARABIC LIGATURE JEEM WITH YEH FINAL FORM
    (Unicode:#$FD1F; Attr:daFinal; Str:#$062E#$0649),   // ARABIC LIGATURE KHAH WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FD20; Attr:daFinal; Str:#$062E#$064A),   // ARABIC LIGATURE KHAH WITH YEH FINAL FORM
    (Unicode:#$FD21; Attr:daFinal; Str:#$0635#$0649),   // ARABIC LIGATURE SAD WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FD22; Attr:daFinal; Str:#$0635#$064A),   // ARABIC LIGATURE SAD WITH YEH FINAL FORM
    (Unicode:#$FD23; Attr:daFinal; Str:#$0636#$0649),   // ARABIC LIGATURE DAD WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FD24; Attr:daFinal; Str:#$0636#$064A),   // ARABIC LIGATURE DAD WITH YEH FINAL FORM
    (Unicode:#$FD25; Attr:daFinal; Str:#$0634#$062C),   // ARABIC LIGATURE SHEEN WITH JEEM FINAL FORM
    (Unicode:#$FD26; Attr:daFinal; Str:#$0634#$062D),   // ARABIC LIGATURE SHEEN WITH HAH FINAL FORM
    (Unicode:#$FD27; Attr:daFinal; Str:#$0634#$062E),   // ARABIC LIGATURE SHEEN WITH KHAH FINAL FORM
    (Unicode:#$FD28; Attr:daFinal; Str:#$0634#$0645),   // ARABIC LIGATURE SHEEN WITH MEEM FINAL FORM
    (Unicode:#$FD29; Attr:daFinal; Str:#$0634#$0631),   // ARABIC LIGATURE SHEEN WITH REH FINAL FORM
    (Unicode:#$FD2A; Attr:daFinal; Str:#$0633#$0631),   // ARABIC LIGATURE SEEN WITH REH FINAL FORM
    (Unicode:#$FD2B; Attr:daFinal; Str:#$0635#$0631),   // ARABIC LIGATURE SAD WITH REH FINAL FORM
    (Unicode:#$FD2C; Attr:daFinal; Str:#$0636#$0631),   // ARABIC LIGATURE DAD WITH REH FINAL FORM
    (Unicode:#$FD2D; Attr:daInitial; Str:#$0634#$062C), // ARABIC LIGATURE SHEEN WITH JEEM INITIAL FORM
    (Unicode:#$FD2E; Attr:daInitial; Str:#$0634#$062D), // ARABIC LIGATURE SHEEN WITH HAH INITIAL FORM
    (Unicode:#$FD2F; Attr:daInitial; Str:#$0634#$062E), // ARABIC LIGATURE SHEEN WITH KHAH INITIAL FORM
    (Unicode:#$FD30; Attr:daInitial; Str:#$0634#$0645), // ARABIC LIGATURE SHEEN WITH MEEM INITIAL FORM
    (Unicode:#$FD31; Attr:daInitial; Str:#$0633#$0647), // ARABIC LIGATURE SEEN WITH HEH INITIAL FORM
    (Unicode:#$FD32; Attr:daInitial; Str:#$0634#$0647), // ARABIC LIGATURE SHEEN WITH HEH INITIAL FORM
    (Unicode:#$FD33; Attr:daInitial; Str:#$0637#$0645), // ARABIC LIGATURE TAH WITH MEEM INITIAL FORM
    (Unicode:#$FD34; Attr:daMedial; Str:#$0633#$062C),  // ARABIC LIGATURE SEEN WITH JEEM MEDIAL FORM
    (Unicode:#$FD35; Attr:daMedial; Str:#$0633#$062D),  // ARABIC LIGATURE SEEN WITH HAH MEDIAL FORM
    (Unicode:#$FD36; Attr:daMedial; Str:#$0633#$062E),  // ARABIC LIGATURE SEEN WITH KHAH MEDIAL FORM
    (Unicode:#$FD37; Attr:daMedial; Str:#$0634#$062C),  // ARABIC LIGATURE SHEEN WITH JEEM MEDIAL FORM
    (Unicode:#$FD38; Attr:daMedial; Str:#$0634#$062D),  // ARABIC LIGATURE SHEEN WITH HAH MEDIAL FORM
    (Unicode:#$FD39; Attr:daMedial; Str:#$0634#$062E),  // ARABIC LIGATURE SHEEN WITH KHAH MEDIAL FORM
    (Unicode:#$FD3A; Attr:daMedial; Str:#$0637#$0645),  // ARABIC LIGATURE TAH WITH MEEM MEDIAL FORM
    (Unicode:#$FD3B; Attr:daMedial; Str:#$0638#$0645),  // ARABIC LIGATURE ZAH WITH MEEM MEDIAL FORM
    (Unicode:#$FD3C; Attr:daFinal; Str:#$0627#$064B),   // ARABIC LIGATURE ALEF WITH FATHATAN FINAL FORM
    (Unicode:#$FD3D; Attr:daIsolated; Str:#$0627#$064B),// ARABIC LIGATURE ALEF WITH FATHATAN ISOLATED FORM
    (Unicode:#$FD50; Attr:daInitial; Str:#$062A#$062C#$0645),  // ARABIC LIGATURE TEH WITH JEEM WITH MEEM INITIAL FORM
    (Unicode:#$FD51; Attr:daFinal; Str:#$062A#$062D#$062C),    // ARABIC LIGATURE TEH WITH HAH WITH JEEM FINAL FORM
    (Unicode:#$FD52; Attr:daInitial; Str:#$062A#$062D#$062C),  // ARABIC LIGATURE TEH WITH HAH WITH JEEM INITIAL FORM
    (Unicode:#$FD53; Attr:daInitial; Str:#$062A#$062D#$0645),  // ARABIC LIGATURE TEH WITH HAH WITH MEEM INITIAL FORM
    (Unicode:#$FD54; Attr:daInitial; Str:#$062A#$062E#$0645),  // ARABIC LIGATURE TEH WITH KHAH WITH MEEM INITIAL FORM
    (Unicode:#$FD55; Attr:daInitial; Str:#$062A#$0645#$062C),  // ARABIC LIGATURE TEH WITH MEEM WITH JEEM INITIAL FORM
    (Unicode:#$FD56; Attr:daInitial; Str:#$062A#$0645#$062D),  // ARABIC LIGATURE TEH WITH MEEM WITH HAH INITIAL FORM
    (Unicode:#$FD57; Attr:daInitial; Str:#$062A#$0645#$062E),  // ARABIC LIGATURE TEH WITH MEEM WITH KHAH INITIAL FORM
    (Unicode:#$FD58; Attr:daFinal; Str:#$062C#$0645#$062D),    // ARABIC LIGATURE JEEM WITH MEEM WITH HAH FINAL FORM
    (Unicode:#$FD59; Attr:daInitial; Str:#$062C#$0645#$062D),  // ARABIC LIGATURE JEEM WITH MEEM WITH HAH INITIAL FORM
    (Unicode:#$FD5A; Attr:daFinal; Str:#$062D#$0645#$064A),    // ARABIC LIGATURE HAH WITH MEEM WITH YEH FINAL FORM
    (Unicode:#$FD5B; Attr:daFinal; Str:#$062D#$0645#$0649),    // ARABIC LIGATURE HAH WITH MEEM WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FD5C; Attr:daInitial; Str:#$0633#$062D#$062C),  // ARABIC LIGATURE SEEN WITH HAH WITH JEEM INITIAL FORM
    (Unicode:#$FD5D; Attr:daInitial; Str:#$0633#$062C#$062D),  // ARABIC LIGATURE SEEN WITH JEEM WITH HAH INITIAL FORM
    (Unicode:#$FD5E; Attr:daFinal; Str:#$0633#$062C#$0649),    // ARABIC LIGATURE SEEN WITH JEEM WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FD5F; Attr:daFinal; Str:#$0633#$0645#$062D),    // ARABIC LIGATURE SEEN WITH MEEM WITH HAH FINAL FORM
    (Unicode:#$FD60; Attr:daInitial; Str:#$0633#$0645#$062D),  // ARABIC LIGATURE SEEN WITH MEEM WITH HAH INITIAL FORM
    (Unicode:#$FD61; Attr:daInitial; Str:#$0633#$0645#$062C),  // ARABIC LIGATURE SEEN WITH MEEM WITH JEEM INITIAL FORM
    (Unicode:#$FD62; Attr:daFinal; Str:#$0633#$0645#$0645),    // ARABIC LIGATURE SEEN WITH MEEM WITH MEEM FINAL FORM
    (Unicode:#$FD63; Attr:daInitial; Str:#$0633#$0645#$0645),  // ARABIC LIGATURE SEEN WITH MEEM WITH MEEM INITIAL FORM
    (Unicode:#$FD64; Attr:daFinal; Str:#$0635#$062D#$062D),    // ARABIC LIGATURE SAD WITH HAH WITH HAH FINAL FORM
    (Unicode:#$FD65; Attr:daInitial; Str:#$0635#$062D#$062D),  // ARABIC LIGATURE SAD WITH HAH WITH HAH INITIAL FORM
    (Unicode:#$FD66; Attr:daFinal; Str:#$0635#$0645#$0645),    // ARABIC LIGATURE SAD WITH MEEM WITH MEEM FINAL FORM
    (Unicode:#$FD67; Attr:daFinal; Str:#$0634#$062D#$0645),    // ARABIC LIGATURE SHEEN WITH HAH WITH MEEM FINAL FORM
    (Unicode:#$FD68; Attr:daInitial; Str:#$0634#$062D#$0645),  // ARABIC LIGATURE SHEEN WITH HAH WITH MEEM INITIAL FORM
    (Unicode:#$FD69; Attr:daFinal; Str:#$0634#$062C#$064A),    // ARABIC LIGATURE SHEEN WITH JEEM WITH YEH FINAL FORM
    (Unicode:#$FD6A; Attr:daFinal; Str:#$0634#$0645#$062E),    // ARABIC LIGATURE SHEEN WITH MEEM WITH KHAH FINAL FORM
    (Unicode:#$FD6B; Attr:daInitial; Str:#$0634#$0645#$062E),  // ARABIC LIGATURE SHEEN WITH MEEM WITH KHAH INITIAL FORM
    (Unicode:#$FD6C; Attr:daFinal; Str:#$0634#$0645#$0645),    // ARABIC LIGATURE SHEEN WITH MEEM WITH MEEM FINAL FORM
    (Unicode:#$FD6D; Attr:daInitial; Str:#$0634#$0645#$0645),  // ARABIC LIGATURE SHEEN WITH MEEM WITH MEEM INITIAL FORM
    (Unicode:#$FD6E; Attr:daFinal; Str:#$0636#$062D#$0649),    // ARABIC LIGATURE DAD WITH HAH WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FD6F; Attr:daFinal; Str:#$0636#$062E#$0645),    // ARABIC LIGATURE DAD WITH KHAH WITH MEEM FINAL FORM
    (Unicode:#$FD70; Attr:daInitial; Str:#$0636#$062E#$0645),  // ARABIC LIGATURE DAD WITH KHAH WITH MEEM INITIAL FORM
    (Unicode:#$FD71; Attr:daFinal; Str:#$0637#$0645#$062D),    // ARABIC LIGATURE TAH WITH MEEM WITH HAH FINAL FORM
    (Unicode:#$FD72; Attr:daInitial; Str:#$0637#$0645#$062D),  // ARABIC LIGATURE TAH WITH MEEM WITH HAH INITIAL FORM
    (Unicode:#$FD73; Attr:daInitial; Str:#$0637#$0645#$0645),  // ARABIC LIGATURE TAH WITH MEEM WITH MEEM INITIAL FORM
    (Unicode:#$FD74; Attr:daFinal; Str:#$0637#$0645#$064A),    // ARABIC LIGATURE TAH WITH MEEM WITH YEH FINAL FORM
    (Unicode:#$FD75; Attr:daFinal; Str:#$0639#$062C#$0645),    // ARABIC LIGATURE AIN WITH JEEM WITH MEEM FINAL FORM
    (Unicode:#$FD76; Attr:daFinal; Str:#$0639#$0645#$0645),    // ARABIC LIGATURE AIN WITH MEEM WITH MEEM FINAL FORM
    (Unicode:#$FD77; Attr:daInitial; Str:#$0639#$0645#$0645),  // ARABIC LIGATURE AIN WITH MEEM WITH MEEM INITIAL FORM
    (Unicode:#$FD78; Attr:daFinal; Str:#$0639#$0645#$0649),    // ARABIC LIGATURE AIN WITH MEEM WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FD79; Attr:daFinal; Str:#$063A#$0645#$0645),    // ARABIC LIGATURE GHAIN WITH MEEM WITH MEEM FINAL FORM
    (Unicode:#$FD7A; Attr:daFinal; Str:#$063A#$0645#$064A),    // ARABIC LIGATURE GHAIN WITH MEEM WITH YEH FINAL FORM
    (Unicode:#$FD7B; Attr:daFinal; Str:#$063A#$0645#$0649),    // ARABIC LIGATURE GHAIN WITH MEEM WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FD7C; Attr:daFinal; Str:#$0641#$062E#$0645),    // ARABIC LIGATURE FEH WITH KHAH WITH MEEM FINAL FORM
    (Unicode:#$FD7D; Attr:daInitial; Str:#$0641#$062E#$0645),  // ARABIC LIGATURE FEH WITH KHAH WITH MEEM INITIAL FORM
    (Unicode:#$FD7E; Attr:daFinal; Str:#$0642#$0645#$062D),    // ARABIC LIGATURE QAF WITH MEEM WITH HAH FINAL FORM
    (Unicode:#$FD7F; Attr:daFinal; Str:#$0642#$0645#$0645),    // ARABIC LIGATURE QAF WITH MEEM WITH MEEM FINAL FORM
    (Unicode:#$FD80; Attr:daFinal; Str:#$0644#$062D#$0645),    // ARABIC LIGATURE LAM WITH HAH WITH MEEM FINAL FORM
    (Unicode:#$FD81; Attr:daFinal; Str:#$0644#$062D#$064A),    // ARABIC LIGATURE LAM WITH HAH WITH YEH FINAL FORM
    (Unicode:#$FD82; Attr:daFinal; Str:#$0644#$062D#$0649),    // ARABIC LIGATURE LAM WITH HAH WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FD83; Attr:daInitial; Str:#$0644#$062C#$062C),  // ARABIC LIGATURE LAM WITH JEEM WITH JEEM INITIAL FORM
    (Unicode:#$FD84; Attr:daFinal; Str:#$0644#$062C#$062C),    // ARABIC LIGATURE LAM WITH JEEM WITH JEEM FINAL FORM
    (Unicode:#$FD85; Attr:daFinal; Str:#$0644#$062E#$0645),    // ARABIC LIGATURE LAM WITH KHAH WITH MEEM FINAL FORM
    (Unicode:#$FD86; Attr:daInitial; Str:#$0644#$062E#$0645),  // ARABIC LIGATURE LAM WITH KHAH WITH MEEM INITIAL FORM
    (Unicode:#$FD87; Attr:daFinal; Str:#$0644#$0645#$062D),    // ARABIC LIGATURE LAM WITH MEEM WITH HAH FINAL FORM
    (Unicode:#$FD88; Attr:daInitial; Str:#$0644#$0645#$062D),  // ARABIC LIGATURE LAM WITH MEEM WITH HAH INITIAL FORM
    (Unicode:#$FD89; Attr:daInitial; Str:#$0645#$062D#$062C),  // ARABIC LIGATURE MEEM WITH HAH WITH JEEM INITIAL FORM
    (Unicode:#$FD8A; Attr:daInitial; Str:#$0645#$062D#$0645),  // ARABIC LIGATURE MEEM WITH HAH WITH MEEM INITIAL FORM
    (Unicode:#$FD8B; Attr:daFinal; Str:#$0645#$062D#$064A),    // ARABIC LIGATURE MEEM WITH HAH WITH YEH FINAL FORM
    (Unicode:#$FD8C; Attr:daInitial; Str:#$0645#$062C#$062D),  // ARABIC LIGATURE MEEM WITH JEEM WITH HAH INITIAL FORM
    (Unicode:#$FD8D; Attr:daInitial; Str:#$0645#$062C#$0645),  // ARABIC LIGATURE MEEM WITH JEEM WITH MEEM INITIAL FORM
    (Unicode:#$FD8E; Attr:daInitial; Str:#$0645#$062E#$062C),  // ARABIC LIGATURE MEEM WITH KHAH WITH JEEM INITIAL FORM
    (Unicode:#$FD8F; Attr:daInitial; Str:#$0645#$062E#$0645),  // ARABIC LIGATURE MEEM WITH KHAH WITH MEEM INITIAL FORM
    (Unicode:#$FD92; Attr:daInitial; Str:#$0645#$062C#$062E),  // ARABIC LIGATURE MEEM WITH JEEM WITH KHAH INITIAL FORM
    (Unicode:#$FD93; Attr:daInitial; Str:#$0647#$0645#$062C),  // ARABIC LIGATURE HEH WITH MEEM WITH JEEM INITIAL FORM
    (Unicode:#$FD94; Attr:daInitial; Str:#$0647#$0645#$0645),  // ARABIC LIGATURE HEH WITH MEEM WITH MEEM INITIAL FORM
    (Unicode:#$FD95; Attr:daInitial; Str:#$0646#$062D#$0645),  // ARABIC LIGATURE NOON WITH HAH WITH MEEM INITIAL FORM
    (Unicode:#$FD96; Attr:daFinal; Str:#$0646#$062D#$0649),    // ARABIC LIGATURE NOON WITH HAH WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FD97; Attr:daFinal; Str:#$0646#$062C#$0645),    // ARABIC LIGATURE NOON WITH JEEM WITH MEEM FINAL FORM
    (Unicode:#$FD98; Attr:daInitial; Str:#$0646#$062C#$0645),  // ARABIC LIGATURE NOON WITH JEEM WITH MEEM INITIAL FORM
    (Unicode:#$FD99; Attr:daFinal; Str:#$0646#$062C#$0649),    // ARABIC LIGATURE NOON WITH JEEM WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FD9A; Attr:daFinal; Str:#$0646#$0645#$064A),    // ARABIC LIGATURE NOON WITH MEEM WITH YEH FINAL FORM
    (Unicode:#$FD9B; Attr:daFinal; Str:#$0646#$0645#$0649),    // ARABIC LIGATURE NOON WITH MEEM WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FD9C; Attr:daFinal; Str:#$064A#$0645#$0645),    // ARABIC LIGATURE YEH WITH MEEM WITH MEEM FINAL FORM
    (Unicode:#$FD9D; Attr:daInitial; Str:#$064A#$0645#$0645),  // ARABIC LIGATURE YEH WITH MEEM WITH MEEM INITIAL FORM
    (Unicode:#$FD9E; Attr:daFinal; Str:#$0628#$062E#$064A),    // ARABIC LIGATURE BEH WITH KHAH WITH YEH FINAL FORM
    (Unicode:#$FD9F; Attr:daFinal; Str:#$062A#$062C#$064A),    // ARABIC LIGATURE TEH WITH JEEM WITH YEH FINAL FORM
    (Unicode:#$FDA0; Attr:daFinal; Str:#$062A#$062C#$0649),    // ARABIC LIGATURE TEH WITH JEEM WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FDA1; Attr:daFinal; Str:#$062A#$062E#$064A),    // ARABIC LIGATURE TEH WITH KHAH WITH YEH FINAL FORM
    (Unicode:#$FDA2; Attr:daFinal; Str:#$062A#$062E#$0649),    // ARABIC LIGATURE TEH WITH KHAH WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FDA3; Attr:daFinal; Str:#$062A#$0645#$064A),    // ARABIC LIGATURE TEH WITH MEEM WITH YEH FINAL FORM
    (Unicode:#$FDA4; Attr:daFinal; Str:#$062A#$0645#$0649),    // ARABIC LIGATURE TEH WITH MEEM WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FDA5; Attr:daFinal; Str:#$062C#$0645#$064A),    // ARABIC LIGATURE JEEM WITH MEEM WITH YEH FINAL FORM
    (Unicode:#$FDA6; Attr:daFinal; Str:#$062C#$062D#$0649),    // ARABIC LIGATURE JEEM WITH HAH WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FDA7; Attr:daFinal; Str:#$062C#$0645#$0649),    // ARABIC LIGATURE JEEM WITH MEEM WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FDA8; Attr:daFinal; Str:#$0633#$062E#$0649),    // ARABIC LIGATURE SEEN WITH KHAH WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FDA9; Attr:daFinal; Str:#$0635#$062D#$064A),    // ARABIC LIGATURE SAD WITH HAH WITH YEH FINAL FORM
    (Unicode:#$FDAA; Attr:daFinal; Str:#$0634#$062D#$064A),    // ARABIC LIGATURE SHEEN WITH HAH WITH YEH FINAL FORM
    (Unicode:#$FDAB; Attr:daFinal; Str:#$0636#$062D#$064A),    // ARABIC LIGATURE DAD WITH HAH WITH YEH FINAL FORM
    (Unicode:#$FDAC; Attr:daFinal; Str:#$0644#$062C#$064A),    // ARABIC LIGATURE LAM WITH JEEM WITH YEH FINAL FORM
    (Unicode:#$FDAD; Attr:daFinal; Str:#$0644#$0645#$064A),    // ARABIC LIGATURE LAM WITH MEEM WITH YEH FINAL FORM
    (Unicode:#$FDAE; Attr:daFinal; Str:#$064A#$062D#$064A),    // ARABIC LIGATURE YEH WITH HAH WITH YEH FINAL FORM
    (Unicode:#$FDAF; Attr:daFinal; Str:#$064A#$062C#$064A),    // ARABIC LIGATURE YEH WITH JEEM WITH YEH FINAL FORM
    (Unicode:#$FDB0; Attr:daFinal; Str:#$064A#$0645#$064A),    // ARABIC LIGATURE YEH WITH MEEM WITH YEH FINAL FORM
    (Unicode:#$FDB1; Attr:daFinal; Str:#$0645#$0645#$064A),    // ARABIC LIGATURE MEEM WITH MEEM WITH YEH FINAL FORM
    (Unicode:#$FDB2; Attr:daFinal; Str:#$0642#$0645#$064A),    // ARABIC LIGATURE QAF WITH MEEM WITH YEH FINAL FORM
    (Unicode:#$FDB3; Attr:daFinal; Str:#$0646#$062D#$064A),    // ARABIC LIGATURE NOON WITH HAH WITH YEH FINAL FORM
    (Unicode:#$FDB4; Attr:daInitial; Str:#$0642#$0645#$062D),  // ARABIC LIGATURE QAF WITH MEEM WITH HAH INITIAL FORM
    (Unicode:#$FDB5; Attr:daInitial; Str:#$0644#$062D#$0645),  // ARABIC LIGATURE LAM WITH HAH WITH MEEM INITIAL FORM
    (Unicode:#$FDB6; Attr:daFinal; Str:#$0639#$0645#$064A),    // ARABIC LIGATURE AIN WITH MEEM WITH YEH FINAL FORM
    (Unicode:#$FDB7; Attr:daFinal; Str:#$0643#$0645#$064A),    // ARABIC LIGATURE KAF WITH MEEM WITH YEH FINAL FORM
    (Unicode:#$FDB8; Attr:daInitial; Str:#$0646#$062C#$062D),  // ARABIC LIGATURE NOON WITH JEEM WITH HAH INITIAL FORM
    (Unicode:#$FDB9; Attr:daFinal; Str:#$0645#$062E#$064A),    // ARABIC LIGATURE MEEM WITH KHAH WITH YEH FINAL FORM
    (Unicode:#$FDBA; Attr:daInitial; Str:#$0644#$062C#$0645),  // ARABIC LIGATURE LAM WITH JEEM WITH MEEM INITIAL FORM
    (Unicode:#$FDBB; Attr:daFinal; Str:#$0643#$0645#$0645),    // ARABIC LIGATURE KAF WITH MEEM WITH MEEM FINAL FORM
    (Unicode:#$FDBC; Attr:daFinal; Str:#$0644#$062C#$0645),    // ARABIC LIGATURE LAM WITH JEEM WITH MEEM FINAL FORM
    (Unicode:#$FDBD; Attr:daFinal; Str:#$0646#$062C#$062D),    // ARABIC LIGATURE NOON WITH JEEM WITH HAH FINAL FORM
    (Unicode:#$FDBE; Attr:daFinal; Str:#$062C#$062D#$064A),    // ARABIC LIGATURE JEEM WITH HAH WITH YEH FINAL FORM
    (Unicode:#$FDBF; Attr:daFinal; Str:#$062D#$062C#$064A),    // ARABIC LIGATURE HAH WITH JEEM WITH YEH FINAL FORM
    (Unicode:#$FDC0; Attr:daFinal; Str:#$0645#$062C#$064A),    // ARABIC LIGATURE MEEM WITH JEEM WITH YEH FINAL FORM
    (Unicode:#$FDC1; Attr:daFinal; Str:#$0641#$0645#$064A),    // ARABIC LIGATURE FEH WITH MEEM WITH YEH FINAL FORM
    (Unicode:#$FDC2; Attr:daFinal; Str:#$0628#$062D#$064A),    // ARABIC LIGATURE BEH WITH HAH WITH YEH FINAL FORM
    (Unicode:#$FDC3; Attr:daInitial; Str:#$0643#$0645#$0645),  // ARABIC LIGATURE KAF WITH MEEM WITH MEEM INITIAL FORM
    (Unicode:#$FDC4; Attr:daInitial; Str:#$0639#$062C#$0645),  // ARABIC LIGATURE AIN WITH JEEM WITH MEEM INITIAL FORM
    (Unicode:#$FDC5; Attr:daInitial; Str:#$0635#$0645#$0645),  // ARABIC LIGATURE SAD WITH MEEM WITH MEEM INITIAL FORM
    (Unicode:#$FDC6; Attr:daFinal; Str:#$0633#$062E#$064A),    // ARABIC LIGATURE SEEN WITH KHAH WITH YEH FINAL FORM
    (Unicode:#$FDC7; Attr:daFinal; Str:#$0646#$062C#$064A),    // ARABIC LIGATURE NOON WITH JEEM WITH YEH FINAL FORM
    (Unicode:#$FDF0; Attr:daIsolated; Str:#$0635#$0644#$06D2), // ARABIC LIGATURE SALLA USED AS KORANIC STOP SIGN ISOLATED FORM
    (Unicode:#$FDF1; Attr:daIsolated; Str:#$0642#$0644#$06D2), // ARABIC LIGATURE QALA USED AS KORANIC STOP SIGN ISOLATED FORM
    (Unicode:#$FDF2; Attr:daIsolated; Str:#$0627#$0644#$0644#$0647),// ARABIC LIGATURE ALLAH ISOLATED FORM
    (Unicode:#$FDF3; Attr:daIsolated; Str:#$0627#$0643#$0628#$0631),// ARABIC LIGATURE AKBAR ISOLATED FORM
    (Unicode:#$FDF4; Attr:daIsolated; Str:#$0645#$062D#$0645#$062F),// ARABIC LIGATURE MOHAMMAD ISOLATED FORM
    (Unicode:#$FDF5; Attr:daIsolated; Str:#$0635#$0644#$0639#$0645),// ARABIC LIGATURE SALAM ISOLATED FORM
    (Unicode:#$FDF6; Attr:daIsolated; Str:#$0631#$0633#$0648#$0644),// ARABIC LIGATURE RASOUL ISOLATED FORM
    (Unicode:#$FDF7; Attr:daIsolated; Str:#$0639#$0644#$064A#$0647),// ARABIC LIGATURE ALAYHE ISOLATED FORM
    (Unicode:#$FDF8; Attr:daIsolated; Str:#$0648#$0633#$0644#$0645),// ARABIC LIGATURE WASALLAM ISOLATED FORM
    (Unicode:#$FDF9; Attr:daIsolated; Str:#$0635#$0644#$0649), // ARABIC LIGATURE SALLA ISOLATED FORM
    (Unicode:#$FDFA; Attr:daIsolated; Str:#$0635#$0644#$0649#$0020#$0627#$0644#$0644#$0647#$0020#$0639#$0644#$064A#$0647#$0020#$0648#$0633#$0644#$0645),// ARABIC LIGATURE SALLALLAHOU ALAYHE WASALLAM
    (Unicode:#$FDFB; Attr:daIsolated; Str:#$062C#$0644#$0020#$062C#$0644#$0627#$0644#$0647),// ARABIC LIGATURE JALLAJALALOUHOU
    (Unicode:#$FE30; Attr:daVertical; Str:#$2025),      // PRESENTATION FORM FOR VERTICAL TWO DOT LEADER
    (Unicode:#$FE31; Attr:daVertical; Str:#$2014),      // PRESENTATION FORM FOR VERTICAL EM DASH
    (Unicode:#$FE32; Attr:daVertical; Str:#$2013),      // PRESENTATION FORM FOR VERTICAL EN DASH
    (Unicode:#$FE33; Attr:daVertical; Str:#$005F),      // PRESENTATION FORM FOR VERTICAL LOW LINE
    (Unicode:#$FE34; Attr:daVertical; Str:#$005F),      // PRESENTATION FORM FOR VERTICAL WAVY LOW LINE
    (Unicode:#$FE35; Attr:daVertical; Str:#$0028),      // PRESENTATION FORM FOR VERTICAL LEFT PARENTHESIS
    (Unicode:#$FE36; Attr:daVertical; Str:#$0029),      // PRESENTATION FORM FOR VERTICAL RIGHT PARENTHESIS
    (Unicode:#$FE37; Attr:daVertical; Str:#$007B),      // PRESENTATION FORM FOR VERTICAL LEFT CURLY BRACKET
    (Unicode:#$FE38; Attr:daVertical; Str:#$007D),      // PRESENTATION FORM FOR VERTICAL RIGHT CURLY BRACKET
    (Unicode:#$FE39; Attr:daVertical; Str:#$3014),      // PRESENTATION FORM FOR VERTICAL LEFT TORTOISE SHELL BRACKET
    (Unicode:#$FE3A; Attr:daVertical; Str:#$3015),      // PRESENTATION FORM FOR VERTICAL RIGHT TORTOISE SHELL BRACKET
    (Unicode:#$FE3B; Attr:daVertical; Str:#$3010),      // PRESENTATION FORM FOR VERTICAL LEFT BLACK LENTICULAR BRACKET
    (Unicode:#$FE3C; Attr:daVertical; Str:#$3011),      // PRESENTATION FORM FOR VERTICAL RIGHT BLACK LENTICULAR BRACKET
    (Unicode:#$FE3D; Attr:daVertical; Str:#$300A),      // PRESENTATION FORM FOR VERTICAL LEFT DOUBLE ANGLE BRACKET
    (Unicode:#$FE3E; Attr:daVertical; Str:#$300B),      // PRESENTATION FORM FOR VERTICAL RIGHT DOUBLE ANGLE BRACKET
    (Unicode:#$FE3F; Attr:daVertical; Str:#$3008),      // PRESENTATION FORM FOR VERTICAL LEFT ANGLE BRACKET
    (Unicode:#$FE40; Attr:daVertical; Str:#$3009),      // PRESENTATION FORM FOR VERTICAL RIGHT ANGLE BRACKET
    (Unicode:#$FE41; Attr:daVertical; Str:#$300C),      // PRESENTATION FORM FOR VERTICAL LEFT CORNER BRACKET
    (Unicode:#$FE42; Attr:daVertical; Str:#$300D),      // PRESENTATION FORM FOR VERTICAL RIGHT CORNER BRACKET
    (Unicode:#$FE43; Attr:daVertical; Str:#$300E),      // PRESENTATION FORM FOR VERTICAL LEFT WHITE CORNER BRACKET
    (Unicode:#$FE44; Attr:daVertical; Str:#$300F),      // PRESENTATION FORM FOR VERTICAL RIGHT WHITE CORNER BRACKET
    (Unicode:#$FE49; Attr:daCompat; Str:#$203E),        // DASHED OVERLINE
    (Unicode:#$FE4A; Attr:daCompat; Str:#$203E),        // CENTRELINE OVERLINE
    (Unicode:#$FE4B; Attr:daCompat; Str:#$203E),        // WAVY OVERLINE
    (Unicode:#$FE4C; Attr:daCompat; Str:#$203E),        // DOUBLE WAVY OVERLINE
    (Unicode:#$FE4D; Attr:daCompat; Str:#$005F),        // DASHED LOW LINE
    (Unicode:#$FE4E; Attr:daCompat; Str:#$005F),        // CENTRELINE LOW LINE
    (Unicode:#$FE4F; Attr:daCompat; Str:#$005F),        // WAVY LOW LINE
    (Unicode:#$FE50; Attr:daSmall; Str:#$002C),         // SMALL COMMA
    (Unicode:#$FE51; Attr:daSmall; Str:#$3001),         // SMALL IDEOGRAPHIC COMMA
    (Unicode:#$FE52; Attr:daSmall; Str:#$002E),         // SMALL FULL STOP
    (Unicode:#$FE54; Attr:daSmall; Str:#$003B),         // SMALL SEMICOLON
    (Unicode:#$FE55; Attr:daSmall; Str:#$003A),         // SMALL COLON
    (Unicode:#$FE56; Attr:daSmall; Str:#$003F),         // SMALL QUESTION MARK
    (Unicode:#$FE57; Attr:daSmall; Str:#$0021),         // SMALL EXCLAMATION MARK
    (Unicode:#$FE58; Attr:daSmall; Str:#$2014),         // SMALL EM DASH
    (Unicode:#$FE59; Attr:daSmall; Str:#$0028),         // SMALL LEFT PARENTHESIS
    (Unicode:#$FE5A; Attr:daSmall; Str:#$0029),         // SMALL RIGHT PARENTHESIS
    (Unicode:#$FE5B; Attr:daSmall; Str:#$007B),         // SMALL LEFT CURLY BRACKET
    (Unicode:#$FE5C; Attr:daSmall; Str:#$007D),         // SMALL RIGHT CURLY BRACKET
    (Unicode:#$FE5D; Attr:daSmall; Str:#$3014),         // SMALL LEFT TORTOISE SHELL BRACKET
    (Unicode:#$FE5E; Attr:daSmall; Str:#$3015),         // SMALL RIGHT TORTOISE SHELL BRACKET
    (Unicode:#$FE5F; Attr:daSmall; Str:#$0023),         // SMALL NUMBER SIGN
    (Unicode:#$FE60; Attr:daSmall; Str:#$0026),         // SMALL AMPERSAND
    (Unicode:#$FE61; Attr:daSmall; Str:#$002A),         // SMALL ASTERISK
    (Unicode:#$FE62; Attr:daSmall; Str:#$002B),         // SMALL PLUS SIGN
    (Unicode:#$FE63; Attr:daSmall; Str:#$002D),         // SMALL HYPHEN-MINUS
    (Unicode:#$FE64; Attr:daSmall; Str:#$003C),         // SMALL LESS-THAN SIGN
    (Unicode:#$FE65; Attr:daSmall; Str:#$003E),         // SMALL GREATER-THAN SIGN
    (Unicode:#$FE66; Attr:daSmall; Str:#$003D),         // SMALL EQUALS SIGN
    (Unicode:#$FE68; Attr:daSmall; Str:#$005C),         // SMALL REVERSE SOLIDUS
    (Unicode:#$FE69; Attr:daSmall; Str:#$0024),         // SMALL DOLLAR SIGN
    (Unicode:#$FE6A; Attr:daSmall; Str:#$0025),         // SMALL PERCENT SIGN
    (Unicode:#$FE6B; Attr:daSmall; Str:#$0040),         // SMALL COMMERCIAL AT
    (Unicode:#$FE70; Attr:daIsolated; Str:#$0020#$064B),// ARABIC FATHATAN ISOLATED FORM
    (Unicode:#$FE71; Attr:daMedial; Str:#$0640#$064B),  // ARABIC TATWEEL WITH FATHATAN ABOVE
    (Unicode:#$FE72; Attr:daIsolated; Str:#$0020#$064C),// ARABIC DAMMATAN ISOLATED FORM
    (Unicode:#$FE74; Attr:daIsolated; Str:#$0020#$064D),// ARABIC KASRATAN ISOLATED FORM
    (Unicode:#$FE76; Attr:daIsolated; Str:#$0020#$064E),// ARABIC FATHA ISOLATED FORM
    (Unicode:#$FE77; Attr:daMedial; Str:#$0640#$064E),  // ARABIC FATHA MEDIAL FORM
    (Unicode:#$FE78; Attr:daIsolated; Str:#$0020#$064F),// ARABIC DAMMA ISOLATED FORM
    (Unicode:#$FE79; Attr:daMedial; Str:#$0640#$064F),  // ARABIC DAMMA MEDIAL FORM
    (Unicode:#$FE7A; Attr:daIsolated; Str:#$0020#$0650),// ARABIC KASRA ISOLATED FORM
    (Unicode:#$FE7B; Attr:daMedial; Str:#$0640#$0650),  // ARABIC KASRA MEDIAL FORM
    (Unicode:#$FE7C; Attr:daIsolated; Str:#$0020#$0651),// ARABIC SHADDA ISOLATED FORM
    (Unicode:#$FE7D; Attr:daMedial; Str:#$0640#$0651),  // ARABIC SHADDA MEDIAL FORM
    (Unicode:#$FE7E; Attr:daIsolated; Str:#$0020#$0652),// ARABIC SUKUN ISOLATED FORM
    (Unicode:#$FE7F; Attr:daMedial; Str:#$0640#$0652),  // ARABIC SUKUN MEDIAL FORM
    (Unicode:#$FE80; Attr:daIsolated; Str:#$0621),      // ARABIC LETTER HAMZA ISOLATED FORM
    (Unicode:#$FE81; Attr:daIsolated; Str:#$0622),      // ARABIC LETTER ALEF WITH MADDA ABOVE ISOLATED FORM
    (Unicode:#$FE82; Attr:daFinal; Str:#$0622),         // ARABIC LETTER ALEF WITH MADDA ABOVE FINAL FORM
    (Unicode:#$FE83; Attr:daIsolated; Str:#$0623),      // ARABIC LETTER ALEF WITH HAMZA ABOVE ISOLATED FORM
    (Unicode:#$FE84; Attr:daFinal; Str:#$0623),         // ARABIC LETTER ALEF WITH HAMZA ABOVE FINAL FORM
    (Unicode:#$FE85; Attr:daIsolated; Str:#$0624),      // ARABIC LETTER WAW WITH HAMZA ABOVE ISOLATED FORM
    (Unicode:#$FE86; Attr:daFinal; Str:#$0624),         // ARABIC LETTER WAW WITH HAMZA ABOVE FINAL FORM
    (Unicode:#$FE87; Attr:daIsolated; Str:#$0625),      // ARABIC LETTER ALEF WITH HAMZA BELOW ISOLATED FORM
    (Unicode:#$FE88; Attr:daFinal; Str:#$0625),         // ARABIC LETTER ALEF WITH HAMZA BELOW FINAL FORM
    (Unicode:#$FE89; Attr:daIsolated; Str:#$0626),      // ARABIC LETTER YEH WITH HAMZA ABOVE ISOLATED FORM
    (Unicode:#$FE8A; Attr:daFinal; Str:#$0626),         // ARABIC LETTER YEH WITH HAMZA ABOVE FINAL FORM
    (Unicode:#$FE8B; Attr:daInitial; Str:#$0626),       // ARABIC LETTER YEH WITH HAMZA ABOVE INITIAL FORM
    (Unicode:#$FE8C; Attr:daMedial; Str:#$0626),        // ARABIC LETTER YEH WITH HAMZA ABOVE MEDIAL FORM
    (Unicode:#$FE8D; Attr:daIsolated; Str:#$0627),      // ARABIC LETTER ALEF ISOLATED FORM
    (Unicode:#$FE8E; Attr:daFinal; Str:#$0627),         // ARABIC LETTER ALEF FINAL FORM
    (Unicode:#$FE8F; Attr:daIsolated; Str:#$0628),      // ARABIC LETTER BEH ISOLATED FORM
    (Unicode:#$FE90; Attr:daFinal; Str:#$0628),         // ARABIC LETTER BEH FINAL FORM
    (Unicode:#$FE91; Attr:daInitial; Str:#$0628),       // ARABIC LETTER BEH INITIAL FORM
    (Unicode:#$FE92; Attr:daMedial; Str:#$0628),        // ARABIC LETTER BEH MEDIAL FORM
    (Unicode:#$FE93; Attr:daIsolated; Str:#$0629),      // ARABIC LETTER TEH MARBUTA ISOLATED FORM
    (Unicode:#$FE94; Attr:daFinal; Str:#$0629),         // ARABIC LETTER TEH MARBUTA FINAL FORM
    (Unicode:#$FE95; Attr:daIsolated; Str:#$062A),      // ARABIC LETTER TEH ISOLATED FORM
    (Unicode:#$FE96; Attr:daFinal; Str:#$062A),         // ARABIC LETTER TEH FINAL FORM
    (Unicode:#$FE97; Attr:daInitial; Str:#$062A),       // ARABIC LETTER TEH INITIAL FORM
    (Unicode:#$FE98; Attr:daMedial; Str:#$062A),        // ARABIC LETTER TEH MEDIAL FORM
    (Unicode:#$FE99; Attr:daIsolated; Str:#$062B),      // ARABIC LETTER THEH ISOLATED FORM
    (Unicode:#$FE9A; Attr:daFinal; Str:#$062B),         // ARABIC LETTER THEH FINAL FORM
    (Unicode:#$FE9B; Attr:daInitial; Str:#$062B),       // ARABIC LETTER THEH INITIAL FORM
    (Unicode:#$FE9C; Attr:daMedial; Str:#$062B),        // ARABIC LETTER THEH MEDIAL FORM
    (Unicode:#$FE9D; Attr:daIsolated; Str:#$062C),      // ARABIC LETTER JEEM ISOLATED FORM
    (Unicode:#$FE9E; Attr:daFinal; Str:#$062C),         // ARABIC LETTER JEEM FINAL FORM
    (Unicode:#$FE9F; Attr:daInitial; Str:#$062C),       // ARABIC LETTER JEEM INITIAL FORM
    (Unicode:#$FEA0; Attr:daMedial; Str:#$062C),        // ARABIC LETTER JEEM MEDIAL FORM
    (Unicode:#$FEA1; Attr:daIsolated; Str:#$062D),      // ARABIC LETTER HAH ISOLATED FORM
    (Unicode:#$FEA2; Attr:daFinal; Str:#$062D),         // ARABIC LETTER HAH FINAL FORM
    (Unicode:#$FEA3; Attr:daInitial; Str:#$062D),       // ARABIC LETTER HAH INITIAL FORM
    (Unicode:#$FEA4; Attr:daMedial; Str:#$062D),        // ARABIC LETTER HAH MEDIAL FORM
    (Unicode:#$FEA5; Attr:daIsolated; Str:#$062E),      // ARABIC LETTER KHAH ISOLATED FORM
    (Unicode:#$FEA6; Attr:daFinal; Str:#$062E),         // ARABIC LETTER KHAH FINAL FORM
    (Unicode:#$FEA7; Attr:daInitial; Str:#$062E),       // ARABIC LETTER KHAH INITIAL FORM
    (Unicode:#$FEA8; Attr:daMedial; Str:#$062E),        // ARABIC LETTER KHAH MEDIAL FORM
    (Unicode:#$FEA9; Attr:daIsolated; Str:#$062F),      // ARABIC LETTER DAL ISOLATED FORM
    (Unicode:#$FEAA; Attr:daFinal; Str:#$062F),         // ARABIC LETTER DAL FINAL FORM
    (Unicode:#$FEAB; Attr:daIsolated; Str:#$0630),      // ARABIC LETTER THAL ISOLATED FORM
    (Unicode:#$FEAC; Attr:daFinal; Str:#$0630),         // ARABIC LETTER THAL FINAL FORM
    (Unicode:#$FEAD; Attr:daIsolated; Str:#$0631),      // ARABIC LETTER REH ISOLATED FORM
    (Unicode:#$FEAE; Attr:daFinal; Str:#$0631),         // ARABIC LETTER REH FINAL FORM
    (Unicode:#$FEAF; Attr:daIsolated; Str:#$0632),      // ARABIC LETTER ZAIN ISOLATED FORM
    (Unicode:#$FEB0; Attr:daFinal; Str:#$0632),         // ARABIC LETTER ZAIN FINAL FORM
    (Unicode:#$FEB1; Attr:daIsolated; Str:#$0633),      // ARABIC LETTER SEEN ISOLATED FORM
    (Unicode:#$FEB2; Attr:daFinal; Str:#$0633),         // ARABIC LETTER SEEN FINAL FORM
    (Unicode:#$FEB3; Attr:daInitial; Str:#$0633),       // ARABIC LETTER SEEN INITIAL FORM
    (Unicode:#$FEB4; Attr:daMedial; Str:#$0633),        // ARABIC LETTER SEEN MEDIAL FORM
    (Unicode:#$FEB5; Attr:daIsolated; Str:#$0634),      // ARABIC LETTER SHEEN ISOLATED FORM
    (Unicode:#$FEB6; Attr:daFinal; Str:#$0634),         // ARABIC LETTER SHEEN FINAL FORM
    (Unicode:#$FEB7; Attr:daInitial; Str:#$0634),       // ARABIC LETTER SHEEN INITIAL FORM
    (Unicode:#$FEB8; Attr:daMedial; Str:#$0634),        // ARABIC LETTER SHEEN MEDIAL FORM
    (Unicode:#$FEB9; Attr:daIsolated; Str:#$0635),      // ARABIC LETTER SAD ISOLATED FORM
    (Unicode:#$FEBA; Attr:daFinal; Str:#$0635),         // ARABIC LETTER SAD FINAL FORM
    (Unicode:#$FEBB; Attr:daInitial; Str:#$0635),       // ARABIC LETTER SAD INITIAL FORM
    (Unicode:#$FEBC; Attr:daMedial; Str:#$0635),        // ARABIC LETTER SAD MEDIAL FORM
    (Unicode:#$FEBD; Attr:daIsolated; Str:#$0636),      // ARABIC LETTER DAD ISOLATED FORM
    (Unicode:#$FEBE; Attr:daFinal; Str:#$0636),         // ARABIC LETTER DAD FINAL FORM
    (Unicode:#$FEBF; Attr:daInitial; Str:#$0636),       // ARABIC LETTER DAD INITIAL FORM
    (Unicode:#$FEC0; Attr:daMedial; Str:#$0636),        // ARABIC LETTER DAD MEDIAL FORM
    (Unicode:#$FEC1; Attr:daIsolated; Str:#$0637),      // ARABIC LETTER TAH ISOLATED FORM
    (Unicode:#$FEC2; Attr:daFinal; Str:#$0637),         // ARABIC LETTER TAH FINAL FORM
    (Unicode:#$FEC3; Attr:daInitial; Str:#$0637),       // ARABIC LETTER TAH INITIAL FORM
    (Unicode:#$FEC4; Attr:daMedial; Str:#$0637),        // ARABIC LETTER TAH MEDIAL FORM
    (Unicode:#$FEC5; Attr:daIsolated; Str:#$0638),      // ARABIC LETTER ZAH ISOLATED FORM
    (Unicode:#$FEC6; Attr:daFinal; Str:#$0638),         // ARABIC LETTER ZAH FINAL FORM
    (Unicode:#$FEC7; Attr:daInitial; Str:#$0638),       // ARABIC LETTER ZAH INITIAL FORM
    (Unicode:#$FEC8; Attr:daMedial; Str:#$0638),        // ARABIC LETTER ZAH MEDIAL FORM
    (Unicode:#$FEC9; Attr:daIsolated; Str:#$0639),      // ARABIC LETTER AIN ISOLATED FORM
    (Unicode:#$FECA; Attr:daFinal; Str:#$0639),         // ARABIC LETTER AIN FINAL FORM
    (Unicode:#$FECB; Attr:daInitial; Str:#$0639),       // ARABIC LETTER AIN INITIAL FORM
    (Unicode:#$FECC; Attr:daMedial; Str:#$0639),        // ARABIC LETTER AIN MEDIAL FORM
    (Unicode:#$FECD; Attr:daIsolated; Str:#$063A),      // ARABIC LETTER GHAIN ISOLATED FORM
    (Unicode:#$FECE; Attr:daFinal; Str:#$063A),         // ARABIC LETTER GHAIN FINAL FORM
    (Unicode:#$FECF; Attr:daInitial; Str:#$063A),       // ARABIC LETTER GHAIN INITIAL FORM
    (Unicode:#$FED0; Attr:daMedial; Str:#$063A),        // ARABIC LETTER GHAIN MEDIAL FORM
    (Unicode:#$FED1; Attr:daIsolated; Str:#$0641),      // ARABIC LETTER FEH ISOLATED FORM
    (Unicode:#$FED2; Attr:daFinal; Str:#$0641),         // ARABIC LETTER FEH FINAL FORM
    (Unicode:#$FED3; Attr:daInitial; Str:#$0641),       // ARABIC LETTER FEH INITIAL FORM
    (Unicode:#$FED4; Attr:daMedial; Str:#$0641),        // ARABIC LETTER FEH MEDIAL FORM
    (Unicode:#$FED5; Attr:daIsolated; Str:#$0642),      // ARABIC LETTER QAF ISOLATED FORM
    (Unicode:#$FED6; Attr:daFinal; Str:#$0642),         // ARABIC LETTER QAF FINAL FORM
    (Unicode:#$FED7; Attr:daInitial; Str:#$0642),       // ARABIC LETTER QAF INITIAL FORM
    (Unicode:#$FED8; Attr:daMedial; Str:#$0642),        // ARABIC LETTER QAF MEDIAL FORM
    (Unicode:#$FED9; Attr:daIsolated; Str:#$0643),      // ARABIC LETTER KAF ISOLATED FORM
    (Unicode:#$FEDA; Attr:daFinal; Str:#$0643),         // ARABIC LETTER KAF FINAL FORM
    (Unicode:#$FEDB; Attr:daInitial; Str:#$0643),       // ARABIC LETTER KAF INITIAL FORM
    (Unicode:#$FEDC; Attr:daMedial; Str:#$0643),        // ARABIC LETTER KAF MEDIAL FORM
    (Unicode:#$FEDD; Attr:daIsolated; Str:#$0644),      // ARABIC LETTER LAM ISOLATED FORM
    (Unicode:#$FEDE; Attr:daFinal; Str:#$0644),         // ARABIC LETTER LAM FINAL FORM
    (Unicode:#$FEDF; Attr:daInitial; Str:#$0644),       // ARABIC LETTER LAM INITIAL FORM
    (Unicode:#$FEE0; Attr:daMedial; Str:#$0644),        // ARABIC LETTER LAM MEDIAL FORM
    (Unicode:#$FEE1; Attr:daIsolated; Str:#$0645),      // ARABIC LETTER MEEM ISOLATED FORM
    (Unicode:#$FEE2; Attr:daFinal; Str:#$0645),         // ARABIC LETTER MEEM FINAL FORM
    (Unicode:#$FEE3; Attr:daInitial; Str:#$0645),       // ARABIC LETTER MEEM INITIAL FORM
    (Unicode:#$FEE4; Attr:daMedial; Str:#$0645),        // ARABIC LETTER MEEM MEDIAL FORM
    (Unicode:#$FEE5; Attr:daIsolated; Str:#$0646),      // ARABIC LETTER NOON ISOLATED FORM
    (Unicode:#$FEE6; Attr:daFinal; Str:#$0646),         // ARABIC LETTER NOON FINAL FORM
    (Unicode:#$FEE7; Attr:daInitial; Str:#$0646),       // ARABIC LETTER NOON INITIAL FORM
    (Unicode:#$FEE8; Attr:daMedial; Str:#$0646),        // ARABIC LETTER NOON MEDIAL FORM
    (Unicode:#$FEE9; Attr:daIsolated; Str:#$0647),      // ARABIC LETTER HEH ISOLATED FORM
    (Unicode:#$FEEA; Attr:daFinal; Str:#$0647),         // ARABIC LETTER HEH FINAL FORM
    (Unicode:#$FEEB; Attr:daInitial; Str:#$0647),       // ARABIC LETTER HEH INITIAL FORM
    (Unicode:#$FEEC; Attr:daMedial; Str:#$0647),        // ARABIC LETTER HEH MEDIAL FORM
    (Unicode:#$FEED; Attr:daIsolated; Str:#$0648),      // ARABIC LETTER WAW ISOLATED FORM
    (Unicode:#$FEEE; Attr:daFinal; Str:#$0648),         // ARABIC LETTER WAW FINAL FORM
    (Unicode:#$FEEF; Attr:daIsolated; Str:#$0649),      // ARABIC LETTER ALEF MAKSURA ISOLATED FORM
    (Unicode:#$FEF0; Attr:daFinal; Str:#$0649),         // ARABIC LETTER ALEF MAKSURA FINAL FORM
    (Unicode:#$FEF1; Attr:daIsolated; Str:#$064A),      // ARABIC LETTER YEH ISOLATED FORM
    (Unicode:#$FEF2; Attr:daFinal; Str:#$064A),         // ARABIC LETTER YEH FINAL FORM
    (Unicode:#$FEF3; Attr:daInitial; Str:#$064A),       // ARABIC LETTER YEH INITIAL FORM
    (Unicode:#$FEF4; Attr:daMedial; Str:#$064A),        // ARABIC LETTER YEH MEDIAL FORM
    (Unicode:#$FEF5; Attr:daIsolated; Str:#$0644#$0622),// ARABIC LIGATURE LAM WITH ALEF WITH MADDA ABOVE ISOLATED FORM
    (Unicode:#$FEF6; Attr:daFinal; Str:#$0644#$0622),   // ARABIC LIGATURE LAM WITH ALEF WITH MADDA ABOVE FINAL FORM
    (Unicode:#$FEF7; Attr:daIsolated; Str:#$0644#$0623),// ARABIC LIGATURE LAM WITH ALEF WITH HAMZA ABOVE ISOLATED FORM
    (Unicode:#$FEF8; Attr:daFinal; Str:#$0644#$0623),   // ARABIC LIGATURE LAM WITH ALEF WITH HAMZA ABOVE FINAL FORM
    (Unicode:#$FEF9; Attr:daIsolated; Str:#$0644#$0625),// ARABIC LIGATURE LAM WITH ALEF WITH HAMZA BELOW ISOLATED FORM
    (Unicode:#$FEFA; Attr:daFinal; Str:#$0644#$0625),   // ARABIC LIGATURE LAM WITH ALEF WITH HAMZA BELOW FINAL FORM
    (Unicode:#$FEFB; Attr:daIsolated; Str:#$0644#$0627),// ARABIC LIGATURE LAM WITH ALEF ISOLATED FORM
    (Unicode:#$FEFC; Attr:daFinal; Str:#$0644#$0627),   // ARABIC LIGATURE LAM WITH ALEF FINAL FORM
    (Unicode:#$FF01; Attr:daWide; Str:#$0021),          // FULLWIDTH EXCLAMATION MARK
    (Unicode:#$FF02; Attr:daWide; Str:#$0022),          // FULLWIDTH QUOTATION MARK
    (Unicode:#$FF03; Attr:daWide; Str:#$0023),          // FULLWIDTH NUMBER SIGN
    (Unicode:#$FF04; Attr:daWide; Str:#$0024),          // FULLWIDTH DOLLAR SIGN
    (Unicode:#$FF05; Attr:daWide; Str:#$0025),          // FULLWIDTH PERCENT SIGN
    (Unicode:#$FF06; Attr:daWide; Str:#$0026),          // FULLWIDTH AMPERSAND
    (Unicode:#$FF07; Attr:daWide; Str:#$0027),          // FULLWIDTH APOSTROPHE
    (Unicode:#$FF08; Attr:daWide; Str:#$0028),          // FULLWIDTH LEFT PARENTHESIS
    (Unicode:#$FF09; Attr:daWide; Str:#$0029),          // FULLWIDTH RIGHT PARENTHESIS
    (Unicode:#$FF0A; Attr:daWide; Str:#$002A),          // FULLWIDTH ASTERISK
    (Unicode:#$FF0B; Attr:daWide; Str:#$002B),          // FULLWIDTH PLUS SIGN
    (Unicode:#$FF0C; Attr:daWide; Str:#$002C),          // FULLWIDTH COMMA
    (Unicode:#$FF0D; Attr:daWide; Str:#$002D),          // FULLWIDTH HYPHEN-MINUS
    (Unicode:#$FF0E; Attr:daWide; Str:#$002E),          // FULLWIDTH FULL STOP
    (Unicode:#$FF0F; Attr:daWide; Str:#$002F),          // FULLWIDTH SOLIDUS
    (Unicode:#$FF10; Attr:daWide; Str:#$0030),          // FULLWIDTH DIGIT ZERO
    (Unicode:#$FF11; Attr:daWide; Str:#$0031),          // FULLWIDTH DIGIT ONE
    (Unicode:#$FF12; Attr:daWide; Str:#$0032),          // FULLWIDTH DIGIT TWO
    (Unicode:#$FF13; Attr:daWide; Str:#$0033),          // FULLWIDTH DIGIT THREE
    (Unicode:#$FF14; Attr:daWide; Str:#$0034),          // FULLWIDTH DIGIT FOUR
    (Unicode:#$FF15; Attr:daWide; Str:#$0035),          // FULLWIDTH DIGIT FIVE
    (Unicode:#$FF16; Attr:daWide; Str:#$0036),          // FULLWIDTH DIGIT SIX
    (Unicode:#$FF17; Attr:daWide; Str:#$0037),          // FULLWIDTH DIGIT SEVEN
    (Unicode:#$FF18; Attr:daWide; Str:#$0038),          // FULLWIDTH DIGIT EIGHT
    (Unicode:#$FF19; Attr:daWide; Str:#$0039),          // FULLWIDTH DIGIT NINE
    (Unicode:#$FF1A; Attr:daWide; Str:#$003A),          // FULLWIDTH COLON
    (Unicode:#$FF1B; Attr:daWide; Str:#$003B),          // FULLWIDTH SEMICOLON
    (Unicode:#$FF1C; Attr:daWide; Str:#$003C),          // FULLWIDTH LESS-THAN SIGN
    (Unicode:#$FF1D; Attr:daWide; Str:#$003D),          // FULLWIDTH EQUALS SIGN
    (Unicode:#$FF1E; Attr:daWide; Str:#$003E),          // FULLWIDTH GREATER-THAN SIGN
    (Unicode:#$FF1F; Attr:daWide; Str:#$003F),          // FULLWIDTH QUESTION MARK
    (Unicode:#$FF20; Attr:daWide; Str:#$0040),          // FULLWIDTH COMMERCIAL AT
    (Unicode:#$FF21; Attr:daWide; Str:#$0041),          // FULLWIDTH LATIN CAPITAL LETTER A
    (Unicode:#$FF22; Attr:daWide; Str:#$0042),          // FULLWIDTH LATIN CAPITAL LETTER B
    (Unicode:#$FF23; Attr:daWide; Str:#$0043),          // FULLWIDTH LATIN CAPITAL LETTER C
    (Unicode:#$FF24; Attr:daWide; Str:#$0044),          // FULLWIDTH LATIN CAPITAL LETTER D
    (Unicode:#$FF25; Attr:daWide; Str:#$0045),          // FULLWIDTH LATIN CAPITAL LETTER E
    (Unicode:#$FF26; Attr:daWide; Str:#$0046),          // FULLWIDTH LATIN CAPITAL LETTER F
    (Unicode:#$FF27; Attr:daWide; Str:#$0047),          // FULLWIDTH LATIN CAPITAL LETTER G
    (Unicode:#$FF28; Attr:daWide; Str:#$0048),          // FULLWIDTH LATIN CAPITAL LETTER H
    (Unicode:#$FF29; Attr:daWide; Str:#$0049),          // FULLWIDTH LATIN CAPITAL LETTER I
    (Unicode:#$FF2A; Attr:daWide; Str:#$004A),          // FULLWIDTH LATIN CAPITAL LETTER J
    (Unicode:#$FF2B; Attr:daWide; Str:#$004B),          // FULLWIDTH LATIN CAPITAL LETTER K
    (Unicode:#$FF2C; Attr:daWide; Str:#$004C),          // FULLWIDTH LATIN CAPITAL LETTER L
    (Unicode:#$FF2D; Attr:daWide; Str:#$004D),          // FULLWIDTH LATIN CAPITAL LETTER M
    (Unicode:#$FF2E; Attr:daWide; Str:#$004E),          // FULLWIDTH LATIN CAPITAL LETTER N
    (Unicode:#$FF2F; Attr:daWide; Str:#$004F),          // FULLWIDTH LATIN CAPITAL LETTER O
    (Unicode:#$FF30; Attr:daWide; Str:#$0050),          // FULLWIDTH LATIN CAPITAL LETTER P
    (Unicode:#$FF31; Attr:daWide; Str:#$0051),          // FULLWIDTH LATIN CAPITAL LETTER Q
    (Unicode:#$FF32; Attr:daWide; Str:#$0052),          // FULLWIDTH LATIN CAPITAL LETTER R
    (Unicode:#$FF33; Attr:daWide; Str:#$0053),          // FULLWIDTH LATIN CAPITAL LETTER S
    (Unicode:#$FF34; Attr:daWide; Str:#$0054),          // FULLWIDTH LATIN CAPITAL LETTER T
    (Unicode:#$FF35; Attr:daWide; Str:#$0055),          // FULLWIDTH LATIN CAPITAL LETTER U
    (Unicode:#$FF36; Attr:daWide; Str:#$0056),          // FULLWIDTH LATIN CAPITAL LETTER V
    (Unicode:#$FF37; Attr:daWide; Str:#$0057),          // FULLWIDTH LATIN CAPITAL LETTER W
    (Unicode:#$FF38; Attr:daWide; Str:#$0058),          // FULLWIDTH LATIN CAPITAL LETTER X
    (Unicode:#$FF39; Attr:daWide; Str:#$0059),          // FULLWIDTH LATIN CAPITAL LETTER Y
    (Unicode:#$FF3A; Attr:daWide; Str:#$005A),          // FULLWIDTH LATIN CAPITAL LETTER Z
    (Unicode:#$FF3B; Attr:daWide; Str:#$005B),          // FULLWIDTH LEFT SQUARE BRACKET
    (Unicode:#$FF3C; Attr:daWide; Str:#$005C),          // FULLWIDTH REVERSE SOLIDUS
    (Unicode:#$FF3D; Attr:daWide; Str:#$005D),          // FULLWIDTH RIGHT SQUARE BRACKET
    (Unicode:#$FF3E; Attr:daWide; Str:#$005E),          // FULLWIDTH CIRCUMFLEX ACCENT
    (Unicode:#$FF3F; Attr:daWide; Str:#$005F),          // FULLWIDTH LOW LINE
    (Unicode:#$FF40; Attr:daWide; Str:#$0060),          // FULLWIDTH GRAVE ACCENT
    (Unicode:#$FF41; Attr:daWide; Str:#$0061),          // FULLWIDTH LATIN SMALL LETTER A
    (Unicode:#$FF42; Attr:daWide; Str:#$0062),          // FULLWIDTH LATIN SMALL LETTER B
    (Unicode:#$FF43; Attr:daWide; Str:#$0063),          // FULLWIDTH LATIN SMALL LETTER C
    (Unicode:#$FF44; Attr:daWide; Str:#$0064),          // FULLWIDTH LATIN SMALL LETTER D
    (Unicode:#$FF45; Attr:daWide; Str:#$0065),          // FULLWIDTH LATIN SMALL LETTER E
    (Unicode:#$FF46; Attr:daWide; Str:#$0066),          // FULLWIDTH LATIN SMALL LETTER F
    (Unicode:#$FF47; Attr:daWide; Str:#$0067),          // FULLWIDTH LATIN SMALL LETTER G
    (Unicode:#$FF48; Attr:daWide; Str:#$0068),          // FULLWIDTH LATIN SMALL LETTER H
    (Unicode:#$FF49; Attr:daWide; Str:#$0069),          // FULLWIDTH LATIN SMALL LETTER I
    (Unicode:#$FF4A; Attr:daWide; Str:#$006A),          // FULLWIDTH LATIN SMALL LETTER J
    (Unicode:#$FF4B; Attr:daWide; Str:#$006B),          // FULLWIDTH LATIN SMALL LETTER K
    (Unicode:#$FF4C; Attr:daWide; Str:#$006C),          // FULLWIDTH LATIN SMALL LETTER L
    (Unicode:#$FF4D; Attr:daWide; Str:#$006D),          // FULLWIDTH LATIN SMALL LETTER M
    (Unicode:#$FF4E; Attr:daWide; Str:#$006E),          // FULLWIDTH LATIN SMALL LETTER N
    (Unicode:#$FF4F; Attr:daWide; Str:#$006F),          // FULLWIDTH LATIN SMALL LETTER O
    (Unicode:#$FF50; Attr:daWide; Str:#$0070),          // FULLWIDTH LATIN SMALL LETTER P
    (Unicode:#$FF51; Attr:daWide; Str:#$0071),          // FULLWIDTH LATIN SMALL LETTER Q
    (Unicode:#$FF52; Attr:daWide; Str:#$0072),          // FULLWIDTH LATIN SMALL LETTER R
    (Unicode:#$FF53; Attr:daWide; Str:#$0073),          // FULLWIDTH LATIN SMALL LETTER S
    (Unicode:#$FF54; Attr:daWide; Str:#$0074),          // FULLWIDTH LATIN SMALL LETTER T
    (Unicode:#$FF55; Attr:daWide; Str:#$0075),          // FULLWIDTH LATIN SMALL LETTER U
    (Unicode:#$FF56; Attr:daWide; Str:#$0076),          // FULLWIDTH LATIN SMALL LETTER V
    (Unicode:#$FF57; Attr:daWide; Str:#$0077),          // FULLWIDTH LATIN SMALL LETTER W
    (Unicode:#$FF58; Attr:daWide; Str:#$0078),          // FULLWIDTH LATIN SMALL LETTER X
    (Unicode:#$FF59; Attr:daWide; Str:#$0079),          // FULLWIDTH LATIN SMALL LETTER Y
    (Unicode:#$FF5A; Attr:daWide; Str:#$007A),          // FULLWIDTH LATIN SMALL LETTER Z
    (Unicode:#$FF5B; Attr:daWide; Str:#$007B),          // FULLWIDTH LEFT CURLY BRACKET
    (Unicode:#$FF5C; Attr:daWide; Str:#$007C),          // FULLWIDTH VERTICAL LINE
    (Unicode:#$FF5D; Attr:daWide; Str:#$007D),          // FULLWIDTH RIGHT CURLY BRACKET
    (Unicode:#$FF5E; Attr:daWide; Str:#$007E),          // FULLWIDTH TILDE
    (Unicode:#$FF61; Attr:daNarrow; Str:#$3002),        // HALFWIDTH IDEOGRAPHIC FULL STOP
    (Unicode:#$FF62; Attr:daNarrow; Str:#$300C),        // HALFWIDTH LEFT CORNER BRACKET
    (Unicode:#$FF63; Attr:daNarrow; Str:#$300D),        // HALFWIDTH RIGHT CORNER BRACKET
    (Unicode:#$FF64; Attr:daNarrow; Str:#$3001),        // HALFWIDTH IDEOGRAPHIC COMMA
    (Unicode:#$FF65; Attr:daNarrow; Str:#$30FB),        // HALFWIDTH KATAKANA MIDDLE DOT
    (Unicode:#$FF66; Attr:daNarrow; Str:#$30F2),        // HALFWIDTH KATAKANA LETTER WO
    (Unicode:#$FF67; Attr:daNarrow; Str:#$30A1),        // HALFWIDTH KATAKANA LETTER SMALL A
    (Unicode:#$FF68; Attr:daNarrow; Str:#$30A3),        // HALFWIDTH KATAKANA LETTER SMALL I
    (Unicode:#$FF69; Attr:daNarrow; Str:#$30A5),        // HALFWIDTH KATAKANA LETTER SMALL U
    (Unicode:#$FF6A; Attr:daNarrow; Str:#$30A7),        // HALFWIDTH KATAKANA LETTER SMALL E
    (Unicode:#$FF6B; Attr:daNarrow; Str:#$30A9),        // HALFWIDTH KATAKANA LETTER SMALL O
    (Unicode:#$FF6C; Attr:daNarrow; Str:#$30E3),        // HALFWIDTH KATAKANA LETTER SMALL YA
    (Unicode:#$FF6D; Attr:daNarrow; Str:#$30E5),        // HALFWIDTH KATAKANA LETTER SMALL YU
    (Unicode:#$FF6E; Attr:daNarrow; Str:#$30E7),        // HALFWIDTH KATAKANA LETTER SMALL YO
    (Unicode:#$FF6F; Attr:daNarrow; Str:#$30C3),        // HALFWIDTH KATAKANA LETTER SMALL TU
    (Unicode:#$FF70; Attr:daNarrow; Str:#$30FC),        // HALFWIDTH KATAKANA-HIRAGANA PROLONGED SOUND MARK
    (Unicode:#$FF71; Attr:daNarrow; Str:#$30A2),        // HALFWIDTH KATAKANA LETTER A
    (Unicode:#$FF72; Attr:daNarrow; Str:#$30A4),        // HALFWIDTH KATAKANA LETTER I
    (Unicode:#$FF73; Attr:daNarrow; Str:#$30A6),        // HALFWIDTH KATAKANA LETTER U
    (Unicode:#$FF74; Attr:daNarrow; Str:#$30A8),        // HALFWIDTH KATAKANA LETTER E
    (Unicode:#$FF75; Attr:daNarrow; Str:#$30AA),        // HALFWIDTH KATAKANA LETTER O
    (Unicode:#$FF76; Attr:daNarrow; Str:#$30AB),        // HALFWIDTH KATAKANA LETTER KA
    (Unicode:#$FF77; Attr:daNarrow; Str:#$30AD),        // HALFWIDTH KATAKANA LETTER KI
    (Unicode:#$FF78; Attr:daNarrow; Str:#$30AF),        // HALFWIDTH KATAKANA LETTER KU
    (Unicode:#$FF79; Attr:daNarrow; Str:#$30B1),        // HALFWIDTH KATAKANA LETTER KE
    (Unicode:#$FF7A; Attr:daNarrow; Str:#$30B3),        // HALFWIDTH KATAKANA LETTER KO
    (Unicode:#$FF7B; Attr:daNarrow; Str:#$30B5),        // HALFWIDTH KATAKANA LETTER SA
    (Unicode:#$FF7C; Attr:daNarrow; Str:#$30B7),        // HALFWIDTH KATAKANA LETTER SI
    (Unicode:#$FF7D; Attr:daNarrow; Str:#$30B9),        // HALFWIDTH KATAKANA LETTER SU
    (Unicode:#$FF7E; Attr:daNarrow; Str:#$30BB),        // HALFWIDTH KATAKANA LETTER SE
    (Unicode:#$FF7F; Attr:daNarrow; Str:#$30BD),        // HALFWIDTH KATAKANA LETTER SO
    (Unicode:#$FF80; Attr:daNarrow; Str:#$30BF),        // HALFWIDTH KATAKANA LETTER TA
    (Unicode:#$FF81; Attr:daNarrow; Str:#$30C1),        // HALFWIDTH KATAKANA LETTER TI
    (Unicode:#$FF82; Attr:daNarrow; Str:#$30C4),        // HALFWIDTH KATAKANA LETTER TU
    (Unicode:#$FF83; Attr:daNarrow; Str:#$30C6),        // HALFWIDTH KATAKANA LETTER TE
    (Unicode:#$FF84; Attr:daNarrow; Str:#$30C8),        // HALFWIDTH KATAKANA LETTER TO
    (Unicode:#$FF85; Attr:daNarrow; Str:#$30CA),        // HALFWIDTH KATAKANA LETTER NA
    (Unicode:#$FF86; Attr:daNarrow; Str:#$30CB),        // HALFWIDTH KATAKANA LETTER NI
    (Unicode:#$FF87; Attr:daNarrow; Str:#$30CC),        // HALFWIDTH KATAKANA LETTER NU
    (Unicode:#$FF88; Attr:daNarrow; Str:#$30CD),        // HALFWIDTH KATAKANA LETTER NE
    (Unicode:#$FF89; Attr:daNarrow; Str:#$30CE),        // HALFWIDTH KATAKANA LETTER NO
    (Unicode:#$FF8A; Attr:daNarrow; Str:#$30CF),        // HALFWIDTH KATAKANA LETTER HA
    (Unicode:#$FF8B; Attr:daNarrow; Str:#$30D2),        // HALFWIDTH KATAKANA LETTER HI
    (Unicode:#$FF8C; Attr:daNarrow; Str:#$30D5),        // HALFWIDTH KATAKANA LETTER HU
    (Unicode:#$FF8D; Attr:daNarrow; Str:#$30D8),        // HALFWIDTH KATAKANA LETTER HE
    (Unicode:#$FF8E; Attr:daNarrow; Str:#$30DB),        // HALFWIDTH KATAKANA LETTER HO
    (Unicode:#$FF8F; Attr:daNarrow; Str:#$30DE),        // HALFWIDTH KATAKANA LETTER MA
    (Unicode:#$FF90; Attr:daNarrow; Str:#$30DF),        // HALFWIDTH KATAKANA LETTER MI
    (Unicode:#$FF91; Attr:daNarrow; Str:#$30E0),        // HALFWIDTH KATAKANA LETTER MU
    (Unicode:#$FF92; Attr:daNarrow; Str:#$30E1),        // HALFWIDTH KATAKANA LETTER ME
    (Unicode:#$FF93; Attr:daNarrow; Str:#$30E2),        // HALFWIDTH KATAKANA LETTER MO
    (Unicode:#$FF94; Attr:daNarrow; Str:#$30E4),        // HALFWIDTH KATAKANA LETTER YA
    (Unicode:#$FF95; Attr:daNarrow; Str:#$30E6),        // HALFWIDTH KATAKANA LETTER YU
    (Unicode:#$FF96; Attr:daNarrow; Str:#$30E8),        // HALFWIDTH KATAKANA LETTER YO
    (Unicode:#$FF97; Attr:daNarrow; Str:#$30E9),        // HALFWIDTH KATAKANA LETTER RA
    (Unicode:#$FF98; Attr:daNarrow; Str:#$30EA),        // HALFWIDTH KATAKANA LETTER RI
    (Unicode:#$FF99; Attr:daNarrow; Str:#$30EB),        // HALFWIDTH KATAKANA LETTER RU
    (Unicode:#$FF9A; Attr:daNarrow; Str:#$30EC),        // HALFWIDTH KATAKANA LETTER RE
    (Unicode:#$FF9B; Attr:daNarrow; Str:#$30ED),        // HALFWIDTH KATAKANA LETTER RO
    (Unicode:#$FF9C; Attr:daNarrow; Str:#$30EF),        // HALFWIDTH KATAKANA LETTER WA
    (Unicode:#$FF9D; Attr:daNarrow; Str:#$30F3),        // HALFWIDTH KATAKANA LETTER N
    (Unicode:#$FF9E; Attr:daNarrow; Str:#$3099),        // HALFWIDTH KATAKANA VOICED SOUND MARK
    (Unicode:#$FF9F; Attr:daNarrow; Str:#$309A),        // HALFWIDTH KATAKANA SEMI-VOICED SOUND MARK
    (Unicode:#$FFA0; Attr:daNarrow; Str:#$3164),        // HALFWIDTH HANGUL FILLER
    (Unicode:#$FFA1; Attr:daNarrow; Str:#$3131),        // HALFWIDTH HANGUL LETTER KIYEOK
    (Unicode:#$FFA2; Attr:daNarrow; Str:#$3132),        // HALFWIDTH HANGUL LETTER SSANGKIYEOK
    (Unicode:#$FFA3; Attr:daNarrow; Str:#$3133),        // HALFWIDTH HANGUL LETTER KIYEOK-SIOS
    (Unicode:#$FFA4; Attr:daNarrow; Str:#$3134),        // HALFWIDTH HANGUL LETTER NIEUN
    (Unicode:#$FFA5; Attr:daNarrow; Str:#$3135),        // HALFWIDTH HANGUL LETTER NIEUN-CIEUC
    (Unicode:#$FFA6; Attr:daNarrow; Str:#$3136),        // HALFWIDTH HANGUL LETTER NIEUN-HIEUH
    (Unicode:#$FFA7; Attr:daNarrow; Str:#$3137),        // HALFWIDTH HANGUL LETTER TIKEUT
    (Unicode:#$FFA8; Attr:daNarrow; Str:#$3138),        // HALFWIDTH HANGUL LETTER SSANGTIKEUT
    (Unicode:#$FFA9; Attr:daNarrow; Str:#$3139),        // HALFWIDTH HANGUL LETTER RIEUL
    (Unicode:#$FFAA; Attr:daNarrow; Str:#$313A),        // HALFWIDTH HANGUL LETTER RIEUL-KIYEOK
    (Unicode:#$FFAB; Attr:daNarrow; Str:#$313B),        // HALFWIDTH HANGUL LETTER RIEUL-MIEUM
    (Unicode:#$FFAC; Attr:daNarrow; Str:#$313C),        // HALFWIDTH HANGUL LETTER RIEUL-PIEUP
    (Unicode:#$FFAD; Attr:daNarrow; Str:#$313D),        // HALFWIDTH HANGUL LETTER RIEUL-SIOS
    (Unicode:#$FFAE; Attr:daNarrow; Str:#$313E),        // HALFWIDTH HANGUL LETTER RIEUL-THIEUTH
    (Unicode:#$FFAF; Attr:daNarrow; Str:#$313F),        // HALFWIDTH HANGUL LETTER RIEUL-PHIEUPH
    (Unicode:#$FFB0; Attr:daNarrow; Str:#$3140),        // HALFWIDTH HANGUL LETTER RIEUL-HIEUH
    (Unicode:#$FFB1; Attr:daNarrow; Str:#$3141),        // HALFWIDTH HANGUL LETTER MIEUM
    (Unicode:#$FFB2; Attr:daNarrow; Str:#$3142),        // HALFWIDTH HANGUL LETTER PIEUP
    (Unicode:#$FFB3; Attr:daNarrow; Str:#$3143),        // HALFWIDTH HANGUL LETTER SSANGPIEUP
    (Unicode:#$FFB4; Attr:daNarrow; Str:#$3144),        // HALFWIDTH HANGUL LETTER PIEUP-SIOS
    (Unicode:#$FFB5; Attr:daNarrow; Str:#$3145),        // HALFWIDTH HANGUL LETTER SIOS
    (Unicode:#$FFB6; Attr:daNarrow; Str:#$3146),        // HALFWIDTH HANGUL LETTER SSANGSIOS
    (Unicode:#$FFB7; Attr:daNarrow; Str:#$3147),        // HALFWIDTH HANGUL LETTER IEUNG
    (Unicode:#$FFB8; Attr:daNarrow; Str:#$3148),        // HALFWIDTH HANGUL LETTER CIEUC
    (Unicode:#$FFB9; Attr:daNarrow; Str:#$3149),        // HALFWIDTH HANGUL LETTER SSANGCIEUC
    (Unicode:#$FFBA; Attr:daNarrow; Str:#$314A),        // HALFWIDTH HANGUL LETTER CHIEUCH
    (Unicode:#$FFBB; Attr:daNarrow; Str:#$314B),        // HALFWIDTH HANGUL LETTER KHIEUKH
    (Unicode:#$FFBC; Attr:daNarrow; Str:#$314C),        // HALFWIDTH HANGUL LETTER THIEUTH
    (Unicode:#$FFBD; Attr:daNarrow; Str:#$314D),        // HALFWIDTH HANGUL LETTER PHIEUPH
    (Unicode:#$FFBE; Attr:daNarrow; Str:#$314E),        // HALFWIDTH HANGUL LETTER HIEUH
    (Unicode:#$FFC2; Attr:daNarrow; Str:#$314F),        // HALFWIDTH HANGUL LETTER A
    (Unicode:#$FFC3; Attr:daNarrow; Str:#$3150),        // HALFWIDTH HANGUL LETTER AE
    (Unicode:#$FFC4; Attr:daNarrow; Str:#$3151),        // HALFWIDTH HANGUL LETTER YA
    (Unicode:#$FFC5; Attr:daNarrow; Str:#$3152),        // HALFWIDTH HANGUL LETTER YAE
    (Unicode:#$FFC6; Attr:daNarrow; Str:#$3153),        // HALFWIDTH HANGUL LETTER EO
    (Unicode:#$FFC7; Attr:daNarrow; Str:#$3154),        // HALFWIDTH HANGUL LETTER E
    (Unicode:#$FFCA; Attr:daNarrow; Str:#$3155),        // HALFWIDTH HANGUL LETTER YEO
    (Unicode:#$FFCB; Attr:daNarrow; Str:#$3156),        // HALFWIDTH HANGUL LETTER YE
    (Unicode:#$FFCC; Attr:daNarrow; Str:#$3157),        // HALFWIDTH HANGUL LETTER O
    (Unicode:#$FFCD; Attr:daNarrow; Str:#$3158),        // HALFWIDTH HANGUL LETTER WA
    (Unicode:#$FFCE; Attr:daNarrow; Str:#$3159),        // HALFWIDTH HANGUL LETTER WAE
    (Unicode:#$FFCF; Attr:daNarrow; Str:#$315A),        // HALFWIDTH HANGUL LETTER OE
    (Unicode:#$FFD2; Attr:daNarrow; Str:#$315B),        // HALFWIDTH HANGUL LETTER YO
    (Unicode:#$FFD3; Attr:daNarrow; Str:#$315C),        // HALFWIDTH HANGUL LETTER U
    (Unicode:#$FFD4; Attr:daNarrow; Str:#$315D),        // HALFWIDTH HANGUL LETTER WEO
    (Unicode:#$FFD5; Attr:daNarrow; Str:#$315E),        // HALFWIDTH HANGUL LETTER WE
    (Unicode:#$FFD6; Attr:daNarrow; Str:#$315F),        // HALFWIDTH HANGUL LETTER WI
    (Unicode:#$FFD7; Attr:daNarrow; Str:#$3160),        // HALFWIDTH HANGUL LETTER YU
    (Unicode:#$FFDA; Attr:daNarrow; Str:#$3161),        // HALFWIDTH HANGUL LETTER EU
    (Unicode:#$FFDB; Attr:daNarrow; Str:#$3162),        // HALFWIDTH HANGUL LETTER YI
    (Unicode:#$FFDC; Attr:daNarrow; Str:#$3163),        // HALFWIDTH HANGUL LETTER I
    (Unicode:#$FFE0; Attr:daWide; Str:#$00A2),          // FULLWIDTH CENT SIGN
    (Unicode:#$FFE1; Attr:daWide; Str:#$00A3),          // FULLWIDTH POUND SIGN
    (Unicode:#$FFE2; Attr:daWide; Str:#$00AC),          // FULLWIDTH NOT SIGN
    (Unicode:#$FFE3; Attr:daWide; Str:#$00AF),          // FULLWIDTH MACRON
    (Unicode:#$FFE4; Attr:daWide; Str:#$00A6),          // FULLWIDTH BROKEN BAR
    (Unicode:#$FFE5; Attr:daWide; Str:#$00A5),          // FULLWIDTH YEN SIGN
    (Unicode:#$FFE6; Attr:daWide; Str:#$20A9),          // FULLWIDTH WON SIGN
    (Unicode:#$FFE8; Attr:daNarrow; Str:#$2502),        // HALFWIDTH FORMS LIGHT VERTICAL
    (Unicode:#$FFE9; Attr:daNarrow; Str:#$2190),        // HALFWIDTH LEFTWARDS ARROW
    (Unicode:#$FFEA; Attr:daNarrow; Str:#$2191),        // HALFWIDTH UPWARDS ARROW
    (Unicode:#$FFEB; Attr:daNarrow; Str:#$2192),        // HALFWIDTH RIGHTWARDS ARROW
    (Unicode:#$FFEC; Attr:daNarrow; Str:#$2193),        // HALFWIDTH DOWNWARDS ARROW
    (Unicode:#$FFED; Attr:daNarrow; Str:#$25A0),        // HALFWIDTH BLACK SQUARE
    (Unicode:#$FFEE; Attr:daNarrow; Str:#$25CB)         // HALFWIDTH WHITE CIRCLE
    );

function LocateDecompositionInfo(const Ch: WideChar): PUnicodeDecompositionInfo;
var L, H, I : Integer;
    D : WideChar;
begin
  if Ord(Ch) < $A0 then // No decompositions for ASCII
    begin
      Result := nil;
      exit;
    end;

  // Binary search
  L := 0;
  H := UnicodeDecompositionEntries - 1;
  Repeat
    I := (L + H) div 2;
    D := UnicodeDecompositionInfo[I].Unicode;
    if D = Ch then
      begin
        Result := @UnicodeDecompositionInfo[I];
        exit;
      end else
    if D > Ch then
      H := I - 1 else
      L := I + 1;
  Until L > H;
  Result := nil;
end;

function GetCharacterDecomposition(const Ch: WideChar): WideString;
var I : PUnicodeDecompositionInfo;
begin
  I := LocateDecompositionInfo(Ch);
  if not Assigned(I) then
    Result := '' else
    Result := I^.Str;
end;

type
  TUnicodeUCS4DecompositionInfo = packed record
    Unicode : UCS4Char;
    Attr    : TUnicodeDecompositionAttr;
    Str     : WideString;
  end;
  PUnicodeUCS4DecompositionInfo = ^TUnicodeUCS4DecompositionInfo;

const
  UnicodeUCS4DecompositionEntries = 1004; // ~14K
  UnicodeUCS4DecompositionInfo : Array[0..UnicodeUCS4DecompositionEntries - 1] of TUnicodeUCS4DecompositionInfo = (
    (Unicode:$1D15E; Attr:daNone; Str:#$1D157#$1D165),     // MUSICAL SYMBOL HALF NOTE
    (Unicode:$1D15F; Attr:daNone; Str:#$1D158#$1D165),     // MUSICAL SYMBOL QUARTER NOTE
    (Unicode:$1D160; Attr:daNone; Str:#$1D15F#$1D16E),     // MUSICAL SYMBOL EIGHTH NOTE
    (Unicode:$1D161; Attr:daNone; Str:#$1D15F#$1D16F),     // MUSICAL SYMBOL SIXTEENTH NOTE
    (Unicode:$1D162; Attr:daNone; Str:#$1D15F#$1D170),     // MUSICAL SYMBOL THIRTY-SECOND NOTE
    (Unicode:$1D163; Attr:daNone; Str:#$1D15F#$1D171),     // MUSICAL SYMBOL SIXTY-FOURTH NOTE
    (Unicode:$1D164; Attr:daNone; Str:#$1D15F#$1D172),     // MUSICAL SYMBOL ONE HUNDRED TWENTY-EIGHTH NOTE
    (Unicode:$1D1BB; Attr:daNone; Str:#$1D1B9#$1D165),     // MUSICAL SYMBOL MINIMA
    (Unicode:$1D1BC; Attr:daNone; Str:#$1D1BA#$1D165),     // MUSICAL SYMBOL MINIMA BLACK
    (Unicode:$1D1BD; Attr:daNone; Str:#$1D1BB#$1D16E),     // MUSICAL SYMBOL SEMIMINIMA WHITE
    (Unicode:$1D1BE; Attr:daNone; Str:#$1D1BC#$1D16E),     // MUSICAL SYMBOL SEMIMINIMA BLACK
    (Unicode:$1D1BF; Attr:daNone; Str:#$1D1BB#$1D16F),     // MUSICAL SYMBOL FUSA WHITE
    (Unicode:$1D1C0; Attr:daNone; Str:#$1D1BC#$1D16F),     // MUSICAL SYMBOL FUSA BLACK
    (Unicode:$1D400; Attr:daFont; Str:#$0041),   // MATHEMATICAL BOLD CAPITAL A
    (Unicode:$1D401; Attr:daFont; Str:#$0042),   // MATHEMATICAL BOLD CAPITAL B
    (Unicode:$1D402; Attr:daFont; Str:#$0043),   // MATHEMATICAL BOLD CAPITAL C
    (Unicode:$1D403; Attr:daFont; Str:#$0044),   // MATHEMATICAL BOLD CAPITAL D
    (Unicode:$1D404; Attr:daFont; Str:#$0045),   // MATHEMATICAL BOLD CAPITAL E
    (Unicode:$1D405; Attr:daFont; Str:#$0046),   // MATHEMATICAL BOLD CAPITAL F
    (Unicode:$1D406; Attr:daFont; Str:#$0047),   // MATHEMATICAL BOLD CAPITAL G
    (Unicode:$1D407; Attr:daFont; Str:#$0048),   // MATHEMATICAL BOLD CAPITAL H
    (Unicode:$1D408; Attr:daFont; Str:#$0049),   // MATHEMATICAL BOLD CAPITAL I
    (Unicode:$1D409; Attr:daFont; Str:#$004A),   // MATHEMATICAL BOLD CAPITAL J
    (Unicode:$1D40A; Attr:daFont; Str:#$004B),   // MATHEMATICAL BOLD CAPITAL K
    (Unicode:$1D40B; Attr:daFont; Str:#$004C),   // MATHEMATICAL BOLD CAPITAL L
    (Unicode:$1D40C; Attr:daFont; Str:#$004D),   // MATHEMATICAL BOLD CAPITAL M
    (Unicode:$1D40D; Attr:daFont; Str:#$004E),   // MATHEMATICAL BOLD CAPITAL N
    (Unicode:$1D40E; Attr:daFont; Str:#$004F),   // MATHEMATICAL BOLD CAPITAL O
    (Unicode:$1D40F; Attr:daFont; Str:#$0050),   // MATHEMATICAL BOLD CAPITAL P
    (Unicode:$1D410; Attr:daFont; Str:#$0051),   // MATHEMATICAL BOLD CAPITAL Q
    (Unicode:$1D411; Attr:daFont; Str:#$0052),   // MATHEMATICAL BOLD CAPITAL R
    (Unicode:$1D412; Attr:daFont; Str:#$0053),   // MATHEMATICAL BOLD CAPITAL S
    (Unicode:$1D413; Attr:daFont; Str:#$0054),   // MATHEMATICAL BOLD CAPITAL T
    (Unicode:$1D414; Attr:daFont; Str:#$0055),   // MATHEMATICAL BOLD CAPITAL U
    (Unicode:$1D415; Attr:daFont; Str:#$0056),   // MATHEMATICAL BOLD CAPITAL V
    (Unicode:$1D416; Attr:daFont; Str:#$0057),   // MATHEMATICAL BOLD CAPITAL W
    (Unicode:$1D417; Attr:daFont; Str:#$0058),   // MATHEMATICAL BOLD CAPITAL X
    (Unicode:$1D418; Attr:daFont; Str:#$0059),   // MATHEMATICAL BOLD CAPITAL Y
    (Unicode:$1D419; Attr:daFont; Str:#$005A),   // MATHEMATICAL BOLD CAPITAL Z
    (Unicode:$1D41A; Attr:daFont; Str:#$0061),   // MATHEMATICAL BOLD SMALL A
    (Unicode:$1D41B; Attr:daFont; Str:#$0062),   // MATHEMATICAL BOLD SMALL B
    (Unicode:$1D41C; Attr:daFont; Str:#$0063),   // MATHEMATICAL BOLD SMALL C
    (Unicode:$1D41D; Attr:daFont; Str:#$0064),   // MATHEMATICAL BOLD SMALL D
    (Unicode:$1D41E; Attr:daFont; Str:#$0065),   // MATHEMATICAL BOLD SMALL E
    (Unicode:$1D41F; Attr:daFont; Str:#$0066),   // MATHEMATICAL BOLD SMALL F
    (Unicode:$1D420; Attr:daFont; Str:#$0067),   // MATHEMATICAL BOLD SMALL G
    (Unicode:$1D421; Attr:daFont; Str:#$0068),   // MATHEMATICAL BOLD SMALL H
    (Unicode:$1D422; Attr:daFont; Str:#$0069),   // MATHEMATICAL BOLD SMALL I
    (Unicode:$1D423; Attr:daFont; Str:#$006A),   // MATHEMATICAL BOLD SMALL J
    (Unicode:$1D424; Attr:daFont; Str:#$006B),   // MATHEMATICAL BOLD SMALL K
    (Unicode:$1D425; Attr:daFont; Str:#$006C),   // MATHEMATICAL BOLD SMALL L
    (Unicode:$1D426; Attr:daFont; Str:#$006D),   // MATHEMATICAL BOLD SMALL M
    (Unicode:$1D427; Attr:daFont; Str:#$006E),   // MATHEMATICAL BOLD SMALL N
    (Unicode:$1D428; Attr:daFont; Str:#$006F),   // MATHEMATICAL BOLD SMALL O
    (Unicode:$1D429; Attr:daFont; Str:#$0070),   // MATHEMATICAL BOLD SMALL P
    (Unicode:$1D42A; Attr:daFont; Str:#$0071),   // MATHEMATICAL BOLD SMALL Q
    (Unicode:$1D42B; Attr:daFont; Str:#$0072),   // MATHEMATICAL BOLD SMALL R
    (Unicode:$1D42C; Attr:daFont; Str:#$0073),   // MATHEMATICAL BOLD SMALL S
    (Unicode:$1D42D; Attr:daFont; Str:#$0074),   // MATHEMATICAL BOLD SMALL T
    (Unicode:$1D42E; Attr:daFont; Str:#$0075),   // MATHEMATICAL BOLD SMALL U
    (Unicode:$1D42F; Attr:daFont; Str:#$0076),   // MATHEMATICAL BOLD SMALL V
    (Unicode:$1D430; Attr:daFont; Str:#$0077),   // MATHEMATICAL BOLD SMALL W
    (Unicode:$1D431; Attr:daFont; Str:#$0078),   // MATHEMATICAL BOLD SMALL X
    (Unicode:$1D432; Attr:daFont; Str:#$0079),   // MATHEMATICAL BOLD SMALL Y
    (Unicode:$1D433; Attr:daFont; Str:#$007A),   // MATHEMATICAL BOLD SMALL Z
    (Unicode:$1D434; Attr:daFont; Str:#$0041),   // MATHEMATICAL ITALIC CAPITAL A
    (Unicode:$1D435; Attr:daFont; Str:#$0042),   // MATHEMATICAL ITALIC CAPITAL B
    (Unicode:$1D436; Attr:daFont; Str:#$0043),   // MATHEMATICAL ITALIC CAPITAL C
    (Unicode:$1D437; Attr:daFont; Str:#$0044),   // MATHEMATICAL ITALIC CAPITAL D
    (Unicode:$1D438; Attr:daFont; Str:#$0045),   // MATHEMATICAL ITALIC CAPITAL E
    (Unicode:$1D439; Attr:daFont; Str:#$0046),   // MATHEMATICAL ITALIC CAPITAL F
    (Unicode:$1D43A; Attr:daFont; Str:#$0047),   // MATHEMATICAL ITALIC CAPITAL G
    (Unicode:$1D43B; Attr:daFont; Str:#$0048),   // MATHEMATICAL ITALIC CAPITAL H
    (Unicode:$1D43C; Attr:daFont; Str:#$0049),   // MATHEMATICAL ITALIC CAPITAL I
    (Unicode:$1D43D; Attr:daFont; Str:#$004A),   // MATHEMATICAL ITALIC CAPITAL J
    (Unicode:$1D43E; Attr:daFont; Str:#$004B),   // MATHEMATICAL ITALIC CAPITAL K
    (Unicode:$1D43F; Attr:daFont; Str:#$004C),   // MATHEMATICAL ITALIC CAPITAL L
    (Unicode:$1D440; Attr:daFont; Str:#$004D),   // MATHEMATICAL ITALIC CAPITAL M
    (Unicode:$1D441; Attr:daFont; Str:#$004E),   // MATHEMATICAL ITALIC CAPITAL N
    (Unicode:$1D442; Attr:daFont; Str:#$004F),   // MATHEMATICAL ITALIC CAPITAL O
    (Unicode:$1D443; Attr:daFont; Str:#$0050),   // MATHEMATICAL ITALIC CAPITAL P
    (Unicode:$1D444; Attr:daFont; Str:#$0051),   // MATHEMATICAL ITALIC CAPITAL Q
    (Unicode:$1D445; Attr:daFont; Str:#$0052),   // MATHEMATICAL ITALIC CAPITAL R
    (Unicode:$1D446; Attr:daFont; Str:#$0053),   // MATHEMATICAL ITALIC CAPITAL S
    (Unicode:$1D447; Attr:daFont; Str:#$0054),   // MATHEMATICAL ITALIC CAPITAL T
    (Unicode:$1D448; Attr:daFont; Str:#$0055),   // MATHEMATICAL ITALIC CAPITAL U
    (Unicode:$1D449; Attr:daFont; Str:#$0056),   // MATHEMATICAL ITALIC CAPITAL V
    (Unicode:$1D44A; Attr:daFont; Str:#$0057),   // MATHEMATICAL ITALIC CAPITAL W
    (Unicode:$1D44B; Attr:daFont; Str:#$0058),   // MATHEMATICAL ITALIC CAPITAL X
    (Unicode:$1D44C; Attr:daFont; Str:#$0059),   // MATHEMATICAL ITALIC CAPITAL Y
    (Unicode:$1D44D; Attr:daFont; Str:#$005A),   // MATHEMATICAL ITALIC CAPITAL Z
    (Unicode:$1D44E; Attr:daFont; Str:#$0061),   // MATHEMATICAL ITALIC SMALL A
    (Unicode:$1D44F; Attr:daFont; Str:#$0062),   // MATHEMATICAL ITALIC SMALL B
    (Unicode:$1D450; Attr:daFont; Str:#$0063),   // MATHEMATICAL ITALIC SMALL C
    (Unicode:$1D451; Attr:daFont; Str:#$0064),   // MATHEMATICAL ITALIC SMALL D
    (Unicode:$1D452; Attr:daFont; Str:#$0065),   // MATHEMATICAL ITALIC SMALL E
    (Unicode:$1D453; Attr:daFont; Str:#$0066),   // MATHEMATICAL ITALIC SMALL F
    (Unicode:$1D454; Attr:daFont; Str:#$0067),   // MATHEMATICAL ITALIC SMALL G
    (Unicode:$1D456; Attr:daFont; Str:#$0069),   // MATHEMATICAL ITALIC SMALL I
    (Unicode:$1D457; Attr:daFont; Str:#$006A),   // MATHEMATICAL ITALIC SMALL J
    (Unicode:$1D458; Attr:daFont; Str:#$006B),   // MATHEMATICAL ITALIC SMALL K
    (Unicode:$1D459; Attr:daFont; Str:#$006C),   // MATHEMATICAL ITALIC SMALL L
    (Unicode:$1D45A; Attr:daFont; Str:#$006D),   // MATHEMATICAL ITALIC SMALL M
    (Unicode:$1D45B; Attr:daFont; Str:#$006E),   // MATHEMATICAL ITALIC SMALL N
    (Unicode:$1D45C; Attr:daFont; Str:#$006F),   // MATHEMATICAL ITALIC SMALL O
    (Unicode:$1D45D; Attr:daFont; Str:#$0070),   // MATHEMATICAL ITALIC SMALL P
    (Unicode:$1D45E; Attr:daFont; Str:#$0071),   // MATHEMATICAL ITALIC SMALL Q
    (Unicode:$1D45F; Attr:daFont; Str:#$0072),   // MATHEMATICAL ITALIC SMALL R
    (Unicode:$1D460; Attr:daFont; Str:#$0073),   // MATHEMATICAL ITALIC SMALL S
    (Unicode:$1D461; Attr:daFont; Str:#$0074),   // MATHEMATICAL ITALIC SMALL T
    (Unicode:$1D462; Attr:daFont; Str:#$0075),   // MATHEMATICAL ITALIC SMALL U
    (Unicode:$1D463; Attr:daFont; Str:#$0076),   // MATHEMATICAL ITALIC SMALL V
    (Unicode:$1D464; Attr:daFont; Str:#$0077),   // MATHEMATICAL ITALIC SMALL W
    (Unicode:$1D465; Attr:daFont; Str:#$0078),   // MATHEMATICAL ITALIC SMALL X
    (Unicode:$1D466; Attr:daFont; Str:#$0079),   // MATHEMATICAL ITALIC SMALL Y
    (Unicode:$1D467; Attr:daFont; Str:#$007A),   // MATHEMATICAL ITALIC SMALL Z
    (Unicode:$1D468; Attr:daFont; Str:#$0041),   // MATHEMATICAL BOLD ITALIC CAPITAL A
    (Unicode:$1D469; Attr:daFont; Str:#$0042),   // MATHEMATICAL BOLD ITALIC CAPITAL B
    (Unicode:$1D46A; Attr:daFont; Str:#$0043),   // MATHEMATICAL BOLD ITALIC CAPITAL C
    (Unicode:$1D46B; Attr:daFont; Str:#$0044),   // MATHEMATICAL BOLD ITALIC CAPITAL D
    (Unicode:$1D46C; Attr:daFont; Str:#$0045),   // MATHEMATICAL BOLD ITALIC CAPITAL E
    (Unicode:$1D46D; Attr:daFont; Str:#$0046),   // MATHEMATICAL BOLD ITALIC CAPITAL F
    (Unicode:$1D46E; Attr:daFont; Str:#$0047),   // MATHEMATICAL BOLD ITALIC CAPITAL G
    (Unicode:$1D46F; Attr:daFont; Str:#$0048),   // MATHEMATICAL BOLD ITALIC CAPITAL H
    (Unicode:$1D470; Attr:daFont; Str:#$0049),   // MATHEMATICAL BOLD ITALIC CAPITAL I
    (Unicode:$1D471; Attr:daFont; Str:#$004A),   // MATHEMATICAL BOLD ITALIC CAPITAL J
    (Unicode:$1D472; Attr:daFont; Str:#$004B),   // MATHEMATICAL BOLD ITALIC CAPITAL K
    (Unicode:$1D473; Attr:daFont; Str:#$004C),   // MATHEMATICAL BOLD ITALIC CAPITAL L
    (Unicode:$1D474; Attr:daFont; Str:#$004D),   // MATHEMATICAL BOLD ITALIC CAPITAL M
    (Unicode:$1D475; Attr:daFont; Str:#$004E),   // MATHEMATICAL BOLD ITALIC CAPITAL N
    (Unicode:$1D476; Attr:daFont; Str:#$004F),   // MATHEMATICAL BOLD ITALIC CAPITAL O
    (Unicode:$1D477; Attr:daFont; Str:#$0050),   // MATHEMATICAL BOLD ITALIC CAPITAL P
    (Unicode:$1D478; Attr:daFont; Str:#$0051),   // MATHEMATICAL BOLD ITALIC CAPITAL Q
    (Unicode:$1D479; Attr:daFont; Str:#$0052),   // MATHEMATICAL BOLD ITALIC CAPITAL R
    (Unicode:$1D47A; Attr:daFont; Str:#$0053),   // MATHEMATICAL BOLD ITALIC CAPITAL S
    (Unicode:$1D47B; Attr:daFont; Str:#$0054),   // MATHEMATICAL BOLD ITALIC CAPITAL T
    (Unicode:$1D47C; Attr:daFont; Str:#$0055),   // MATHEMATICAL BOLD ITALIC CAPITAL U
    (Unicode:$1D47D; Attr:daFont; Str:#$0056),   // MATHEMATICAL BOLD ITALIC CAPITAL V
    (Unicode:$1D47E; Attr:daFont; Str:#$0057),   // MATHEMATICAL BOLD ITALIC CAPITAL W
    (Unicode:$1D47F; Attr:daFont; Str:#$0058),   // MATHEMATICAL BOLD ITALIC CAPITAL X
    (Unicode:$1D480; Attr:daFont; Str:#$0059),   // MATHEMATICAL BOLD ITALIC CAPITAL Y
    (Unicode:$1D481; Attr:daFont; Str:#$005A),   // MATHEMATICAL BOLD ITALIC CAPITAL Z
    (Unicode:$1D482; Attr:daFont; Str:#$0061),   // MATHEMATICAL BOLD ITALIC SMALL A
    (Unicode:$1D483; Attr:daFont; Str:#$0062),   // MATHEMATICAL BOLD ITALIC SMALL B
    (Unicode:$1D484; Attr:daFont; Str:#$0063),   // MATHEMATICAL BOLD ITALIC SMALL C
    (Unicode:$1D485; Attr:daFont; Str:#$0064),   // MATHEMATICAL BOLD ITALIC SMALL D
    (Unicode:$1D486; Attr:daFont; Str:#$0065),   // MATHEMATICAL BOLD ITALIC SMALL E
    (Unicode:$1D487; Attr:daFont; Str:#$0066),   // MATHEMATICAL BOLD ITALIC SMALL F
    (Unicode:$1D488; Attr:daFont; Str:#$0067),   // MATHEMATICAL BOLD ITALIC SMALL G
    (Unicode:$1D489; Attr:daFont; Str:#$0068),   // MATHEMATICAL BOLD ITALIC SMALL H
    (Unicode:$1D48A; Attr:daFont; Str:#$0069),   // MATHEMATICAL BOLD ITALIC SMALL I
    (Unicode:$1D48B; Attr:daFont; Str:#$006A),   // MATHEMATICAL BOLD ITALIC SMALL J
    (Unicode:$1D48C; Attr:daFont; Str:#$006B),   // MATHEMATICAL BOLD ITALIC SMALL K
    (Unicode:$1D48D; Attr:daFont; Str:#$006C),   // MATHEMATICAL BOLD ITALIC SMALL L
    (Unicode:$1D48E; Attr:daFont; Str:#$006D),   // MATHEMATICAL BOLD ITALIC SMALL M
    (Unicode:$1D48F; Attr:daFont; Str:#$006E),   // MATHEMATICAL BOLD ITALIC SMALL N
    (Unicode:$1D490; Attr:daFont; Str:#$006F),   // MATHEMATICAL BOLD ITALIC SMALL O
    (Unicode:$1D491; Attr:daFont; Str:#$0070),   // MATHEMATICAL BOLD ITALIC SMALL P
    (Unicode:$1D492; Attr:daFont; Str:#$0071),   // MATHEMATICAL BOLD ITALIC SMALL Q
    (Unicode:$1D493; Attr:daFont; Str:#$0072),   // MATHEMATICAL BOLD ITALIC SMALL R
    (Unicode:$1D494; Attr:daFont; Str:#$0073),   // MATHEMATICAL BOLD ITALIC SMALL S
    (Unicode:$1D495; Attr:daFont; Str:#$0074),   // MATHEMATICAL BOLD ITALIC SMALL T
    (Unicode:$1D496; Attr:daFont; Str:#$0075),   // MATHEMATICAL BOLD ITALIC SMALL U
    (Unicode:$1D497; Attr:daFont; Str:#$0076),   // MATHEMATICAL BOLD ITALIC SMALL V
    (Unicode:$1D498; Attr:daFont; Str:#$0077),   // MATHEMATICAL BOLD ITALIC SMALL W
    (Unicode:$1D499; Attr:daFont; Str:#$0078),   // MATHEMATICAL BOLD ITALIC SMALL X
    (Unicode:$1D49A; Attr:daFont; Str:#$0079),   // MATHEMATICAL BOLD ITALIC SMALL Y
    (Unicode:$1D49B; Attr:daFont; Str:#$007A),   // MATHEMATICAL BOLD ITALIC SMALL Z
    (Unicode:$1D49C; Attr:daFont; Str:#$0041),   // MATHEMATICAL SCRIPT CAPITAL A
    (Unicode:$1D49E; Attr:daFont; Str:#$0043),   // MATHEMATICAL SCRIPT CAPITAL C
    (Unicode:$1D49F; Attr:daFont; Str:#$0044),   // MATHEMATICAL SCRIPT CAPITAL D
    (Unicode:$1D4A2; Attr:daFont; Str:#$0047),   // MATHEMATICAL SCRIPT CAPITAL G
    (Unicode:$1D4A5; Attr:daFont; Str:#$004A),   // MATHEMATICAL SCRIPT CAPITAL J
    (Unicode:$1D4A6; Attr:daFont; Str:#$004B),   // MATHEMATICAL SCRIPT CAPITAL K
    (Unicode:$1D4A9; Attr:daFont; Str:#$004E),   // MATHEMATICAL SCRIPT CAPITAL N
    (Unicode:$1D4AA; Attr:daFont; Str:#$004F),   // MATHEMATICAL SCRIPT CAPITAL O
    (Unicode:$1D4AB; Attr:daFont; Str:#$0050),   // MATHEMATICAL SCRIPT CAPITAL P
    (Unicode:$1D4AC; Attr:daFont; Str:#$0051),   // MATHEMATICAL SCRIPT CAPITAL Q
    (Unicode:$1D4AE; Attr:daFont; Str:#$0053),   // MATHEMATICAL SCRIPT CAPITAL S
    (Unicode:$1D4AF; Attr:daFont; Str:#$0054),   // MATHEMATICAL SCRIPT CAPITAL T
    (Unicode:$1D4B0; Attr:daFont; Str:#$0055),   // MATHEMATICAL SCRIPT CAPITAL U
    (Unicode:$1D4B1; Attr:daFont; Str:#$0056),   // MATHEMATICAL SCRIPT CAPITAL V
    (Unicode:$1D4B2; Attr:daFont; Str:#$0057),   // MATHEMATICAL SCRIPT CAPITAL W
    (Unicode:$1D4B3; Attr:daFont; Str:#$0058),   // MATHEMATICAL SCRIPT CAPITAL X
    (Unicode:$1D4B4; Attr:daFont; Str:#$0059),   // MATHEMATICAL SCRIPT CAPITAL Y
    (Unicode:$1D4B5; Attr:daFont; Str:#$005A),   // MATHEMATICAL SCRIPT CAPITAL Z
    (Unicode:$1D4B6; Attr:daFont; Str:#$0061),   // MATHEMATICAL SCRIPT SMALL A
    (Unicode:$1D4B7; Attr:daFont; Str:#$0062),   // MATHEMATICAL SCRIPT SMALL B
    (Unicode:$1D4B8; Attr:daFont; Str:#$0063),   // MATHEMATICAL SCRIPT SMALL C
    (Unicode:$1D4B9; Attr:daFont; Str:#$0064),   // MATHEMATICAL SCRIPT SMALL D
    (Unicode:$1D4BB; Attr:daFont; Str:#$0066),   // MATHEMATICAL SCRIPT SMALL F
    (Unicode:$1D4BD; Attr:daFont; Str:#$0068),   // MATHEMATICAL SCRIPT SMALL H
    (Unicode:$1D4BE; Attr:daFont; Str:#$0069),   // MATHEMATICAL SCRIPT SMALL I
    (Unicode:$1D4BF; Attr:daFont; Str:#$006A),   // MATHEMATICAL SCRIPT SMALL J
    (Unicode:$1D4C0; Attr:daFont; Str:#$006B),   // MATHEMATICAL SCRIPT SMALL K
    (Unicode:$1D4C2; Attr:daFont; Str:#$006D),   // MATHEMATICAL SCRIPT SMALL M
    (Unicode:$1D4C3; Attr:daFont; Str:#$006E),   // MATHEMATICAL SCRIPT SMALL N
    (Unicode:$1D4C5; Attr:daFont; Str:#$0070),   // MATHEMATICAL SCRIPT SMALL P
    (Unicode:$1D4C6; Attr:daFont; Str:#$0071),   // MATHEMATICAL SCRIPT SMALL Q
    (Unicode:$1D4C7; Attr:daFont; Str:#$0072),   // MATHEMATICAL SCRIPT SMALL R
    (Unicode:$1D4C8; Attr:daFont; Str:#$0073),   // MATHEMATICAL SCRIPT SMALL S
    (Unicode:$1D4C9; Attr:daFont; Str:#$0074),   // MATHEMATICAL SCRIPT SMALL T
    (Unicode:$1D4CA; Attr:daFont; Str:#$0075),   // MATHEMATICAL SCRIPT SMALL U
    (Unicode:$1D4CB; Attr:daFont; Str:#$0076),   // MATHEMATICAL SCRIPT SMALL V
    (Unicode:$1D4CC; Attr:daFont; Str:#$0077),   // MATHEMATICAL SCRIPT SMALL W
    (Unicode:$1D4CD; Attr:daFont; Str:#$0078),   // MATHEMATICAL SCRIPT SMALL X
    (Unicode:$1D4CE; Attr:daFont; Str:#$0079),   // MATHEMATICAL SCRIPT SMALL Y
    (Unicode:$1D4CF; Attr:daFont; Str:#$007A),   // MATHEMATICAL SCRIPT SMALL Z
    (Unicode:$1D4D0; Attr:daFont; Str:#$0041),   // MATHEMATICAL BOLD SCRIPT CAPITAL A
    (Unicode:$1D4D1; Attr:daFont; Str:#$0042),   // MATHEMATICAL BOLD SCRIPT CAPITAL B
    (Unicode:$1D4D2; Attr:daFont; Str:#$0043),   // MATHEMATICAL BOLD SCRIPT CAPITAL C
    (Unicode:$1D4D3; Attr:daFont; Str:#$0044),   // MATHEMATICAL BOLD SCRIPT CAPITAL D
    (Unicode:$1D4D4; Attr:daFont; Str:#$0045),   // MATHEMATICAL BOLD SCRIPT CAPITAL E
    (Unicode:$1D4D5; Attr:daFont; Str:#$0046),   // MATHEMATICAL BOLD SCRIPT CAPITAL F
    (Unicode:$1D4D6; Attr:daFont; Str:#$0047),   // MATHEMATICAL BOLD SCRIPT CAPITAL G
    (Unicode:$1D4D7; Attr:daFont; Str:#$0048),   // MATHEMATICAL BOLD SCRIPT CAPITAL H
    (Unicode:$1D4D8; Attr:daFont; Str:#$0049),   // MATHEMATICAL BOLD SCRIPT CAPITAL I
    (Unicode:$1D4D9; Attr:daFont; Str:#$004A),   // MATHEMATICAL BOLD SCRIPT CAPITAL J
    (Unicode:$1D4DA; Attr:daFont; Str:#$004B),   // MATHEMATICAL BOLD SCRIPT CAPITAL K
    (Unicode:$1D4DB; Attr:daFont; Str:#$004C),   // MATHEMATICAL BOLD SCRIPT CAPITAL L
    (Unicode:$1D4DC; Attr:daFont; Str:#$004D),   // MATHEMATICAL BOLD SCRIPT CAPITAL M
    (Unicode:$1D4DD; Attr:daFont; Str:#$004E),   // MATHEMATICAL BOLD SCRIPT CAPITAL N
    (Unicode:$1D4DE; Attr:daFont; Str:#$004F),   // MATHEMATICAL BOLD SCRIPT CAPITAL O
    (Unicode:$1D4DF; Attr:daFont; Str:#$0050),   // MATHEMATICAL BOLD SCRIPT CAPITAL P
    (Unicode:$1D4E0; Attr:daFont; Str:#$0051),   // MATHEMATICAL BOLD SCRIPT CAPITAL Q
    (Unicode:$1D4E1; Attr:daFont; Str:#$0052),   // MATHEMATICAL BOLD SCRIPT CAPITAL R
    (Unicode:$1D4E2; Attr:daFont; Str:#$0053),   // MATHEMATICAL BOLD SCRIPT CAPITAL S
    (Unicode:$1D4E3; Attr:daFont; Str:#$0054),   // MATHEMATICAL BOLD SCRIPT CAPITAL T
    (Unicode:$1D4E4; Attr:daFont; Str:#$0055),   // MATHEMATICAL BOLD SCRIPT CAPITAL U
    (Unicode:$1D4E5; Attr:daFont; Str:#$0056),   // MATHEMATICAL BOLD SCRIPT CAPITAL V
    (Unicode:$1D4E6; Attr:daFont; Str:#$0057),   // MATHEMATICAL BOLD SCRIPT CAPITAL W
    (Unicode:$1D4E7; Attr:daFont; Str:#$0058),   // MATHEMATICAL BOLD SCRIPT CAPITAL X
    (Unicode:$1D4E8; Attr:daFont; Str:#$0059),   // MATHEMATICAL BOLD SCRIPT CAPITAL Y
    (Unicode:$1D4E9; Attr:daFont; Str:#$005A),   // MATHEMATICAL BOLD SCRIPT CAPITAL Z
    (Unicode:$1D4EA; Attr:daFont; Str:#$0061),   // MATHEMATICAL BOLD SCRIPT SMALL A
    (Unicode:$1D4EB; Attr:daFont; Str:#$0062),   // MATHEMATICAL BOLD SCRIPT SMALL B
    (Unicode:$1D4EC; Attr:daFont; Str:#$0063),   // MATHEMATICAL BOLD SCRIPT SMALL C
    (Unicode:$1D4ED; Attr:daFont; Str:#$0064),   // MATHEMATICAL BOLD SCRIPT SMALL D
    (Unicode:$1D4EE; Attr:daFont; Str:#$0065),   // MATHEMATICAL BOLD SCRIPT SMALL E
    (Unicode:$1D4EF; Attr:daFont; Str:#$0066),   // MATHEMATICAL BOLD SCRIPT SMALL F
    (Unicode:$1D4F0; Attr:daFont; Str:#$0067),   // MATHEMATICAL BOLD SCRIPT SMALL G
    (Unicode:$1D4F1; Attr:daFont; Str:#$0068),   // MATHEMATICAL BOLD SCRIPT SMALL H
    (Unicode:$1D4F2; Attr:daFont; Str:#$0069),   // MATHEMATICAL BOLD SCRIPT SMALL I
    (Unicode:$1D4F3; Attr:daFont; Str:#$006A),   // MATHEMATICAL BOLD SCRIPT SMALL J
    (Unicode:$1D4F4; Attr:daFont; Str:#$006B),   // MATHEMATICAL BOLD SCRIPT SMALL K
    (Unicode:$1D4F5; Attr:daFont; Str:#$006C),   // MATHEMATICAL BOLD SCRIPT SMALL L
    (Unicode:$1D4F6; Attr:daFont; Str:#$006D),   // MATHEMATICAL BOLD SCRIPT SMALL M
    (Unicode:$1D4F7; Attr:daFont; Str:#$006E),   // MATHEMATICAL BOLD SCRIPT SMALL N
    (Unicode:$1D4F8; Attr:daFont; Str:#$006F),   // MATHEMATICAL BOLD SCRIPT SMALL O
    (Unicode:$1D4F9; Attr:daFont; Str:#$0070),   // MATHEMATICAL BOLD SCRIPT SMALL P
    (Unicode:$1D4FA; Attr:daFont; Str:#$0071),   // MATHEMATICAL BOLD SCRIPT SMALL Q
    (Unicode:$1D4FB; Attr:daFont; Str:#$0072),   // MATHEMATICAL BOLD SCRIPT SMALL R
    (Unicode:$1D4FC; Attr:daFont; Str:#$0073),   // MATHEMATICAL BOLD SCRIPT SMALL S
    (Unicode:$1D4FD; Attr:daFont; Str:#$0074),   // MATHEMATICAL BOLD SCRIPT SMALL T
    (Unicode:$1D4FE; Attr:daFont; Str:#$0075),   // MATHEMATICAL BOLD SCRIPT SMALL U
    (Unicode:$1D4FF; Attr:daFont; Str:#$0076),   // MATHEMATICAL BOLD SCRIPT SMALL V
    (Unicode:$1D500; Attr:daFont; Str:#$0077),   // MATHEMATICAL BOLD SCRIPT SMALL W
    (Unicode:$1D501; Attr:daFont; Str:#$0078),   // MATHEMATICAL BOLD SCRIPT SMALL X
    (Unicode:$1D502; Attr:daFont; Str:#$0079),   // MATHEMATICAL BOLD SCRIPT SMALL Y
    (Unicode:$1D503; Attr:daFont; Str:#$007A),   // MATHEMATICAL BOLD SCRIPT SMALL Z
    (Unicode:$1D504; Attr:daFont; Str:#$0041),   // MATHEMATICAL FRAKTUR CAPITAL A
    (Unicode:$1D505; Attr:daFont; Str:#$0042),   // MATHEMATICAL FRAKTUR CAPITAL B
    (Unicode:$1D507; Attr:daFont; Str:#$0044),   // MATHEMATICAL FRAKTUR CAPITAL D
    (Unicode:$1D508; Attr:daFont; Str:#$0045),   // MATHEMATICAL FRAKTUR CAPITAL E
    (Unicode:$1D509; Attr:daFont; Str:#$0046),   // MATHEMATICAL FRAKTUR CAPITAL F
    (Unicode:$1D50A; Attr:daFont; Str:#$0047),   // MATHEMATICAL FRAKTUR CAPITAL G
    (Unicode:$1D50D; Attr:daFont; Str:#$004A),   // MATHEMATICAL FRAKTUR CAPITAL J
    (Unicode:$1D50E; Attr:daFont; Str:#$004B),   // MATHEMATICAL FRAKTUR CAPITAL K
    (Unicode:$1D50F; Attr:daFont; Str:#$004C),   // MATHEMATICAL FRAKTUR CAPITAL L
    (Unicode:$1D510; Attr:daFont; Str:#$004D),   // MATHEMATICAL FRAKTUR CAPITAL M
    (Unicode:$1D511; Attr:daFont; Str:#$004E),   // MATHEMATICAL FRAKTUR CAPITAL N
    (Unicode:$1D512; Attr:daFont; Str:#$004F),   // MATHEMATICAL FRAKTUR CAPITAL O
    (Unicode:$1D513; Attr:daFont; Str:#$0050),   // MATHEMATICAL FRAKTUR CAPITAL P
    (Unicode:$1D514; Attr:daFont; Str:#$0051),   // MATHEMATICAL FRAKTUR CAPITAL Q
    (Unicode:$1D516; Attr:daFont; Str:#$0053),   // MATHEMATICAL FRAKTUR CAPITAL S
    (Unicode:$1D517; Attr:daFont; Str:#$0054),   // MATHEMATICAL FRAKTUR CAPITAL T
    (Unicode:$1D518; Attr:daFont; Str:#$0055),   // MATHEMATICAL FRAKTUR CAPITAL U
    (Unicode:$1D519; Attr:daFont; Str:#$0056),   // MATHEMATICAL FRAKTUR CAPITAL V
    (Unicode:$1D51A; Attr:daFont; Str:#$0057),   // MATHEMATICAL FRAKTUR CAPITAL W
    (Unicode:$1D51B; Attr:daFont; Str:#$0058),   // MATHEMATICAL FRAKTUR CAPITAL X
    (Unicode:$1D51C; Attr:daFont; Str:#$0059),   // MATHEMATICAL FRAKTUR CAPITAL Y
    (Unicode:$1D51E; Attr:daFont; Str:#$0061),   // MATHEMATICAL FRAKTUR SMALL A
    (Unicode:$1D51F; Attr:daFont; Str:#$0062),   // MATHEMATICAL FRAKTUR SMALL B
    (Unicode:$1D520; Attr:daFont; Str:#$0063),   // MATHEMATICAL FRAKTUR SMALL C
    (Unicode:$1D521; Attr:daFont; Str:#$0064),   // MATHEMATICAL FRAKTUR SMALL D
    (Unicode:$1D522; Attr:daFont; Str:#$0065),   // MATHEMATICAL FRAKTUR SMALL E
    (Unicode:$1D523; Attr:daFont; Str:#$0066),   // MATHEMATICAL FRAKTUR SMALL F
    (Unicode:$1D524; Attr:daFont; Str:#$0067),   // MATHEMATICAL FRAKTUR SMALL G
    (Unicode:$1D525; Attr:daFont; Str:#$0068),   // MATHEMATICAL FRAKTUR SMALL H
    (Unicode:$1D526; Attr:daFont; Str:#$0069),   // MATHEMATICAL FRAKTUR SMALL I
    (Unicode:$1D527; Attr:daFont; Str:#$006A),   // MATHEMATICAL FRAKTUR SMALL J
    (Unicode:$1D528; Attr:daFont; Str:#$006B),   // MATHEMATICAL FRAKTUR SMALL K
    (Unicode:$1D529; Attr:daFont; Str:#$006C),   // MATHEMATICAL FRAKTUR SMALL L
    (Unicode:$1D52A; Attr:daFont; Str:#$006D),   // MATHEMATICAL FRAKTUR SMALL M
    (Unicode:$1D52B; Attr:daFont; Str:#$006E),   // MATHEMATICAL FRAKTUR SMALL N
    (Unicode:$1D52C; Attr:daFont; Str:#$006F),   // MATHEMATICAL FRAKTUR SMALL O
    (Unicode:$1D52D; Attr:daFont; Str:#$0070),   // MATHEMATICAL FRAKTUR SMALL P
    (Unicode:$1D52E; Attr:daFont; Str:#$0071),   // MATHEMATICAL FRAKTUR SMALL Q
    (Unicode:$1D52F; Attr:daFont; Str:#$0072),   // MATHEMATICAL FRAKTUR SMALL R
    (Unicode:$1D530; Attr:daFont; Str:#$0073),   // MATHEMATICAL FRAKTUR SMALL S
    (Unicode:$1D531; Attr:daFont; Str:#$0074),   // MATHEMATICAL FRAKTUR SMALL T
    (Unicode:$1D532; Attr:daFont; Str:#$0075),   // MATHEMATICAL FRAKTUR SMALL U
    (Unicode:$1D533; Attr:daFont; Str:#$0076),   // MATHEMATICAL FRAKTUR SMALL V
    (Unicode:$1D534; Attr:daFont; Str:#$0077),   // MATHEMATICAL FRAKTUR SMALL W
    (Unicode:$1D535; Attr:daFont; Str:#$0078),   // MATHEMATICAL FRAKTUR SMALL X
    (Unicode:$1D536; Attr:daFont; Str:#$0079),   // MATHEMATICAL FRAKTUR SMALL Y
    (Unicode:$1D537; Attr:daFont; Str:#$007A),   // MATHEMATICAL FRAKTUR SMALL Z
    (Unicode:$1D538; Attr:daFont; Str:#$0041),   // MATHEMATICAL DOUBLE-STRUCK CAPITAL A
    (Unicode:$1D539; Attr:daFont; Str:#$0042),   // MATHEMATICAL DOUBLE-STRUCK CAPITAL B
    (Unicode:$1D53B; Attr:daFont; Str:#$0044),   // MATHEMATICAL DOUBLE-STRUCK CAPITAL D
    (Unicode:$1D53C; Attr:daFont; Str:#$0045),   // MATHEMATICAL DOUBLE-STRUCK CAPITAL E
    (Unicode:$1D53D; Attr:daFont; Str:#$0046),   // MATHEMATICAL DOUBLE-STRUCK CAPITAL F
    (Unicode:$1D53E; Attr:daFont; Str:#$0047),   // MATHEMATICAL DOUBLE-STRUCK CAPITAL G
    (Unicode:$1D540; Attr:daFont; Str:#$0049),   // MATHEMATICAL DOUBLE-STRUCK CAPITAL I
    (Unicode:$1D541; Attr:daFont; Str:#$004A),   // MATHEMATICAL DOUBLE-STRUCK CAPITAL J
    (Unicode:$1D542; Attr:daFont; Str:#$004B),   // MATHEMATICAL DOUBLE-STRUCK CAPITAL K
    (Unicode:$1D543; Attr:daFont; Str:#$004C),   // MATHEMATICAL DOUBLE-STRUCK CAPITAL L
    (Unicode:$1D544; Attr:daFont; Str:#$004D),   // MATHEMATICAL DOUBLE-STRUCK CAPITAL M
    (Unicode:$1D546; Attr:daFont; Str:#$004F),   // MATHEMATICAL DOUBLE-STRUCK CAPITAL O
    (Unicode:$1D54A; Attr:daFont; Str:#$0053),   // MATHEMATICAL DOUBLE-STRUCK CAPITAL S
    (Unicode:$1D54B; Attr:daFont; Str:#$0054),   // MATHEMATICAL DOUBLE-STRUCK CAPITAL T
    (Unicode:$1D54C; Attr:daFont; Str:#$0055),   // MATHEMATICAL DOUBLE-STRUCK CAPITAL U
    (Unicode:$1D54D; Attr:daFont; Str:#$0056),   // MATHEMATICAL DOUBLE-STRUCK CAPITAL V
    (Unicode:$1D54E; Attr:daFont; Str:#$0057),   // MATHEMATICAL DOUBLE-STRUCK CAPITAL W
    (Unicode:$1D54F; Attr:daFont; Str:#$0058),   // MATHEMATICAL DOUBLE-STRUCK CAPITAL X
    (Unicode:$1D550; Attr:daFont; Str:#$0059),   // MATHEMATICAL DOUBLE-STRUCK CAPITAL Y
    (Unicode:$1D552; Attr:daFont; Str:#$0061),   // MATHEMATICAL DOUBLE-STRUCK SMALL A
    (Unicode:$1D553; Attr:daFont; Str:#$0062),   // MATHEMATICAL DOUBLE-STRUCK SMALL B
    (Unicode:$1D554; Attr:daFont; Str:#$0063),   // MATHEMATICAL DOUBLE-STRUCK SMALL C
    (Unicode:$1D555; Attr:daFont; Str:#$0064),   // MATHEMATICAL DOUBLE-STRUCK SMALL D
    (Unicode:$1D556; Attr:daFont; Str:#$0065),   // MATHEMATICAL DOUBLE-STRUCK SMALL E
    (Unicode:$1D557; Attr:daFont; Str:#$0066),   // MATHEMATICAL DOUBLE-STRUCK SMALL F
    (Unicode:$1D558; Attr:daFont; Str:#$0067),   // MATHEMATICAL DOUBLE-STRUCK SMALL G
    (Unicode:$1D559; Attr:daFont; Str:#$0068),   // MATHEMATICAL DOUBLE-STRUCK SMALL H
    (Unicode:$1D55A; Attr:daFont; Str:#$0069),   // MATHEMATICAL DOUBLE-STRUCK SMALL I
    (Unicode:$1D55B; Attr:daFont; Str:#$006A),   // MATHEMATICAL DOUBLE-STRUCK SMALL J
    (Unicode:$1D55C; Attr:daFont; Str:#$006B),   // MATHEMATICAL DOUBLE-STRUCK SMALL K
    (Unicode:$1D55D; Attr:daFont; Str:#$006C),   // MATHEMATICAL DOUBLE-STRUCK SMALL L
    (Unicode:$1D55E; Attr:daFont; Str:#$006D),   // MATHEMATICAL DOUBLE-STRUCK SMALL M
    (Unicode:$1D55F; Attr:daFont; Str:#$006E),   // MATHEMATICAL DOUBLE-STRUCK SMALL N
    (Unicode:$1D560; Attr:daFont; Str:#$006F),   // MATHEMATICAL DOUBLE-STRUCK SMALL O
    (Unicode:$1D561; Attr:daFont; Str:#$0070),   // MATHEMATICAL DOUBLE-STRUCK SMALL P
    (Unicode:$1D562; Attr:daFont; Str:#$0071),   // MATHEMATICAL DOUBLE-STRUCK SMALL Q
    (Unicode:$1D563; Attr:daFont; Str:#$0072),   // MATHEMATICAL DOUBLE-STRUCK SMALL R
    (Unicode:$1D564; Attr:daFont; Str:#$0073),   // MATHEMATICAL DOUBLE-STRUCK SMALL S
    (Unicode:$1D565; Attr:daFont; Str:#$0074),   // MATHEMATICAL DOUBLE-STRUCK SMALL T
    (Unicode:$1D566; Attr:daFont; Str:#$0075),   // MATHEMATICAL DOUBLE-STRUCK SMALL U
    (Unicode:$1D567; Attr:daFont; Str:#$0076),   // MATHEMATICAL DOUBLE-STRUCK SMALL V
    (Unicode:$1D568; Attr:daFont; Str:#$0077),   // MATHEMATICAL DOUBLE-STRUCK SMALL W
    (Unicode:$1D569; Attr:daFont; Str:#$0078),   // MATHEMATICAL DOUBLE-STRUCK SMALL X
    (Unicode:$1D56A; Attr:daFont; Str:#$0079),   // MATHEMATICAL DOUBLE-STRUCK SMALL Y
    (Unicode:$1D56B; Attr:daFont; Str:#$007A),   // MATHEMATICAL DOUBLE-STRUCK SMALL Z
    (Unicode:$1D56C; Attr:daFont; Str:#$0041),   // MATHEMATICAL BOLD FRAKTUR CAPITAL A
    (Unicode:$1D56D; Attr:daFont; Str:#$0042),   // MATHEMATICAL BOLD FRAKTUR CAPITAL B
    (Unicode:$1D56E; Attr:daFont; Str:#$0043),   // MATHEMATICAL BOLD FRAKTUR CAPITAL C
    (Unicode:$1D56F; Attr:daFont; Str:#$0044),   // MATHEMATICAL BOLD FRAKTUR CAPITAL D
    (Unicode:$1D570; Attr:daFont; Str:#$0045),   // MATHEMATICAL BOLD FRAKTUR CAPITAL E
    (Unicode:$1D571; Attr:daFont; Str:#$0046),   // MATHEMATICAL BOLD FRAKTUR CAPITAL F
    (Unicode:$1D572; Attr:daFont; Str:#$0047),   // MATHEMATICAL BOLD FRAKTUR CAPITAL G
    (Unicode:$1D573; Attr:daFont; Str:#$0048),   // MATHEMATICAL BOLD FRAKTUR CAPITAL H
    (Unicode:$1D574; Attr:daFont; Str:#$0049),   // MATHEMATICAL BOLD FRAKTUR CAPITAL I
    (Unicode:$1D575; Attr:daFont; Str:#$004A),   // MATHEMATICAL BOLD FRAKTUR CAPITAL J
    (Unicode:$1D576; Attr:daFont; Str:#$004B),   // MATHEMATICAL BOLD FRAKTUR CAPITAL K
    (Unicode:$1D577; Attr:daFont; Str:#$004C),   // MATHEMATICAL BOLD FRAKTUR CAPITAL L
    (Unicode:$1D578; Attr:daFont; Str:#$004D),   // MATHEMATICAL BOLD FRAKTUR CAPITAL M
    (Unicode:$1D579; Attr:daFont; Str:#$004E),   // MATHEMATICAL BOLD FRAKTUR CAPITAL N
    (Unicode:$1D57A; Attr:daFont; Str:#$004F),   // MATHEMATICAL BOLD FRAKTUR CAPITAL O
    (Unicode:$1D57B; Attr:daFont; Str:#$0050),   // MATHEMATICAL BOLD FRAKTUR CAPITAL P
    (Unicode:$1D57C; Attr:daFont; Str:#$0051),   // MATHEMATICAL BOLD FRAKTUR CAPITAL Q
    (Unicode:$1D57D; Attr:daFont; Str:#$0052),   // MATHEMATICAL BOLD FRAKTUR CAPITAL R
    (Unicode:$1D57E; Attr:daFont; Str:#$0053),   // MATHEMATICAL BOLD FRAKTUR CAPITAL S
    (Unicode:$1D57F; Attr:daFont; Str:#$0054),   // MATHEMATICAL BOLD FRAKTUR CAPITAL T
    (Unicode:$1D580; Attr:daFont; Str:#$0055),   // MATHEMATICAL BOLD FRAKTUR CAPITAL U
    (Unicode:$1D581; Attr:daFont; Str:#$0056),   // MATHEMATICAL BOLD FRAKTUR CAPITAL V
    (Unicode:$1D582; Attr:daFont; Str:#$0057),   // MATHEMATICAL BOLD FRAKTUR CAPITAL W
    (Unicode:$1D583; Attr:daFont; Str:#$0058),   // MATHEMATICAL BOLD FRAKTUR CAPITAL X
    (Unicode:$1D584; Attr:daFont; Str:#$0059),   // MATHEMATICAL BOLD FRAKTUR CAPITAL Y
    (Unicode:$1D585; Attr:daFont; Str:#$005A),   // MATHEMATICAL BOLD FRAKTUR CAPITAL Z
    (Unicode:$1D586; Attr:daFont; Str:#$0061),   // MATHEMATICAL BOLD FRAKTUR SMALL A
    (Unicode:$1D587; Attr:daFont; Str:#$0062),   // MATHEMATICAL BOLD FRAKTUR SMALL B
    (Unicode:$1D588; Attr:daFont; Str:#$0063),   // MATHEMATICAL BOLD FRAKTUR SMALL C
    (Unicode:$1D589; Attr:daFont; Str:#$0064),   // MATHEMATICAL BOLD FRAKTUR SMALL D
    (Unicode:$1D58A; Attr:daFont; Str:#$0065),   // MATHEMATICAL BOLD FRAKTUR SMALL E
    (Unicode:$1D58B; Attr:daFont; Str:#$0066),   // MATHEMATICAL BOLD FRAKTUR SMALL F
    (Unicode:$1D58C; Attr:daFont; Str:#$0067),   // MATHEMATICAL BOLD FRAKTUR SMALL G
    (Unicode:$1D58D; Attr:daFont; Str:#$0068),   // MATHEMATICAL BOLD FRAKTUR SMALL H
    (Unicode:$1D58E; Attr:daFont; Str:#$0069),   // MATHEMATICAL BOLD FRAKTUR SMALL I
    (Unicode:$1D58F; Attr:daFont; Str:#$006A),   // MATHEMATICAL BOLD FRAKTUR SMALL J
    (Unicode:$1D590; Attr:daFont; Str:#$006B),   // MATHEMATICAL BOLD FRAKTUR SMALL K
    (Unicode:$1D591; Attr:daFont; Str:#$006C),   // MATHEMATICAL BOLD FRAKTUR SMALL L
    (Unicode:$1D592; Attr:daFont; Str:#$006D),   // MATHEMATICAL BOLD FRAKTUR SMALL M
    (Unicode:$1D593; Attr:daFont; Str:#$006E),   // MATHEMATICAL BOLD FRAKTUR SMALL N
    (Unicode:$1D594; Attr:daFont; Str:#$006F),   // MATHEMATICAL BOLD FRAKTUR SMALL O
    (Unicode:$1D595; Attr:daFont; Str:#$0070),   // MATHEMATICAL BOLD FRAKTUR SMALL P
    (Unicode:$1D596; Attr:daFont; Str:#$0071),   // MATHEMATICAL BOLD FRAKTUR SMALL Q
    (Unicode:$1D597; Attr:daFont; Str:#$0072),   // MATHEMATICAL BOLD FRAKTUR SMALL R
    (Unicode:$1D598; Attr:daFont; Str:#$0073),   // MATHEMATICAL BOLD FRAKTUR SMALL S
    (Unicode:$1D599; Attr:daFont; Str:#$0074),   // MATHEMATICAL BOLD FRAKTUR SMALL T
    (Unicode:$1D59A; Attr:daFont; Str:#$0075),   // MATHEMATICAL BOLD FRAKTUR SMALL U
    (Unicode:$1D59B; Attr:daFont; Str:#$0076),   // MATHEMATICAL BOLD FRAKTUR SMALL V
    (Unicode:$1D59C; Attr:daFont; Str:#$0077),   // MATHEMATICAL BOLD FRAKTUR SMALL W
    (Unicode:$1D59D; Attr:daFont; Str:#$0078),   // MATHEMATICAL BOLD FRAKTUR SMALL X
    (Unicode:$1D59E; Attr:daFont; Str:#$0079),   // MATHEMATICAL BOLD FRAKTUR SMALL Y
    (Unicode:$1D59F; Attr:daFont; Str:#$007A),   // MATHEMATICAL BOLD FRAKTUR SMALL Z
    (Unicode:$1D5A0; Attr:daFont; Str:#$0041),   // MATHEMATICAL SANS-SERIF CAPITAL A
    (Unicode:$1D5A1; Attr:daFont; Str:#$0042),   // MATHEMATICAL SANS-SERIF CAPITAL B
    (Unicode:$1D5A2; Attr:daFont; Str:#$0043),   // MATHEMATICAL SANS-SERIF CAPITAL C
    (Unicode:$1D5A3; Attr:daFont; Str:#$0044),   // MATHEMATICAL SANS-SERIF CAPITAL D
    (Unicode:$1D5A4; Attr:daFont; Str:#$0045),   // MATHEMATICAL SANS-SERIF CAPITAL E
    (Unicode:$1D5A5; Attr:daFont; Str:#$0046),   // MATHEMATICAL SANS-SERIF CAPITAL F
    (Unicode:$1D5A6; Attr:daFont; Str:#$0047),   // MATHEMATICAL SANS-SERIF CAPITAL G
    (Unicode:$1D5A7; Attr:daFont; Str:#$0048),   // MATHEMATICAL SANS-SERIF CAPITAL H
    (Unicode:$1D5A8; Attr:daFont; Str:#$0049),   // MATHEMATICAL SANS-SERIF CAPITAL I
    (Unicode:$1D5A9; Attr:daFont; Str:#$004A),   // MATHEMATICAL SANS-SERIF CAPITAL J
    (Unicode:$1D5AA; Attr:daFont; Str:#$004B),   // MATHEMATICAL SANS-SERIF CAPITAL K
    (Unicode:$1D5AB; Attr:daFont; Str:#$004C),   // MATHEMATICAL SANS-SERIF CAPITAL L
    (Unicode:$1D5AC; Attr:daFont; Str:#$004D),   // MATHEMATICAL SANS-SERIF CAPITAL M
    (Unicode:$1D5AD; Attr:daFont; Str:#$004E),   // MATHEMATICAL SANS-SERIF CAPITAL N
    (Unicode:$1D5AE; Attr:daFont; Str:#$004F),   // MATHEMATICAL SANS-SERIF CAPITAL O
    (Unicode:$1D5AF; Attr:daFont; Str:#$0050),   // MATHEMATICAL SANS-SERIF CAPITAL P
    (Unicode:$1D5B0; Attr:daFont; Str:#$0051),   // MATHEMATICAL SANS-SERIF CAPITAL Q
    (Unicode:$1D5B1; Attr:daFont; Str:#$0052),   // MATHEMATICAL SANS-SERIF CAPITAL R
    (Unicode:$1D5B2; Attr:daFont; Str:#$0053),   // MATHEMATICAL SANS-SERIF CAPITAL S
    (Unicode:$1D5B3; Attr:daFont; Str:#$0054),   // MATHEMATICAL SANS-SERIF CAPITAL T
    (Unicode:$1D5B4; Attr:daFont; Str:#$0055),   // MATHEMATICAL SANS-SERIF CAPITAL U
    (Unicode:$1D5B5; Attr:daFont; Str:#$0056),   // MATHEMATICAL SANS-SERIF CAPITAL V
    (Unicode:$1D5B6; Attr:daFont; Str:#$0057),   // MATHEMATICAL SANS-SERIF CAPITAL W
    (Unicode:$1D5B7; Attr:daFont; Str:#$0058),   // MATHEMATICAL SANS-SERIF CAPITAL X
    (Unicode:$1D5B8; Attr:daFont; Str:#$0059),   // MATHEMATICAL SANS-SERIF CAPITAL Y
    (Unicode:$1D5B9; Attr:daFont; Str:#$005A),   // MATHEMATICAL SANS-SERIF CAPITAL Z
    (Unicode:$1D5BA; Attr:daFont; Str:#$0061),   // MATHEMATICAL SANS-SERIF SMALL A
    (Unicode:$1D5BB; Attr:daFont; Str:#$0062),   // MATHEMATICAL SANS-SERIF SMALL B
    (Unicode:$1D5BC; Attr:daFont; Str:#$0063),   // MATHEMATICAL SANS-SERIF SMALL C
    (Unicode:$1D5BD; Attr:daFont; Str:#$0064),   // MATHEMATICAL SANS-SERIF SMALL D
    (Unicode:$1D5BE; Attr:daFont; Str:#$0065),   // MATHEMATICAL SANS-SERIF SMALL E
    (Unicode:$1D5BF; Attr:daFont; Str:#$0066),   // MATHEMATICAL SANS-SERIF SMALL F
    (Unicode:$1D5C0; Attr:daFont; Str:#$0067),   // MATHEMATICAL SANS-SERIF SMALL G
    (Unicode:$1D5C1; Attr:daFont; Str:#$0068),   // MATHEMATICAL SANS-SERIF SMALL H
    (Unicode:$1D5C2; Attr:daFont; Str:#$0069),   // MATHEMATICAL SANS-SERIF SMALL I
    (Unicode:$1D5C3; Attr:daFont; Str:#$006A),   // MATHEMATICAL SANS-SERIF SMALL J
    (Unicode:$1D5C4; Attr:daFont; Str:#$006B),   // MATHEMATICAL SANS-SERIF SMALL K
    (Unicode:$1D5C5; Attr:daFont; Str:#$006C),   // MATHEMATICAL SANS-SERIF SMALL L
    (Unicode:$1D5C6; Attr:daFont; Str:#$006D),   // MATHEMATICAL SANS-SERIF SMALL M
    (Unicode:$1D5C7; Attr:daFont; Str:#$006E),   // MATHEMATICAL SANS-SERIF SMALL N
    (Unicode:$1D5C8; Attr:daFont; Str:#$006F),   // MATHEMATICAL SANS-SERIF SMALL O
    (Unicode:$1D5C9; Attr:daFont; Str:#$0070),   // MATHEMATICAL SANS-SERIF SMALL P
    (Unicode:$1D5CA; Attr:daFont; Str:#$0071),   // MATHEMATICAL SANS-SERIF SMALL Q
    (Unicode:$1D5CB; Attr:daFont; Str:#$0072),   // MATHEMATICAL SANS-SERIF SMALL R
    (Unicode:$1D5CC; Attr:daFont; Str:#$0073),   // MATHEMATICAL SANS-SERIF SMALL S
    (Unicode:$1D5CD; Attr:daFont; Str:#$0074),   // MATHEMATICAL SANS-SERIF SMALL T
    (Unicode:$1D5CE; Attr:daFont; Str:#$0075),   // MATHEMATICAL SANS-SERIF SMALL U
    (Unicode:$1D5CF; Attr:daFont; Str:#$0076),   // MATHEMATICAL SANS-SERIF SMALL V
    (Unicode:$1D5D0; Attr:daFont; Str:#$0077),   // MATHEMATICAL SANS-SERIF SMALL W
    (Unicode:$1D5D1; Attr:daFont; Str:#$0078),   // MATHEMATICAL SANS-SERIF SMALL X
    (Unicode:$1D5D2; Attr:daFont; Str:#$0079),   // MATHEMATICAL SANS-SERIF SMALL Y
    (Unicode:$1D5D3; Attr:daFont; Str:#$007A),   // MATHEMATICAL SANS-SERIF SMALL Z
    (Unicode:$1D5D4; Attr:daFont; Str:#$0041),   // MATHEMATICAL SANS-SERIF BOLD CAPITAL A
    (Unicode:$1D5D5; Attr:daFont; Str:#$0042),   // MATHEMATICAL SANS-SERIF BOLD CAPITAL B
    (Unicode:$1D5D6; Attr:daFont; Str:#$0043),   // MATHEMATICAL SANS-SERIF BOLD CAPITAL C
    (Unicode:$1D5D7; Attr:daFont; Str:#$0044),   // MATHEMATICAL SANS-SERIF BOLD CAPITAL D
    (Unicode:$1D5D8; Attr:daFont; Str:#$0045),   // MATHEMATICAL SANS-SERIF BOLD CAPITAL E
    (Unicode:$1D5D9; Attr:daFont; Str:#$0046),   // MATHEMATICAL SANS-SERIF BOLD CAPITAL F
    (Unicode:$1D5DA; Attr:daFont; Str:#$0047),   // MATHEMATICAL SANS-SERIF BOLD CAPITAL G
    (Unicode:$1D5DB; Attr:daFont; Str:#$0048),   // MATHEMATICAL SANS-SERIF BOLD CAPITAL H
    (Unicode:$1D5DC; Attr:daFont; Str:#$0049),   // MATHEMATICAL SANS-SERIF BOLD CAPITAL I
    (Unicode:$1D5DD; Attr:daFont; Str:#$004A),   // MATHEMATICAL SANS-SERIF BOLD CAPITAL J
    (Unicode:$1D5DE; Attr:daFont; Str:#$004B),   // MATHEMATICAL SANS-SERIF BOLD CAPITAL K
    (Unicode:$1D5DF; Attr:daFont; Str:#$004C),   // MATHEMATICAL SANS-SERIF BOLD CAPITAL L
    (Unicode:$1D5E0; Attr:daFont; Str:#$004D),   // MATHEMATICAL SANS-SERIF BOLD CAPITAL M
    (Unicode:$1D5E1; Attr:daFont; Str:#$004E),   // MATHEMATICAL SANS-SERIF BOLD CAPITAL N
    (Unicode:$1D5E2; Attr:daFont; Str:#$004F),   // MATHEMATICAL SANS-SERIF BOLD CAPITAL O
    (Unicode:$1D5E3; Attr:daFont; Str:#$0050),   // MATHEMATICAL SANS-SERIF BOLD CAPITAL P
    (Unicode:$1D5E4; Attr:daFont; Str:#$0051),   // MATHEMATICAL SANS-SERIF BOLD CAPITAL Q
    (Unicode:$1D5E5; Attr:daFont; Str:#$0052),   // MATHEMATICAL SANS-SERIF BOLD CAPITAL R
    (Unicode:$1D5E6; Attr:daFont; Str:#$0053),   // MATHEMATICAL SANS-SERIF BOLD CAPITAL S
    (Unicode:$1D5E7; Attr:daFont; Str:#$0054),   // MATHEMATICAL SANS-SERIF BOLD CAPITAL T
    (Unicode:$1D5E8; Attr:daFont; Str:#$0055),   // MATHEMATICAL SANS-SERIF BOLD CAPITAL U
    (Unicode:$1D5E9; Attr:daFont; Str:#$0056),   // MATHEMATICAL SANS-SERIF BOLD CAPITAL V
    (Unicode:$1D5EA; Attr:daFont; Str:#$0057),   // MATHEMATICAL SANS-SERIF BOLD CAPITAL W
    (Unicode:$1D5EB; Attr:daFont; Str:#$0058),   // MATHEMATICAL SANS-SERIF BOLD CAPITAL X
    (Unicode:$1D5EC; Attr:daFont; Str:#$0059),   // MATHEMATICAL SANS-SERIF BOLD CAPITAL Y
    (Unicode:$1D5ED; Attr:daFont; Str:#$005A),   // MATHEMATICAL SANS-SERIF BOLD CAPITAL Z
    (Unicode:$1D5EE; Attr:daFont; Str:#$0061),   // MATHEMATICAL SANS-SERIF BOLD SMALL A
    (Unicode:$1D5EF; Attr:daFont; Str:#$0062),   // MATHEMATICAL SANS-SERIF BOLD SMALL B
    (Unicode:$1D5F0; Attr:daFont; Str:#$0063),   // MATHEMATICAL SANS-SERIF BOLD SMALL C
    (Unicode:$1D5F1; Attr:daFont; Str:#$0064),   // MATHEMATICAL SANS-SERIF BOLD SMALL D
    (Unicode:$1D5F2; Attr:daFont; Str:#$0065),   // MATHEMATICAL SANS-SERIF BOLD SMALL E
    (Unicode:$1D5F3; Attr:daFont; Str:#$0066),   // MATHEMATICAL SANS-SERIF BOLD SMALL F
    (Unicode:$1D5F4; Attr:daFont; Str:#$0067),   // MATHEMATICAL SANS-SERIF BOLD SMALL G
    (Unicode:$1D5F5; Attr:daFont; Str:#$0068),   // MATHEMATICAL SANS-SERIF BOLD SMALL H
    (Unicode:$1D5F6; Attr:daFont; Str:#$0069),   // MATHEMATICAL SANS-SERIF BOLD SMALL I
    (Unicode:$1D5F7; Attr:daFont; Str:#$006A),   // MATHEMATICAL SANS-SERIF BOLD SMALL J
    (Unicode:$1D5F8; Attr:daFont; Str:#$006B),   // MATHEMATICAL SANS-SERIF BOLD SMALL K
    (Unicode:$1D5F9; Attr:daFont; Str:#$006C),   // MATHEMATICAL SANS-SERIF BOLD SMALL L
    (Unicode:$1D5FA; Attr:daFont; Str:#$006D),   // MATHEMATICAL SANS-SERIF BOLD SMALL M
    (Unicode:$1D5FB; Attr:daFont; Str:#$006E),   // MATHEMATICAL SANS-SERIF BOLD SMALL N
    (Unicode:$1D5FC; Attr:daFont; Str:#$006F),   // MATHEMATICAL SANS-SERIF BOLD SMALL O
    (Unicode:$1D5FD; Attr:daFont; Str:#$0070),   // MATHEMATICAL SANS-SERIF BOLD SMALL P
    (Unicode:$1D5FE; Attr:daFont; Str:#$0071),   // MATHEMATICAL SANS-SERIF BOLD SMALL Q
    (Unicode:$1D5FF; Attr:daFont; Str:#$0072),   // MATHEMATICAL SANS-SERIF BOLD SMALL R
    (Unicode:$1D600; Attr:daFont; Str:#$0073),   // MATHEMATICAL SANS-SERIF BOLD SMALL S
    (Unicode:$1D601; Attr:daFont; Str:#$0074),   // MATHEMATICAL SANS-SERIF BOLD SMALL T
    (Unicode:$1D602; Attr:daFont; Str:#$0075),   // MATHEMATICAL SANS-SERIF BOLD SMALL U
    (Unicode:$1D603; Attr:daFont; Str:#$0076),   // MATHEMATICAL SANS-SERIF BOLD SMALL V
    (Unicode:$1D604; Attr:daFont; Str:#$0077),   // MATHEMATICAL SANS-SERIF BOLD SMALL W
    (Unicode:$1D605; Attr:daFont; Str:#$0078),   // MATHEMATICAL SANS-SERIF BOLD SMALL X
    (Unicode:$1D606; Attr:daFont; Str:#$0079),   // MATHEMATICAL SANS-SERIF BOLD SMALL Y
    (Unicode:$1D607; Attr:daFont; Str:#$007A),   // MATHEMATICAL SANS-SERIF BOLD SMALL Z
    (Unicode:$1D608; Attr:daFont; Str:#$0041),   // MATHEMATICAL SANS-SERIF ITALIC CAPITAL A
    (Unicode:$1D609; Attr:daFont; Str:#$0042),   // MATHEMATICAL SANS-SERIF ITALIC CAPITAL B
    (Unicode:$1D60A; Attr:daFont; Str:#$0043),   // MATHEMATICAL SANS-SERIF ITALIC CAPITAL C
    (Unicode:$1D60B; Attr:daFont; Str:#$0044),   // MATHEMATICAL SANS-SERIF ITALIC CAPITAL D
    (Unicode:$1D60C; Attr:daFont; Str:#$0045),   // MATHEMATICAL SANS-SERIF ITALIC CAPITAL E
    (Unicode:$1D60D; Attr:daFont; Str:#$0046),   // MATHEMATICAL SANS-SERIF ITALIC CAPITAL F
    (Unicode:$1D60E; Attr:daFont; Str:#$0047),   // MATHEMATICAL SANS-SERIF ITALIC CAPITAL G
    (Unicode:$1D60F; Attr:daFont; Str:#$0048),   // MATHEMATICAL SANS-SERIF ITALIC CAPITAL H
    (Unicode:$1D610; Attr:daFont; Str:#$0049),   // MATHEMATICAL SANS-SERIF ITALIC CAPITAL I
    (Unicode:$1D611; Attr:daFont; Str:#$004A),   // MATHEMATICAL SANS-SERIF ITALIC CAPITAL J
    (Unicode:$1D612; Attr:daFont; Str:#$004B),   // MATHEMATICAL SANS-SERIF ITALIC CAPITAL K
    (Unicode:$1D613; Attr:daFont; Str:#$004C),   // MATHEMATICAL SANS-SERIF ITALIC CAPITAL L
    (Unicode:$1D614; Attr:daFont; Str:#$004D),   // MATHEMATICAL SANS-SERIF ITALIC CAPITAL M
    (Unicode:$1D615; Attr:daFont; Str:#$004E),   // MATHEMATICAL SANS-SERIF ITALIC CAPITAL N
    (Unicode:$1D616; Attr:daFont; Str:#$004F),   // MATHEMATICAL SANS-SERIF ITALIC CAPITAL O
    (Unicode:$1D617; Attr:daFont; Str:#$0050),   // MATHEMATICAL SANS-SERIF ITALIC CAPITAL P
    (Unicode:$1D618; Attr:daFont; Str:#$0051),   // MATHEMATICAL SANS-SERIF ITALIC CAPITAL Q
    (Unicode:$1D619; Attr:daFont; Str:#$0052),   // MATHEMATICAL SANS-SERIF ITALIC CAPITAL R
    (Unicode:$1D61A; Attr:daFont; Str:#$0053),   // MATHEMATICAL SANS-SERIF ITALIC CAPITAL S
    (Unicode:$1D61B; Attr:daFont; Str:#$0054),   // MATHEMATICAL SANS-SERIF ITALIC CAPITAL T
    (Unicode:$1D61C; Attr:daFont; Str:#$0055),   // MATHEMATICAL SANS-SERIF ITALIC CAPITAL U
    (Unicode:$1D61D; Attr:daFont; Str:#$0056),   // MATHEMATICAL SANS-SERIF ITALIC CAPITAL V
    (Unicode:$1D61E; Attr:daFont; Str:#$0057),   // MATHEMATICAL SANS-SERIF ITALIC CAPITAL W
    (Unicode:$1D61F; Attr:daFont; Str:#$0058),   // MATHEMATICAL SANS-SERIF ITALIC CAPITAL X
    (Unicode:$1D620; Attr:daFont; Str:#$0059),   // MATHEMATICAL SANS-SERIF ITALIC CAPITAL Y
    (Unicode:$1D621; Attr:daFont; Str:#$005A),   // MATHEMATICAL SANS-SERIF ITALIC CAPITAL Z
    (Unicode:$1D622; Attr:daFont; Str:#$0061),   // MATHEMATICAL SANS-SERIF ITALIC SMALL A
    (Unicode:$1D623; Attr:daFont; Str:#$0062),   // MATHEMATICAL SANS-SERIF ITALIC SMALL B
    (Unicode:$1D624; Attr:daFont; Str:#$0063),   // MATHEMATICAL SANS-SERIF ITALIC SMALL C
    (Unicode:$1D625; Attr:daFont; Str:#$0064),   // MATHEMATICAL SANS-SERIF ITALIC SMALL D
    (Unicode:$1D626; Attr:daFont; Str:#$0065),   // MATHEMATICAL SANS-SERIF ITALIC SMALL E
    (Unicode:$1D627; Attr:daFont; Str:#$0066),   // MATHEMATICAL SANS-SERIF ITALIC SMALL F
    (Unicode:$1D628; Attr:daFont; Str:#$0067),   // MATHEMATICAL SANS-SERIF ITALIC SMALL G
    (Unicode:$1D629; Attr:daFont; Str:#$0068),   // MATHEMATICAL SANS-SERIF ITALIC SMALL H
    (Unicode:$1D62A; Attr:daFont; Str:#$0069),   // MATHEMATICAL SANS-SERIF ITALIC SMALL I
    (Unicode:$1D62B; Attr:daFont; Str:#$006A),   // MATHEMATICAL SANS-SERIF ITALIC SMALL J
    (Unicode:$1D62C; Attr:daFont; Str:#$006B),   // MATHEMATICAL SANS-SERIF ITALIC SMALL K
    (Unicode:$1D62D; Attr:daFont; Str:#$006C),   // MATHEMATICAL SANS-SERIF ITALIC SMALL L
    (Unicode:$1D62E; Attr:daFont; Str:#$006D),   // MATHEMATICAL SANS-SERIF ITALIC SMALL M
    (Unicode:$1D62F; Attr:daFont; Str:#$006E),   // MATHEMATICAL SANS-SERIF ITALIC SMALL N
    (Unicode:$1D630; Attr:daFont; Str:#$006F),   // MATHEMATICAL SANS-SERIF ITALIC SMALL O
    (Unicode:$1D631; Attr:daFont; Str:#$0070),   // MATHEMATICAL SANS-SERIF ITALIC SMALL P
    (Unicode:$1D632; Attr:daFont; Str:#$0071),   // MATHEMATICAL SANS-SERIF ITALIC SMALL Q
    (Unicode:$1D633; Attr:daFont; Str:#$0072),   // MATHEMATICAL SANS-SERIF ITALIC SMALL R
    (Unicode:$1D634; Attr:daFont; Str:#$0073),   // MATHEMATICAL SANS-SERIF ITALIC SMALL S
    (Unicode:$1D635; Attr:daFont; Str:#$0074),   // MATHEMATICAL SANS-SERIF ITALIC SMALL T
    (Unicode:$1D636; Attr:daFont; Str:#$0075),   // MATHEMATICAL SANS-SERIF ITALIC SMALL U
    (Unicode:$1D637; Attr:daFont; Str:#$0076),   // MATHEMATICAL SANS-SERIF ITALIC SMALL V
    (Unicode:$1D638; Attr:daFont; Str:#$0077),   // MATHEMATICAL SANS-SERIF ITALIC SMALL W
    (Unicode:$1D639; Attr:daFont; Str:#$0078),   // MATHEMATICAL SANS-SERIF ITALIC SMALL X
    (Unicode:$1D63A; Attr:daFont; Str:#$0079),   // MATHEMATICAL SANS-SERIF ITALIC SMALL Y
    (Unicode:$1D63B; Attr:daFont; Str:#$007A),   // MATHEMATICAL SANS-SERIF ITALIC SMALL Z
    (Unicode:$1D63C; Attr:daFont; Str:#$0041),   // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL A
    (Unicode:$1D63D; Attr:daFont; Str:#$0042),   // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL B
    (Unicode:$1D63E; Attr:daFont; Str:#$0043),   // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL C
    (Unicode:$1D63F; Attr:daFont; Str:#$0044),   // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL D
    (Unicode:$1D640; Attr:daFont; Str:#$0045),   // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL E
    (Unicode:$1D641; Attr:daFont; Str:#$0046),   // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL F
    (Unicode:$1D642; Attr:daFont; Str:#$0047),   // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL G
    (Unicode:$1D643; Attr:daFont; Str:#$0048),   // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL H
    (Unicode:$1D644; Attr:daFont; Str:#$0049),   // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL I
    (Unicode:$1D645; Attr:daFont; Str:#$004A),   // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL J
    (Unicode:$1D646; Attr:daFont; Str:#$004B),   // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL K
    (Unicode:$1D647; Attr:daFont; Str:#$004C),   // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL L
    (Unicode:$1D648; Attr:daFont; Str:#$004D),   // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL M
    (Unicode:$1D649; Attr:daFont; Str:#$004E),   // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL N
    (Unicode:$1D64A; Attr:daFont; Str:#$004F),   // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL O
    (Unicode:$1D64B; Attr:daFont; Str:#$0050),   // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL P
    (Unicode:$1D64C; Attr:daFont; Str:#$0051),   // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL Q
    (Unicode:$1D64D; Attr:daFont; Str:#$0052),   // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL R
    (Unicode:$1D64E; Attr:daFont; Str:#$0053),   // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL S
    (Unicode:$1D64F; Attr:daFont; Str:#$0054),   // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL T
    (Unicode:$1D650; Attr:daFont; Str:#$0055),   // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL U
    (Unicode:$1D651; Attr:daFont; Str:#$0056),   // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL V
    (Unicode:$1D652; Attr:daFont; Str:#$0057),   // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL W
    (Unicode:$1D653; Attr:daFont; Str:#$0058),   // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL X
    (Unicode:$1D654; Attr:daFont; Str:#$0059),   // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL Y
    (Unicode:$1D655; Attr:daFont; Str:#$005A),   // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL Z
    (Unicode:$1D656; Attr:daFont; Str:#$0061),   // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL A
    (Unicode:$1D657; Attr:daFont; Str:#$0062),   // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL B
    (Unicode:$1D658; Attr:daFont; Str:#$0063),   // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL C
    (Unicode:$1D659; Attr:daFont; Str:#$0064),   // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL D
    (Unicode:$1D65A; Attr:daFont; Str:#$0065),   // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL E
    (Unicode:$1D65B; Attr:daFont; Str:#$0066),   // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL F
    (Unicode:$1D65C; Attr:daFont; Str:#$0067),   // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL G
    (Unicode:$1D65D; Attr:daFont; Str:#$0068),   // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL H
    (Unicode:$1D65E; Attr:daFont; Str:#$0069),   // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL I
    (Unicode:$1D65F; Attr:daFont; Str:#$006A),   // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL J
    (Unicode:$1D660; Attr:daFont; Str:#$006B),   // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL K
    (Unicode:$1D661; Attr:daFont; Str:#$006C),   // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL L
    (Unicode:$1D662; Attr:daFont; Str:#$006D),   // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL M
    (Unicode:$1D663; Attr:daFont; Str:#$006E),   // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL N
    (Unicode:$1D664; Attr:daFont; Str:#$006F),   // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL O
    (Unicode:$1D665; Attr:daFont; Str:#$0070),   // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL P
    (Unicode:$1D666; Attr:daFont; Str:#$0071),   // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL Q
    (Unicode:$1D667; Attr:daFont; Str:#$0072),   // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL R
    (Unicode:$1D668; Attr:daFont; Str:#$0073),   // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL S
    (Unicode:$1D669; Attr:daFont; Str:#$0074),   // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL T
    (Unicode:$1D66A; Attr:daFont; Str:#$0075),   // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL U
    (Unicode:$1D66B; Attr:daFont; Str:#$0076),   // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL V
    (Unicode:$1D66C; Attr:daFont; Str:#$0077),   // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL W
    (Unicode:$1D66D; Attr:daFont; Str:#$0078),   // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL X
    (Unicode:$1D66E; Attr:daFont; Str:#$0079),   // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL Y
    (Unicode:$1D66F; Attr:daFont; Str:#$007A),   // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL Z
    (Unicode:$1D670; Attr:daFont; Str:#$0041),   // MATHEMATICAL MONOSPACE CAPITAL A
    (Unicode:$1D671; Attr:daFont; Str:#$0042),   // MATHEMATICAL MONOSPACE CAPITAL B
    (Unicode:$1D672; Attr:daFont; Str:#$0043),   // MATHEMATICAL MONOSPACE CAPITAL C
    (Unicode:$1D673; Attr:daFont; Str:#$0044),   // MATHEMATICAL MONOSPACE CAPITAL D
    (Unicode:$1D674; Attr:daFont; Str:#$0045),   // MATHEMATICAL MONOSPACE CAPITAL E
    (Unicode:$1D675; Attr:daFont; Str:#$0046),   // MATHEMATICAL MONOSPACE CAPITAL F
    (Unicode:$1D676; Attr:daFont; Str:#$0047),   // MATHEMATICAL MONOSPACE CAPITAL G
    (Unicode:$1D677; Attr:daFont; Str:#$0048),   // MATHEMATICAL MONOSPACE CAPITAL H
    (Unicode:$1D678; Attr:daFont; Str:#$0049),   // MATHEMATICAL MONOSPACE CAPITAL I
    (Unicode:$1D679; Attr:daFont; Str:#$004A),   // MATHEMATICAL MONOSPACE CAPITAL J
    (Unicode:$1D67A; Attr:daFont; Str:#$004B),   // MATHEMATICAL MONOSPACE CAPITAL K
    (Unicode:$1D67B; Attr:daFont; Str:#$004C),   // MATHEMATICAL MONOSPACE CAPITAL L
    (Unicode:$1D67C; Attr:daFont; Str:#$004D),   // MATHEMATICAL MONOSPACE CAPITAL M
    (Unicode:$1D67D; Attr:daFont; Str:#$004E),   // MATHEMATICAL MONOSPACE CAPITAL N
    (Unicode:$1D67E; Attr:daFont; Str:#$004F),   // MATHEMATICAL MONOSPACE CAPITAL O
    (Unicode:$1D67F; Attr:daFont; Str:#$0050),   // MATHEMATICAL MONOSPACE CAPITAL P
    (Unicode:$1D680; Attr:daFont; Str:#$0051),   // MATHEMATICAL MONOSPACE CAPITAL Q
    (Unicode:$1D681; Attr:daFont; Str:#$0052),   // MATHEMATICAL MONOSPACE CAPITAL R
    (Unicode:$1D682; Attr:daFont; Str:#$0053),   // MATHEMATICAL MONOSPACE CAPITAL S
    (Unicode:$1D683; Attr:daFont; Str:#$0054),   // MATHEMATICAL MONOSPACE CAPITAL T
    (Unicode:$1D684; Attr:daFont; Str:#$0055),   // MATHEMATICAL MONOSPACE CAPITAL U
    (Unicode:$1D685; Attr:daFont; Str:#$0056),   // MATHEMATICAL MONOSPACE CAPITAL V
    (Unicode:$1D686; Attr:daFont; Str:#$0057),   // MATHEMATICAL MONOSPACE CAPITAL W
    (Unicode:$1D687; Attr:daFont; Str:#$0058),   // MATHEMATICAL MONOSPACE CAPITAL X
    (Unicode:$1D688; Attr:daFont; Str:#$0059),   // MATHEMATICAL MONOSPACE CAPITAL Y
    (Unicode:$1D689; Attr:daFont; Str:#$005A),   // MATHEMATICAL MONOSPACE CAPITAL Z
    (Unicode:$1D68A; Attr:daFont; Str:#$0061),   // MATHEMATICAL MONOSPACE SMALL A
    (Unicode:$1D68B; Attr:daFont; Str:#$0062),   // MATHEMATICAL MONOSPACE SMALL B
    (Unicode:$1D68C; Attr:daFont; Str:#$0063),   // MATHEMATICAL MONOSPACE SMALL C
    (Unicode:$1D68D; Attr:daFont; Str:#$0064),   // MATHEMATICAL MONOSPACE SMALL D
    (Unicode:$1D68E; Attr:daFont; Str:#$0065),   // MATHEMATICAL MONOSPACE SMALL E
    (Unicode:$1D68F; Attr:daFont; Str:#$0066),   // MATHEMATICAL MONOSPACE SMALL F
    (Unicode:$1D690; Attr:daFont; Str:#$0067),   // MATHEMATICAL MONOSPACE SMALL G
    (Unicode:$1D691; Attr:daFont; Str:#$0068),   // MATHEMATICAL MONOSPACE SMALL H
    (Unicode:$1D692; Attr:daFont; Str:#$0069),   // MATHEMATICAL MONOSPACE SMALL I
    (Unicode:$1D693; Attr:daFont; Str:#$006A),   // MATHEMATICAL MONOSPACE SMALL J
    (Unicode:$1D694; Attr:daFont; Str:#$006B),   // MATHEMATICAL MONOSPACE SMALL K
    (Unicode:$1D695; Attr:daFont; Str:#$006C),   // MATHEMATICAL MONOSPACE SMALL L
    (Unicode:$1D696; Attr:daFont; Str:#$006D),   // MATHEMATICAL MONOSPACE SMALL M
    (Unicode:$1D697; Attr:daFont; Str:#$006E),   // MATHEMATICAL MONOSPACE SMALL N
    (Unicode:$1D698; Attr:daFont; Str:#$006F),   // MATHEMATICAL MONOSPACE SMALL O
    (Unicode:$1D699; Attr:daFont; Str:#$0070),   // MATHEMATICAL MONOSPACE SMALL P
    (Unicode:$1D69A; Attr:daFont; Str:#$0071),   // MATHEMATICAL MONOSPACE SMALL Q
    (Unicode:$1D69B; Attr:daFont; Str:#$0072),   // MATHEMATICAL MONOSPACE SMALL R
    (Unicode:$1D69C; Attr:daFont; Str:#$0073),   // MATHEMATICAL MONOSPACE SMALL S
    (Unicode:$1D69D; Attr:daFont; Str:#$0074),   // MATHEMATICAL MONOSPACE SMALL T
    (Unicode:$1D69E; Attr:daFont; Str:#$0075),   // MATHEMATICAL MONOSPACE SMALL U
    (Unicode:$1D69F; Attr:daFont; Str:#$0076),   // MATHEMATICAL MONOSPACE SMALL V
    (Unicode:$1D6A0; Attr:daFont; Str:#$0077),   // MATHEMATICAL MONOSPACE SMALL W
    (Unicode:$1D6A1; Attr:daFont; Str:#$0078),   // MATHEMATICAL MONOSPACE SMALL X
    (Unicode:$1D6A2; Attr:daFont; Str:#$0079),   // MATHEMATICAL MONOSPACE SMALL Y
    (Unicode:$1D6A3; Attr:daFont; Str:#$007A),   // MATHEMATICAL MONOSPACE SMALL Z
    (Unicode:$1D6A8; Attr:daFont; Str:#$0391),   // MATHEMATICAL BOLD CAPITAL ALPHA
    (Unicode:$1D6A9; Attr:daFont; Str:#$0392),   // MATHEMATICAL BOLD CAPITAL BETA
    (Unicode:$1D6AA; Attr:daFont; Str:#$0393),   // MATHEMATICAL BOLD CAPITAL GAMMA
    (Unicode:$1D6AB; Attr:daFont; Str:#$0394),   // MATHEMATICAL BOLD CAPITAL DELTA
    (Unicode:$1D6AC; Attr:daFont; Str:#$0395),   // MATHEMATICAL BOLD CAPITAL EPSILON
    (Unicode:$1D6AD; Attr:daFont; Str:#$0396),   // MATHEMATICAL BOLD CAPITAL ZETA
    (Unicode:$1D6AE; Attr:daFont; Str:#$0397),   // MATHEMATICAL BOLD CAPITAL ETA
    (Unicode:$1D6AF; Attr:daFont; Str:#$0398),   // MATHEMATICAL BOLD CAPITAL THETA
    (Unicode:$1D6B0; Attr:daFont; Str:#$0399),   // MATHEMATICAL BOLD CAPITAL IOTA
    (Unicode:$1D6B1; Attr:daFont; Str:#$039A),   // MATHEMATICAL BOLD CAPITAL KAPPA
    (Unicode:$1D6B2; Attr:daFont; Str:#$039B),   // MATHEMATICAL BOLD CAPITAL LAMDA
    (Unicode:$1D6B3; Attr:daFont; Str:#$039C),   // MATHEMATICAL BOLD CAPITAL MU
    (Unicode:$1D6B4; Attr:daFont; Str:#$039D),   // MATHEMATICAL BOLD CAPITAL NU
    (Unicode:$1D6B5; Attr:daFont; Str:#$039E),   // MATHEMATICAL BOLD CAPITAL XI
    (Unicode:$1D6B6; Attr:daFont; Str:#$039F),   // MATHEMATICAL BOLD CAPITAL OMICRON
    (Unicode:$1D6B7; Attr:daFont; Str:#$03A0),   // MATHEMATICAL BOLD CAPITAL PI
    (Unicode:$1D6B8; Attr:daFont; Str:#$03A1),   // MATHEMATICAL BOLD CAPITAL RHO
    (Unicode:$1D6B9; Attr:daFont; Str:#$03F4),   // MATHEMATICAL BOLD CAPITAL THETA SYMBOL
    (Unicode:$1D6BA; Attr:daFont; Str:#$03A3),   // MATHEMATICAL BOLD CAPITAL SIGMA
    (Unicode:$1D6BB; Attr:daFont; Str:#$03A4),   // MATHEMATICAL BOLD CAPITAL TAU
    (Unicode:$1D6BC; Attr:daFont; Str:#$03A5),   // MATHEMATICAL BOLD CAPITAL UPSILON
    (Unicode:$1D6BD; Attr:daFont; Str:#$03A6),   // MATHEMATICAL BOLD CAPITAL PHI
    (Unicode:$1D6BE; Attr:daFont; Str:#$03A7),   // MATHEMATICAL BOLD CAPITAL CHI
    (Unicode:$1D6BF; Attr:daFont; Str:#$03A8),   // MATHEMATICAL BOLD CAPITAL PSI
    (Unicode:$1D6C0; Attr:daFont; Str:#$03A9),   // MATHEMATICAL BOLD CAPITAL OMEGA
    (Unicode:$1D6C1; Attr:daFont; Str:#$2207),   // MATHEMATICAL BOLD NABLA
    (Unicode:$1D6C2; Attr:daFont; Str:#$03B1),   // MATHEMATICAL BOLD SMALL ALPHA
    (Unicode:$1D6C3; Attr:daFont; Str:#$03B2),   // MATHEMATICAL BOLD SMALL BETA
    (Unicode:$1D6C4; Attr:daFont; Str:#$03B3),   // MATHEMATICAL BOLD SMALL GAMMA
    (Unicode:$1D6C5; Attr:daFont; Str:#$03B4),   // MATHEMATICAL BOLD SMALL DELTA
    (Unicode:$1D6C6; Attr:daFont; Str:#$03B5),   // MATHEMATICAL BOLD SMALL EPSILON
    (Unicode:$1D6C7; Attr:daFont; Str:#$03B6),   // MATHEMATICAL BOLD SMALL ZETA
    (Unicode:$1D6C8; Attr:daFont; Str:#$03B7),   // MATHEMATICAL BOLD SMALL ETA
    (Unicode:$1D6C9; Attr:daFont; Str:#$03B8),   // MATHEMATICAL BOLD SMALL THETA
    (Unicode:$1D6CA; Attr:daFont; Str:#$03B9),   // MATHEMATICAL BOLD SMALL IOTA
    (Unicode:$1D6CB; Attr:daFont; Str:#$03BA),   // MATHEMATICAL BOLD SMALL KAPPA
    (Unicode:$1D6CC; Attr:daFont; Str:#$03BB),   // MATHEMATICAL BOLD SMALL LAMDA
    (Unicode:$1D6CD; Attr:daFont; Str:#$03BC),   // MATHEMATICAL BOLD SMALL MU
    (Unicode:$1D6CE; Attr:daFont; Str:#$03BD),   // MATHEMATICAL BOLD SMALL NU
    (Unicode:$1D6CF; Attr:daFont; Str:#$03BE),   // MATHEMATICAL BOLD SMALL XI
    (Unicode:$1D6D0; Attr:daFont; Str:#$03BF),   // MATHEMATICAL BOLD SMALL OMICRON
    (Unicode:$1D6D1; Attr:daFont; Str:#$03C0),   // MATHEMATICAL BOLD SMALL PI
    (Unicode:$1D6D2; Attr:daFont; Str:#$03C1),   // MATHEMATICAL BOLD SMALL RHO
    (Unicode:$1D6D3; Attr:daFont; Str:#$03C2),   // MATHEMATICAL BOLD SMALL FINAL SIGMA
    (Unicode:$1D6D4; Attr:daFont; Str:#$03C3),   // MATHEMATICAL BOLD SMALL SIGMA
    (Unicode:$1D6D5; Attr:daFont; Str:#$03C4),   // MATHEMATICAL BOLD SMALL TAU
    (Unicode:$1D6D6; Attr:daFont; Str:#$03C5),   // MATHEMATICAL BOLD SMALL UPSILON
    (Unicode:$1D6D7; Attr:daFont; Str:#$03C6),   // MATHEMATICAL BOLD SMALL PHI
    (Unicode:$1D6D8; Attr:daFont; Str:#$03C7),   // MATHEMATICAL BOLD SMALL CHI
    (Unicode:$1D6D9; Attr:daFont; Str:#$03C8),   // MATHEMATICAL BOLD SMALL PSI
    (Unicode:$1D6DA; Attr:daFont; Str:#$03C9),   // MATHEMATICAL BOLD SMALL OMEGA
    (Unicode:$1D6DB; Attr:daFont; Str:#$2202),   // MATHEMATICAL BOLD PARTIAL DIFFERENTIAL
    (Unicode:$1D6DC; Attr:daFont; Str:#$03F5),   // MATHEMATICAL BOLD EPSILON SYMBOL
    (Unicode:$1D6DD; Attr:daFont; Str:#$03D1),   // MATHEMATICAL BOLD THETA SYMBOL
    (Unicode:$1D6DE; Attr:daFont; Str:#$03F0),   // MATHEMATICAL BOLD KAPPA SYMBOL
    (Unicode:$1D6DF; Attr:daFont; Str:#$03D5),   // MATHEMATICAL BOLD PHI SYMBOL
    (Unicode:$1D6E0; Attr:daFont; Str:#$03F1),   // MATHEMATICAL BOLD RHO SYMBOL
    (Unicode:$1D6E1; Attr:daFont; Str:#$03D6),   // MATHEMATICAL BOLD PI SYMBOL
    (Unicode:$1D6E2; Attr:daFont; Str:#$0391),   // MATHEMATICAL ITALIC CAPITAL ALPHA
    (Unicode:$1D6E3; Attr:daFont; Str:#$0392),   // MATHEMATICAL ITALIC CAPITAL BETA
    (Unicode:$1D6E4; Attr:daFont; Str:#$0393),   // MATHEMATICAL ITALIC CAPITAL GAMMA
    (Unicode:$1D6E5; Attr:daFont; Str:#$0394),   // MATHEMATICAL ITALIC CAPITAL DELTA
    (Unicode:$1D6E6; Attr:daFont; Str:#$0395),   // MATHEMATICAL ITALIC CAPITAL EPSILON
    (Unicode:$1D6E7; Attr:daFont; Str:#$0396),   // MATHEMATICAL ITALIC CAPITAL ZETA
    (Unicode:$1D6E8; Attr:daFont; Str:#$0397),   // MATHEMATICAL ITALIC CAPITAL ETA
    (Unicode:$1D6E9; Attr:daFont; Str:#$0398),   // MATHEMATICAL ITALIC CAPITAL THETA
    (Unicode:$1D6EA; Attr:daFont; Str:#$0399),   // MATHEMATICAL ITALIC CAPITAL IOTA
    (Unicode:$1D6EB; Attr:daFont; Str:#$039A),   // MATHEMATICAL ITALIC CAPITAL KAPPA
    (Unicode:$1D6EC; Attr:daFont; Str:#$039B),   // MATHEMATICAL ITALIC CAPITAL LAMDA
    (Unicode:$1D6ED; Attr:daFont; Str:#$039C),   // MATHEMATICAL ITALIC CAPITAL MU
    (Unicode:$1D6EE; Attr:daFont; Str:#$039D),   // MATHEMATICAL ITALIC CAPITAL NU
    (Unicode:$1D6EF; Attr:daFont; Str:#$039E),   // MATHEMATICAL ITALIC CAPITAL XI
    (Unicode:$1D6F0; Attr:daFont; Str:#$039F),   // MATHEMATICAL ITALIC CAPITAL OMICRON
    (Unicode:$1D6F1; Attr:daFont; Str:#$03A0),   // MATHEMATICAL ITALIC CAPITAL PI
    (Unicode:$1D6F2; Attr:daFont; Str:#$03A1),   // MATHEMATICAL ITALIC CAPITAL RHO
    (Unicode:$1D6F3; Attr:daFont; Str:#$03F4),   // MATHEMATICAL ITALIC CAPITAL THETA SYMBOL
    (Unicode:$1D6F4; Attr:daFont; Str:#$03A3),   // MATHEMATICAL ITALIC CAPITAL SIGMA
    (Unicode:$1D6F5; Attr:daFont; Str:#$03A4),   // MATHEMATICAL ITALIC CAPITAL TAU
    (Unicode:$1D6F6; Attr:daFont; Str:#$03A5),   // MATHEMATICAL ITALIC CAPITAL UPSILON
    (Unicode:$1D6F7; Attr:daFont; Str:#$03A6),   // MATHEMATICAL ITALIC CAPITAL PHI
    (Unicode:$1D6F8; Attr:daFont; Str:#$03A7),   // MATHEMATICAL ITALIC CAPITAL CHI
    (Unicode:$1D6F9; Attr:daFont; Str:#$03A8),   // MATHEMATICAL ITALIC CAPITAL PSI
    (Unicode:$1D6FA; Attr:daFont; Str:#$03A9),   // MATHEMATICAL ITALIC CAPITAL OMEGA
    (Unicode:$1D6FB; Attr:daFont; Str:#$2207),   // MATHEMATICAL ITALIC NABLA
    (Unicode:$1D6FC; Attr:daFont; Str:#$03B1),   // MATHEMATICAL ITALIC SMALL ALPHA
    (Unicode:$1D6FD; Attr:daFont; Str:#$03B2),   // MATHEMATICAL ITALIC SMALL BETA
    (Unicode:$1D6FE; Attr:daFont; Str:#$03B3),   // MATHEMATICAL ITALIC SMALL GAMMA
    (Unicode:$1D6FF; Attr:daFont; Str:#$03B4),   // MATHEMATICAL ITALIC SMALL DELTA
    (Unicode:$1D700; Attr:daFont; Str:#$03B5),   // MATHEMATICAL ITALIC SMALL EPSILON
    (Unicode:$1D701; Attr:daFont; Str:#$03B6),   // MATHEMATICAL ITALIC SMALL ZETA
    (Unicode:$1D702; Attr:daFont; Str:#$03B7),   // MATHEMATICAL ITALIC SMALL ETA
    (Unicode:$1D703; Attr:daFont; Str:#$03B8),   // MATHEMATICAL ITALIC SMALL THETA
    (Unicode:$1D704; Attr:daFont; Str:#$03B9),   // MATHEMATICAL ITALIC SMALL IOTA
    (Unicode:$1D705; Attr:daFont; Str:#$03BA),   // MATHEMATICAL ITALIC SMALL KAPPA
    (Unicode:$1D706; Attr:daFont; Str:#$03BB),   // MATHEMATICAL ITALIC SMALL LAMDA
    (Unicode:$1D707; Attr:daFont; Str:#$03BC),   // MATHEMATICAL ITALIC SMALL MU
    (Unicode:$1D708; Attr:daFont; Str:#$03BD),   // MATHEMATICAL ITALIC SMALL NU
    (Unicode:$1D709; Attr:daFont; Str:#$03BE),   // MATHEMATICAL ITALIC SMALL XI
    (Unicode:$1D70A; Attr:daFont; Str:#$03BF),   // MATHEMATICAL ITALIC SMALL OMICRON
    (Unicode:$1D70B; Attr:daFont; Str:#$03C0),   // MATHEMATICAL ITALIC SMALL PI
    (Unicode:$1D70C; Attr:daFont; Str:#$03C1),   // MATHEMATICAL ITALIC SMALL RHO
    (Unicode:$1D70D; Attr:daFont; Str:#$03C2),   // MATHEMATICAL ITALIC SMALL FINAL SIGMA
    (Unicode:$1D70E; Attr:daFont; Str:#$03C3),   // MATHEMATICAL ITALIC SMALL SIGMA
    (Unicode:$1D70F; Attr:daFont; Str:#$03C4),   // MATHEMATICAL ITALIC SMALL TAU
    (Unicode:$1D710; Attr:daFont; Str:#$03C5),   // MATHEMATICAL ITALIC SMALL UPSILON
    (Unicode:$1D711; Attr:daFont; Str:#$03C6),   // MATHEMATICAL ITALIC SMALL PHI
    (Unicode:$1D712; Attr:daFont; Str:#$03C7),   // MATHEMATICAL ITALIC SMALL CHI
    (Unicode:$1D713; Attr:daFont; Str:#$03C8),   // MATHEMATICAL ITALIC SMALL PSI
    (Unicode:$1D714; Attr:daFont; Str:#$03C9),   // MATHEMATICAL ITALIC SMALL OMEGA
    (Unicode:$1D715; Attr:daFont; Str:#$2202),   // MATHEMATICAL ITALIC PARTIAL DIFFERENTIAL
    (Unicode:$1D716; Attr:daFont; Str:#$03F5),   // MATHEMATICAL ITALIC EPSILON SYMBOL
    (Unicode:$1D717; Attr:daFont; Str:#$03D1),   // MATHEMATICAL ITALIC THETA SYMBOL
    (Unicode:$1D718; Attr:daFont; Str:#$03F0),   // MATHEMATICAL ITALIC KAPPA SYMBOL
    (Unicode:$1D719; Attr:daFont; Str:#$03D5),   // MATHEMATICAL ITALIC PHI SYMBOL
    (Unicode:$1D71A; Attr:daFont; Str:#$03F1),   // MATHEMATICAL ITALIC RHO SYMBOL
    (Unicode:$1D71B; Attr:daFont; Str:#$03D6),   // MATHEMATICAL ITALIC PI SYMBOL
    (Unicode:$1D71C; Attr:daFont; Str:#$0391),   // MATHEMATICAL BOLD ITALIC CAPITAL ALPHA
    (Unicode:$1D71D; Attr:daFont; Str:#$0392),   // MATHEMATICAL BOLD ITALIC CAPITAL BETA
    (Unicode:$1D71E; Attr:daFont; Str:#$0393),   // MATHEMATICAL BOLD ITALIC CAPITAL GAMMA
    (Unicode:$1D71F; Attr:daFont; Str:#$0394),   // MATHEMATICAL BOLD ITALIC CAPITAL DELTA
    (Unicode:$1D720; Attr:daFont; Str:#$0395),   // MATHEMATICAL BOLD ITALIC CAPITAL EPSILON
    (Unicode:$1D721; Attr:daFont; Str:#$0396),   // MATHEMATICAL BOLD ITALIC CAPITAL ZETA
    (Unicode:$1D722; Attr:daFont; Str:#$0397),   // MATHEMATICAL BOLD ITALIC CAPITAL ETA
    (Unicode:$1D723; Attr:daFont; Str:#$0398),   // MATHEMATICAL BOLD ITALIC CAPITAL THETA
    (Unicode:$1D724; Attr:daFont; Str:#$0399),   // MATHEMATICAL BOLD ITALIC CAPITAL IOTA
    (Unicode:$1D725; Attr:daFont; Str:#$039A),   // MATHEMATICAL BOLD ITALIC CAPITAL KAPPA
    (Unicode:$1D726; Attr:daFont; Str:#$039B),   // MATHEMATICAL BOLD ITALIC CAPITAL LAMDA
    (Unicode:$1D727; Attr:daFont; Str:#$039C),   // MATHEMATICAL BOLD ITALIC CAPITAL MU
    (Unicode:$1D728; Attr:daFont; Str:#$039D),   // MATHEMATICAL BOLD ITALIC CAPITAL NU
    (Unicode:$1D729; Attr:daFont; Str:#$039E),   // MATHEMATICAL BOLD ITALIC CAPITAL XI
    (Unicode:$1D72A; Attr:daFont; Str:#$039F),   // MATHEMATICAL BOLD ITALIC CAPITAL OMICRON
    (Unicode:$1D72B; Attr:daFont; Str:#$03A0),   // MATHEMATICAL BOLD ITALIC CAPITAL PI
    (Unicode:$1D72C; Attr:daFont; Str:#$03A1),   // MATHEMATICAL BOLD ITALIC CAPITAL RHO
    (Unicode:$1D72D; Attr:daFont; Str:#$03F4),   // MATHEMATICAL BOLD ITALIC CAPITAL THETA SYMBOL
    (Unicode:$1D72E; Attr:daFont; Str:#$03A3),   // MATHEMATICAL BOLD ITALIC CAPITAL SIGMA
    (Unicode:$1D72F; Attr:daFont; Str:#$03A4),   // MATHEMATICAL BOLD ITALIC CAPITAL TAU
    (Unicode:$1D730; Attr:daFont; Str:#$03A5),   // MATHEMATICAL BOLD ITALIC CAPITAL UPSILON
    (Unicode:$1D731; Attr:daFont; Str:#$03A6),   // MATHEMATICAL BOLD ITALIC CAPITAL PHI
    (Unicode:$1D732; Attr:daFont; Str:#$03A7),   // MATHEMATICAL BOLD ITALIC CAPITAL CHI
    (Unicode:$1D733; Attr:daFont; Str:#$03A8),   // MATHEMATICAL BOLD ITALIC CAPITAL PSI
    (Unicode:$1D734; Attr:daFont; Str:#$03A9),   // MATHEMATICAL BOLD ITALIC CAPITAL OMEGA
    (Unicode:$1D735; Attr:daFont; Str:#$2207),   // MATHEMATICAL BOLD ITALIC NABLA
    (Unicode:$1D736; Attr:daFont; Str:#$03B1),   // MATHEMATICAL BOLD ITALIC SMALL ALPHA
    (Unicode:$1D737; Attr:daFont; Str:#$03B2),   // MATHEMATICAL BOLD ITALIC SMALL BETA
    (Unicode:$1D738; Attr:daFont; Str:#$03B3),   // MATHEMATICAL BOLD ITALIC SMALL GAMMA
    (Unicode:$1D739; Attr:daFont; Str:#$03B4),   // MATHEMATICAL BOLD ITALIC SMALL DELTA
    (Unicode:$1D73A; Attr:daFont; Str:#$03B5),   // MATHEMATICAL BOLD ITALIC SMALL EPSILON
    (Unicode:$1D73B; Attr:daFont; Str:#$03B6),   // MATHEMATICAL BOLD ITALIC SMALL ZETA
    (Unicode:$1D73C; Attr:daFont; Str:#$03B7),   // MATHEMATICAL BOLD ITALIC SMALL ETA
    (Unicode:$1D73D; Attr:daFont; Str:#$03B8),   // MATHEMATICAL BOLD ITALIC SMALL THETA
    (Unicode:$1D73E; Attr:daFont; Str:#$03B9),   // MATHEMATICAL BOLD ITALIC SMALL IOTA
    (Unicode:$1D73F; Attr:daFont; Str:#$03BA),   // MATHEMATICAL BOLD ITALIC SMALL KAPPA
    (Unicode:$1D740; Attr:daFont; Str:#$03BB),   // MATHEMATICAL BOLD ITALIC SMALL LAMDA
    (Unicode:$1D741; Attr:daFont; Str:#$03BC),   // MATHEMATICAL BOLD ITALIC SMALL MU
    (Unicode:$1D742; Attr:daFont; Str:#$03BD),   // MATHEMATICAL BOLD ITALIC SMALL NU
    (Unicode:$1D743; Attr:daFont; Str:#$03BE),   // MATHEMATICAL BOLD ITALIC SMALL XI
    (Unicode:$1D744; Attr:daFont; Str:#$03BF),   // MATHEMATICAL BOLD ITALIC SMALL OMICRON
    (Unicode:$1D745; Attr:daFont; Str:#$03C0),   // MATHEMATICAL BOLD ITALIC SMALL PI
    (Unicode:$1D746; Attr:daFont; Str:#$03C1),   // MATHEMATICAL BOLD ITALIC SMALL RHO
    (Unicode:$1D747; Attr:daFont; Str:#$03C2),   // MATHEMATICAL BOLD ITALIC SMALL FINAL SIGMA
    (Unicode:$1D748; Attr:daFont; Str:#$03C3),   // MATHEMATICAL BOLD ITALIC SMALL SIGMA
    (Unicode:$1D749; Attr:daFont; Str:#$03C4),   // MATHEMATICAL BOLD ITALIC SMALL TAU
    (Unicode:$1D74A; Attr:daFont; Str:#$03C5),   // MATHEMATICAL BOLD ITALIC SMALL UPSILON
    (Unicode:$1D74B; Attr:daFont; Str:#$03C6),   // MATHEMATICAL BOLD ITALIC SMALL PHI
    (Unicode:$1D74C; Attr:daFont; Str:#$03C7),   // MATHEMATICAL BOLD ITALIC SMALL CHI
    (Unicode:$1D74D; Attr:daFont; Str:#$03C8),   // MATHEMATICAL BOLD ITALIC SMALL PSI
    (Unicode:$1D74E; Attr:daFont; Str:#$03C9),   // MATHEMATICAL BOLD ITALIC SMALL OMEGA
    (Unicode:$1D74F; Attr:daFont; Str:#$2202),   // MATHEMATICAL BOLD ITALIC PARTIAL DIFFERENTIAL
    (Unicode:$1D750; Attr:daFont; Str:#$03F5),   // MATHEMATICAL BOLD ITALIC EPSILON SYMBOL
    (Unicode:$1D751; Attr:daFont; Str:#$03D1),   // MATHEMATICAL BOLD ITALIC THETA SYMBOL
    (Unicode:$1D752; Attr:daFont; Str:#$03F0),   // MATHEMATICAL BOLD ITALIC KAPPA SYMBOL
    (Unicode:$1D753; Attr:daFont; Str:#$03D5),   // MATHEMATICAL BOLD ITALIC PHI SYMBOL
    (Unicode:$1D754; Attr:daFont; Str:#$03F1),   // MATHEMATICAL BOLD ITALIC RHO SYMBOL
    (Unicode:$1D755; Attr:daFont; Str:#$03D6),   // MATHEMATICAL BOLD ITALIC PI SYMBOL
    (Unicode:$1D756; Attr:daFont; Str:#$0391),   // MATHEMATICAL SANS-SERIF BOLD CAPITAL ALPHA
    (Unicode:$1D757; Attr:daFont; Str:#$0392),   // MATHEMATICAL SANS-SERIF BOLD CAPITAL BETA
    (Unicode:$1D758; Attr:daFont; Str:#$0393),   // MATHEMATICAL SANS-SERIF BOLD CAPITAL GAMMA
    (Unicode:$1D759; Attr:daFont; Str:#$0394),   // MATHEMATICAL SANS-SERIF BOLD CAPITAL DELTA
    (Unicode:$1D75A; Attr:daFont; Str:#$0395),   // MATHEMATICAL SANS-SERIF BOLD CAPITAL EPSILON
    (Unicode:$1D75B; Attr:daFont; Str:#$0396),   // MATHEMATICAL SANS-SERIF BOLD CAPITAL ZETA
    (Unicode:$1D75C; Attr:daFont; Str:#$0397),   // MATHEMATICAL SANS-SERIF BOLD CAPITAL ETA
    (Unicode:$1D75D; Attr:daFont; Str:#$0398),   // MATHEMATICAL SANS-SERIF BOLD CAPITAL THETA
    (Unicode:$1D75E; Attr:daFont; Str:#$0399),   // MATHEMATICAL SANS-SERIF BOLD CAPITAL IOTA
    (Unicode:$1D75F; Attr:daFont; Str:#$039A),   // MATHEMATICAL SANS-SERIF BOLD CAPITAL KAPPA
    (Unicode:$1D760; Attr:daFont; Str:#$039B),   // MATHEMATICAL SANS-SERIF BOLD CAPITAL LAMDA
    (Unicode:$1D761; Attr:daFont; Str:#$039C),   // MATHEMATICAL SANS-SERIF BOLD CAPITAL MU
    (Unicode:$1D762; Attr:daFont; Str:#$039D),   // MATHEMATICAL SANS-SERIF BOLD CAPITAL NU
    (Unicode:$1D763; Attr:daFont; Str:#$039E),   // MATHEMATICAL SANS-SERIF BOLD CAPITAL XI
    (Unicode:$1D764; Attr:daFont; Str:#$039F),   // MATHEMATICAL SANS-SERIF BOLD CAPITAL OMICRON
    (Unicode:$1D765; Attr:daFont; Str:#$03A0),   // MATHEMATICAL SANS-SERIF BOLD CAPITAL PI
    (Unicode:$1D766; Attr:daFont; Str:#$03A1),   // MATHEMATICAL SANS-SERIF BOLD CAPITAL RHO
    (Unicode:$1D767; Attr:daFont; Str:#$03F4),   // MATHEMATICAL SANS-SERIF BOLD CAPITAL THETA SYMBOL
    (Unicode:$1D768; Attr:daFont; Str:#$03A3),   // MATHEMATICAL SANS-SERIF BOLD CAPITAL SIGMA
    (Unicode:$1D769; Attr:daFont; Str:#$03A4),   // MATHEMATICAL SANS-SERIF BOLD CAPITAL TAU
    (Unicode:$1D76A; Attr:daFont; Str:#$03A5),   // MATHEMATICAL SANS-SERIF BOLD CAPITAL UPSILON
    (Unicode:$1D76B; Attr:daFont; Str:#$03A6),   // MATHEMATICAL SANS-SERIF BOLD CAPITAL PHI
    (Unicode:$1D76C; Attr:daFont; Str:#$03A7),   // MATHEMATICAL SANS-SERIF BOLD CAPITAL CHI
    (Unicode:$1D76D; Attr:daFont; Str:#$03A8),   // MATHEMATICAL SANS-SERIF BOLD CAPITAL PSI
    (Unicode:$1D76E; Attr:daFont; Str:#$03A9),   // MATHEMATICAL SANS-SERIF BOLD CAPITAL OMEGA
    (Unicode:$1D76F; Attr:daFont; Str:#$2207),   // MATHEMATICAL SANS-SERIF BOLD NABLA
    (Unicode:$1D770; Attr:daFont; Str:#$03B1),   // MATHEMATICAL SANS-SERIF BOLD SMALL ALPHA
    (Unicode:$1D771; Attr:daFont; Str:#$03B2),   // MATHEMATICAL SANS-SERIF BOLD SMALL BETA
    (Unicode:$1D772; Attr:daFont; Str:#$03B3),   // MATHEMATICAL SANS-SERIF BOLD SMALL GAMMA
    (Unicode:$1D773; Attr:daFont; Str:#$03B4),   // MATHEMATICAL SANS-SERIF BOLD SMALL DELTA
    (Unicode:$1D774; Attr:daFont; Str:#$03B5),   // MATHEMATICAL SANS-SERIF BOLD SMALL EPSILON
    (Unicode:$1D775; Attr:daFont; Str:#$03B6),   // MATHEMATICAL SANS-SERIF BOLD SMALL ZETA
    (Unicode:$1D776; Attr:daFont; Str:#$03B7),   // MATHEMATICAL SANS-SERIF BOLD SMALL ETA
    (Unicode:$1D777; Attr:daFont; Str:#$03B8),   // MATHEMATICAL SANS-SERIF BOLD SMALL THETA
    (Unicode:$1D778; Attr:daFont; Str:#$03B9),   // MATHEMATICAL SANS-SERIF BOLD SMALL IOTA
    (Unicode:$1D779; Attr:daFont; Str:#$03BA),   // MATHEMATICAL SANS-SERIF BOLD SMALL KAPPA
    (Unicode:$1D77A; Attr:daFont; Str:#$03BB),   // MATHEMATICAL SANS-SERIF BOLD SMALL LAMDA
    (Unicode:$1D77B; Attr:daFont; Str:#$03BC),   // MATHEMATICAL SANS-SERIF BOLD SMALL MU
    (Unicode:$1D77C; Attr:daFont; Str:#$03BD),   // MATHEMATICAL SANS-SERIF BOLD SMALL NU
    (Unicode:$1D77D; Attr:daFont; Str:#$03BE),   // MATHEMATICAL SANS-SERIF BOLD SMALL XI
    (Unicode:$1D77E; Attr:daFont; Str:#$03BF),   // MATHEMATICAL SANS-SERIF BOLD SMALL OMICRON
    (Unicode:$1D77F; Attr:daFont; Str:#$03C0),   // MATHEMATICAL SANS-SERIF BOLD SMALL PI
    (Unicode:$1D780; Attr:daFont; Str:#$03C1),   // MATHEMATICAL SANS-SERIF BOLD SMALL RHO
    (Unicode:$1D781; Attr:daFont; Str:#$03C2),   // MATHEMATICAL SANS-SERIF BOLD SMALL FINAL SIGMA
    (Unicode:$1D782; Attr:daFont; Str:#$03C3),   // MATHEMATICAL SANS-SERIF BOLD SMALL SIGMA
    (Unicode:$1D783; Attr:daFont; Str:#$03C4),   // MATHEMATICAL SANS-SERIF BOLD SMALL TAU
    (Unicode:$1D784; Attr:daFont; Str:#$03C5),   // MATHEMATICAL SANS-SERIF BOLD SMALL UPSILON
    (Unicode:$1D785; Attr:daFont; Str:#$03C6),   // MATHEMATICAL SANS-SERIF BOLD SMALL PHI
    (Unicode:$1D786; Attr:daFont; Str:#$03C7),   // MATHEMATICAL SANS-SERIF BOLD SMALL CHI
    (Unicode:$1D787; Attr:daFont; Str:#$03C8),   // MATHEMATICAL SANS-SERIF BOLD SMALL PSI
    (Unicode:$1D788; Attr:daFont; Str:#$03C9),   // MATHEMATICAL SANS-SERIF BOLD SMALL OMEGA
    (Unicode:$1D789; Attr:daFont; Str:#$2202),   // MATHEMATICAL SANS-SERIF BOLD PARTIAL DIFFERENTIAL
    (Unicode:$1D78A; Attr:daFont; Str:#$03F5),   // MATHEMATICAL SANS-SERIF BOLD EPSILON SYMBOL
    (Unicode:$1D78B; Attr:daFont; Str:#$03D1),   // MATHEMATICAL SANS-SERIF BOLD THETA SYMBOL
    (Unicode:$1D78C; Attr:daFont; Str:#$03F0),   // MATHEMATICAL SANS-SERIF BOLD KAPPA SYMBOL
    (Unicode:$1D78D; Attr:daFont; Str:#$03D5),   // MATHEMATICAL SANS-SERIF BOLD PHI SYMBOL
    (Unicode:$1D78E; Attr:daFont; Str:#$03F1),   // MATHEMATICAL SANS-SERIF BOLD RHO SYMBOL
    (Unicode:$1D78F; Attr:daFont; Str:#$03D6),   // MATHEMATICAL SANS-SERIF BOLD PI SYMBOL
    (Unicode:$1D790; Attr:daFont; Str:#$0391),   // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL ALPHA
    (Unicode:$1D791; Attr:daFont; Str:#$0392),   // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL BETA
    (Unicode:$1D792; Attr:daFont; Str:#$0393),   // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL GAMMA
    (Unicode:$1D793; Attr:daFont; Str:#$0394),   // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL DELTA
    (Unicode:$1D794; Attr:daFont; Str:#$0395),   // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL EPSILON
    (Unicode:$1D795; Attr:daFont; Str:#$0396),   // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL ZETA
    (Unicode:$1D796; Attr:daFont; Str:#$0397),   // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL ETA
    (Unicode:$1D797; Attr:daFont; Str:#$0398),   // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL THETA
    (Unicode:$1D798; Attr:daFont; Str:#$0399),   // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL IOTA
    (Unicode:$1D799; Attr:daFont; Str:#$039A),   // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL KAPPA
    (Unicode:$1D79A; Attr:daFont; Str:#$039B),   // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL LAMDA
    (Unicode:$1D79B; Attr:daFont; Str:#$039C),   // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL MU
    (Unicode:$1D79C; Attr:daFont; Str:#$039D),   // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL NU
    (Unicode:$1D79D; Attr:daFont; Str:#$039E),   // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL XI
    (Unicode:$1D79E; Attr:daFont; Str:#$039F),   // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL OMICRON
    (Unicode:$1D79F; Attr:daFont; Str:#$03A0),   // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL PI
    (Unicode:$1D7A0; Attr:daFont; Str:#$03A1),   // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL RHO
    (Unicode:$1D7A1; Attr:daFont; Str:#$03F4),   // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL THETA SYMBOL
    (Unicode:$1D7A2; Attr:daFont; Str:#$03A3),   // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL SIGMA
    (Unicode:$1D7A3; Attr:daFont; Str:#$03A4),   // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL TAU
    (Unicode:$1D7A4; Attr:daFont; Str:#$03A5),   // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL UPSILON
    (Unicode:$1D7A5; Attr:daFont; Str:#$03A6),   // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL PHI
    (Unicode:$1D7A6; Attr:daFont; Str:#$03A7),   // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL CHI
    (Unicode:$1D7A7; Attr:daFont; Str:#$03A8),   // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL PSI
    (Unicode:$1D7A8; Attr:daFont; Str:#$03A9),   // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL OMEGA
    (Unicode:$1D7A9; Attr:daFont; Str:#$2207),   // MATHEMATICAL SANS-SERIF BOLD ITALIC NABLA
    (Unicode:$1D7AA; Attr:daFont; Str:#$03B1),   // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL ALPHA
    (Unicode:$1D7AB; Attr:daFont; Str:#$03B2),   // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL BETA
    (Unicode:$1D7AC; Attr:daFont; Str:#$03B3),   // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL GAMMA
    (Unicode:$1D7AD; Attr:daFont; Str:#$03B4),   // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL DELTA
    (Unicode:$1D7AE; Attr:daFont; Str:#$03B5),   // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL EPSILON
    (Unicode:$1D7AF; Attr:daFont; Str:#$03B6),   // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL ZETA
    (Unicode:$1D7B0; Attr:daFont; Str:#$03B7),   // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL ETA
    (Unicode:$1D7B1; Attr:daFont; Str:#$03B8),   // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL THETA
    (Unicode:$1D7B2; Attr:daFont; Str:#$03B9),   // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL IOTA
    (Unicode:$1D7B3; Attr:daFont; Str:#$03BA),   // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL KAPPA
    (Unicode:$1D7B4; Attr:daFont; Str:#$03BB),   // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL LAMDA
    (Unicode:$1D7B5; Attr:daFont; Str:#$03BC),   // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL MU
    (Unicode:$1D7B6; Attr:daFont; Str:#$03BD),   // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL NU
    (Unicode:$1D7B7; Attr:daFont; Str:#$03BE),   // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL XI
    (Unicode:$1D7B8; Attr:daFont; Str:#$03BF),   // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL OMICRON
    (Unicode:$1D7B9; Attr:daFont; Str:#$03C0),   // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL PI
    (Unicode:$1D7BA; Attr:daFont; Str:#$03C1),   // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL RHO
    (Unicode:$1D7BB; Attr:daFont; Str:#$03C2),   // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL FINAL SIGMA
    (Unicode:$1D7BC; Attr:daFont; Str:#$03C3),   // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL SIGMA
    (Unicode:$1D7BD; Attr:daFont; Str:#$03C4),   // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL TAU
    (Unicode:$1D7BE; Attr:daFont; Str:#$03C5),   // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL UPSILON
    (Unicode:$1D7BF; Attr:daFont; Str:#$03C6),   // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL PHI
    (Unicode:$1D7C0; Attr:daFont; Str:#$03C7),   // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL CHI
    (Unicode:$1D7C1; Attr:daFont; Str:#$03C8),   // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL PSI
    (Unicode:$1D7C2; Attr:daFont; Str:#$03C9),   // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL OMEGA
    (Unicode:$1D7C3; Attr:daFont; Str:#$2202),   // MATHEMATICAL SANS-SERIF BOLD ITALIC PARTIAL DIFFERENTIAL
    (Unicode:$1D7C4; Attr:daFont; Str:#$03F5),   // MATHEMATICAL SANS-SERIF BOLD ITALIC EPSILON SYMBOL
    (Unicode:$1D7C5; Attr:daFont; Str:#$03D1),   // MATHEMATICAL SANS-SERIF BOLD ITALIC THETA SYMBOL
    (Unicode:$1D7C6; Attr:daFont; Str:#$03F0),   // MATHEMATICAL SANS-SERIF BOLD ITALIC KAPPA SYMBOL
    (Unicode:$1D7C7; Attr:daFont; Str:#$03D5),   // MATHEMATICAL SANS-SERIF BOLD ITALIC PHI SYMBOL
    (Unicode:$1D7C8; Attr:daFont; Str:#$03F1),   // MATHEMATICAL SANS-SERIF BOLD ITALIC RHO SYMBOL
    (Unicode:$1D7C9; Attr:daFont; Str:#$03D6),   // MATHEMATICAL SANS-SERIF BOLD ITALIC PI SYMBOL
    (Unicode:$1D7CE; Attr:daFont; Str:#$0030),   // MATHEMATICAL BOLD DIGIT ZERO
    (Unicode:$1D7CF; Attr:daFont; Str:#$0031),   // MATHEMATICAL BOLD DIGIT ONE
    (Unicode:$1D7D0; Attr:daFont; Str:#$0032),   // MATHEMATICAL BOLD DIGIT TWO
    (Unicode:$1D7D1; Attr:daFont; Str:#$0033),   // MATHEMATICAL BOLD DIGIT THREE
    (Unicode:$1D7D2; Attr:daFont; Str:#$0034),   // MATHEMATICAL BOLD DIGIT FOUR
    (Unicode:$1D7D3; Attr:daFont; Str:#$0035),   // MATHEMATICAL BOLD DIGIT FIVE
    (Unicode:$1D7D4; Attr:daFont; Str:#$0036),   // MATHEMATICAL BOLD DIGIT SIX
    (Unicode:$1D7D5; Attr:daFont; Str:#$0037),   // MATHEMATICAL BOLD DIGIT SEVEN
    (Unicode:$1D7D6; Attr:daFont; Str:#$0038),   // MATHEMATICAL BOLD DIGIT EIGHT
    (Unicode:$1D7D7; Attr:daFont; Str:#$0039),   // MATHEMATICAL BOLD DIGIT NINE
    (Unicode:$1D7D8; Attr:daFont; Str:#$0030),   // MATHEMATICAL DOUBLE-STRUCK DIGIT ZERO
    (Unicode:$1D7D9; Attr:daFont; Str:#$0031),   // MATHEMATICAL DOUBLE-STRUCK DIGIT ONE
    (Unicode:$1D7DA; Attr:daFont; Str:#$0032),   // MATHEMATICAL DOUBLE-STRUCK DIGIT TWO
    (Unicode:$1D7DB; Attr:daFont; Str:#$0033),   // MATHEMATICAL DOUBLE-STRUCK DIGIT THREE
    (Unicode:$1D7DC; Attr:daFont; Str:#$0034),   // MATHEMATICAL DOUBLE-STRUCK DIGIT FOUR
    (Unicode:$1D7DD; Attr:daFont; Str:#$0035),   // MATHEMATICAL DOUBLE-STRUCK DIGIT FIVE
    (Unicode:$1D7DE; Attr:daFont; Str:#$0036),   // MATHEMATICAL DOUBLE-STRUCK DIGIT SIX
    (Unicode:$1D7DF; Attr:daFont; Str:#$0037),   // MATHEMATICAL DOUBLE-STRUCK DIGIT SEVEN
    (Unicode:$1D7E0; Attr:daFont; Str:#$0038),   // MATHEMATICAL DOUBLE-STRUCK DIGIT EIGHT
    (Unicode:$1D7E1; Attr:daFont; Str:#$0039),   // MATHEMATICAL DOUBLE-STRUCK DIGIT NINE
    (Unicode:$1D7E2; Attr:daFont; Str:#$0030),   // MATHEMATICAL SANS-SERIF DIGIT ZERO
    (Unicode:$1D7E3; Attr:daFont; Str:#$0031),   // MATHEMATICAL SANS-SERIF DIGIT ONE
    (Unicode:$1D7E4; Attr:daFont; Str:#$0032),   // MATHEMATICAL SANS-SERIF DIGIT TWO
    (Unicode:$1D7E5; Attr:daFont; Str:#$0033),   // MATHEMATICAL SANS-SERIF DIGIT THREE
    (Unicode:$1D7E6; Attr:daFont; Str:#$0034),   // MATHEMATICAL SANS-SERIF DIGIT FOUR
    (Unicode:$1D7E7; Attr:daFont; Str:#$0035),   // MATHEMATICAL SANS-SERIF DIGIT FIVE
    (Unicode:$1D7E8; Attr:daFont; Str:#$0036),   // MATHEMATICAL SANS-SERIF DIGIT SIX
    (Unicode:$1D7E9; Attr:daFont; Str:#$0037),   // MATHEMATICAL SANS-SERIF DIGIT SEVEN
    (Unicode:$1D7EA; Attr:daFont; Str:#$0038),   // MATHEMATICAL SANS-SERIF DIGIT EIGHT
    (Unicode:$1D7EB; Attr:daFont; Str:#$0039),   // MATHEMATICAL SANS-SERIF DIGIT NINE
    (Unicode:$1D7EC; Attr:daFont; Str:#$0030),   // MATHEMATICAL SANS-SERIF BOLD DIGIT ZERO
    (Unicode:$1D7ED; Attr:daFont; Str:#$0031),   // MATHEMATICAL SANS-SERIF BOLD DIGIT ONE
    (Unicode:$1D7EE; Attr:daFont; Str:#$0032),   // MATHEMATICAL SANS-SERIF BOLD DIGIT TWO
    (Unicode:$1D7EF; Attr:daFont; Str:#$0033),   // MATHEMATICAL SANS-SERIF BOLD DIGIT THREE
    (Unicode:$1D7F0; Attr:daFont; Str:#$0034),   // MATHEMATICAL SANS-SERIF BOLD DIGIT FOUR
    (Unicode:$1D7F1; Attr:daFont; Str:#$0035),   // MATHEMATICAL SANS-SERIF BOLD DIGIT FIVE
    (Unicode:$1D7F2; Attr:daFont; Str:#$0036),   // MATHEMATICAL SANS-SERIF BOLD DIGIT SIX
    (Unicode:$1D7F3; Attr:daFont; Str:#$0037),   // MATHEMATICAL SANS-SERIF BOLD DIGIT SEVEN
    (Unicode:$1D7F4; Attr:daFont; Str:#$0038),   // MATHEMATICAL SANS-SERIF BOLD DIGIT EIGHT
    (Unicode:$1D7F5; Attr:daFont; Str:#$0039),   // MATHEMATICAL SANS-SERIF BOLD DIGIT NINE
    (Unicode:$1D7F6; Attr:daFont; Str:#$0030),   // MATHEMATICAL MONOSPACE DIGIT ZERO
    (Unicode:$1D7F7; Attr:daFont; Str:#$0031),   // MATHEMATICAL MONOSPACE DIGIT ONE
    (Unicode:$1D7F8; Attr:daFont; Str:#$0032),   // MATHEMATICAL MONOSPACE DIGIT TWO
    (Unicode:$1D7F9; Attr:daFont; Str:#$0033),   // MATHEMATICAL MONOSPACE DIGIT THREE
    (Unicode:$1D7FA; Attr:daFont; Str:#$0034),   // MATHEMATICAL MONOSPACE DIGIT FOUR
    (Unicode:$1D7FB; Attr:daFont; Str:#$0035),   // MATHEMATICAL MONOSPACE DIGIT FIVE
    (Unicode:$1D7FC; Attr:daFont; Str:#$0036),   // MATHEMATICAL MONOSPACE DIGIT SIX
    (Unicode:$1D7FD; Attr:daFont; Str:#$0037),   // MATHEMATICAL MONOSPACE DIGIT SEVEN
    (Unicode:$1D7FE; Attr:daFont; Str:#$0038),   // MATHEMATICAL MONOSPACE DIGIT EIGHT
    (Unicode:$1D7FF; Attr:daFont; Str:#$0039)    // MATHEMATICAL MONOSPACE DIGIT NINE
    );

function LocateHighUCS4DecompositionInfo(const Ch: UCS4Char): PUnicodeUCS4DecompositionInfo;
var L, H, I : Integer;
    D : UCS4Char;
begin
  if (Ch < $1D000) or (Ch > $1D7FF) then
    begin
      Result := nil;
      exit;
    end;
  // Binary search
  L := 0;
  H := UnicodeUCS4DecompositionEntries - 1;
  Repeat
    I := (L + H) div 2;
    D := UnicodeUCS4DecompositionInfo[I].Unicode;
    if D = Ch then
      begin
        Result := @UnicodeUCS4DecompositionInfo[I];
        exit;
      end else
    if D > Ch then
      H := I - 1 else
      L := I + 1;
  Until L > H;
  Result := nil;
end;

function GetCharacterDecomposition(const Ch: UCS4Char): WideString;
var I : PUnicodeUCS4DecompositionInfo;
begin
  if Ch < $10000 then
    Result := GetCharacterDecomposition(WideChar(Ch)) else
    begin
      I := LocateHighUCS4DecompositionInfo(Ch);
      if not Assigned(I) then
        Result := '' else
        Result := I^.Str;
    end;
end;



end.

