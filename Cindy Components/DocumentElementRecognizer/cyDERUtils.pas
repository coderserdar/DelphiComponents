unit cyDERUtils;

{ cyDERUtils
  Description: Document elements recognition utilities

  Definitions:
  Elements: value types recognized on documents like date, money, webiste, webmail, percentage, float, integer and etNumber
  - etDate: Date recognition in any formats ('00/00', '00-00-0000' etc ...)
  - etPeriodDate: New: Period date recognition
  - etMoney: money values recognition ('30.56€', '$30.56' etc ...)
  - etwebSite: webSite recognition
  - etWebMail: webmail recognition
  - etPercentage: percentage values recognition like VAT ('5%')
  - etFloat: float values recognition
  - etInteger: integer values recognition
  - etNumbers: Integer, Postal code, telefone, fax recognition
  - etExpressionKeyWord: text that was been defined has a key

  DERString: avoid trash cars from OCR's isolated word using StringToDERCharSet() function

  DERNString: charset for date, money, percentage, float and integer elements recognition

    $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    $  €€€ Accept any PAYPAL DONATION $$$  €
    $      to: mauricio_box@yahoo.com      €
    €€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€

    * ***** BEGIN LICENSE BLOCK *****
    *
    * Version: MPL 1.1
    *
    * The contents of this file are subject to the Mozilla Public License Version
    * 1.1 (the "License"); you may not use this file except in compliance with the
    * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
    *
    * Software distributed under the License is distributed on an "AS IS" basis,
    * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
    * the specific language governing rights and limitations under the License.
    *
    * The Initial Developer of the Original Code is Mauricio
    * (https://sourceforge.net/projects/tcycomponents/).
    *
    * Donations: see Donation section on Description.txt
    *
    * Alternatively, the contents of this file may be used under the terms of
    * either the GNU General Public License Version 2 or later (the "GPL"), or the
    * GNU Lesser General Public License Version 2.1 or later (the "LGPL"), in which
    * case the provisions of the GPL or the LGPL are applicable instead of those
    * above. If you wish to allow use of your version of this file only under the
    * terms of either the GPL or the LGPL, and not to allow others to use your
    * version of this file under the terms of the MPL, indicate your decision by
    * deleting the provisions above and replace them with the notice and other
    * provisions required by the LGPL or the GPL. If you do not delete the
    * provisions above, a recipient may use your version of this file under the
    * terms of any one of the MPL, the GPL or the LGPL.
    *
    * ***** END LICENSE BLOCK *****}

interface

uses Classes, Windows, Controls, SysUtils, cyDateUtils, cyStrUtils;

type
  DERString = String;
  DERChar = Char;
  TElementsType = (etText, etExpressionKeyWord, etNumbers, etInteger, etFloat, etPercentage, etwebSite, etWebMail, etMoney, etDate, etTextLine, etParagraph, etMonthYear, etID);
  // 2013-07-22 Added etTextLine, etParagraph only for elements recognition (expressions are divided into single words)
  // 2016-10-10 Added etMonthYear
  // 2017-02-03 Added etID for invoice number/client number/contract number recognition
  TElementsTypes = Set of TElementsType;

  // For smart numbers Recognition :
  DERNString = String;

var
  DERMoneyCars: String = '€';         // Add/redefine money chars
  DERDateSeparators: String = '/-_.'; // Add/redefine date separators
  DERExceptionCars: String = '';      // Add/redefine exceptions chars

  LocalFormatSettings: TFormatSettings;

const
  DERDecimalSeparator = '.';
  DERPeriodSeparators = '/-';  // Do not include '.' !!!

  // 2016-10-26 Added '\()' ...
  DERDefaultChars = '\()+º@/%-_.:0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';  // Charset used to recognize all elements

  DERNDefaultChars = '/%-.0123456789abcdefghjkmnopqrstuvwxyz'; // Charset used to recognize etInteger, etnumbers, etFloat, etPercentage, etMoney, etDate, etMonthYear elements


// *** General functions *** //
function SanytizeID(aStr: String): string;
function ValidateDate(const aDERStr: DERString; var RsltFormat: String): Boolean;
function ValidatePeriod(const aDERStr: String; var RsltFormat: String): Boolean;
function DERStrToDate(const aDERStr, aFormat: String): TDate;


// *** DER functions *** //
// Know if a char belongs to the DER charset
function IsDERChar(const aChar: Char): Boolean;

// Know if a char is a defined on DERDefaultChars variable
function IsDERDefaultChar(const aChar: Char): Boolean;

// Know if a char is a money char defined on DERMoneyCars variable
function IsDERMoneyChar(const aChar: Char): Boolean;

// Know if a char is a date separator char defined on DERDateSeparator variable
function IsDERDateSeparatorChar(const aChar: Char): Boolean;

// Know if a char is an exception char defined on DERExceptionCars variable
function IsDERExceptionCar(const aChar: Char): Boolean;

// Know if a DER char is not in 'a..z', 'A..Z', '0'..'9'
function IsDERSymbol(const aDERChar: DERChar): Boolean;

// Know if a DERString only contains symbols
function IsDERSymbols(const aDERString: String): Boolean;

// Convert a string to DERString :
function StringToDERCharSet(const aStr: String; const DERValueAsString: Boolean; const SpaceCarConversion: Char = #0): DERString;

// *** DERN functions *** //
function IsDERNDefaultChar(const aChar: Char): Boolean;

function IsDERNChar(const aChar: Char): Boolean;

// Convert a DERString into a DERNString that was made for numbers Recognition :
function DERToDERNCharset(const aDERStr: DERString; const KeepDERSymbols: boolean = false): DERNString;

// *** Other functions *** //
// Get any webSite present in a DERString :
function DERExtractwebSite(aDERStr: DERString; const SmartRecognition: Boolean): String;

function DERExtractID(aDERStr: DERString; const SmartRecognition: Boolean): String;

// Get any webmail present in a DERString :
function DERExtractWebMail(const aDERStr: DERString): String;

function DERExtractPhoneNr(aDERStr: DERString; const KeepParenthesis: Boolean): String;

// Recognize from string any integer/float/percentage/money/date value :
function DERExecute(const aDERStr: DERString; const SmartNumbersRecognition, SmartWebsiteRecognition: Boolean): TElementsType; overload;

function DERExecute(const aDERStr: DERString; var RsltNumbers, RsltInteger, RsltFloat, RsltPercentage, RsltwebSite, RsltWebMail, RsltMoney, RsltDate: String;
                     const SmartNumbersRecognition, SmartWebsiteRecognition: Boolean): TElementsType; overload;

function RetrieveElementValue(const aStr: String; const SmartNumbersRecognition, SmartWebsiteRecognition: Boolean; const aElementsType: TElementsType): String; overload;
procedure RetrieveElementValue(const aStr: String; const SmartNumbersRecognition, SmartWebsiteRecognition: Boolean; var RsltDERStr: DERString; var RsltElementType: TElementsType); overload;


implementation

// Sanytize invoice number, client number etc ...
// alpha numeric for exemple :   VH00301675

// 1 ) Any "O", "o" followed by numeric value must be transformed on 0 ...
// 2 ) Any "H", "l" or "I" between 2 numeric values must be converted to "11" or "1"
//     exemple: "2OlHIO25" will be "201111025"
function SanytizeID(aStr: String): string;
var
  aChar: String;
  c: Integer;
  AlphaAdded: Boolean;
begin
  Result := '';
  AlphaAdded := false;
  for c := Length(aStr) downto 1 do
  begin
    aChar := aStr[c];
    {$IFDEF UNICODE}
    if not CharInSet(aChar[1], ['0'..'9']) then
    {$ELSE}
    if not (aChar[1] in ['0'..'9']) then
    {$ENDIF}
    begin
      if not AlphaAdded then
      begin
        {$IFDEF UNICODE}
        if CharInSet(aChar[1], ['o', 'O']) then  // Letter O ...
        {$ELSE}
        if aChar[1] in ['o', 'O'] then  // Letter O ...
        {$ENDIF}
          aChar := '0'; // Zero

        if (aChar[1] = 'H') or (aChar[1] = 'l') or (aChar[1] = 'I') then  // H Majuscule ou l minuscule
        begin
          // Check if next car is numeric :
          if c > 1 then
            {$IFDEF UNICODE}
            if CharInSet(aStr[c - 1], ['o', 'O', 'H', 'l', 'I', '0'..'9']) then
            {$ELSE}
            if aStr[c - 1] in ['o', 'O', 'H', 'l', 'I', '0'..'9'] then
            {$ENDIF}
            begin
              if aChar = 'H' then aChar := '11';
              if aChar = 'l' then aChar := '1';
              if aChar = 'I' then aChar := '1';
            end;
        end;
      end;

      {$IFDEF UNICODE}
      if not CharInSet(aChar[1], ['0'..'9', '/', '-']) then
      {$ELSE}
      if not (aChar[1] in ['0'..'9', '/', '-']) then
      {$ENDIF}
        AlphaAdded := true;
    end;

    if aChar <> ' ' then
      Result := aChar + Result;
  end;
end;

// Valid Dates exemples: (0 is optional compared to 9):
// 09 + separator + 09 + separator + 0099
// 09 + separator + 09 + separator + 99
// 0099 + separator + 09 + separator + 09
function ValidateDate(const aDERStr: String; var RsltFormat: String): Boolean;
var
  SeparatorChar: String;
  DigitGroupIndex, YearGroupIndex, i: Integer;
  DigitsGroup: Array[1..3] of integer;

      function DetermineYearGroupIndex: Integer;
      begin
        Result := -1;

        if DigitsGroup[3] in [2, 4] then
          Result := 3;

        if DigitsGroup[2] = 4 then
          Result := -1;

        if DigitsGroup[1] = 4 then
          if DigitsGroup[3] = 4
          then Result := -1
          else Result := 1;
      end;

      function ReturnLetter(aLetter: Char; Times: Integer): String;
      var i: Integer;
      begin
        Result := '';
        for i:= 1 to Times do
          Result := Result + aLetter;
      end;

begin
  Result := false;
  RsltFormat := '';

  SeparatorChar := '';
  DigitGroupIndex := 1;
  DigitsGroup[1] := 0; DigitsGroup[2] := 0; DigitsGroup[3] := 0;

  for i := 1 to length(aDERStr) do
    {$IFDEF UNICODE}
    if not CharInSet(aDERStr[i], ['0'..'9']) then
    {$ELSE}
    if not (aDERStr[i] in ['0'..'9']) then
    {$ENDIF}
    begin
      if SeparatorChar = ''
      then
        SeparatorChar := aDERStr[i]
      else
        if SeparatorChar <> aDERStr[i] then
          Exit;

      if DigitGroupIndex = 3
      then Exit
      else Inc(DigitGroupIndex);
    end
    else
      Inc(DigitsGroup[DigitGroupIndex]);

  // Validate Date :
  if not ( (DigitsGroup[1] in [1, 2, 4]) and (DigitsGroup[2] in [1, 2, 4]) and (DigitsGroup[3] in [1, 2, 4]) ) then
    Exit;

  YearGroupIndex := DetermineYearGroupIndex;
  if YearGroupIndex = -1 then
    Exit;

  if YearGroupIndex = 3 then
  begin
    RsltFormat := ReturnLetter('d', DigitsGroup[1]) + SeparatorChar + ReturnLetter('m', DigitsGroup[2]) + SeparatorChar + ReturnLetter('y', DigitsGroup[3]);

    try
      Result := DERStrToDate(aDERStr, RsltFormat) <> 0;
    except
    end;
  end
  else begin
    RsltFormat := ReturnLetter('y', DigitsGroup[1]) + SeparatorChar + ReturnLetter('m', DigitsGroup[2]) + SeparatorChar + ReturnLetter('d', DigitsGroup[3]);

    try
      Result := DERStrToDate(aDERStr, RsltFormat) <> 0;
      // 2016-10-12 Why i did this ?  Result := true;
    except
    end;
  end;
end;

// Valid period exemples: (0 is optional compared to 9):
// 09 + separator + 0099
// 09 + separator + 99
// 0099 + separator + 09
function ValidatePeriod(const aDERStr: String; var RsltFormat: String): Boolean;
var
  SeparatorChar: String;
  DigitGroupIndex, YearGroupIndex, i: Integer;
  DigitsGroup: Array[1..3] of integer;

      function DetermineYearGroupIndex: Integer;
      begin
        Result := -1;

        if DigitsGroup[1] = 4 then
          Result := 1;

        if DigitsGroup[2] = 4 then
          Result := 2;
      end;

      function ReturnLetter(aLetter: Char; Times: Integer): String;
      var i: Integer;
      begin
        Result := '';
        for i:= 1 to Times do
          Result := Result + aLetter;
      end;

begin
  Result := false;
  RsltFormat := '';

  SeparatorChar := '';
  DigitGroupIndex := 1;
  DigitsGroup[1] := 0; DigitsGroup[2] := 0; DigitsGroup[3] := 0;

  for i := 1 to length(aDERStr) do
    {$IFDEF UNICODE}
    if not CharInSet(aDERStr[i], ['0'..'9']) then
    {$ELSE}
    if not (aDERStr[i] in ['0'..'9']) then
    {$ENDIF}
    begin
      if SeparatorChar = '' then
        SeparatorChar := aDERStr[i]
      else
        if SeparatorChar <> aDERStr[i] then
          Exit;

      if DigitGroupIndex = 3
      then Exit
      else Inc(DigitGroupIndex);
    end
    else
      Inc(DigitsGroup[DigitGroupIndex]);

  // Validate period :
  if DigitsGroup[3] <> 0 then Exit;
  if pos(SeparatorChar, DERPeriodSeparators) = 0 then Exit;

  if not ( (DigitsGroup[1] in [2, 4]) and (DigitsGroup[2] in [4, 2]) ) then
    Exit;

  YearGroupIndex := DetermineYearGroupIndex;
  if YearGroupIndex = -1 then
    Exit;

  if YearGroupIndex = 2 then
  begin
    RsltFormat := ReturnLetter('m', DigitsGroup[1]) + SeparatorChar + ReturnLetter('y', DigitsGroup[2]);

    try
      Result := DERStrToDate(aDERStr, RsltFormat) <> 0;
    except
    end;
  end
  else begin
    RsltFormat := ReturnLetter('y', DigitsGroup[1]) + SeparatorChar + ReturnLetter('m', DigitsGroup[2]);

    try
      Result := DERStrToDate(aDERStr, RsltFormat) <> 0;
    except
    end;
  end;
end;

function DERStrToDate(const aDERStr, aFormat: String): TDate;
var TmpDateTime: TDateTime;

      function GetNumbers(Pattern: Char): String;
      var i: Integer;
      begin
        Result := '';
        for i := 1 to length(aDERStr) do
          if aFormat[i] = Pattern then
            Result := Result + aDERStr[i];
      end;

var sYear, sMonth, sDay: String;
begin
  Result := 0;

  sYear  := GetNumbers('y');
  sMonth := GetNumbers('m');
  sDay   := GetNumbers('d');

  while length(sYear) < 3 do
    sYear := '0' + sYear;

  if length(sYear) = 3 then
    sYear := '2' + sYear;

  while length(sMonth) < 2 do
    sMonth := '0' + sMonth;

  // 2016-10-12 :
  if sDay = '' then
    sDay := '01'
  else
    while length(sDay) < 2 do
      sDay := '0' + sDay;

  if TryToEncodeDate(StrToInt(sYear), StrToInt(sMonth), StrToInt(sDay), TmpDateTime) then
    Result := Trunc(TmpDateTime);
end;

function IsDERSymbol(const aDERChar: DERChar): Boolean;
begin
  {$IFDEF UNICODE}
  Result := not CharInSet(aDERChar, ['0'..'9', 'a'..'z', 'A'..'Z']);
  {$ELSE}
  Result := not (aDERChar in ['0'..'9', 'a'..'z', 'A'..'Z']);
  {$ENDIF}
end;

function IsDERSymbols(const aDERString: String): Boolean;
var i: Integer;
begin
  Result := true;

  for i := 1 to length(aDERString) do
    if not (IsDERSymbol(aDERString[i])) then // 2016-10-26 ... aDERString[i] in ['0'..'9', 'a'..'z', 'A'..'Z'] then
    begin
      Result := false;
      Break;
    end;
end;

function IsDERChar(const aChar: Char): Boolean;
begin
  Result := IsDERDefaultChar(aChar);

  if not Result then
    Result := IsDERMoneyChar(aChar);

  if not Result then
    Result := IsDERExceptionCar(aChar);
end;

function IsDERDefaultChar(const aChar: Char): Boolean;
var i: Integer;
begin
  Result := false;

  for i := 1 to length(DERDefaultChars) do
    if DERDefaultChars[i] = aChar then
    begin
      Result := true;
      Exit;
    end;
end;

function IsDERMoneyChar(const aChar: Char): Boolean;
var i: Integer;
begin
  Result := false;

  for i := 1 to length(DERMoneyCars) do
    if DERMoneyCars[i] = aChar then
    begin
      Result := true;
      Exit;
    end;
end;

function IsDERDateSeparatorChar(const aChar: Char): Boolean;
var i: Integer;
begin
  Result := false;

  for i := 1 to length(DERDateSeparators) do
    if DERDateSeparators[i] = aChar then
    begin
      Result := true;
      Exit;
    end;
end;

function IsDERExceptionCar(const aChar: Char): Boolean;
var i: Integer;
begin
  Result := false;
  for i := 1 to length(DERExceptionCars) do
    if DERExceptionCars[i] = aChar then
    begin
      Result := true;
      Exit;
    end;
end;

function StringToDERCharSet(const aStr: String; const DERValueAsString: Boolean; const SpaceCarConversion: Char = #0): DERString;
var
  lengthResult, i: Integer;
begin
  Result := aStr;
  lengthResult := Length(Result);

  for i := 1 to lengthResult do
  begin
    if not DERValueAsString then
    begin
      if Result[i] = ';' then Result[i] := ':';  // ';' will be converted to '.' for decimal recognition

      // Replace unexpected symbols:
      if Result[i] = '|' then Result[i] := 'l';  // Will be replaced by "1" on DERToDERNCharset() ...
      if Result[i] = '×' then Result[i] := 'x';  // × = char(215)
      if Result[i] = '·' then Result[i] := '.';
    end;

    // Decimal/mail/webSite recognition :
    if Result[i] = ',' then Result[i] := '.';

    // Other :
    if Result[i] = '°' then Result[i] := 'º';    // Not same character!!!    Ord('°') = 176     Ord('º') = 186

    // Remove accents :
    if Result[i] = 'é' then Result[i] := 'e';
    if Result[i] = 'è' then Result[i] := 'e';
    if Result[i] = 'ê' then Result[i] := 'e';
    if Result[i] = 'ë' then Result[i] := 'e';

    if Result[i] = 'à' then Result[i] := 'a';
    if Result[i] = 'á' then Result[i] := 'a';
    if Result[i] = 'â' then Result[i] := 'a';
    if Result[i] = 'ã' then Result[i] := 'a';
    if Result[i] = 'ä' then Result[i] := 'a';

    if Result[i] = 'í' then Result[i] := 'i';
    if Result[i] = 'ì' then Result[i] := 'i';
    if Result[i] = 'î' then Result[i] := 'i';
    if Result[i] = 'ï' then Result[i] := 'i';

    if Result[i] = 'ò' then Result[i] := 'o';
    if Result[i] = 'ó' then Result[i] := 'o';
    if Result[i] = 'ô' then Result[i] := 'o';
    if Result[i] = 'õ' then Result[i] := 'o';
    if Result[i] = 'ö' then Result[i] := 'o';

    if Result[i] = 'ú' then Result[i] := 'u';
    if Result[i] = 'ù' then Result[i] := 'u';
    if Result[i] = 'û' then Result[i] := 'u';
    if Result[i] = 'ü' then Result[i] := 'u';

    if Result[i] = 'É' then Result[i] := 'E';
    if Result[i] = 'È' then Result[i] := 'E';
    if Result[i] = 'Ê' then Result[i] := 'E';
    if Result[i] = 'Ë' then Result[i] := 'E';

    if Result[i] = 'À' then Result[i] := 'A';
    if Result[i] = 'Á' then Result[i] := 'A';
    if Result[i] = 'Â' then Result[i] := 'A';
    if Result[i] = 'Ã' then Result[i] := 'A';
    if Result[i] = 'Ä' then Result[i] := 'A';

    if Result[i] = 'Í' then Result[i] := 'I';
    if Result[i] = 'Ì' then Result[i] := 'I';
    if Result[i] = 'Î' then Result[i] := 'I';
    if Result[i] = 'Ï' then Result[i] := 'I';

    if Result[i] = 'Ò' then Result[i] := 'O';
    if Result[i] = 'Ó' then Result[i] := 'O';
    if Result[i] = 'Ô' then Result[i] := 'O';
    if Result[i] = 'Õ' then Result[i] := 'O';
    if Result[i] = 'Ö' then Result[i] := 'O';

    if Result[i] = 'Ú' then Result[i] := 'U';
    if Result[i] = 'Ù' then Result[i] := 'U';
    if Result[i] = 'Û' then Result[i] := 'U';
    if Result[i] = 'Ü' then Result[i] := 'U';
  end;

  if SpaceCarConversion <> #0 then
    String_SubstFast(' ', SpaceCarConversion, Result);

  // Remove non DER chars :
  if not DERValueAsString then
    for i := lengthResult downto 1 do
      if not IsDERChar(Result[i]) and (Result[i] <> SpaceCarConversion) then
        Delete(Result, i, 1);
end;

function IsDERNDefaultChar(const aChar: Char): Boolean;
var i: Integer;
begin
  Result := false;

  for i := 1 to length(DERNDefaultChars) do
    if DERNDefaultChars[i] = aChar then
    begin
      Result := true;
      Exit;
    end;
end;

function IsDERNChar(const aChar: Char): Boolean;
begin
  Result := IsDERNDefaultChar(aChar);

  if not Result then
    Result := IsDERMoneyChar(aChar);
end;

function DERToDERNCharset(const aDERStr: DERString; const KeepDERSymbols: boolean = false): DERNString;
var
  lengthResult, i: Integer;
begin
  Result := aDERStr;
  lengthResult := length(Result);

  for i := 1 to lengthResult do
  begin
    // Numbers recognition :
    if Result[i] = ':' then Result[i] := '.';
    if Result[i] = 'O' then Result[i] := '0';
    if Result[i] = 'o' then Result[i] := '0';
    if Result[i] = 'I' then Result[i] := '1';
    if Result[i] = 'l' then Result[i] := '1';
    if Result[i] = 'S' then Result[i] := '5';

    // Negative / Date separator recognition :
    if Result[i] = '_' then Result[i] := '-';
  end;

  Result := AnsiLowerCase(Result);

  // Remove non DERN chars :
  for i := lengthResult downto 1 do
    if not IsDERNChar(Result[i]) then
      if KeepDERSymbols then
      begin
        if not IsDERSymbol(Result[i]) then
          Delete(Result, i, 1);
      end
      else
        Delete(Result, i, 1);

end;

function DERExtractwebSite(aDERStr: DERString; const SmartRecognition: Boolean): String;
var
  p, i, vCount: Integer;
  LowerCaseDERStr: String;
begin
  Result := '';
  if pos('@', aDERStr) <> 0 then Exit;


  // OCR text as "VWVW", "WVVW" and variants solution:
  if SmartRecognition then
  begin
    vCount := 0;
    p := pos('.', aDERStr);

    for i := p-1 downto 1 do
      {$IFDEF UNICODE}
      if CharInSet(aDERStr[i], ['v', 'V']) then
      {$ELSE}
      if aDERStr[i] in ['v', 'V'] then
      {$ENDIF}
        vCount := vCount + 1
      else
        {$IFDEF UNICODE}
        if CharInSet(aDERStr[i], ['w', 'W'])
        {$ELSE}
        if aDERStr[i] in ['w', 'W']
        {$ENDIF}
        then vCount := vCount + 2
        else Break;

    if vCount = 6 then  // We have 6 "v" that is equal to "W W W"
    begin
      Delete(aDERStr, i+1, p-i-1);
      Insert('www', aDERStr, i+1);
    end;
  end;

  LowerCaseDERStr := AnsiLowerCase(aDERStr);
  p := pos('http:', LowerCaseDERStr);
  if p = 0 then
    p := pos('www.', LowerCaseDERStr);

  if p <> 0 then
  begin
    // Extract from p :
    for i := p to length(aDERStr) do
      if isValidWebSiteChar(aDERStr[i])
      then Result := Result + aDERStr[i]
      else Break; // End of website string ...

    // Validate :
    if not isValidwebSite(Result) then
      Result := '';
  end;

  while (pos('.', aDERStr) > 0) and (Result = '') do
  begin
    // We Will try to extract website from here :
    p := pos('.', aDERStr);

    if (p > 1) and (p < length(aDERStr)) then
    begin
      for i := p downto 1 do
        if isValidWebSiteChar(aDERStr[i])
        then Result := aDERStr[i] + Result
        else Break;

      for i := p+1 to length(aDERStr) do
        if isValidWebSiteChar(aDERStr[i])
        then Result := Result + aDERStr[i]
        else Break;

      // Remove '.' char from the beginning or end :
      if Result <> '' then
        while Result[1] = '.' do
        begin
          Delete(Result, 1, 1);
          if Result = '' then Break;
        end;

      if Result <> '' then
        while Result[length(Result)] = '.' do
        begin
          Delete(Result, length(Result), 1);
          if Result = '' then Break;
        end;

      if not isValidWebsite(Result) then
        Result := '';
    end;

    if not isValidwebSite(Result) then
      Result := '';

    // Try searching forward :
    if Result = '' then
      Delete(aDERStr, 1, p);
  end;
end;

// Extract invoice number, client number etc ...
// Rules for ID recognition:
// Alpha cars allowed but must be in uppercase with on single error allowed
// At least find a sequence of 2 numeric values   OR   at least 2 uppercase letters !
function DERExtractID(aDERStr: DERString; const SmartRecognition: Boolean): String;
var
  i, CountNum, CountLowercase, CountUppercase: Integer;
  PriorIsNum, NumSequenceFound: Boolean;
begin
  // Sanytize :
  if SmartRecognition
  then Result := SanytizeID(aDERStr)
  else Result := aDERStr;

  // Statistics for validation :
  PriorIsNum := false;
  NumSequenceFound := false;
  CountNum := 0;
  CountLowercase := 0;
  CountUppercase := 0;

  for i := 1 to Length(Result) do
  begin
    case Result[i] of
      '0'..'9':
      begin
        inc(CountNum);

        if PriorIsNum then
          NumSequenceFound := true;

        PriorIsNum := true;
      end;

      'a'..'z':
      begin
        inc(CountLowercase);
        PriorIsNum := false;
      end;

      'A'..'Z':
      begin
        inc(CountUppercase);
        PriorIsNum := false;
      end;

    end;
  end;

  // Validate from stats :
  if NumSequenceFound then
  begin
    // We consider that is ok ...
  end
  else begin
    if CountUppercase < 2 then
      Result := ''
    else
      if CountLowercase > 1 then
        Result := '';
  end;
end;

function DERExtractWebMail(const aDERStr: DERString): String;
var
  p, i: Integer;
begin
  Result := '';

  // The better strategy is to detect '@' char :
  p := pos('@', aDERStr);

  if (p > 1) and (p < length(aDERStr)) then
  begin
    // Get '@' + prefix :
    for i := p downto 1 do
      if isValidWebMailChar(aDERStr[i])
      then Result := aDERStr[i] + Result
      else Break;

    // Get sufix :
    for i := p+1 to length(aDERStr) do
      if isValidWebMailChar(aDERStr[i])
      then Result := Result + aDERStr[i]
      else Break;

    // Remove '.' char from the beginning or end :
    if Result <> '' then
      while (Result)[1] = '.' do
      begin
        Delete(Result, 1, 1);
        if Result = '' then Break;
      end;

    if Result <> '' then
      while Result[length(Result)] = '.' do
      begin
        Delete(Result, length(Result), 1);
        if Result = '' then Break;
      end;

    if not isValidWebMail(Result) then
      Result := '';
  end;
end;

// Retrieve numbers and '+' cars :
function DERExtractPhoneNr(aDERStr: DERString; const KeepParenthesis: Boolean): String;
var
  i: Integer;
begin
  Result := '';

  if pos('+', aDERStr) <> 0 then
  begin
    aDERStr := copy(aDERStr, pos('+', aDERStr)+1, length(aDERStr));
    Result := '+';
  end;

  for i := 1 to Length(aDERStr) do
    {$IFDEF UNICODE}
    if CharInSet(aDERStr[i], ['(', ')']) then
    {$ELSE}
    if aDERStr[i] in ['(', ')'] then
    {$ENDIF}
    begin
      if KeepParenthesis then
        Result := Result + aDERStr[i];
    end
    else
      {$IFDEF UNICODE}
      if CharInSet((DERToDERNCharset(aDERStr[i]) + ' ')[1], ['0'..'9']) then
      {$ELSE}
      if (DERToDERNCharset(aDERStr[i]) + ' ')[1] in ['0'..'9'] then
      {$ENDIF}
        Result := Result + DERToDERNCharset(aDERStr[i]);
end;

function DERExecute(const aDERStr: DERString; const SmartNumbersRecognition, SmartWebsiteRecognition: Boolean): TElementsType;
var RsltNumbers, RsltInteger, RsltFloat, RsltPercentage, RsltwebSite, RsltWebMail, RsltMoney, RsltDate: String;
begin
  Result := DERExecute(aDERStr, RsltNumbers, RsltInteger, RsltFloat, RsltPercentage, RsltwebSite, RsltWebMail,
                                RsltMoney, RsltDate, SmartNumbersRecognition, SmartWebsiteRecognition);
end;

// Recognize from DERString any numbers/integer/float/money/percentage/date/period/website and webmail:
// 23.526,59 float/money handling ok
// 23 526,59 float/money handling ok
// 25-02-589 returns as numbers (2502589) and not as Integer (-589)
// NEW 2016-10-12   '0031' must be recognized as numbers and not Integer !
function DERExecute(const aDERStr: DERString; var RsltNumbers, RsltInteger, RsltFloat, RsltPercentage, RsltwebSite, RsltWebMail, RsltMoney, RsltDate: String;
                     const SmartNumbersRecognition, SmartWebsiteRecognition: Boolean): TElementsType;

                  // Clear bounds from non numbers cars:
                  function _DERExtractDate(aDate: String): String;    // Exemple:  '.15.01.1977.'
                  var i: Integer;
                  begin
                    Result := aDate;

                    {$IFDEF UNICODE}
                    while not CharInSet((Result + '0')[1], ['0'..'9']) do
                    {$ELSE}
                    while not ((Result + '0')[1] in ['0'..'9']) do
                    {$ENDIF}
                      Delete(Result, 1, 1);

                    for i := length(Result) downto 1 do
                      {$IFDEF UNICODE}
                      if CharInSet(Result[i], ['0'..'9'])
                      {$ELSE}
                      if Result[i] in ['0'..'9']
                      {$ENDIF}
                      then Break
                      else Delete(Result, i, 1);
                  end;

                  (*function IsNumbers(aDERNStr: DERNString): Boolean;
                  var i: Integer;
                  begin
                    Result := true;

                    for i := 1 to length(aDERNStr) do
                      if aDERNStr[i] in ['a'..'z'] then
                      begin
                        Result := false;
                        Break;
                      end;
                  end;        *)

var
  aDERNStr: DERNString;
  _DateSeparator, DateFormat: String;
  isPeriod: Boolean;
  i, p, AlphaCount: Integer;
  HandledCar, ReadInteger, ReadFloat, ReadPercentage, ReadDate: Boolean;
  FloatValue: Extended;
begin
  if SmartNumbersRecognition
  then aDERNStr := DERToDERNCharset(aDERStr)
  else aDERNStr := AnsiLowerCase(aDERStr);     // 2014-10-30  Added AnsiLowerCase() !

  AlphaCount := 0;

  for i := 1 to length(aDERNStr) do
  begin
    {$IFDEF UNICODE}
    if CharInSet(aDERNStr[i], ['a'..'z', 'A'..'Z']) then
    {$ELSE}
    if aDERNStr[i] in ['a'..'z', 'A'..'Z'] then
    {$ENDIF}
      inc(AlphaCount);
  end;

  _DateSeparator := '';

  RsltWebMail    := DERExtractWebMail(aDERStr);
  RsltwebSite    := DERExtractwebSite(aDERStr, SmartWebsiteRecognition);
  RsltNumbers    := '';
  RsltInteger    := '';
  RsltFloat      := '';
  RsltMoney      := '';
  RsltPercentage := '';
  RsltDate       := '';

  ReadInteger := true;
  ReadFloat := true;
  ReadPercentage := true;
  ReadDate := true;

  // Read string from the end in order to detect decimal separator :
  for i := length(aDERNStr) downto 1 do
  begin
    HandledCar := false;

    if IsDERMoneyChar(aDERNStr[i]) then
    begin
      // The value can be before or after money char ...
      HandledCar := True;
      RsltMoney := aDERNStr[i];
    end
    else
      if aDERNStr[i] = '%' then
      begin
        HandledCar := True;

        if ReadPercentage then
        begin
          RsltPercentage := aDERNStr[i];
          ReadPercentage := false;

          // Detecting percentage values is more important than integer or float values, so we reset actual Integer/float encountered :
          ReadInteger := true;
          RsltInteger := '';

          ReadFloat := true;
          RsltFloat := '';
        end;
      end
      else
        if (aDERNStr[i] = '-') or (aDERNStr[i] = '_') then
        begin
          HandledCar := True;

          if ReadInteger and (RsltInteger <> '') then
          begin
            RsltInteger := '-' + RsltInteger;
            ReadInteger := false;
          end;

          if ReadFloat and (RsltFloat <> '') then
          begin
            RsltFloat := aDERNStr[i] + RsltFloat;
            ReadFloat := false;
          end;

          if ReadDate and (RsltDate <> '') then
            if (_DateSeparator = '') or (_DateSeparator = '-') then
            begin
              _DateSeparator := '-';
              RsltDate := '-' + RsltDate;
            end;
        end
        else
          if (aDERNStr[i] = '.') or (aDERNStr[i] = ':') then  // ':' can be returned by OCR engine because of paper dirt
          begin
            HandledCar := True;

            if ReadInteger and (RsltInteger <> '') then
              ReadInteger := false;

            if ReadFloat and (RsltFloat <> '') then
            begin
              p := Pos('.', RsltFloat);

              if p <> 0 then
              begin
                // 2017-01-04 Validate thousand separator :
                if (p-1) mod 3 <> 0 then // not 3 digits groups since first decimal separator...
                  ReadFloat := false;          //  RsltFloat=90.90 / p=3      i=9     aDERNStr = '05.22.34.90.90'
              end
              else
                RsltFloat := '.' + RsltFloat;
            end;

              if ReadDate and (RsltDate <> '') then
                if (_DateSeparator = '') or (_DateSeparator = '.') then
                begin
                  _DateSeparator := '.';
                  RsltDate := '.' + RsltDate;
                end;
          end
          else
            if aDERNStr[i] = '/' then
            begin
              HandledCar := True;

              if ReadDate and (RsltDate <> '') then
                if (_DateSeparator = '') or (_DateSeparator = '/') then
                begin
                  _DateSeparator := '/';
                  RsltDate := '/' + RsltDate;
                end;
            end
            else
              {$IFDEF UNICODE}
              if CharInSet(aDERNStr[i], ['0'..'9']) then
              {$ELSE}
              if aDERNStr[i] in ['0'..'9'] then
              {$ENDIF}
              begin
                HandledCar := True;

                RsltNumbers := aDERNStr[i] + RsltNumbers;

                if ReadDate then
                  RsltDate := aDERNStr[i] + RsltDate;

                if ReadInteger then
                  RsltInteger := aDERNStr[i] + RsltInteger;

                if ReadFloat then
                  RsltFloat := aDERNStr[i] + RsltFloat;
              end;

    if not HandledCar then
    begin
      if (RsltMoney <> '') and (RsltFloat = '') then // Found Money symbol before but no number before/after this symbol !
        RsltMoney := '';   // Remove symbol

      if (RsltPercentage <> '') and (RsltFloat = '') then // Found Percentage symbol before but no number after this symbol !
      begin
        RsltPercentage := '';   // Remove symbol
        ReadPercentage := true;
      end;

      if ReadInteger and (RsltInteger <> '') then
        ReadInteger := false;

      if ReadFloat and (RsltFloat <> '') then
        ReadFloat := false;

      if ReadDate and (_DateSeparator <> '') then
      begin
        if (aDERNStr[i] = 'h') and SmartNumbersRecognition then     // 3O/H/2OH -> 30/11/2011
        begin
          RsltDate := '11' + RsltDate;
        end
        else
          ReadDate := false;
      end
      else
        if (aDERNStr[i] = 'h') and SmartNumbersRecognition then    // 3O/H/2OH -> 30/11/2011
          RsltDate := '11' + RsltDate;
    end;
  end;

  // Validate date / period :
  if RsltDate <> '' then
    if Length(RsltDate) > 6 then // 2016-10-12 Period is 00/0000 ...
    begin
      // Remove extra chars from RsltDate:
      RsltDate := _DERExtractDate(RsltDate);

      if ValidateDate(RsltDate, DateFormat) then
      begin
        RsltDate := intToStr( Trunc(DERStrToDate(RsltDate, DateFormat)) );
        isPeriod := false;
      end
      else
        if ValidatePeriod(RsltDate, DateFormat) then
        begin
          RsltDate := intToStr( Trunc(DERStrToDate(RsltDate, DateFormat)) );
          isPeriod := true;
        end
        else
          RsltDate := '';
    end
    else
      RsltDate := '';

  // Validate float:
  if RsltFloat <> '' then
    begin
      // Apply system decimal separator :
      if LocalFormatSettings.DecimalSeparator <> DERDecimalSeparator then
        String_SubstFast(DERDecimalSeparator, LocalFormatSettings.DecimalSeparator, RsltFloat);

      if RsltFloat[1] = LocalFormatSettings.DecimalSeparator then  // 2016-03-31 Avoid '.0' error conversion ...
        RsltFloat := ''
      else
        if not TryStrToFloat(RsltFloat, FloatValue) then
          RsltFloat := '';
    end
    else
      RsltFloat := '';

  // Validate money:
  if RsltMoney <> '' then   // RsltMoney contains money symbol
    if RsltFloat = ''
    then RsltMoney := ''
    else RsltMoney := RsltFloat + RsltMoney;

  // Validate percentage:
  if RsltPercentage <> '' then   // RsltPercentage contains '%'
    if RsltFloat = ''
    then RsltPercentage := ''
    else RsltPercentage := RsltFloat + RsltPercentage;


  Result := etText;  // By default




  if RsltDate <> '' then
  begin
    if isPeriod
    then Result := etMonthYear
    else Result := etDate;
  end
  else
    if RsltMoney <> '' then
      Result := etMoney
    else
      if (RsltWebsite <> '') and (length(RsltWebsite) > length(RsltFloat)) then // "614.00" recognized as website ...
        Result := etWebsite
      else
        if RsltWebMail <> '' then
          Result := etWebMail
        else
          if RsltPercentage <> '' then
            Result := etPercentage
          else
            if AlphaCount = 0 then // 2016-10-26 ... IsNumbers(aDERNStr) then      recognize if expression is really a float/integer or numbers
              if (RsltFloat <> '') and (RsltFloat <> RsltInteger) and (length(RsltFloat) >= length(RsltNumbers)) then
                Result := etFloat
              else
                // 2016-10-26 .... aDERStr may have symbols chars like "()/" or "-" in the middle (separator on invoice nº for exemple) ...
                // if (RsltInteger <> '') and (length(RsltInteger) >= length(RsltNumbers)) then
                if (RsltInteger <> '') and (length(RsltInteger) = length(aDERStr)) then
                  Result := etInteger
                else
                  if RsltNumbers <> '' then
                    Result := etNumbers;
end;

function RetrieveElementValue(const aStr: String; const SmartNumbersRecognition, SmartWebsiteRecognition: Boolean; const aElementsType: TElementsType): String;
var
  DERStr: DERString;
  RsltNumbers, RsltInteger, RsltFloat, RsltPercentage, RsltwebSite, RsltWebMail, RsltMoney, RsltDate: String;

const
  cDERValueAsString = false; // 2017-02-06 Parameter not needed for value type recognition (not keeping DERValue)
begin
  Result := '';
  DERStr := StringToDERCharSet(aStr, cDERValueAsString);
  DERExecute(DERStr, RsltNumbers, RsltInteger, RsltFloat, RsltPercentage, RsltwebSite, RsltWebMail,
               RsltMoney, RsltDate, SmartNumbersRecognition, SmartWebsiteRecognition);

  case aElementsType of
    //etText
    //etExpressionKeyWord,
    // etID
    etNumbers:    Result := RsltNumbers;
    etInteger:    Result := RsltInteger;
    etFloat:      Result := RsltFloat;
    etPercentage: Result := RsltPercentage;
    etwebSite:    Result := RsltWebsite;
    etWebMail:    Result := RsltWebMail;
    etMoney:      Result := RsltMoney;
    etDate:       Result := RsltDate;
    etMonthYear:  Result := RsltDate;     // DERExecute does not recognize writtn months for now ...
    else
                  Result := aStr;
  end;
end;

procedure RetrieveElementValue(const aStr: String; const SmartNumbersRecognition, SmartWebsiteRecognition: Boolean; var RsltDERStr: DERString; var RsltElementType: TElementsType);
var
  RsltNumbers, RsltInteger, RsltFloat, RsltPercentage, RsltwebSite, RsltWebMail, RsltMoney, RsltDate: String;

const
  cDERValueAsString = false; // 2017-02-06 Parameter not needed for value type recognition (not keeping DERValue)
begin
  RsltDERStr := '';
  RsltDERStr := StringToDERCharSet(aStr, cDERValueAsString);
  RsltElementType := DERExecute(RsltDERStr, RsltNumbers, RsltInteger, RsltFloat, RsltPercentage, RsltwebSite, RsltWebMail,
                              RsltMoney, RsltDate, SmartNumbersRecognition, SmartWebsiteRecognition);

  case RsltElementType of
    //etText
    //etExpressionKeyWord,
    // etID
    etNumbers:    RsltDERStr := RsltNumbers;
    etInteger:    RsltDERStr := RsltInteger;
    etFloat:      RsltDERStr := RsltFloat;
    etPercentage: RsltDERStr := RsltPercentage;
    etwebSite:    RsltDERStr := RsltWebsite;
    etWebMail:    RsltDERStr := RsltWebMail;
    etMoney:      RsltDERStr := RsltMoney;
    etDate:       RsltDERStr := RsltDate;
    etMonthYear:  RsltDERStr := RsltDate;     // DERExecute does not recognize writtn months for now ...
    else
                  RsltDERStr := aStr;
  end;
end;

initialization

GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, LocalFormatSettings);

end.
