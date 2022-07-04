unit _CHFunction;

{ ##############################################################################
  _CHFunction
  Version 1.0.1
  Autor : Christian Hämmerle
  eMail : chaemmerle@uni.de
  Internet : http://www.Blue-Xplosion.de (German/English)

  Lizenz : Freeware

  Hinweis: Nicht alle hier aufgeführten Funktionen wurden komplett von mir geschieben.
  Der Sourcecode einiger Funktionen und Prozeduren stammt aus Foren bzw. Tips&Tricks
  Seiten und wurde von mir lediglich angepasst.

  History:
  1.0.0:  - First Release:
  1.1.0:  - ADD: LoadJPEGFromRes, ChangeResolution, GetScreenResolution, GetScreenBPP
  1.2.0:  - ADD: BmpToIco
  1.3.0:  - ADD: PixelToCm, CmToPixel
  1.4.0:	-	add. ExtractFileExt

  ############################################################################ }

{$I CH.INC}

interface

uses
  Forms, Windows, Messages, SysUtils, Classes, Controls, Graphics,
  ShellApi, Math, JPEG, Printers, Dialogs, {$IFDEF DEF_Delphi5} FileCtrl, {$ENDIF}

  _CHTypes;

type
  TPercent = 1..100;
  TCrypt = 1..120;


{ String }
function IsUnderline(const Str : string) : Boolean;
function IsUnderlineChar(const Str : string; CC : Integer) : Boolean;
function ExpandStringRight(var Str : string; ExpChar : char; EndLength : word) : Boolean;
function ExpandStringLeft(var Str : string; ExpChar : char; EndLength : word) : Boolean;
function GetStringPartFromLeft(var Str : string; Delimiter : char;
  DeleteResultPart, DeleteOnlyDelimiter : Boolean ) : string;
function GetStringPartFromRight(var Str : string; Delimiter : char;
  DeleteResultPart, DeleteOnlyDelimiter : Boolean ) : string;
function GetStringRight(const Str : string; Count : Word) : string;
function GetStringLeft(const Str : string; Count : Word) : string;
function GetStringMiddle(const Str : string; Index, Count : Word) : string;
function IsCharInStr(const Str : string; const Chars : array of char) : Boolean;
function ReplaceStr(var Str : string; Find, Replace : char) : Boolean;
function DeleteChars(var Str : string; Chars : array of char) : Boolean;
function TrimRight(var Str : string; const Char : char) : Boolean;
function TrimLeft(var Str : string; const Char : char; MinLen : Integer) : Boolean;
function TrimDoubleSpaces(var Str : string) : Boolean;
function TabToSpace(var Str : string; SpaceCount : Word): Boolean;
function IsStringAlpha (Str: String) : Boolean;
function IsStringNumeric (Str: String) : Boolean;
function IsStringFloat (Str: String) : Boolean;
function IsStrMatch(Str1, Str2 : String) : Double;
function AddThousandSeparator(var Str : string; Chr : char): Boolean;
function ReverseString(const Str : String) : string;
function StringToPChar(var Str : string) : PChar;
function NextPos(Str, Find : String; Position : integer) : Integer;
function StrToBoolean(Value : string) : Boolean;
function BooleanToStr(Value : Boolean) : string;
function BooleanToStrExt(Value : Boolean) : string;


{ Integer }
function GetLargerInt(nValue1, nValue2 : Integer) : Integer;
function GetRoundedInt(const Value : Integer; RoundDown : Boolean) : Integer;
function GetMaxInt(A, B : Longint) : Longint;
function GetMinInt(A, B : Longint) : Longint;
function GetMaxIntArray(const Values : array of Longint) : Longint;
function GetMinIntArray(const Values : array of Longint) : Longint;
function GetMaxFloatArray(const Values : array of Extended) : Extended;
function GetMinFloatArray(const Values : array of Extended) : Extended;
function RandomInteger(Min, Max : Integer) : Integer;
function RoundDecimal(Value : Double; Decimals : Integer) : Double;
function IntToBoolean(Value : Word) : Boolean;
function IntToByte(const Value : Integer) : Byte;


{ Date, Time }
function DateTimeToString(const DT : TDateTime; Format : String; DateSep, TimeSep : Char) : string;
function IsLeapYear(dYear : Integer) : Boolean;
function DaysPerMonth(dYear, dMonth : Integer) : Integer;
function GetMaxDateArray(const Values : array of TDateTime) : TDateTime;
function GetMinDateArray(const Values: array of TDateTime) : TDateTime;
function AddDays (const D : TDateTime; const N : integer) : TDateTime;
function GetDay (const D : TDateTime) : Word;
function GetMonth (const D : TDateTime) : Word;
function GetYear (const D : TDateTime) : Word;
function GetSecond (const D : TDateTime) : Word;
function GetMinute (const D : TDateTime) : Word;
function GetHour (const D : TDateTime) : Word;


{ System }
function RunExeAndWait(const Exename, Params  :string; WindowState : word) : Boolean;
procedure Delay(MSecs : Longint);
procedure MinimizeToTray(Wnd : HWND);
function GetSysTrayRect : TRect;
function GetShiftState : TShiftState;
function ChangeResolution(SizeX, SizeY, bpp : DWORD) : Boolean;
function GetScreenResolution : TPoint;
function GetScreenBPP : Word;


{ File, Direction }
function AddBackslash(var Pathname : string) : Boolean;
function DelBackslash(var Pathname : string) : Boolean;
function ExtractFileNameNoExt(FileName : string) : string;
function ExtractFileExtNoPeriod(FileName: string): string;
function CopyFile(const SourceFileName, TargetFileName : string) : Boolean;
function GetDiskFree(Drive : Char) : Int64;
function GetDiskUse(Drive : Char) : Int64;
function GetDiskSize(Drive : Char) : Int64;
function IsDirEmpty(const DirName : string) : Boolean;
function CreateDir(const DirName : string) : Boolean;
function CopyDir(const SourceDir, TargetDir : string) : Boolean;
function MoveDir(const SourceDir, TargetDir : string) : Boolean;
function DeleteDir(const DirName : string) : Boolean;
function GetDirSize (DirName : string; SubDir, ResultInMByte : Boolean) : Double;
function GetFileSize(Filename: string): Integer;
function ScanFile(const FileName, Text : string; StartPos : Integer; caseSensitive: Boolean): Longint;

{ Graphic }
function RgbToHSV(R, G, B : Word; var H, S, V : Double) : Boolean;
function JPEGtoBMP(SourceFile, TargetFile : string) : Boolean;
function BMPtoJPEG(SourceFile, TargetFile : string; CompressLevel : Word) : Boolean;
function GetHighlightColor(cColor: TColor; Percent : TPercent): TColor;
function GetShadowColor(cColor: TColor; Percent : TPercent): TColor;
function LoadJPEGFromRes(SourceJPG : string; TargetPICTURE : TPicture) : Boolean;
procedure BMPtoICO(Bitmap : TBitmap; var Icon : TIcon);
procedure ChangeBMPColor(var Bitmap : TBitmap; OldColor, NewColor : TColor);


{ Math }
function PointInRect(const Point: TPoint; const Rect : TRect) : Boolean;
function DistanceTweenPoints(Point1, Point2: TPoint): Extended;
function CmToPixel(Cm : Double) : Cardinal;
function PixelToCm(Pixel : Cardinal) : Double;


{ Printer }
function PrintMM(Graphic : TGraphic; Size : double;
  SizeIsWidth : Boolean; MarginLeft, MarginTop : Word) : Boolean;
function GetDefaultPrinter : string;
procedure SetDefaultPrinter(NewDefaultPrinter : string);


{ Other }
function Check_Direction(Direction1, Direction2  : TDirection; Depth : Integer) : Integer;
function WriteLogFile(Text, Logfile : string; SetDate : Boolean): Boolean;
function WordCount(Text : String) : Longint;
function PointToComma(var Str : string) : Boolean;
function CommaToPoint(var Str : string) : Boolean;
function EncryptText(Text : string; Key1, Key2, Key3, Key4 : TCrypt) : string;
function DecryptText(Text : string; Key1, Key2, Key3, Key4 : TCrypt) : string;
function GetCheckSum(FileName : string) : DWORD;



implementation

const
  OneDay         = 1.0;
  OneHour        = OneDay / 24.0;
  OneMinute      = OneHour / 60.0;
  OneSecond      = OneMinute / 60.0;
  OneMillisecond = OneSecond / 1000.0;


{ ############################################################################################### }
{ ############################################################################################### }
{ STRING }
{ ############################################################################################### }
{ ############################################################################################### }


{ sucht ein kaufmännisches '&' im übergebenen String und liefert TRUE bei erfolgreicher Suche }

function IsUnderline(const Str : string) : Boolean;
var
  UnderlinePos: Integer;
  bUnderline : Boolean;
const
  kAnd = '&';
begin
  bUnderline := False;

  try
    UnderlinePos := Pos(kAnd, Str);
    if UnderlinePos > 0 then
    begin
      if Str[UnderlinePos + 1] = '&' then
        bUnderline := False
      else
        bUnderline := True;
    end;
  except
    bUnderline := False;
  end;
  Result := bUnderline;
end;


{ +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{  }
function IsUnderlineChar(const Str : string; CC : Integer) : Boolean;
var
  UnderlinePos : Integer;
  AKey, UnderlineText : string;
  bUnderline : Boolean;
const
  KU = '&';
begin
  try
    bUnderline := False;
    UnderlinePos := Pos(KU, Str);
    AKey := AnsiUpperCase(Char(CC));

    if UnderlinePos < Length(Str) then
    begin
      UnderlineText := AnsiUpperCase(Str[UnderlinePos + 1]);
      bUnderline := (UnderlinePos > 0) and
        (UnderlinePos < Length(Str)) and
        (AKey = UnderlineText);
    end;
  except
    bUnderline := False;
  end;
  Result := bUnderline;
end;


{ +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ erweitert einen String rechts, um ein angegebenes Zeichen
  z.B.
  - s := 'Hallo'
    ExpandStringRight(s, 'A', 10)  ->  s = 'HalloAAAAA'
    Result -> True

  - s := 'Hallo'
    ExpandStringRight(s, 'A', 4)  ->  s = 'Hallo'
    Result -> False
}
function ExpandStringRight(var Str : string; ExpChar : char; EndLength : word) : Boolean;
var
  bExpand : Boolean;
begin
  bExpand := False;
  while Length(Str) < EndLength do
  begin
    Str := Str + ExpChar;
    bExpand := True;
  end;
  Result := bExpand;
end;


{ +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ erweitert einen String links, um ein angegebenes Zeichen
  z.B.
  - s := 'Hallo'
    ExpandStringRight(s, 'A', 10)  ->  s = 'AAAAAHallo'
    Result -> True

  - s := 'Hallo'
    ExpandStringRight(s, 'A', 4)  ->  s = 'Hallo'
    Result -> False
}
function ExpandStringLeft(var Str : string; ExpChar : char; EndLength : word) : Boolean;
var
  bExpand : Boolean;
begin
  bExpand := False;
  while Length(Str) < EndLength do
  begin
    Str := ExpChar + Str;
    bExpand := True;
  end;
  Result := bExpand;
end;


{ +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ gibt einen Teilstring bis zum angegebenen Delimiter von links zurück.
  - DeleteGetPartInString: löscht den zurückgegebenen Teilstring im Original, damit
    mit einem erneutem Aufruf ein weiterer Teilstring zurückgegeben werden kann.
  - DeleteDelimiter: löscht den Delimiter, damit
    mit einem erneutem Aufruf ein weiterer Teilstring zurückgegeben werden kann.
}
function GetStringPartFromLeft(var Str : string; Delimiter : char;
  DeleteResultPart, DeleteOnlyDelimiter : Boolean ) : string;
var
  LenText, TextPos : Integer;
  StringText : string;
begin
  LenText := Length(Str);
  TextPos := 1;
  StringText := '';

  while (TextPos <= LenText) and (Str[TextPos] <> Delimiter) do
  begin
    StringText := StringText + Str[TextPos];
    Inc(TextPos);
  end;

  if DeleteResultPart then
  begin
    Delete(Str, 1, TextPos);
  end
  else
  begin
    if DeleteOnlyDelimiter then
      Delete(Str, TextPos, 1);
  end;

  Result := StringText;
end;


{ +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ gibt einen Teilstring bis zum angegebenen von rechts Delimiter zurück.
  - DeleteGetPartInString: löscht den zurückgegebenen Teilstring im Original, damit
    mit einem erneutem Aufruf ein weiterer Teilstring zurückgegeben werden kann.
  - DeleteDelimiter: löscht den Delimiter, damit
    mit einem erneutem Aufruf ein weiterer Teilstring zurückgegeben werden kann.
}
function GetStringPartFromRight(var Str : string; Delimiter : char;
  DeleteResultPart, DeleteOnlyDelimiter : Boolean ) : string;
var
  LenText, TextPos : Integer;
  StringText : string;
begin
  LenText := Length(Str);
  TextPos := LenText;
  StringText := '';

  while (TextPos >= 1) and (Str[TextPos] <> Delimiter) do
  begin
    StringText := Str[TextPos] + StringText;
    Dec(TextPos);
  end;

  if TextPos = 0 then
    TextPos := 1;

  if DeleteResultPart then
  begin
    Delete(Str, TextPos, (LenText - TextPos) + 1);
  end
  else
  begin
    if DeleteOnlyDelimiter and (TextPos > 1) then
      Delete(Str, TextPos, 1);
  end;

  Result := StringText;
end;


{ +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ ersetzt alle Chars in einem String
  z.B.
  - s := 'Hello World'
    ReplaceStr(s, 'l', 'x') ->  s = Hexxo Worxd
    Result -> True

  - s := 'Hello World'
    ReplaceStr(s, 'h', 'x') ->  s = Hello World'
    Result -> False
}
function ReplaceStr(var Str : string; Find, Replace : char) : Boolean;
var
  LenSource, nPos : Integer;
  bReplace : Boolean;
begin
  bReplace := False;
  LenSource := Length(Str);
  if LenSource > 0 then
  begin
    for nPos := 1 to LenSource do
    begin
      if Str[nPos] = Find then
      begin
        Str[nPos] := Replace;
        bReplace := True;
      end;
    end;
  end;
  Result := bReplace;
end;


{ +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ entfernt alle Chars aus einem String
  z.B.
  - s := 'Hello World'
    DeleteChars(s, ['l', 'o'])  ->  s = 'He Wrd'
    Result -> True

  - s := 'Hello World'
    DeleteChars(s, ['A', 'm', 'x'])  ->  s ='Hello World'
    Result -> False
}
function DeleteChars(var Str : string; Chars : array of char) : Boolean;
var
  StrPos, StrLen, I : Word;
  bDelete : Boolean;
begin
  bDelete := False;
  for I := 0 to High(Chars) do
  begin
    StrLen := Length(Str);
    StrPos := 1;
    while StrPos <= StrLen do
    begin
      if Chars[I] = Str[StrPos] then
        begin
          Delete(Str, StrPos, 1);
          Dec(StrLen);
          bDelete := True;
        end
        else
          Inc(StrPos);
    end;
  end;
  Result := bDelete;
end;


{ +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ entfernt das angegebene Zeichen von rechts
  z.B.
  - s := 'green   '
    TrimRight(s, ' ')  ->  s = 'green'
    Result -> True

  - s := 'green   '
    TrimRight(s, 'x')  ->  s = 'green   '
    Result -> False
}
function TrimRight(var Str : string; const Char : char) : Boolean;
var
  StrLen : Word;
  bTrim : Boolean;
begin
  bTrim := False;
  StrLen := Length(Str);
  while Str[StrLen] = Char do
  begin
    Delete(Str, StrLen, 1);
    StrLen := Length(Str);
    bTrim := True;
  end;
  Result := bTrim;
end;

{ +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ entfernt das angegebene Zeichen von links
  z.B.
  - s := 'mmmHello'
    TrimLeft(s, 'm')  ->  s = 'Hello'
    Result -> True

  - s := 'mmmHello'
    TrimLeft(s, 'x')  ->  s = 'mmmHello'
    Result -> False
}
function TrimLeft(var Str : string; const Char : Char; MinLen : Integer) : Boolean;
var
  bTrim : Boolean;
begin
  bTrim := False;
  while (Length(Str) > MinLen) and (Str[1] = Char) do
  begin
    Delete(Str, 1, 1);
    bTrim := True;
  end;
  Result := bTrim;
end;

{ +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ entfernt alle doppelten Leerzeichen
  z.b.
  - s := 'This is  my     car'
    TrimDoubleSpaces(s)  ->  s = 'This is my car'
    Result -> True

  - s := 'No double spaces'
    TrimDoubleSpaces(s)  ->  s = 'No double spaces'
    Result -> False

}
function TrimDoubleSpaces(var Str : string) : Boolean;
var
  StrLen, StrPos : Word;
  bDouble : Boolean;
begin
  StrLen := Length(Str);
  StrPos := 1;
  bDouble := False;

  while StrPos <= StrLen do
  begin
    if Str[StrPos] = ' ' then
    begin
      if Str[StrPos + 1] = ' ' then
      begin
        Delete(Str, StrPos, 1);
        bDouble := True;
        Dec(StrLen);
      end
      else
        Inc(StrPos);
    end
    else
      Inc(StrPos);
  end;
  Result := bDouble;
end;

{ +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ prüft, ob ein oder mehrere Char im angegebenen String vorhanden sind
  z.b.
  - s := 'Hello World'
    IsCharInStr(s, ['e', 'y'])  ->  True
    IsCharInStr(s, ['x', 'y'])  ->  False
}
function IsCharInStr(const Str : string; const Chars : array of char) : Boolean;
var
  StrPos, StrLen, I : Word;
  bFound : Boolean;
begin
  StrLen := Length(Str);
  StrPos := 1;
  bFound := False;

  for I := 0 to High(Chars) do
  begin
    while StrPos <= StrLen do
    begin
      if Chars[I] = Str[StrPos] then
      begin
        bFound := True;
        StrPos := StrLen;
      end
      else
        Inc(StrPos);
    end;
  end;
  Result := bFound;
end;

{ +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ wandelt alle TAB Zeichen in Leerzeichen um.
  - SpaceCount: Anzahl der Leerzeichen für ein TAB
  z.b.
  - s := 'This    is    a    TAB-Text'
    TabToSpace(s, 1)  ->  s = 'This is a Tab-Text'
    TabToSpace(s, 8)  ->  s = 'This        is        a        Tab-Text'
    Result -> True

  - s := 'No TAB in this text'
    TabToSpace(s, 3)  ->  s = 'No TAB in this text'
    Result -> False
}
function TabToSpace(var Str : string; SpaceCount : Word) : Boolean;
var
  StrPos, StrLen, Tab : Word;
  bTab : Boolean;
begin
  bTab := False;
  StrPos := 1;
  StrLen := Length(Str);

  while StrPos <= StrLen do
  begin
    if Str[StrPos] = Chr(9) then
    begin
      Delete(Str, StrPos, 1);
      for Tab := 1 to SpaceCount do
      begin
        Insert(' ', Str, StrPos);
      end;
      bTab := True;
    end;
    Inc(StrPos);
  end;
  Result := bTab;
end;

{ +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ gibt einen String mit Count Zeichen von links zurück }
function GetStringLeft(const Str : string; Count : Word) : string;
var
  StrLen : Word;
begin
  StrLen := Length(Str);

  if (StrLen >= 1) and (Count >= 1) then
    Result := Copy(Str, 1, Count)
  else
    Result := '';
end;

{ +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ gibt einen String mit Count Zeichen von rechts zurück }
function GetStringRight(const Str : string; Count : Word) : string;
var
  StrLen : Word;
begin
  StrLen := Length(Str);

  if (StrLen >= 1) and (Count >= 1) then
    Result := Copy(Str, StrLen - (Count - 1), Count)
  else
    Result := '';
end;

{ +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ gibt einen String mit Count Zeichen von einer bestimmten Position zurück }
function GetStringMiddle(const Str : string; Index, Count : Word) : string;
var
  StrLen : Word;
begin
  StrLen := Length(Str);

  if (StrLen >= 1) and (Count >= 1) then
    Result := Copy(Str, Index, Count)
  else
    Result := '';
end;

{ +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ prüft, ob der angegebene String nur Buchstaben enthält
  z.B.
  - s := 'Hello World'
    IsStringAlpha(s) -> True

  - s := 'www.Blue-Xplosion.de\Index.html'
    IsStringAlpha(s) -> True

  - s := 'Hello World 1234'
    IsStringAlpha(s) -> False
}
function IsStringAlpha(Str: String) : Boolean;
var
  StrPos : Word;
  bAlpha : Boolean;
  cSonder : set of Char;
begin
  cSonder := [' ', '!', '"', '§', '$', '%', '/', '(', ')', '=',
    '?', '\', '`', '+', '*', '~', '#', '<', '>', ',', ';', '.',
    ':', '-', '_', '{', '}', '[', ']', '°'];
  bAlpha := True;
  StrPos := 1;
  while (StrPos <= Length(Str)) and (bAlpha = True) do
  begin
    if (Str[StrPos] in ['A'..'Z']) or (Str[StrPos] in ['a'..'z']) or (Str[StrPos] in cSonder) then
      bAlpha := True
    else
      bAlpha := False;
    Inc(StrPos);
  end;
  Result := bAlpha;
end;

{ +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ prüft, ob der angegebene String eine Zahl ist
  z.B.
  - s := '12345'
    IsStringNumeric(s) -> True

  - s := '12 Cars'
    IsStringNumeric(s) -> False
}
function IsStringNumeric(Str: String) : Boolean;
var
  StrPos : Word;
  bNumeric : Boolean;
begin
  StrPos := 1;
  bNumeric := True;
  while (StrPos <= Length(Str)) and (bNumeric = True) do
  begin
    if (Str[StrPos] in ['0'..'9']) then
      bNumeric := True
    else
      bNumeric := False;
    Inc(StrPos);
  end;
  Result := bNumeric;
end;

{ +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ prüft, ob der angegebene String ein Float ist
  z.b.
  - s := '10,55'
    IsStringFloat(s) -> True

  - s := '10'
    IsStringFloat(s) -> True

  - s := '10,55 abc'
    IsStringFloat(s) -> False
}
function IsStringFloat(Str: String) : Boolean;
begin
  try
    StrToFloat(Str);
    result := true;
  except
    result := false;
  end;
end;

{ +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ vergleicht zwei Strings, um gibt als Rückgabe den Unterschied in Prozent zurück
  z.b.
  - IsStrMatch('Hello', 'Hello')  -> 100
  - IsStrMatch('Hello', 'He')     -> 40
  - IsStrMatch('Hello', 'World')  -> 0
}
function IsStrMatch(Str1, Str2 : String) : Double;
var
  i, iMin, iMax, iSameCount: Integer;
begin
  iMax := Max(Length(Str1), Length(Str2));
  iMin := Min(Length(Str1), Length(Str2));
  iSameCount := -1;
  for i := 0 to iMax do
  begin
    if i > iMin then
      break;
    if Str1[i] = Str2[i] then
      Inc(iSameCount)
    else
      break;
  end;
  if iSameCount > 0 then
    Result := (iSameCount / iMax) * 100
  else
    Result := 0.00;
end;

{ +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ fügt in einen String ein Tausenderzeichen ein
  z.B.
  - AddThousandSeparator(123456789, '.')  ->  123.456.789
}
function AddThousandSeparator(var Str : string; Chr : char) : Boolean;
var
  I: integer;
  bOk : Boolean;
begin
  bOk := False;
  I := Length(Str) - 2;
  while I > 1 do begin
    Insert(Chr, Str, I);
    I := I - 3;
    bOk := True;
  end;
  Result := bOk;
end;

{ +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ dreht einen String um
  z.b.
  - ReverseString('Hello')  ->  'olleH'
}
function ReverseString(const Str : String) : string;
var
  i : integer;
begin
  Result := '';
  for i := Length(Str) downto 1 do
    Result := Result + Str[i];
end;

{ +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ wandelt einen String zu einem PChar um }
function StringToPChar(var Str : string) : PChar;
begin
  Str := Str + #0;
  Result := @Str[1];
end;

{ +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ sucht in einem String, ab einer bestimmten Position }
function NextPos(Str, Find : String; Position : integer) : Integer;
begin
  Delete(Str, 1, Position - 1);
  Result := Pos(Find, Str);
  If Result = 0 then
    exit;
  If (Length(Str) > 0) and (Length(Find) > 0) then
    Result := Result + Position - 1;
end;

{ +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function StrToBoolean(Value : string) : Boolean;
begin
  if (AnsiUpperCase(Value) = 'TRUE') or (Strtoint(Value) > 0) then
    Result := True
  else
    Result := False;
end;

{ +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function BooleanToStr(Value : Boolean) : string;
begin
  if Value = True then
    Result := 'True'
  else
    Result := 'False';
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function BooleanToStrExt(Value : Boolean) : string;
begin
  if Value = True then
    Result := '1'
  else
    Result := '0';
end;


{ ############################################################################ }
{ ############################################################################ }
{ INTEGER }
{ ############################################################################ }
{ ############################################################################ }

{ gibt den größeren der beiden Werte zurück }
function GetLargerInt(nValue1, nValue2 : Integer) : Integer;
begin
  if nValue1 < nValue2 then
    Result := nValue2
  else
    Result := nValue1;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ Rundet eine Integerzahl
  z.b.
    n := 55
    - GetRoundedInt(n, False)  ->  56
    - GetRoundedInt(n, True)   ->  54
}
function GetRoundedInt(const Value : Integer; RoundDown : Boolean) : Integer;
var
  nMod, nResult : Integer;
begin
  nMod := Value mod 2;

  if nMod <> 0 then
  begin
    if RoundDown then
      nResult := Value -1
    else
      nResult := Value +1;
  end
  else
    nResult := Value;

  Result := nResult;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ gibt die größere der beiden Zahlen zurück }
function GetMaxInt(A, B : Longint) : Longint;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ gibt die die kleinere der beiden Zahlen zurück }
function GetMinInt(A, B : Longint) : Longint;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ gibt die größte Zahl in einem Array zurück }
function GetMaxIntArray(const Values : array of Longint) : Longint;
var
 I : Cardinal;
begin
  if High(Values) > 0 then
  begin
    Result := Values[0];
    for I := 0 to High(Values) do
    begin
      if Values[I] > Result then
        Result := Values[I];
    end;
  end
  else
    Result := 0;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ gibt die kleinste Zahl in einem Array zurück }
function GetMinIntArray(const Values : array of Longint) : Longint;
var
  I: Cardinal;
begin
  if High(Values) > 0 then
  begin
    Result := Values[0];
    for I := 0 to High(Values) do
    begin
      if Values[I] < Result then
        Result := Values[I];
    end;
  end
  else
    Result := 0;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ gibt den größten Float in einem Array zurück }
function GetMaxFloatArray(const Values : array of Extended) : Extended;
var
  I: Cardinal;
begin
  if High(Values) > 0 then
  begin
    Result := Values[0];
    for I := 0 to High(Values) do
    begin
      if Values[I] > Result then
        Result := Values[I];
    end;
  end
  else
    Result := 0;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ gibt den kleinsten Float in einem Array zurück }
function GetMinFloatArray(const Values : array of Extended) : Extended;
var
  I: Cardinal;
begin
  if High(Values) > 0 then
  begin
    Result := Values[0];
    for I := 0 to High(Values) do
    begin
      if Values[I] < Result then
        Result := Values[I];
    end;
  end
  else
    Result := 0;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ gibt eine Zufallszahl zwischen Min und Max zurück }
function RandomInteger(Min, Max : Integer) : Integer;
Var
  RandRange: Integer;
  RandValue: Integer;
Begin
  if Max <= Min Then
  Begin
    Result := Min;
    Exit;
  End;

  Randomize;
  RandRange := Max - Min;
  RandValue := Random(RandRange);
  Result := RandValue + Min;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ rundet einen Decimalwert auf eine bestimmte Anzahl von Nachkommastellen }
function RoundDecimal(Value: Double; Decimals: Integer): Double;
var
  j : Integer;
  A : Double;
begin
  A := 1;
  Case Decimals of
    0 : A := 1;
    1 : A := 10;
  else
    for j := 1 to Decimals do
      A := A * 10;
  end;
  Result := Int((Value * A) + 0.5) / A;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function IntToBoolean(Value : Word) : Boolean;
begin
  if Value = 0 then
    Result := False
  else
    Result := True;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function IntToByte(const Value : Integer) : Byte;
begin
  if Value > 255 then
    Result := 255
  else if Value < 0 then
    Result := 0
  else
    Result := Value;
end;



{ ############################################################################ }
{ ############################################################################ }
{ DATE / TIME }
{ ############################################################################ }
{ ############################################################################ }

{ Konvertiert einen DateTime Wert in einen String
  z.b.
  - DateTimeToString(37165.5, 'dd/mm/yyyy', '.', ':')  ->  01.10.2001 12:00:00

  - DateTimeToString(37165, 'dd/mm/yyyy', '.', ':')  ->  01.10.2001
  - Format: d/m/yy         ->  1.10.01
            dd/mm/yyyy     ->  01.10.2001
            dd/mmm/yyyy    ->  01.Oct.2001
            dd/mmmm/yyyy   ->  01.October.2001

  - Seperator: '-'  ->  01-10-2001
               '.'  ->  01.10.2001
               '/'  ->  01/10/2001
}
function DateTimeToString(const DT : TDateTime; Format : String; DateSep, TimeSep : Char) : string;
begin
  Application.UpdateFormatSettings := False;
  if DT > 0 then
  begin
    DateSeparator := DateSep;
    TimeSeparator	:= TimeSep;

    ShortDateFormat := Format
  end;

  Result := DateTimeToStr(DT);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ prüft, ob das angegebene Jahr ein Schaltjahr ist }
function IsLeapYear(dYear : Integer) : Boolean;
begin
  Result := (dYear mod 4 = 0) and ((dYear mod 100 <> 0) or (dYear mod 400 = 0));
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ ermittelt, wieviele Tage der angegebenen Monat hat }
function DaysPerMonth(dYear, dMonth : Integer) : Integer;
const
  DaysInMonth: array[1..12] of Integer =
    (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
begin
  Result := DaysInMonth[dMonth];
  // Ist Schaltjahr, dann Februar = 29 Tage
  if (dMonth = 2) and IsLeapYear(dYear) then
    Inc(Result);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ gibt das größte Datum in einem Array zurück }
function GetMaxDateArray(const Values : array of TDateTime) : TDateTime;
var
  I: Cardinal;
begin
  if High(Values) > 0 then
  begin
    Result := Values[0];
    for I := 0 to High(Values) do
    begin
      if Values[I] > Result then
        Result := Values[I];
    end;
  end
  else
    Result := 0;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ gibt das kleinste Datum in einem Array zurück }
function GetMinDateArray(const Values: array of TDateTime) : TDateTime;
var
  I: Cardinal;
begin
  if High(Values) > 0 then
  begin
    Result := Values[0];
    for I := 0 to High(Values) do
    begin
      if Values[I] < Result then
        Result := Values[I];
    end;
  end
  else
    Result := 0;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ zählt zu einem Datum N-Tage dazu }
function AddDays (const D : TDateTime; const N : integer) : TDateTime;
begin
  Result := D + N;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function GetDay (const D : TDateTime) : Word;
var
  Ye, Mo : Word;
begin
  DecodeDate (D, Ye, Mo, Result);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function GetMonth (const D : TDateTime) : Word;
var
  Ye, Da : Word;
begin
  DecodeDate (D, Ye, Result, Da);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function GetYear (const D : TDateTime) : Word;
var
  Mo, Da : Word;
begin
  DecodeDate (D, Result, Mo, Da);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function GetSecond (const D : TDateTime) : Word;
var
  Ho, Mi, MS : Word;
begin
  DecodeTime (D, Ho, Mi, Result, MS);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function GetMinute (const D : TDateTime) : Word;
var
  Ho, Se, MS : Word;
begin
  DecodeTime (D, Ho, Result, Se, MS);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function GetHour (const D : TDateTime) : Word;
var
  Mi, Se, MS : Word;
begin
  DecodeTime (D, Result, Mi, Se, MS);
end;


{ ############################################################################ }
{ ############################################################################ }
{ SYSTEM }
{ ############################################################################ }
{ ############################################################################ }

{ startet eine Anwendung (mit Paramerter) und wartet auf desen Ende }
function RunExeAndWait(const Exename, Params  :string; WindowState : word) : Boolean;
var
  sCommando : string;
  StartUpInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
begin
  sCommando := '"' + Exename + '" ' + Params;
  FillChar(StartUpInfo, Sizeof(StartUpInfo), #0);

  with StartUpInfo do
  begin
    cb := Sizeof(StartUpInfo);
    dwFlags := STARTF_USESHOWWINDOW;
    wShowWindow := WindowState;
  end;

  Result := CreateProcess(nil, PChar(sCommando), nil, nil, False,
                          CREATE_NEW_CONSOLE or
                          NORMAL_PRIORITY_CLASS, nil,
                          PChar(ExtractFilePath(Exename)),
                          StartUpInfo, ProcessInfo);
  { Wait while EXE is finished. }
  if Result then
    WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ versetzt die aktuelle Anwendung in eine Pause }
procedure Delay(MSecs : Longint);
var
  FirstTickCount, Now : Longint;
begin
  FirstTickCount := GetTickCount;
  repeat
    Application.ProcessMessages;
    Now := GetTickCount;
  until (Now - FirstTickCount >= MSecs) or (Now < FirstTickCount);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ minimiert eine Anwendung
  z.b.
  - MinimizeToTray(FindWindow(nil, 'Winamp 2.80'))
}
procedure MinimizeToTray(Wnd : HWND);
var
  WinPlace : TWindowPlacement;
  Rect : TRect;
begin
  WinPlace.Length := SizeOf(TWindowPlacement);
  GetWindowPlacement(Wnd, @WinPlace);
  WinPlace.flags := WPF_SETMINPOSITION;
  Rect := GetSysTrayRect;
  WinPlace.ptMinPosition.x := Rect.Left;
  WinPlace.ptMinPosition.y := Rect.Top;
  SetWindowPlacement(Wnd, @WinPlace);
  SendMessage(Wnd, WM_SYSCOMMAND, SC_MINIMIZE, 0);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ gibt den Bereich der SysTraybar zurück }
function GetSysTrayRect : TRect;
var
  H1, H2 : HWND;
begin
  SetRectEmpty(result);
  H1 := FindWindow('Shell_TrayWnd', nil);
  if H1 <> 0 then
  begin
    H2 := FindWindowEx(H1, 0, 'TrayNotifyWnd', nil);
    if H2 <> 0 then
       GetWindowRect(H2, result);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ ermittelt, ob STRG, und/oder SHIFT, und/oder ALT gedrückt ist }
function GetShiftState : TShiftState;
begin
  Result := [];
  if GetKeyState(VK_MENU) < 0 then
    Include(Result, ssAlt);
  if GetKeyState(VK_CONTROL) < 0 then
    Include(Result, ssCtrl);
  if GetKeyState(VK_Shift) < 0 then
    Include(Result, ssShift);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ Auflösung ändern
  z.b. ChangeResolution(1024, 768, 32)
}
function ChangeResolution(SizeX, SizeY, bpp : DWORD) : Boolean;
var
  DeviceMode : TDeviceModeA;
  Modeindex : Integer;
begin
  Modeindex := 0;
  Result := False;
  while EnumDisplaySettings(nil, Modeindex, DeviceMode) do
  begin
    with DeviceMode do
      if (dmPelsWidth = SizeX) and (dmPelsHeight = SizeY) and (dmBitsPerPel = bpp) then
      begin
        // erst testen, bevor wir umschalten!
        case ChangeDisplaySettings(DeviceMode, CDS_TEST) of
          // es wird klappen!
          DISP_CHANGE_SUCCESSFUL : Result := True;

          // Neustart erforderlich.
          DISP_CHANGE_RESTART : Showmessage('You have to restart');

          // Nicht unbedingt ein Fehler,
          DISP_CHANGE_BADFLAGS : Showmessage('Bad display mode flags');

          // es klappt nicht, ohne nähere Begründung
          DISP_CHANGE_FAILED : Showmessage('Failed in changing resolution');

          // diese Auflösung gibt es nicht
          DISP_CHANGE_BADMODE : Showmessage('Display mode not supported');

          // Nur Windows NT
          DISP_CHANGE_NOTUPDATED : Showmessage('Registry could not be updated');

        else
          Result := True;
        end;

        if Result then 
          //jetzt wird umgeschaltet
          ChangeDisplaySettings(DeviceMode, CDS_FULLSCREEN)
      end;
    Inc(Modeindex);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ Auflösung }
function GetScreenResolution : TPoint;
var
  DC : hDC;
begin
  DC := GetDC(HWND_DESKTOP);
  try
    Result.X := GetDeviceCaps(DC, HORZRES);
    Result.Y := GetDeviceCaps(DC, VERTRES);
  finally
    ReleaseDC(0, DC);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ Farbtiefe (Desktop) }
function GetScreenBPP : Word;
var
  DC : hDC;
begin
  DC := GetDC(HWND_DESKTOP);
  try
    Result := GetDeviceCaps(DC, BITSPIXEL);
  finally
    ReleaseDC(0, DC);
  end;
end;


{ ############################################################################ }
{ ############################################################################ }
{ FILE / DIRECTION }
{ ############################################################################ }
{ ############################################################################ }

{ hängt an einen Pfad ein '\' an, falls noch keins vorhanden ist }
function AddBackslash(var Pathname : string) : Boolean;
var
  bAdd : Boolean;
begin
  if (Length(Pathname) > 0) and (Pathname[length(Pathname)] <> '\') then
  begin
    Pathname := Pathname + '\';
    bAdd := True;
  end
  else
  begin
    Pathname := Pathname;
    bAdd := False;
  end;
  Result := bAdd;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ entfernt das '\' am Ende eines Pfades, falls dieses vorhanden ist }
function DelBackslash(var Pathname : string) : Boolean;
var
  bDel : Boolean;
begin
  if (Length(Pathname) > 0) and (Pathname[Length(Pathname)] = '\') then
  begin
    Delete(Pathname, Length(Pathname) ,1);
    bDel := True;
  end
  else
  begin
    Pathname := Pathname;
    bDel := False;
  end;
  Result := bDel;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ gibt nur den Namen einer Datei zurück
  z.B. Explorer.exe --> Explorer }
function ExtractFileNameNoExt(FileName : string) : string;
var
  FileExt, Ext: string;
  LenExt, LenFileExt, LenFile : integer;
begin
  FileExt := ExtractFileName(FileName);
  Ext := ExtractFileExt(FileName);

  LenFileExt := Length(FileExt);
  LenExt := Length(Ext);
  LenFile := LenFileExt - LenExt;

  if LenExt <> 0 then
    Result := Copy(FileExt, 1, LenFile)
  else
    Result := FileName;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ gibt nur die Erweiterung ohne Pfad, Name zurück
  z.B. C:\winnt\Explorer.exe --> exe }
function ExtractFileExtNoPeriod(FileName: string): string;
var
  FileExtString: string;
begin
  FileExtString := ExtractFileExt(FileName);
  DeleteChars(FileExtString, ['.']);
  Result := FileExtString;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ kopiert eine Datei }
function CopyFile(const SourceFileName, TargetFileName : string) : Boolean;
var
  Source, Target : TFileStream;
  bOk : Boolean;
begin
  bOk := True;
  if FileExists(SourceFileName) and (TargetFileName <> '') then
  begin
    try
      Source := TFileStream.Create(SourceFileName, fmOpenRead);
      try
        Target := TFileStream.Create(TargetFileName, fmCreate or fmOpenWrite);
        try
          Target.CopyFrom(Source, Source.Size);
          FileSetDate(Target.Handle, FileGetDate(Source.Handle));
        finally
          Target.Free;
        end;
      finally
        Source.Free;
      end;
    except
      bOk := False;
    end;
  end
  else
    bOk := False;
  Result := bOk;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ ermittelt den freien Festplattenspeicher }
function GetDiskFree(Drive : Char) : Int64;
var
  DriveNumber : Integer;
begin
  DriveNumber := Ord(Drive) - 64;
  Result := DiskFree(DriveNumber);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ ermittelt den verwendeten Festplattenspeicher }
function GetDiskUse(Drive : Char) : Int64;
var
  DriveNumber : Integer;
begin
  DriveNumber := Ord(Drive) - 64;
  Result := DiskSize(DriveNumber) - DiskFree(DriveNumber);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ ermittelt den gesamten Festplattenspeicher  }
function GetDiskSize(Drive : Char) : Int64;
var
  DriveNumber : integer;
begin
  DriveNumber := Ord(Drive) - 64;
  Result := DiskSize(DriveNumber);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ prüft, ob ein Verzeichnis leer ist. }
function IsDirEmpty(const DirName : string) : Boolean;
var
  sRec: TSearchRec;
  i : Integer;
  sDir : string;
begin
  i := 0;
  sDir := DirName;
  if DirectoryExists(sDir) then
  begin
    try
      DelBackslash(sDir);

      if FindFirst(sDir + '\*.*', faAnyFile, sRec) = 0 then
      repeat
        if (SRec.Name <> '.') and (SRec.Name <> '..') then
          Inc(i);
      until FindNext(SRec) <> 0;

      if i > 0 then
        Result := False
      else
        Result := True;
    finally
      FindClose(sRec);
    end;
  end
  else
    Result := False;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ erstellt ein Verzeichnis }
function CreateDir(const DirName : string) : Boolean;
begin
  if DirectoryExists(DirName) then
    Result := True
  else
  begin
    try
      ForceDirectories(DirName);
      Result := True;
    except
      Result := False;
      Abort;
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ kopiert ein Verzeichnis inkl. aller Unterverzeichnisse }
function CopyDir(const SourceDir, TargetDir : string) : Boolean;
var
  FOStruct : TSHFileOpStruct;
begin
  ZeroMemory(@FOStruct, Sizeof(FOStruct));
  with FOStruct do
  begin
    wFunc := FO_COPY;
    fFlags := FOF_FILESONLY;
    pFrom := PChar(SourceDir + #0);
    pTo := PChar(TargetDir)
  end;
  Result := (0 = ShFileOperation(FOStruct));
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ verschiebt ein Verzeichniss inkl. aller Unterverzeichnisse }
function MoveDir(const SourceDir, TargetDir : string) : Boolean;
var
  FOStruct : TSHFileOpStruct;
begin
  ZeroMemory(@FOStruct, Sizeof(FOStruct));
  with FOStruct do
  begin
    wFunc := FO_MOVE;
    fFlags := FOF_FILESONLY;
    pFrom := PChar(SourceDir + #0);
    pto := PChar(TargetDir)
  end;
  Result := (0 = ShFileOperation(FOStruct));
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ löscht ein Verzeichnis inkl. aller Unterverzeichnisse }
function DeleteDir(const DirName : string) : Boolean;
var
  FOStruct : TSHFileOpStruct;
begin
  ZeroMemory(@FOStruct, Sizeof(FOStruct));
  with FOStruct do
  begin
    wFunc := FO_DELETE;
    fFlags := FOF_SILENT or FOF_NOCONFIRMATION;
    pFrom := PChar(DirName + #0);
  end;
  Result := (0 = ShFileOperation(FOStruct));
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ ermittelt die Größe des angegebenen Verzeichnisses
  - SubDir: True --> Unterverzeichnisse werden mit einbezogen
  - ResultInMByte: True --> gibt das Ergebniss in MB zurück }
function GetDirSize (DirName : string; SubDir, ResultInMByte : Boolean) : Double;
var
  Rec : TSearchRec;
  Found, Size : integer;
begin
  Size := 0;

  if DirName[Length(DirName)] <> '\' then
    DirName := DirName + '\';
  Found := FindFirst(DirName + '*.*', faAnyFile, Rec);

  while Found = 0 do
  begin
    Inc(Size, Rec.Size);
    if (Rec.Attr and faDirectory > 0) and (rec.Name[1] <> '.') and (Subdir = True) then
      Inc(Size, Round(GetDirSize(DirName + Rec.Name, True, False)));

    Found := FindNext(Rec);
  end;
  findclose(Rec);

  if ResultInMByte then
    Result := Size / sqr(1024)
  else
    Result := Size;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
FUNCTION GetFileSize(Filename: STRING): integer;
VAR
  SR: TSearchRec;
BEGIN
  IF FindFirst(Filename, faAnyFile, SR) = 0 THEN
    Result:=SR.Size
  ELSE
    Result:= -1;
  FindClose(SR);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function ScanFile(const FileName, Text : string; StartPos : Integer; caseSensitive: Boolean): LongInt;
const
  BufferSize= $8001;  { 32K+1 bytes }
var
  pBuf, pEnd,
  pScan, pPos    : PChar;
  filesize       : LongInt;
  bytesRemaining : LongInt;
  bytesToRead    : Integer;
  F              : File;
  SearchFor      : PChar;
  oldMode        : Word;
  nSeekPoint : Integer;
begin
  Result := -1;
  if (Length(Text) = 0) or (Length(FileName) = 0) then
    Exit;
  SearchFor := nil;
  pBuf      := nil;
  AssignFile(F, filename);
  oldMode := FileMode;
  FileMode := 0;
  Reset(F, 1);
  FileMode := oldMode;
  bytesToRead := 0;
  nSeekPoint := 0;
  try
    SearchFor := StrAlloc(Length(Text)+1);
    StrPCopy(SearchFor, Text);
    If not caseSensitive then
      AnsiUpper(SearchFor);
    GetMem(pBuf, BufferSize);
    filesize := System.Filesize(F);
    bytesRemaining := filesize;
    pPos := Nil;
    while bytesRemaining > 0 do
    begin
      Seek(F, StartPos + nSeekPoint);
      If bytesRemaining >= BufferSize Then
        bytesToRead := Pred(BufferSize)
      else
        bytesToRead := bytesRemaining;
      nSeekPoint := nSeekPoint + bytesToRead;
      BlockRead(F, pBuf^, bytesToRead, bytesToRead);
      pEnd := @pBuf[bytesToRead];
      pEnd^:= #0;
      pScan := pBuf;
      while pScan < pEnd Do
      begin
        If not caseSensitive then
          AnsiUpper(pScan);
        pPos := StrPos(pScan, SearchFor);
        If pPos <> nil then
        begin
          Result := FileSize - bytesRemaining + LongInt(pPos) - LongInt(pBuf);
          Break;
        End;
        pScan := StrEnd(pScan);
        Inc(pScan);
      end;
      If pPos <> nil then
        Break;
      bytesRemaining := bytesRemaining - bytesToRead;
      If bytesRemaining > 0 then
      begin
        Seek(F, FilePos(F)-Length(Text));
        bytesRemaining := bytesRemaining + Length(Text);
      End;
    end;
  finally
    CloseFile(F);
    If SearchFor <> nil then
      StrDispose(SearchFor);
    If pBuf <> nil then
      FreeMem(pBuf, BufferSize);
  end;
end;


{ ############################################################################ }
{ ############################################################################ }
{ GRAPHIC }
{ ############################################################################ }
{ ############################################################################ }


{ wandelt einen RBG-Wert in einen HSV-Wert um }
function RgbToHSV(R, G, B : Word; var H, S, V : Double) : Boolean;
var
   Delta, Min, Max : Double;
   bOk : Boolean;
begin
  bOk := True;
  try
    Min := MinValue( [R, G, B] );
    Max := MaxValue( [R, G, B] );

    V := Max;
    Delta := Max - Min;
    if Max = 0 then
      S := 0
    else
      S := (255 * Delta) / Max;
    if S = 0 then
      H := 0
    else
    begin
      if R = Max then
        H := (60 * (G - B)) / Delta
      else if G = Max then
        H := 120 + (60 * (B - R)) / Delta
      else H := 240 + (60 * (R - G)) / Delta;

      if H < 0 then
        H := H + 360;
    end;
  except
    bOk := False;
  end;
  Result := bOk;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ wandelt ein JPEG in ein BMP um }
function JPEGtoBMP(SourceFile, TargetFile : string) : Boolean;
var
  jpeg : TJPEGImage;
  bmp  : TBitmap;
  bOk : Boolean;
begin
  bOk := False;
  if FileExists(SourceFile) then
  begin
    try
      jpeg:= TJPEGImage.Create;
      try
        jpeg.LoadFromFile(SourceFile);
        jpeg.CompressionQuality := 100;
        bmp:= TBitmap.Create;
        try
         bmp.Assign(jpeg);
         bmp.SaveToFile(TargetFile);
         bOk := True;
        finally
         bmp.free
        end;
      finally
        jpeg.free
      end;
    except
      bOk := False;
    end;
  end;
  Result := bOk;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ wnadelt ein BMP zu einem JPEG um.
  CompressLevel: 0-100 (0 = schlechte Qualität, 100 = beste Qualität) }
function BMPtoJPEG(SourceFile, TargetFile : string; CompressLevel : Word) : Boolean;
var
  jpeg : TJPEGImage;
  bmp  : TBitmap;
  bOk : Boolean;
begin
  bOk := False;
  if FileExists(SourceFile) then
  begin
    try
      bmp:= TBitmap.Create;
      try
        bmp.LoadFromFile(SourceFile);
        jpeg:= TJPEGImage.Create;
        try
         jpeg.Assign(bmp);
         jpeg.CompressionQuality := CompressLevel;
         jpeg.Compress;
         jpeg.SaveToFile(TargetFile);
         bOk := True;
        finally
         jpeg.free
        end;
      finally
        bmp.free
      end;
    except
      bOk := False;
    end;
  end;
  Result := bOk;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ gibt einen helleren Farbwert der angegebenen Farbe zurück }
function GetHighlightColor(cColor: TColor; Percent : TPercent): TColor;
begin
  Result := RGB(Min(GetRValue(ColorToRGB(cColor)) + (Percent * 2), 255),
    Min(GetGValue(ColorToRGB(cColor)) + (Percent * 2), 255),
    Min(GetBValue(ColorToRGB(cColor)) + (Percent * 2), 255));
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ gibt einen dunkleren Farbwert der angegebenen Farbe zurück }
function GetShadowColor(cColor: TColor; Percent : TPercent): TColor;
begin
  Result := RGB(Max(GetRValue(ColorToRGB(cColor)) - (Percent * 2), 0),
    Max(GetGValue(ColorToRGB(cColor)) - (Percent * 2), 0),
    Max(GetBValue(ColorToRGB(cColor)) - (Percent * 2), 0));
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function LoadJPEGFromRes(SourceJPG : string; TargetPICTURE : TPicture) : Boolean;
var
  ResH, MemH : THandle;
  MemStream : TMemoryStream;
  ResPtr : PByte;
  ResSize : Longint;
  JPEGImage : TJPEGImage;
  bOk : Boolean;
begin
  bOk := True;
  try
    ResH := FindResource(HInstance, PChar(SourceJPG), 'JPEG');
    MemH := LoadResource(HInstance, ResH);
    ResPtr := LockResource(MemH);
    MemStream := TMemoryStream.Create;
    JPEGImage := TJPEGImage.Create;
    try
      ResSize := SizeofResource(HInstance, ResH);
      MemStream.SetSize(ResSize);
      MemStream.Write(ResPtr^, ResSize);
      MemStream.Seek(0, 0);

      JPEGImage.LoadFromStream(MemStream);
      TargetPICTURE.Assign(JPEGImage);
    finally
    FreeResource(MemH);
    JPEGImage.Free;
    MemStream.Free;
    end;
  except
    bOk := False;
  end;
  Result := bOk;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure BMPtoICO(Bitmap : TBitmap; var Icon : TIcon);
var
  ImageList : TImageList;
begin
  ImageList := TImageList.CreateSize(Bitmap.Width, Bitmap.Height);
  try
    ImageList.AddMasked(Bitmap, Bitmap.TransparentColor);
    ImageList.GetIcon(0, Icon);
  finally
    ImageList.Free;
   end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure ChangeBMPColor(var Bitmap : TBitmap; OldColor, NewColor : TColor);
var
  BmpRect : TRect;
  tmpBmp : TBitmap;
begin
  tmpBmp := TBitmap.Create;
  try
    tmpBmp.Width := Bitmap.Width;
    tmpBmp.Height := Bitmap.Height;
    tmpBmp.Canvas.Brush.Color := NewColor;
    BmpRect := Rect(0, 0, tmpBmp.Width, tmpBmp.Height);
    tmpBmp.Canvas.BrushCopy(BmpRect, Bitmap, BmpRect, OldColor);
    Bitmap.Assign(tmpBmp);
  finally
    tmpBmp.Free;
  end;
end;


{ ############################################################################ }
{ ###############################################'############################ }
{ MATH }
{ ############################################################################ }
{ ############################################################################ }


{ prüft, ob sich der angegebene Punkt in einem Rechteck befindet }
function PointInRect(const Point: TPoint; const Rect : TRect) : Boolean;
begin
  Result :=
    (Rect.Left <= Point.X) and
    (Rect.Top <= Point.Y) and
    (Rect.Right >= Point.X) and
    (Rect.Bottom >= Point.Y);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ ermittelt die Distance zwischen zwei Punkten }
function DistanceTweenPoints(Point1, Point2: TPoint): Extended;
begin
  try
    Result := sqrt(((Point1.x-Point2.x)*(Point1.x-Point2.x)) +
      ((Point1.y-Point2.y)*(Point1.y-Point2.y)));
  except
    Result := 0.00;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function CmToPixel(Cm : Double) : Cardinal;
var
	hGDI : HDC;
  pValue : TPoint;
begin
	pValue.X := Trunc(Cm * 100);
  hGDI := GetDC(Application.Handle);
  try
	  SaveDC(hGDI);
  	SetMapMode(hGDI, MM_LOMETRIC);
	  LPtoDP(hGDI, pValue, 1);
  	RestoreDC(hGDI, -1);
  finally
  	ReleaseDC(Application.Handle, hGDI);
  end;
  Result := pValue.X;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function PixelToCm(Pixel : Cardinal) : Double;
var
	hGDI : HDC;
  pValue : TPoint;
begin
	pValue.X := Pixel;
  hGDI := GetDC(Application.Handle);
  try
	  SaveDC(hGDI);
  	SetMapMode(hGDI, MM_LOMETRIC);
	  DPtoLP(hGDI, pValue, 1);
  	RestoreDC(hGDI, -1);
  finally
  	ReleaseDC(Application.Handle, hGDI);
  end;
  Result := pValue.X / 100;
end;


{ ############################################################################ }
{ ############################################################################ }
{ PRINTER }
{ ############################################################################ }
{ ############################################################################ }

{ druckt eine Grafik in der angegebenen Größe (Size) in Millimeter, MarginLeft-Millimeter von links
  und MarginTop-Millimeter von oben.
  SizeIsWidth: True  --> Size entspricht der Breite (Höhe wird autom. angepasst)
               False --> Size entspricht der Höhe (Breite wird autom. angepasst) }
function PrintMM(Graphic : TGraphic; Size : double;
  SizeIsWidth : Boolean; MarginLeft, MarginTop : Word) : Boolean;
var
  Handle : THandle;
  HorPixel, VertPixel : integer;
  ImgWidth, ImgHeight : Double;
  Rec : TRect;
  bOk : Boolean;

  function DoWorkSize(a : integer; w : double):integer;
  begin
    Result := round((a * w) / 25.4);
  end;

begin
  bOk := True;
  try
    Handle := printer.Handle;

    if SizeIsWidth then
    begin
      ImgHeight := Size * (Graphic.height / Graphic.width);
      ImgWidth := Size;
    end
    else
    begin
      ImgWidth := Size * (Graphic.width / Graphic.height);
      ImgHeight := Size;
    end;
    HorPixel := getdevicecaps(Handle, logpixelsx);
    VertPixel := getdevicecaps(Handle, logpixelsy);
    MarginLeft := DoWorkSize(HorPixel, MarginLeft) - getdevicecaps(Handle, physicaloffsetx);
    MarginTop := DoWorkSize(VertPixel, MarginTop) - getdevicecaps(Handle, physicaloffsety);
    Rec := Rect(MarginLeft, MarginTop, MarginLeft + DoWorkSize(HorPixel, ImgWidth),
    MarginTop + DoWorkSize(VertPixel, ImgHeight));
    with printer do
    begin
      begindoc;
      try
        canvas.stretchdraw(Rec, Graphic);
      finally
        enddoc;
      end;
    end;
  except
    bOk := False;
  end;
  Result := bOk
end;

{ +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ ermittelt den Standarddrucker }
function GetDefaultPrinter : string;
var
  ResStr : array[0..255] of char;
begin
  GetProfileString('Windows', 'device', '', ResStr, 255);
  Result := StrPas(ResStr);
end;

{ +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ setzen eines Standarddruckers }
procedure SetDefaultPrinter(NewDefaultPrinter : string);
var
  ResStr : array[0..255] of char;
begin
  StrPCopy(ResStr,NewDefaultPrinter);
  WriteProfileString ('windows', 'device', ResStr);
  StrCopy (ResStr, 'windows');
  SendMessage (HWND_BROADCAST, WM_WININICHANGE, 0, LongInt(@ResStr));
end;


{ ############################################################################################### }
{ ############################################################################################### }
{ OTHER}
{ ############################################################################################### }
{ ############################################################################################### }


{ schreibt einen Text in ein Logfile.
  ist das Logfile nicht vorhanden, wird es erstellt.
  ist das Logfile vorhanden, wird der Text ans Ende angehängt.
  SetDate: True --> schreibt von den Text das aktuelle Datum + Zeit }
function WriteLogFile(Text, Logfile : string; SetDate : Boolean): Boolean;
var
  tFile : TextFile;
  bOk : Boolean;
begin
  bOk := True;
  try
    if FileExists(Logfile) then
    begin
      AssignFile(tFile, Logfile);
      Append(tFile);
      if SetDate then
        Writeln(tFile, DateTimeToStr(Now) + ' ' + Text)
      else
        Writeln(tFile, Text);
    end
    else
    begin
      AssignFile(tFile, Logfile);
      Rewrite(tFile);
      if SetDate then
        Writeln(tFile, DateTimeToStr(Now) + ' ' + Text)
      else
        Writeln(tFile, Text);
    end;
  except
    bOk := False;
  end;
  CloseFile(tFile);
  Result := bOk;
end;

{ +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ zählt die Wörter in einem Text }
function WordCount(Text : String) : Longint;
  function Seps(txtChar: Char): Boolean;
  begin
    Seps := txtChar In
      [#0..#$1F, ' ', '.', ',', '?', ':', ';', '(',')', '/', '\'];
  end;

var
  I : Word;
  Count : Longint;
begin
  Count := 0;
  I := 1;
  while I  <= Length(Text) do
  begin
    while (I <= Length(Text)) and (Seps(Text[I])) do
      Inc(I);
    if I <= Length(Text) then
    begin
      Inc(Count);

      while (I <= Length(Text)) and (not Seps(Text[I])) do
        Inc(I);

    end;
  end;
  Result := Count;
end;

{ +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ ersetzt alle Punkte im angegebenen String durch ein Komma
  z.b. 12.50 --> 12,50 }
function PointToComma(var Str : string) : Boolean;
var
  I : Integer;
  bOk : Boolean;
begin
  bOk := False;
  try
    if Length(Str)>0 then
      for I := 1 to Length(Str) do
        if Str[I]='.' then
        begin
          Str[I]:=',';
          bOk := True;
        end;
  except
    bOk := False;
  end;
  Result := bOk;
end;

{ +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ ersetzt alle Komma im angegebenen String durch einen Punkt
  z.b. 12,50 --> 12.50 }
function CommaToPoint(var Str : string) : Boolean;
var
  I : Integer;
  bOk : Boolean;
begin
  bOk := False;
  try
    if Length(Str)>0 then
      for I := 1 to Length(Str) do
        if Str[I]=',' then
        begin
          Str[I]:='.';
          bOk := True;
        end;
  except
    bOk := False;
  end;
  Result := bOk;
end;

{ +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ verschlüsselt einen Text }
function EncryptText(Text : string; Key1, Key2, Key3, Key4 : TCrypt) : string;
var
  BufS, Hexa, Hexa1, Hexa2 : string;
  BufI, BufI2, Sc, Sl, Num1, Num2, Num3, Num4, Res1, Res2, Res3, Res4 : Integer;
begin
  BufS := '';
  if Text <> '' then
  begin
    Sl := Length(Text);
    Sc := 0;

    if (Key1 in [1 .. 120]) and (Key2 in [1 .. 120]) and (Key3 in [1 .. 120]) and (Key4 in [1 .. 120]) then
    begin
      BufI := Key1 * Key4;
      BufI2 := Key3 * Key2;
      BufI := BufI - BufI2;
      if BufI = 0 then
      begin
      Result := '';
      Exit;
      end;
    end
    else
    begin
      Result := '';
      Exit;
    end;

    repeat
      Inc(Sc);
      if Sc > Sl then
        Num1 := 0
      else
        Num1 := Ord(Text[Sc]);

      Inc(Sc);
      if Sc > Sl then
        Num2 := 0
      else
        Num2 := Ord(Text[Sc]);

      Inc(Sc);
      if Sc > Sl then
        Num3 := 0
      else
        Num3 := Ord(Text[Sc]);

      Inc(sc);
      if Sc > Sl then
        Num4 := 0
      else
        Num4 := Ord(Text[Sc]);

      Res1 := Num1 * Key1;
      BufI := Num2 * Key3;
      Res1 := Res1 + BufI;
      Res2 := Num1 * Key2;
      BufI := Num2 * Key4;
      Res2 := Res2 + BufI;
      Res3 := Num3 * Key1;
      BufI := Num4 * Key3;
      Res3 := Res3 + BufI;
      Res4 := Num3 * Key2;
      BufI := Num4 * Key4;
      Res4 := Res4 + BufI;

      for BufI := 1 to 4 do
      begin
        case BufI of
          1 : Hexa := IntToHex(Res1, 4);
          2 : Hexa := IntToHex(Res2, 4);
          3 : Hexa := IntToHex(Res3, 4);
          4 : Hexa := IntToHex(Res4, 4);
        end;

      Hexa1 := '$' + Hexa[1] + Hexa[2];
      Hexa2 := '$' + Hexa[3] + Hexa[4];
      if (Hexa1 = '$00') and (Hexa2 = '$00') then
      begin
        Hexa1 := '$FF';
        Hexa2 := '$FF';
      end;
      if Hexa1 = '$00' then
        Hexa1 := '$FE';
      if Hexa2 = '$00' then
      begin
        Hexa2 := Hexa1;
        Hexa1 := '$FD';
      end;
      BufS := BufS + Chr(StrToInt(Hexa1)) + Chr(StrToInt(Hexa2));
    end;
    until Sc >= Sl;
  end;
  Result := BufS;
end;

{ +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ entschlüsselt einen mit "EncryptText" verschlüsselten Text }
function DecryptText(Text : string; Key1, Key2, Key3, Key4 : TCrypt) : string;
var
  BufS, Hexa1, Hexa2 : string;
  BufI, BufI2, Divzr, Sc, Sl, Num1, Num2, Num3, Num4, Res1, Res2, Res3, Res4 : Integer;
begin
  BufS := '';
  if Text <> '' then
  begin
    Res1 := 0;
    Res2 := 0;
    Res3 := 0;
    Res4 := 0;
    Sl := Length(Text);
    Sc := 0;

    if (Key1 in [1 .. 120]) and (Key2 in [1 .. 120]) and (Key3 in [1 .. 120]) and (Key4 in [1 .. 120]) then
    begin
      Divzr := Key1 * Key4;
      BufI2 := Key3 * Key2;
      Divzr := Divzr - BufI2;
      if Divzr = 0 then
      begin
        Result := '';
        Exit;
      end;
    end
    else
    begin
      Result := '';
      Exit;
    end;

    repeat
      for BufI := 1 to 4 do
      begin
        Inc(Sc);
        Hexa1 := IntToHex(Ord(Text[Sc]), 2);
        Inc(Sc);
        Hexa2 := IntToHex(Ord(Text[Sc]), 2);
        if Hexa1 = 'FF' then
        begin
          Hexa1 := '00';
          Hexa2 := '00';
        end;
        if Hexa1 = 'FE' then
          Hexa1 := '00';
        if Hexa1 = 'FD' then
        begin
          Hexa1 := Hexa2;
          Hexa2 := '00';
        end;
        case BufI of
          1 : Res1 := StrToInt('$' + Hexa1 + Hexa2);
          2 : Res2 := StrToInt('$' + Hexa1 + Hexa2);
          3 : Res3 := StrToInt('$' + Hexa1 + Hexa2);
          4 : Res4 := StrToInt('$' + Hexa1 + Hexa2);
        end;
      end;

      BufI := Res1 * Key4;
      BufI2 := Res2 * Key3;
      Num1 := BufI - BufI2;
      Num1 := Num1 div Divzr;
      BufI := Res2 * Key1;
      BufI2 := Res1 * Key2;
      Num2 := BufI - BufI2;
      Num2 := Num2 div Divzr;
      BufI := Res3 * Key4;
      BufI2 := Res4 * Key3;
      Num3 := BufI - BufI2;
      Num3 := Num3 div Divzr;
      BufI := Res4 * Key1;
      BufI2 := Res3 * Key2;
      Num4 := BufI - BufI2;
      Num4 := Num4 div Divzr;
      BufS := BufS + Chr(Num1) + Chr(Num2) + Chr(Num3) + Chr(Num4);
    until Sc >= Sl;
  end;
  Result := BufS;
end;

{ +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ ermittelt eine einfache Checksumme einer Datei }
function GetCheckSum(FileName : string) : DWORD;
var
  F : File of DWORD;
  P : Pointer;
  Fsize : DWORD;
  Buffer : Array [0..500] of DWORD;
begin  
  FileMode := 0;  
  AssignFile(F,FileName);  
  Reset(F);  
  Seek(F,FileSize(F) div 2);  
  Fsize := FileSize(F) -1 -FilePos(F);  
  if Fsize > 500 then
    Fsize := 500;

  BlockRead(F, Buffer, Fsize);
  Close (F);
  P := @Buffer;

  asm
    xor eax, eax
    xor ecx, ecx
    mov edi , p
    @again:
      add eax, [edi + 4*ecx]
      inc ecx
      cmp ecx, fsize
    jl @again
    mov @result, eax
  end;
end;

{ +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{  }
function Check_Direction(Direction1, Direction2 : TDirection; Depth : Integer) : Integer;
begin
  if (Direction1 <> Direction2) then
    Depth := 0;

  Result := Depth;
end;

end.
