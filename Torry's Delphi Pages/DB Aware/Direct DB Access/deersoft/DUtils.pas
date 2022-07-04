unit DUtils;

interface
       
uses Windows, SysUtils, Graphics, Classes, Forms,
     {$IFDEF MSWINDOWS}
     Variants,
     {$ENDIF}
     ShellAPI, DB;

const caRomanNumber : Array[1..20] of String = ('I'  , 'II'  , 'III'  , 'IV' , 'V' ,
                                                'VI' , 'VII' , 'VIII' , 'IX' , 'X' ,
                                                'XI' , 'XII' , 'XIII' , 'XIV', 'XV',
                                                'XVI', 'XVII', 'XVIII', 'XIX', 'XX');
      cniC1Const = 52845;
      cniC2Const = 22719;

      // ADO spec. constans
      cnsADOProvider  = 'Provider';
      cnsADODriver    = 'DRIVER';
      cnsADOFileDSN   = 'DBQ';
      cnsADOSystemDSN = 'DSN';
      cnsADOServer    = 'SERVER';
      cnsADODatabase  = 'DATABASE';
      cnsADOUsername  = 'UID';
      cnsADOPassword  = 'PWD';
      cnsADOSecurity  = 'Integrated Security';
      cnsADOTrustNT   = 'Trusted_Connection';
      cnsDefProvider  = 'MSDASQL';
      cnsDefSecurity  = 'SSPI';
      cnsDefTrustNT   = 'yes';
      cnsDefODBC      = 'MSDASQL';
      cnsDefOleDB     = 'SQLOLEDB';
      cnsUDLExt       = '.UDL';
      cnsDemoVersion  = 'This DEMO version works only with Delphi IDE!';  

type TDirFile   = (dfName, dfFile, dfFull);

{******************************************************************************}

// Exchange a String to other one
function StrTran(const S, FromStr: String; ToStr: String = ''): String;

// Count a String
function StrCount(const S: String; const WhatStr: String = '|'): Integer;

// Position of a string one n item
function StrPos(const S: String; const WhatStr: String = '|'; Position: Integer = 1): Integer;

// Get the part of a string
function GetSubStr(const S: String; Position: Integer = 1; const Separator: String = '|'): String;

// Equalize: more Space -> one space Example: abc   cde  ghi = abc cde ghi
function Equalize(const S: String): String;

// Left N character
function LeftStr(const S: String; n: Integer): String;
// Right N character
function RightStr(const S: String; n: Integer): String;

// Get a SubStr
function SubStr(const S: String; Start, Len: Integer): String;

// Is a Sub string
function IsSub(const Sub, S: String): Boolean;
// Empty the string?
function IsEmptyStr(const S: String): Boolean;
// all parts of string is only digits?
function IsDigits(const S: String): Boolean;

// Fill to Right
function PadR(const S: String; Len: Integer; Fill: Char = ' '): String;
// Fill to Left
function PadL(const S: String; Len: Integer; Fill: Char = ' '): String;
// Fill to Center
function PadC(const S: String; Len: Integer; Fill: Char = ' '): String;

// Count the position from Right
function RPos(const S, WhatStr: String; Position: Integer = 1): Integer;
// Baloldalról számolja az adott karaktert
function LPos(const S, WhatStr: String; Position: Integer = 0): Integer;

// Fill with zero
function FillZero(const S: String; Len: Integer): String; overload;
function FillZero(I: Integer; Len: Integer): String; overload;
// Clear from zero
function ClearZero(const S: String): String;

// Conversion for equaliton
function StrConvert(const S: String): String;

{******************************************************************************}

// First Captial all small
function UpperFirst(const S: String): String;

// All first letter is Capital
function InitCap(const S: String): String;

// Conversion to Captial
function HunUpperCase(const Key: Char): Char;

// Conversion to small
function HunLowerCase(const Key: Char): Char;

{******************************************************************************}

// Top secret routines
function EnCrypt(const S, Psw: String): String;
function DeCrypt(const S, Psw: String): String;
function DecodePassword(const Psw: String): Word;
function StrToHex(const S: String): String;
function HexToStr(const HexStr: String): String;
function DecToRoman(const DecNumber: Word): String;
function RomanToDec(const RomanStr: String): Word;

// Font conversion to String and back
function FontToStr(Font: TFont): String;
function StrToFont(S: String): TFont;

// String-StringLista átalakitás
procedure StrToStrings(const S: String; List: TStrings; Separator: String = '|');
function StringsToStr(const List: TStrings; Separator: String = '|'; AddEmpty: Boolean = True): String;

// Logical data transformation to String and back
function LogToStr(Log: Boolean): String;
function StrToLog(const S: String): Boolean;

// String value back in one line
function IIF(Condition : Boolean; Alfa, Beta: String): String; overload;
// Integer value back in one line
function IIF(Condition : Boolean; Alfa, Beta: Integer): Integer; overload;
// Font value back in one line
function IIF(Condition : Boolean; Alfa, Beta: TFont): TFont; overload;
// DateTime value back in one line
function IIF(Condition : Boolean; Alfa, Beta: TDateTime): TDateTime; overload;

{******************************************************************************}

// Conversion between WindowState <=> integer
function WindowStateToInt(State: TWindowState): Integer;
function IntToWindowState(State: Integer): TWindowState;

// Conversion between Align <=> Integer
function AlignToInt(Align: TAlignment): Integer;
function IntToAlign(Align: Integer): TAlignment;
// Conversion between Align <=> String
function AlignToStr(Align: TAlignment): String;
function StrToAlign(const Align: String): TAlignment;

// Extract the table name
function ExtractTable(const Item: String; Sign: String = '.'): String;
// Extract the field name
function ExtractField(const Item: String; Sign: String = '.'): String;
// Extract data from SQL Text
function ExtractText(const Str: String; aLast: array of String): String;
function ExtractWhere(const SQLText: String): String;
function ExtractTableName(const SQLText: String; MainTable: Boolean = True): String;
procedure ExtractFieldList(const SQLText: String; List: TStrings);

// Clean Text
function CleanText(const S: String): String;
// Clean Number
function CleanNumber(const S: String): String;

// Is it a primary key
//function IsPrimaryKey(const FieldName: String): Boolean;
// Clear the tablename
function ClearTableName(const TableName: String): String;

// Create IN condition
function CreateINCondition(const FieldName, Values: String; UseStr: Boolean = True): String;

// Extract Year, Month, Day from a date
function ExtractYear(Date: TDateTime): Word;
function ExtractMonth(Date: TDateTime): Word;
function ExtractDay(Date: TDateTime): Word;
function ExtractDigits(const S: String; DefStr: String = '0'): String;

// Calculate EuroTime
function EuroTime(T: TDateTime): TDateTime;
function DateText(D: TDateTime): String;
function DateToStd(D: TDateTime; Separator: String = ''): String;
function StdToDate(const S: String): TDateTime;

// Get the last item of StringList
function GetLastItem(Items: TStrings): String;
// Set the last item of StringList
procedure SetLastItem(Items: TStrings; const S: String);

// Field type conversion
function  FieldTypeToText(DataType: TFieldType): String; overload;
function  FieldTypeToText(Field: TField): String; overload;
procedure FieldTypeToList(List: TStrings);
function  FieldTypeToStr(DataType: TFieldType): String; overload;
function  FieldTypeToStr(Field: TField): String; overload;
function  StrToFieldType(const DataType: String): TFieldType;

// Variant Conversion and Type
function VarToText(const V: Variant): String;

{******************************************************************************}

// Get the EXE Dir
function GetExeDir: String;
// Get the WORK Dir
function GetWorkDir(SubDir: String = 'Source'): String;
// Get the Enviroment
function GetEnviroment(const EnvName: String): String;
// Delete all file and directory
function DeleteFiles(const FileSpec: String): Boolean;

// File from a directory to Strings
procedure DirToStrings(const FileSpec: String; List: TStrings; Part: TDirFile; SubDir: Boolean = False);
// File number and size back
function FileNumbers(const FileSpec: String; SubDir, Size: Boolean): Integer;
// only the file name without extension and path
function ExtractName(const FileName: String): String;

// Get the info from the Volume
function GetVolumeID(DriveName : char) : DWord;

// File futtatása: File-név, Paraméterek (pl. megnyitandó file), indulási könyvtár,
// ablakméret (1: normál ablakban, 2: ikon állapotban, 3: maximalizált ablakban
function ExecuteProgram(const FileName, StartParameters, DefaultDir: String; WindowState: TWindowState): THandle;
// Delphi IDE is running?
function IsDelphiRun: boolean;

{******************************************************************************}

// Handle ADO Connection String
function GetADOPart(const ADOStr, Section: String): String;
function SetADOPart(const ADOStr, Section, Value: String; AutoAdd: Boolean = True): String;

{******************************************************************************}

implementation

//uses MsgWait, MsgWork, MsgOpen;


// Exchange a String to other one
function StrTran(const S, FromStr: String; ToStr: String = ''): String;
begin
     Result := '';
     if not ((S = '') or (FromStr = '')) then
     begin
          Result := StringReplace(S, FromStr, ToStr, [rfReplaceAll, rfIgnoreCase]);
     end;
end;


// Count a String
function StrCount(const S: String; const WhatStr: String = '|'): Integer;
var
   i : Integer;
begin
     Result := 0;
     if not (S = '') then
     begin
          if Pos(WhatStr, S) > 0 then
          begin
               for i := 1 to Length(S) do
               begin
                    if S[i] = WhatStr then Inc(Result);
               end;
          end;
     end;
end;


// Position of a string one n item
function StrPos(const S: String; const WhatStr: String = '|'; Position: Integer = 1): Integer;
var
   i : Integer;
   j : Integer;
begin
     j      := 1;
     Result := 0;
     if (Length(S) > 0) then
     begin
          for i := 1 to Length(S) do
          begin
               if (S[i] = WhatStr) then
               begin
                    if j < Position
                    then
                         Inc(j)
                    else
                    begin
                         Result := i;
                         Break;
                    end;
               end;
          end;
     end;
end;


// Get the part of a string
function GetSubStr(const S: String; Position: Integer = 1; const Separator: String = '|'): String;
var
   sTmp : String;
begin
     if (Position > 0) and (Pos(Separator, S) > 0) then
     begin
          if Position = 1
          then
              sTmp := Copy(S, 1, Pos(Separator, S)-1)
          else
              sTmp := Copy(S, StrPos(S, Separator, Position-1)+1, Length(S));
          if (Pos(Separator, sTmp) > 0) then sTmp := Copy(sTmp, 1, Pos(Separator, sTmp)-1);
     end;
     Result := sTmp;
end;


// Equalize: more Space -> one space Example: abc   cde  ghi = abc cde ghi
function Equalize(const S: String): String;
var
   i      : Integer;
   bSpace : Boolean;
begin
     Result := '';
     bSpace := False;
     if not (S = '') then
     begin
          for i := 1 to Length(S) do
          begin
               if (S[i] = ' ') then
               begin
                    if not bSpace then
                    begin
                         Result := Result + ' ';
                         bSpace := True;
                    end;
               end
               else
               begin
                    Result := Result + S[i];
                    bSpace := False;
               end;
          end;
     end;
end;


// Left N character
function LeftStr(const S: String; n: Integer): String;
begin
     Result := S;
     if (Length(S) >= n) then
     begin
          Result := Copy(S, 1, n);
     end;
end;


// Right N character
function RightStr(const S: String; n: Integer): String;
begin
     Result := S;
     if (Length(S) >= n) then
     begin
          Result := Copy(S, Length(S)+1-n, n);
     end;
end;


// Get a SubStr
function SubStr(const S: String; Start, Len: Integer): String;
begin
     Result := Copy(S, Start, Len);
end;


// Is a Sub string
function IsSub(const Sub, S: String): Boolean;
begin
     Result := False;
     if not ((S = '') or (Sub = '')) then
     begin
          Result := (Pos(Sub, S) > 0);
     end;
end;


// Empty the string?
function IsEmptyStr(const S: String): Boolean;
var
   i : Integer;
begin
     Result := True;
     if (S <> '') then
     begin
          for i := 1 to Length(S) do
          begin
               if (S[i] <> ' ') then
               begin
                    Result := False;
                    Break;
               end;
          end;
     end;
end;


// all parts of string is only digits?
function IsDigits(const S: String): Boolean;
var
   i : Integer;
begin
     if S = '' then Result := False else
     begin
          Result := True;
          for i := 1 to Length(S) do
          begin
               if not IsSub(S[i], '0123456789.,') then
               begin
                    Result := False;
                    Break;
               end;
          end;
     end;
end;


// Fill to Right
function PadR(const S: String; Len: Integer; Fill: Char = ' '): String;
begin
     if Length(S) >= Len then Result := Copy(S, 1, Len);
     if Length(S) <  Len then Result := S + StringOfChar(Fill, Len-Length(S));
end;


// Fill to Left
function PadL(const S: String; Len: Integer; Fill: Char = ' '): String;
begin
     if Length(S) >= Len then Result := Copy(S, 1, Len);
     if Length(S) <  Len then Result := StringOfChar(Fill, Len-Length(S)) + S;
end;


// Fill to Center
function PadC(const S: String; Len: Integer; Fill: Char = ' '): String;
var
   iLeft  : Integer;
   iRight : Integer;
begin
     if Length(S) >= Len then Result := Copy(S, 1, Len);
     if Length(S) <  Len then
     begin
          if Odd(Len-Length(S)) then
          begin
               iLeft  := Round((Len-(Length(S)+1)) / 2);
               iRight := (Len-Length(S))-iLeft;
          end
          else
          begin
               iLeft  := Round((Len-Length(S)) / 2);
               iRight := iLeft;
          end;
          Result := StringOfChar(Fill, iLeft) + S + StringOfChar(Fill, iRight);
     end;
end;


// Count the position from Right
function RPos(const S, WhatStr: String; Position: Integer = 1): Integer;
var
   i : Integer;
   n : Integer;
begin
     Result := 0;
     n := 1;
     for i := Length(S) downto 1 do
     begin
          if S[i] = WhatStr then
          begin
               if n < Position then Inc(n) else
               begin
                    Result := i+1;
                    Break;
               end;
          end;
     end;
end;


// Baloldalról számolja az adott karaktert
function LPos(const S, WhatStr: String; Position: Integer = 0): Integer;
var
   i : Integer;
   n : Integer;
begin
     Result := 0;
     n := 1;
     if Position = 0 then Result := Pos(S, WhatStr) else
     begin
          for i := 1 to Length(S) do
          begin
               if S[i] = WhatStr then
               begin
                    if n < Position then Inc(n) else
                    begin
                         Result := i;
                         Break;
                    end;
               end;
          end;
     end;
end;


// Fill with zero
function FillZero(const S: String; Len: Integer): String;
begin
     Result := S;
     if Len > Length(S) then Result := StringOfChar('0', Len - Length(S)) + S;
     if Len < Length(S) then Result := LeftStr(S, Len);
end;


function FillZero(I: Integer; Len: Integer): String;
begin
     Result := FillZero(IntToStr(I), Len);
end;


// Clear from zero
function ClearZero(const S: String): String;
var
   i    : Integer;
   iTmp : Integer;
begin
     iTmp := 1;
     for i := 1 to Length(S) do
     begin
          if not (S[i] = '0') then
          begin
               iTmp := i;
               Break;
          end;
     end;
     Result := Copy(S, iTmp, Length(S));
end;


// Conversion for equaliton
function StrConvert(const S: String): String;
const
     cnsHun = 'ÁÉÓÖÕÍÚÜÛ';
     cnsEng = 'AEOOOIUUU';
var
   i    : Integer;
   iPos : Integer;

begin
     Result := '';
     for i := 1 to Length(S) do
     begin
          iPos := Pos(S[i], cnsHun);
          if iPos > 0 then
              Result := Result + cnsEng[iPos]
          else
              Result := Result + S[i];
     end;
end;


// First Captial all small
function UpperFirst(const S: String): String;
var
   sTmp : String;
begin
     Result := S;
     if not (S = '') then
     begin
          sTmp   := AnsiLowerCase(S);
          Result := HunUpperCase(sTmp[1]) + Copy(sTmp, 2, Length(sTmp));
     end;
end;


// All first letter is Capital
function InitCap(const S: String): String;
var
   i    : Integer;
   bBig : Boolean;
   sTmp : String;
begin
     if not (S = '') then
     begin
          sTmp := HunUpperCase(S[1]);
          bBig := False;
          for i := 2 to Length(S) do
          begin
               if (S[i] = ' ') then
               begin
                    bBig := True;
                    sTmp := sTmp + ' ';
               end
               else
               begin
                    if bBig then
                    begin
                         sTmp := sTmp + HunUpperCase(S[i]);
                         bBig := False;
                    end
                    else
                         sTmp := sTmp + S[i];
               end;
          end;
     end;
     Result := sTmp;
end;


// Conversion to Captial
function HunUpperCase(const Key: Char): Char;
begin
     case Key of
          'á': Result := 'Á';
          'é': Result := 'É';
          'ó': Result := 'Ó';
          'í': Result := 'Í';
          'ú': Result := 'Ú';
          'ö': Result := 'Ö';
          'õ': Result := 'Õ';
          'ü': Result := 'Ü';
          'û': Result := 'Û';
     else
         Result := UpCase(Key);
     end;
end;


// Conversion to small
function HunLowerCase(const Key: Char): Char;
begin
     case Key of
          'Á': Result := 'á';
          'É': Result := 'é';
          'Ó': Result := 'ó';
          'Í': Result := 'í';
          'Ú': Result := 'ú';
          'Ö': Result := 'ö';
          'Õ': Result := 'õ';
          'Ü': Result := 'ü';
          'Û': Result := 'û';
     else
         if Key in ['A'..'Z'] then // A-Z betük
         begin
              Result := Chr(Ord(Key) + 32);
         end
         else
              Result := Key;
     end;
end;


// Encoding
function EnCrypt(const S, Psw: String): String;
var
   i    : Byte;
   wKey : Word;
begin
     wKey := DecodePassword(StrToHex(Psw));
     SetLength(Result, Length(S));
     for i := 1 to Length(S) do
     begin
         Result[i] := Char(Byte(S[i]) xor (wKey shr 8));
         wKey := (Byte(Result[i]) + wKey) * cniC1Const + cniC2Const;
     end;
end;


// Decoding
function DeCrypt(const S, Psw: String): String;
var
   i    : Byte;
   wKey : Word;
begin
     wKey := DecodePassword(StrToHex(Psw));
     SetLength(Result, Length(S));
     for i := 1 to Length(S) do
     begin
          Result[i] := Char(Byte(S[i]) xor (wKey shr 8));
          wKey := (Byte(S[i]) + wKey) * cniC1Const + cniC2Const;
     end;
end;


// Password coding
function DecodePassword(const Psw: String): Word;
var
   i : Integer;
begin
     Result := 0;
     for i := 1 to Length(Psw) do
     begin
          Result := Result + (Ord(Psw[i]) * i);
     end;
end;


// String to hexa code
function StrToHex(const S: String): String;
var
   i : Integer;
begin
     Result := '';
     for i := 1 to Length(S) do
     begin
          Result := Result + IntToHex(Ord(S[i]), 2);
     end;
end;


// Hexa code to String
function HexToStr(const HexStr: String): String;
var
   i    : Integer;
   sTmp : String;
begin
     Result := '';
     for i := 1 to Length(HexStr) do
     begin
          sTmp := sTmp + HexStr[i];
          if not Odd(i) then
          begin
               Result := Result + Chr(StrToInt('$' + sTmp));
               sTmp   := '';
          end;
     end;
end;


function DecToRoman(const DecNumber: Word): String;
begin
     Result := '';
     if (DecNumber > 0) and (DecNumber < 21) then Result := caRomanNumber[DecNumber];
end;


function RomanToDec(const RomanStr: String): Word;
var
   i : Integer;
begin
     Result := 0;
     for i := 1 to 20 do
     begin
          if caRomanNumber[i] = RomanStr then
          begin
               Result := i;
               Break;
          end;
     end;
end;


// Font conversion to String
function FontToStr(Font: TFont): String;
const
     cnsSubSeparator = '~';
var
   sTmp : String;
begin
     Result := '';
     if not (Font = nil) then
     begin
          sTmp := IntToStr(Font.CharSet);
          sTmp := sTmp + cnsSubSeparator + IntToStr(Font.Color);
          sTmp := sTmp + cnsSubSeparator + IntToStr(Font.Height);
          sTmp := sTmp + cnsSubSeparator + Font.Name;
          sTmp := sTmp + cnsSubSeparator + IntToStr(Font.Size);

          case Font.Pitch of
               fpDefault  : sTmp := sTmp + cnsSubSeparator + 'D';
               fpVariable : sTmp := sTmp + cnsSubSeparator + 'V';
               fpFixed    : sTmp := sTmp + cnsSubSeparator + 'F';
          end;
          sTmp := sTmp + cnsSubSeparator;
          sTmp := sTmp + IIF(fsBold      in Font.Style, 'B', 'N');
          sTmp := sTmp + IIF(fsItalic    in Font.Style, 'I', 'N');
          sTmp := sTmp + IIF(fsUnderLine in Font.Style, 'U', 'N');
          sTmp := sTmp + IIF(fsStrikeOut in Font.Style, 'S', 'N');
          Result := sTmp;
     end;
end;


// String conversion to Font
function StrToFont(S: String): TFont;
const
     cnsDefFont      = '0/0/-11/MS Sans Serif/8/D/NNNN';
     cnsSubSeparator = '~';
var
   sTmp : String;
   iPos : Integer;
begin
     if S = '' then S := cnsDefFont;  // Üres a font
     Result := TFont.Create;
     iPos := Pos(cnsSubSeparator, S);
     sTmp := Copy(S, 1, iPos-1);
     Result.CharSet := StrToInt(sTmp);
     Delete(S, 1, iPos);

     iPos := Pos(cnsSubSeparator, S);
     sTmp := Copy(S, 1, iPos-1);
     Result.Color := StrToInt(sTmp);
     Delete(S, 1, iPos);

     iPos := Pos(cnsSubSeparator, S);
     sTmp := Copy(S, 1, iPos-1);
     Result.Height := StrToInt(sTmp);
     Delete(S, 1, iPos);

     iPos := Pos(cnsSubSeparator, S);
     sTmp := Copy(S, 1, iPos-1);
     Result.Name := sTmp;
     Delete(S, 1, iPos);

     iPos := Pos(cnsSubSeparator, S);
     sTmp := Copy(S, 1, iPos-1);
     Result.Size := StrToInt(sTmp);
     Delete(S, 1, iPos);

     iPos := Pos(cnsSubSeparator, S);
     sTmp := Copy(S, 1, iPos-1);
     case sTmp[1] of
          'D' : Result.Pitch := fpDefault;
          'V' : Result.Pitch := fpVariable;
          'F' : Result.Pitch := fpFixed;
     end;
     Delete(S, 1, iPos);

     Result.Style := [];
     sTmp := S;
     if sTmp[1]='B' then Result.Style := Result.Style + [fsBold];
     if sTmp[2]='I' then Result.Style := Result.Style + [fsItalic];
     if sTmp[3]='U' then Result.Style := Result.Style + [fsUnderLine];
     if sTmp[4]='S' then Result.Style := Result.Style + [fsStrikeOut];
end;


// String conversion to StringList
procedure StrToStrings(const S: String; List: TStrings; Separator: String = '|');
var
   sTmp : String;
   i    : Integer;
begin
     sTmp := '';
     for i := 1 to Length(S) do
     begin
          if S[i] = Separator then
          begin
               List.Add(Trim(sTmp));
               sTmp := '';
          end
          else
          begin
               if not (S = ' ') then sTmp := sTmp + S[i];
          end;
     end;
     if not (sTmp = '') then List.Add(Trim(sTmp));
end;


// StringList conversion to String
function StringsToStr(const List: TStrings; Separator: String = '|'; AddEmpty: Boolean = True): String;
var
   sTmp : String;
   i    : Integer;
begin
     sTmp  := '';
     for i := 0 to List.Count - 1 do
     begin
          if AddEmpty then sTmp := sTmp + Separator + List.Strings[i] else
          begin
               if List.Strings[i] <> '' then sTmp := sTmp + Separator + List.Strings[i];
          end;
     end;
     Result := sTmp;
end;


// Logical data conversion to String
function LogToStr(Log: Boolean): String;
begin
     Result := IIF(Log, 'True', 'False');
end;


// String data conversion to Logical data
function StrToLog(const S: String): Boolean;
begin
     if S = '' then Result := False else Result := IsSub(AnsiUpperCase(S), 'T,Y,1,TRUE');
end;


{******************************************************************************}


// String value back in one line
function IIF(Condition : Boolean; Alfa, Beta: String): String;
begin
     if Condition then Result := Alfa else Result := Beta;
end;


// Integer value back in one line
function IIF(Condition : Boolean; Alfa, Beta: Integer): Integer;
begin
     if Condition then Result := Alfa else Result := Beta;
end;


// Font value back in one line
function IIF(Condition : Boolean; Alfa, Beta: TFont): TFont;
begin
     if Condition then Result := Alfa else Result := Beta;
end;


// DateTime value back in one line
function IIF(Condition : Boolean; Alfa, Beta: TDateTime): TDateTime; overload;
begin
     if Condition then Result := Alfa else Result := Beta;
end;


// Conversion between WindowState <=> integer
function WindowStateToInt(State: TWindowState): Integer;
begin
     Result := 2; // Default the NORMAL
     case State of
          wsMinimized : Result := 1;
          wsNormal    : Result := 2;
          wsMaximized : Result := 3;
     end;
end;


// Conversion between integer <=> WindowState
function IntToWindowState(State: Integer): TWindowState;
begin
     Result := wsNormal;
     case State of
          1: Result := wsMinimized;
          2: Result := wsNormal;
          3: Result := wsMaximized;
     end;
end;


// Conversion between Align <=> Integer
function AlignToInt(Align: TAlignment): Integer;
begin
     Result := 0;
     case Align of
          taLeftJustify  : Result := 0;
          taCenter       : Result := 1;
          taRightJustify : Result := 2;
     end;
end;


// Conversion between Integer <=> Align
function IntToAlign(Align: Integer): TAlignment;
begin
     Result := taLeftJustify;
     case Align of
          0 : Result := taLeftJustify;
          1 : Result := taCenter;
          2 : Result := taRightJustify;
     end;
end;


// Conversion between Align <=> String
function AlignToStr(Align: TAlignment): String;
begin
     Result := 'L';
     case Align of
          taLeftJustify  : Result := 'L';
          taCenter       : Result := 'C';
          taRightJustify : Result := 'R';
     end;
end;


// Conversion between Integer <=> Align
function StrToAlign(const Align: String): TAlignment;
begin
     Result := taLeftJustify;
     if Align = 'L' then Result := taLeftJustify;
     if Align = 'C' then Result := taCenter;
     if Align = 'R' then Result := taRightJustify;
end;


// Extract the table name
function ExtractTable(const Item: String; Sign: String = '.'): String;
var
   sTmp : String;
begin
     sTmp   := ClearTableName(Item);
     Result := sTmp;
     // Table name contains space and other spec char
     if LeftStr(sTmp, 1) = '"' then
     begin
          Result := Copy(sTmp, 2, Length(sTmp));
          Result := Copy(Result, 1, Pos('"', Result)-1);
     end
     else
     begin
          if IsSub(Sign, sTmp) then Result := Copy(sTmp, 1, Pos(Sign, sTmp)-1);
     end;
end;


// Extract the field name
function ExtractField(const Item: String; Sign: String = '.'): String;
var
   sTmp : String;
begin
     sTmp   := ClearTableName(Item);
     Result := sTmp;
     if LeftStr(sTmp, 1) = '"' then
     begin
          Result := Copy(sTmp, 2, Length(sTmp));
          Result := Copy(Result, Pos('"', Result)+1, Length(Result));
          if IsSub(Sign, Result) then Result := Copy(Result, Pos(Sign, Result)+1, Length(Result)) else Result := sTmp;
     end
     else
     begin
          if IsSub(Sign, sTmp) then Result := Copy(sTmp, Pos(Sign, sTmp)+1, Length(sTmp));
     end;
end;


{
function IsPrimaryKey(const FieldName: String): Boolean;
begin
     Result := False;
     if RightStr(AnsiLowerCase(FieldName), 3) = SKeyField then
     begin
          Result := (StrCount(FieldName, '_') = 1);
     end;
end;
}


// Clear the tablename
function ClearTableName(const TableName: String): String;
begin
     Result := TableName;
     if IsSub('dbo.', TableName) then Result := Copy(TableName, Pos('dbo.', TableName) + 4, Length(TableName));
end;


// Create IN condition
function CreateINCondition(const FieldName, Values: String; UseStr: Boolean = True): String;
var
   sTmp : String;
   oTmp : TStringList;
   i    : Integer;
begin
     sTmp := StrTran(Values, ';', ',');
     sTmp := StrTran(sTmp, ' ', ',');
     oTmp := TStringList.Create;
     try
     begin
          StrToStrings(sTmp, oTmp, ',');
          Result := FieldName + ' IN (';
          for i := 0 to oTmp.Count-1 do
          begin
               if UseStr
               then
                   Result := Result + ('''' + oTmp.Strings[i] + ''', ')
               else
                   Result := Result + (oTmp.Strings[i] + ', ');
          end;
          Result := LeftStr(Result, Length(Result)-2) + ')';
     end;
     finally
          oTmp.Free;
     end;
end;


// Extract Year, Month, Day from a date
function ExtractYear(Date: TDateTime): Word;
var
   wYear  : Word;
   wMonth : Word;
   wDay   : Word;
begin
     DecodeDate(Date, wYear, wMonth, wDay);
     Result := wYear;
end;


function ExtractMonth(Date: TDateTime): Word;
var
   wYear  : Word;
   wMonth : Word;
   wDay   : Word;
begin
     DecodeDate(Date, wYear, wMonth, wDay);
     Result := wMonth;
end;


function ExtractDay(Date: TDateTime): Word;
var
   wYear  : Word;
   wMonth : Word;
   wDay   : Word;
begin
     DecodeDate(Date, wYear, wMonth, wDay);
     Result := wDay;
end;


function ExtractDigits(const S: String; DefStr: String = '0'): String;
var
   i : Integer;
begin
     Result := '';
     if S <> '' then
     begin
          for i := 1 to Length(S) do
          begin
               if Pos(S[i], '0123456789') > 0 then
               begin
                    Result := Result + S[i];
               end;
          end;
     end;
     if Result = '' then Result := DefStr;
end;


function ExtractText(const Str: String; aLast: array of String): String;
var
   iPos : Integer;
   i    : Integer;
begin
     Result := Str;
     for i := 0 to High(aLast)-1 do
     begin
          iPos := Pos(AnsiUpperCase(aLast[i]), AnsiUpperCase(Str));
          if iPos > 0 then Result := Copy(Str, 1, iPos-1);
     end;
end;


function ExtractWhere(const SQLText: String): String;
var
   iPos : Integer;
begin
     Result := '';
     iPos := Pos('WHERE', AnsiUpperCase(SQLText));
     if iPos > 0 then
     begin
          Result := Copy(SQLText, iPos + 6, Length(SQLText));
          Result := ExtractText(Result, ['GROUP BY',
                                         'HAVING',
                                         'ORDER BY',
                                         'COMPUTE',
                                         'FOR BROWSE']);
     end;
     Result := Trim(Equalize(StrTran(Result, #$D#$A)));
end;


function ExtractTableName(const SQLText: String; MainTable: Boolean = True): String;
var
   iPos : Integer;
begin
     Result := '';
     iPos := Pos('FROM', AnsiUpperCase(SQLText));
     if iPos > 0 then
     begin
          Result := Copy(SQLText, iPos + 5, Length(SQLText));
          if MainTable
          then
              Result := ExtractText(Result,[',',
                                            'WHERE',
                                            'GROUP BY',
                                            'HAVING',
                                            'ORDER BY',
                                            'COMPUTE',
                                            'FOR BROWSE'])
          else
              Result := ExtractText(Result,['WHERE',
                                            'GROUP BY',
                                            'HAVING',
                                            'ORDER BY',
                                            'COMPUTE',
                                            'FOR BROWSE']);
     end;
     Result := Trim(Equalize(StrTran(Result, #$D#$A)));
end;


procedure ExtractFieldList(const SQLText: String; List: TStrings);
var
   iPos : Integer;
   sTmp : String;
   i    : Integer;
   oTab : TStringList;
   sTab : String;
   sFld : String;
begin
     sTmp := '';
     iPos := Pos('SELECT', AnsiUpperCase(SQLText));
     if iPos > 0 then
     begin
          sTmp := Copy(SQLText, iPos + 7, Length(SQLText));
          sTmp := Copy(sTmp, 1, Pos('FROM', sTmp)-1);
          sTmp := StrTran(sTmp, '"' + #$D#$A);
          List.Clear;
          if Trim(sTmp) <> '*' then
          begin
               List.CommaText := sTmp;
               oTab := TStringList.Create;
               try
               begin
                    StrToStrings(ExtractTableName(SQLText, False), oTab, ',');
                    for i := 0 to oTab.Count-1 do
                    begin
                         oTab.Strings[i] := StrTran(oTab.Strings[i], ' ', '=');
                         oTab.Strings[i] := oTab.Values[oTab.Names[i]] + '=' + oTab.Names[i];
                    end;
                    for i := 0 to List.Count-1 do
                    begin
                    sTmp := List.Strings[i];
                         if IsSub('.', sTmp) then
                         begin
                              iPos := oTab.IndexOfName(Copy(sTmp, 1, Pos('.', sTmp) - 1));
                              if iPos > -1 then
                              begin
                                   sTab := oTab.Values[oTab.Names[iPos]];
                                   sFld := Copy(sTmp, Pos('.', sTmp) + 1, Length(sTmp));
                                   List.Strings[i] := sFld + '=' + sTab + '.' + sFld;
                              end;
                         end
                         else List.Strings[i] := sTmp + '=' + oTab.Values[oTab.Names[0]] + '.' + sTmp;
                    end;
               end;
               finally
                    oTab.Free;
               end;
          end;
     end;
end;


// Clean Text
function CleanText(const S: String): String;
begin
     Result := Trim(Equalize(StrTran(S, #$D#$A, ' ')));
end;


// Clean Number
function CleanNumber(const S: String): String;
begin
     Result := StrTran(S, CurrencyString);
     Result := StrTran(Result, ThousandSeparator);
     if Result = '' then Result := '0';
end;


// Calculate EuroTime
function EuroTime(T: TDateTime): TDateTime;
var
   wHour : Word;
   wMin  : Word;
   wSec  : Word;
   wMSec : Word;
begin
     DecodeTime(T, wHour, wMin, wSec, wMSec);
     if wHour <= 15 then wHour := wHour + 9 else wHour := wHour - 15;
     if wHour = 24 then wHour := 0;
     Result := EncodeTime(wHour, wMin, wSec, wMSec);
end;


function DateText(D: TDateTime): String;
var
   wYear  : Word;
   wMonth : Word;
   wDay   : Word;
begin
     DecodeDate(D, wYear, wMonth, wDay);
     Result := LongMonthNames[wMonth] + ' ' +
               IntToStr(wDay) + ', ' +
               IntToStr(wYear) + '  (' +
               LongDayNames[DayOfWeek(D)] + ')';
end;


function DateToStd(D: TDateTime; Separator: String = ''): String;
var
   xTmp : TTimeStamp;
begin
     Result := '';
     xTmp := DateTimeToTimeStamp(D);
     if xTmp.Date > 0 then Result := FormatDateTime('yyyy-mm-dd', D);
     if xTmp.Time > 0 then Result := IIF(Result = '', '', Result + ' ') + FormatDateTime('hh:nn:ss AM/PM', D);
end;


function StdToDate(const S: String): TDateTime;
var
   sTmp   : String;
   sYear  : String;
   wYear  : Word;
   sMonth : String;
   wMonth : Word;
   sDay   : String;
   wDay   : Word;
   xDate  : TTimeStamp;
   xTime  : TTimeStamp;
   sTime  : String;
begin
     Result := EncodeDate(1900, 1, 1);
     if S <> '' then
     begin
          sTmp   := ExtractDigits(S);
          sYear  := SubStr(sTmp, 1, 4);
          if sYear = '' then sYear := '1900';
          wYear  := StrToInt(sYear);
          sMonth := SubStr(sTmp, 5, 2);
          if sMonth = '' then sMonth := '1';
          wMonth := StrToInt(sMonth);
          sDay   := SubStr(sTmp, 7, 2);
          if sDay = '' then sDay := '1';
          wDay   := StrToInt(sDay);

          sTime  := '';
          if Length(S) > 10 then sTime := Copy(S, Pos(' ', S) + 1, Length(S));
          xDate := DateTimeToTimeStamp(EncodeDate(wYear, wMonth, wDay));
          if sTime <> '' then
          begin
               xTime := DateTimeToTimeStamp(StrToTime(sTime));
               xDate.Time := xTime.Time;
          end;
          Result := TimeStampToDateTime(xDate);
     end;
end;


// Get the last item of StringList
function GetLastItem(Items: TStrings): String;
begin
     Result := '';
     if (Items.Count > 0) then Result := Items.Strings[ Items.Count-1 ];
end;


// Set the last item of StringList
procedure SetLastItem(Items: TStrings; const S: String);
begin
     if (Items.Count > 0) then Items.Strings[ Items.Count-1 ] := S;
end;


// Field type conversion
function FieldTypeToText(DataType: TFieldType): String;
begin
     Result := 'Unknown or undetermined';
     case DataType of
          ftString     : Result := 'Character or string field';
          ftSmallint   : Result := '16-bit integer field';
          ftInteger    : Result := '32-bit integer field';
          ftWord       : Result := '16-bit unsigned integer field';
          ftBoolean    : Result := 'Boolean field';
          ftFloat      : Result := 'Floating-point numeric field';
          ftCurrency   : Result := 'Money field';
          ftBCD	       : Result := 'Binary-Coded Decimal field as Currency';
          ftDate       : Result := 'Date field';
          ftTime       : Result := 'Time field';
          ftDateTime   : Result := 'Date and time field';
          ftBytes      : Result := 'Fixed number of bytes (binary storage)';
          ftVarBytes   : Result := 'Variable number of bytes (binary storage)';
          ftAutoInc    : Result := 'Auto-incrementing 32-bit integer counter field';
          ftBlob       : Result := 'Binary Large OBject field';
          ftMemo       : Result := 'Text memo field';
          ftGraphic    : Result := 'Bitmap field';
          ftFmtMemo    : Result := 'Formatted text memo field';
          ftParadoxOle : Result := 'Paradox OLE field';
          ftDBaseOle   : Result := 'dBASE OLE field';
          ftTypedBinary: Result := 'Typed binary field';
          ftCursor     : Result := 'Output cursor from an Oracle stored procedure (TParam only)';
          ftFixedChar  : Result := 'Fixed character field';
          ftWideString : Result := 'Wide string field';
          ftLargeInt   : Result := 'Large integer field';
          ftADT	       : Result := 'Abstract Data Type field';
          ftArray      : Result := 'Array field';
          ftReference  : Result := 'REF field';
          ftDataSet    : Result := 'DataSet field';
     end;
end;


function FieldTypeToText(Field: TField): String;
begin
     Result := FieldTypeToText(Field.DataType);
end;


procedure FieldTypeToList(List: TStrings);
begin
     List.Clear;
     List.Add('ftUnknown');
     List.Add('ftString');
     List.Add('ftSmallint');
     List.Add('ftInteger');
     List.Add('ftWord');
     List.Add('ftBoolean');
     List.Add('ftFloat');
     List.Add('ftCurrency');
     List.Add('ftBCD');
     List.Add('ftDate');
     List.Add('ftTime');
     List.Add('ftDateTime');
     List.Add('ftBytes');
     List.Add('ftVarBytes');
     List.Add('ftAutoInc');
     List.Add('ftBlob');
     List.Add('ftMemo');
     List.Add('ftGraphic');
     List.Add('ftFmtMemo');
     List.Add('ftParadoxOle');
     List.Add('ftDBaseOle');
     List.Add('ftTypedBinary');
     List.Add('ftCursor');
     List.Add('ftFixedChar');
     List.Add('ftWideString');
     List.Add('ftLargeInt');
     List.Add('ftADT');
     List.Add('ftArray');
     List.Add('ftReference');
     List.Add('ftDataSet');
end;


function FieldTypeToStr(DataType: TFieldType): String;
begin
     Result := 'Default';
     case DataType of
          ftString     : Result := 'String';
          ftSmallint   : Result := 'Smallint';
          ftInteger    : Result := 'Integer';
          ftWord       : Result := 'Word';
          ftBoolean    : Result := 'Boolean';
          ftFloat      : Result := 'Float';
          ftCurrency   : Result := 'Currency';
          ftBCD	       : Result := 'BCD';
          ftDate       : Result := 'Date';
          ftTime       : Result := 'Time';
          ftDateTime   : Result := 'DateTime';
          ftBytes      : Result := 'Bytes';
          ftVarBytes   : Result := 'VarBytes';
          ftAutoInc    : Result := 'AutoInc';
          ftBlob       : Result := 'Blob';
          ftMemo       : Result := 'Memo';
          ftGraphic    : Result := 'Graphic';
          ftFmtMemo    : Result := 'FmtMemo';
          ftParadoxOle : Result := 'ParadoxOle';
          ftDBaseOle   : Result := 'DBaseOle';
          ftTypedBinary: Result := 'TypedBinary';
          ftCursor     : Result := 'Cursor';
          ftFixedChar  : Result := 'FixedChar';
          ftWideString : Result := 'WideString';
          ftLargeInt   : Result := 'LargeInt';
          ftADT	       : Result := 'ADT';
          ftArray      : Result := 'Array';
          ftReference  : Result := 'Reference';
          ftDataSet    : Result := 'DataSet';
     end;
end;


function FieldTypeToStr(Field: TField): String;
begin
     Result := FieldTypeToStr(Field.DataType);
end;


function StrToFieldType(const DataType: String): TFieldType;
begin
     Result := ftUnknown;
     if DataType = 'String'      then Result := ftString;
     if DataType = 'Smallint'    then Result := ftSmallInt;
     if DataType = 'Integer'     then Result := ftInteger;
     if DataType = 'Word'        then Result := ftWord;
     if DataType = 'Boolean'     then Result := ftBoolean;
     if DataType = 'Float'       then Result := ftFloat;
     if DataType = 'Currency'    then Result := ftCurrency;
     if DataType = 'BCD'         then Result := ftBCD;
     if DataType = 'Date'        then Result := ftDate;
     if DataType = 'Time'        then Result := ftTime;
     if DataType = 'DateTime'    then Result := ftDateTime;
     if DataType = 'Bytes'       then Result := ftBytes;
     if DataType = 'VarBytes'    then Result := ftVarBytes;
     if DataType = 'AutoInc'     then Result := ftAutoInc;
     if DataType = 'Blob'        then Result := ftBlob;
     if DataType = 'Memo'        then Result := ftMemo;
     if DataType = 'Graphic'     then Result := ftGraphic;
     if DataType = 'FmtMemo'     then Result := ftFmtMemo;
     if DataType = 'ParadoxOle'  then Result := ftParadoxOle;
     if DataType = 'DBaseOle'    then Result := ftDBaseOle;
     if DataType = 'TypedBinary' then Result := ftTypedBinary;
     if DataType = 'Cursor'      then Result := ftCursor;
     if DataType = 'FixedChar'   then Result := ftFixedChar;
     if DataType = 'WideString'  then Result := ftWideString;
     if DataType = 'LargeInt'    then Result := ftLargeInt;
     if DataType = 'ADT'         then Result := ftADT;
     if DataType = 'Array'       then Result := ftArray;
     if DataType = 'Reference'   then Result := ftReference;
     if DataType = 'DataSet'     then Result := ftDataSet;
end;


function VarToText(const V: Variant): String;
begin
     Result := 'Error';
     case VarType(V) of
          varEmpty    : Result := 'Empty';
          varNull     : Result := 'Null';
          varSmallint : Result := 'Smallint';
          varInteger  : Result := 'Integer';
          varSingle   : Result := 'Single';
          varDouble   : Result := 'Double';
          varCurrency : Result := 'Curreny';
          varDate     : Result := 'Date';
          varOleStr   : Result := 'OleStr';
          varDispatch : Result := 'Dispatch';
          varError    : Result := 'Error';
          varBoolean  : Result := 'Boolean';
          varVariant  : Result := 'Variant';
          varUnknown  : Result := 'Unknown';
          varByte     : Result := 'Byte';
          varString   : Result := 'String';
          varTypeMask : Result := 'TypeMask';
          varArray    : Result := 'Array';
          varByRef    : Result := 'ByRef';
     end;
end;


// Get the EXE Dir
function GetExeDir: String;
begin
     Result := ExtractFilePath(ParamStr(0));
end;


// Get the WORK Dir
function GetWorkDir(SubDir: String = 'Source'): String;
var
   iTmp : Integer;
   sTmp : String;
begin
     GetDir(0, sTmp);
     iTmp := Pos(SubDir, AnsiUpperCase(sTmp));
     if iTmp > 0 then sTmp := Copy(sTmp, 1, iTmp-1);
     Result := IIF(RightStr(sTmp, 1) = '\', sTmp, sTmp + '\');
end;


// Get the Enviroment
function GetEnviroment(const EnvName: String): String;
var
   sTmp : String;
   pTmp : PChar;
   pEnv : PChar;
begin
     sTmp := '';
     pTmp := StrAlloc(251);                         // Foglalok helyet
     pEnv := StrAlloc(100);
     StrPCopy(pEnv, EnvName);
     GetEnvironmentVariable(pEnv, pTmp, 250);       // SET <Prognev> beolvasása
     sTmp := AnsiUpperCase(StrPas(pTmp));         // Átalakitom Pascal Stringre
     StrDispose(pTmp);                              // Felszabaditom
     StrDispose(pEnv);
     Result := sTmp;
end;


// Delete all file and directory
function DeleteFiles(const FileSpec: String): Boolean;
var
   oFile : TStringList;
   i     : Integer;
begin
     Result := False;
     oFile := TStringList.Create;
     try
     begin
          DirToStrings(FileSpec, oFile, dfFull);
          for i := 0 to oFile.Count-1 do
          begin
               DeleteFile(oFile.Strings[i]);
               Result := True;
          end;
     end;
     finally
          oFile.Free;
     end;
end;


// File from a directory to Strings
procedure DirToStrings(const FileSpec: String; List: TStrings; Part: TDirFile; SubDir: Boolean = False);
var
   recSearch : TSearchRec;
   oDir      : TStringList;
   sTmp      : String;
   sExt      : String;
   i         : Integer;
   j         : Integer;
begin
     // Collect directories
     oDir := TStringList.Create;
     try
     begin
          sExt := Copy(FileSpec, RPos(FileSpec, '\', 0), Length(FileSpec));
          oDir.Clear;
          oDir.Add(FileSpec);  // Searching in the base dir

          if SubDir then  // if need the subdirs
          begin
               // Search in the subdirs
               i := FindFirst(FileSpec, faDirectory, recSearch);
               while i = 0 do
               begin
                    if not (recSearch.Name[1] = '.') then
                       oDir.Add(ExtractFilePath(FileSpec) + recSearch.Name + '\' + sExt);
                    i := FindNext(recSearch);
               end;
               FindClose(recSearch);
          end;

          // Read it one by one
          for j := 0 to oDir.Count-1 do
          begin
               i := FindFirst(oDir.Strings[j], faAnyFile, recSearch);
               while i = 0 do
               begin
                    sTmp := '';
                    if recSearch.Attr <> faDirectory then
                    begin
                         case Part of
                              dfName: sTmp := Copy(recSearch.Name, 1, Pos('.', recSearch.Name)-1);
                              dfFile: sTmp := recSearch.Name;
                              dfFull: sTmp := ExtractFilePath(oDir.Strings[j]) + recSearch.Name;
                         end;
                         if (sTmp <> '') and
                            (RightStr(sTmp, 1)  <> '.') and
                            (ExtractName(sTmp) <> '') then List.Add(sTmp);
                    end;
                    i := FindNext(recSearch);
               end;
               FindClose(recSearch);
          end;
     end;
     finally
          oDir.Free;
     end;
end;


// File number and size back
function FileNumbers(const FileSpec: String; SubDir, Size: Boolean): Integer;
var
   recSearch : TSearchRec;
   oDir      : TStringList;
   sExt      : String;
   i         : Integer;
   j         : Integer;
   iTmp      : Integer;
begin
     iTmp := 0;
     // Könyvtárak begyüjtése
     oDir := TStringList.Create;
     try
     begin
          sExt := Copy(FileSpec, RPos(FileSpec, '\', 0), Length(FileSpec));
          oDir.Clear;
          oDir.Add(FileSpec);  // Searching in the base dir

          if SubDir then  // if need the subdirs
          begin
               // Search in the subdirs
               i := FindFirst(FileSpec, faDirectory, recSearch);
               while i = 0 do
               begin
                    if not (recSearch.Name[1] = '.') then
                       oDir.Add(ExtractFilePath(FileSpec) + recSearch.Name + '\' + sExt);
                    i := FindNext(recSearch);
               end;
               FindClose(recSearch);
          end;

          // Read it one by one
          for j := 0 to oDir.Count-1 do
          begin
               i := FindFirst(oDir.Strings[j], faAnyFile, recSearch);
               while i = 0 do
               begin
                    if Size // Ha az összméret a kérdés
                    then
                        iTmp := iTmp + recSearch.Size
                    else
                        iTmp := iTmp + 1;

                    i := FindNext(recSearch);
               end;
               FindClose(recSearch);
          end;
     end;
     finally
          oDir.Free;
     end;
     Result := iTmp;
end;


// only the file name without extension and path
function ExtractName(const FileName: String): String;
var
   sTmp : String;
begin
     sTmp := ExtractFileName(FileName);
     if IsSub('.', sTmp)
     then
         Result := Copy(sTmp, 1, Pos('.', sTmp)-1)
     else
         Result := sTmp;
end;


// Get the info from the Volume
function GetVolumeID(DriveName : char) : DWord;
var
  dwTemp1 : DWord;
  dwTemp2 : DWord;
begin
     GetVolumeInformation(PChar(DriveName + ':\'), Nil, 0, @Result, dwTemp1, dwTemp2, Nil, 0);
end;


// Execute the application: File-name, Parameters (pl. file name what want to open), start dir,
// window size: (1: normal, 2: icon state, 3: maximal window
function ExecuteProgram(const FileName, StartParameters, DefaultDir: String; WindowState: TWindowState): THandle;
var
   acFileName,
   acParams,
   acDir : array[0..127] of Char;
   ShowCmd : Integer;
begin
     ShowCmd := 1;
     case WindowState of
          wsNormal    : ShowCmd := 1;
          wsMinimized : ShowCmd := 2;
          wsMaximized : ShowCmd := 3;
     end;
     Result := ShellExecute(Application.MainForm.Handle, nil,
                             StrPCopy(acFileName, FileName),
                             StrPCopy(acParams, StartParameters),
                             StrPCopy(acDir, DefaultDir), ShowCmd);
end;


function IsDelphiRun: boolean;
var
   H1, H2, H3, H4 : Hwnd;
const
     A1 : array[0..12] of char = 'TApplication'#0;
     A2 : array[0..15] of char = 'TAlignPalette'#0;
     A3 : array[0..18] of char = 'TPropertyInspector'#0;
     A4 : array[0..11] of char = 'TAppBuilder'#0;
     T1 : array[0..8]  of char = 'Delphi 4'#0;
begin
     H1 := FindWindow(A1, T1);
     H2 := FindWindow(A2, nil);
     H3 := FindWindow(A3, nil);
     H4 := FindWindow(A4, nil);
     Result := (H1 <> 0) and (H2 <> 0) and (H3 <> 0) and (H4 <> 0);
end;


{******************************************************************************}


function GetADOPart(const ADOStr, Section: String): String;
var
   iPos : Integer;
begin
     Result := '';
     iPos := Pos(Section, ADOStr);
     if iPos > 0 then
     begin
          Result := Copy(ADOStr, iPos + Length(Section) + 1, Length(ADOStr));
          iPos := Pos(';', Result);
          if iPos > 0 then Result := Trim(Copy(Result, 1, iPos - 1));
     end;
end;


function SetADOPart(const ADOStr, Section, Value: String; AutoAdd: Boolean = True): String;
var
   oTmp : TStringList;
   iPos : Integer;
begin
     oTmp := TStringList.Create;
     try
     begin
          StrToStrings(Trim(ADOStr), oTmp, ';');
          iPos := oTmp.IndexOfName(Section);
          if iPos = -1 then
          begin
               if AutoAdd then oTmp.Add(Section + '=' + Trim(Value));
          end
          else oTmp.Strings[iPos] := Section + '=' + Trim(Value);
          Result := StringsToStr(oTmp, ';', False);
          if LeftStr(Result, 1) = ';' then Result := RightStr(Result, Length(Result)-1);
     end;
     finally
          oTmp.Free;
     end;
end;


end.
