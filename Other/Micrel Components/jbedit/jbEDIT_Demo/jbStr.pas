{*******************************************************}
{                                                       }
{       Basic Operation for string manipulation         }
{       Ver 1.49                                        }
{                                                       }
{       Copyright (C) 1991-2013 Jaro Benes              }
{       All right reserved                              }
{                                                       }
{       E-mail: micrel@micrel.cz                        }
{       WWW home: http://www.micrel.cz/delphi/          }
{                                                       }
{*******************************************************}
unit jbStr;
{$INCLUDE  jb.inc}
interface

uses SysUtils, Classes, Windows
  {$IFDEF CLR}, System.IO{$ELSE}
  {$IFDEF UNICODE}, AnsiStrings, Character{$ENDIF}{$ENDIF};
{
Actualization:
--------------
31.01.2013 Added function TestEndTo() and variations
10.04.2012 Change identifier JustName to ExtractNameOfFileOnly
29.02.2012 Revision inline code for speed optimalization
10.11.2011 Revision code for Center procedure
09.09.2011 Revision for .NET BDS 2007
15.08.2011 Revision TestTo() for case sensitive
09.01.2011 Add simple xml function as helper
18.08.2010 Revision code to port Delphi for .Net
22.04.2010 Reapir of functions works with words
24.11.2009 Revision functions and port to Unicode Delphi v. 2.0
16.10.2007 Replace old DOS file functions to SysUtils synonimic functions
13.04.2007 New function added (FormatStr4CGI)
15.08.2006 New function added (SwitchTo)
22.01.2004 Function RemoveCSPDiacritics added
11.04.2003 Replace function added
09.07.2002 Fixbug FORM function (Real number formatting)
21.03.2002 New actualization of source
21.01.2001 New acualize of source
28.08.2000 FixBug on all LongStrings
27.08.2000 Fixbug in Form function
}

{$LONGSTRINGS ON}

{$IFDEF CLR}
{$ELSE}
  {$IFDEF UNICODE}
    {$DEFINE WINUC}
  {$ENDIF}
{$ENDIF}

type
{$IFNDEF VER4UP}
  TSysCharSet = set of Char;
{$ENDIF}
  CharSet = {$IFNDEF CLR}TSysCharSet{$ELSE}set of AnsiChar{$ENDIF};
  TWordOperation = (woUpper, woLower, woCapitalize, woTrim, woLeftTrim, woRightTrim);
  TWordOperations = set of TWordOperation;

function AddLastChar(const DirName: string; C: Char = {$IFDEF CLR}'\'{$ELSE}{$IFDEF VER6UP}Sysutils.PathDelim{$ELSE}'\'{$ENDIF}{$ENDIF}): string;{$IFDEF SUPP_INL}inline;{$ENDIF}
function Alter(Str, AlterStr: string): string; {$IFDEF SUPP_INL}inline;{$ENDIF}
function AlterTo(Str, CondicStr, AlterStr: string): string; {$IFDEF SUPP_INL}inline;{$ENDIF}
function AppendWord(Wrd, S: string; WordDelim: Char = ','): string; {$IFDEF SUPP_INL}inline;{$ENDIF}
function BitIsSet(Num, B: Byte): Boolean; {$IFDEF SUPP_INL}inline;{$ENDIF}
function BitReSet(Num, B: Byte): Byte; {$IFDEF SUPP_INL}inline;{$ENDIF}
function BitSet(Num, B: Byte): Byte; {$IFDEF SUPP_INL}inline;{$ENDIF}
function BitSetToggle(Num, B: Byte): Byte; {$IFDEF SUPP_INL}inline;{$ENDIF}
function CapitalizeWord(const S: string): string;
function Center(const S: string; Width: Integer): string; {$IFDEF SUPP_INL}inline;{$ENDIF}
function CenterCh(const S: string; CH: Char; Width: Integer): string; {$IFDEF SUPP_INL}inline;{$ENDIF}
function Change(S: string; Source, Dest: Char): string; {$IFDEF SUPP_INL}inline;{$ENDIF}
function ChangeFilename(Filename: string; const prefix, postfix: string): string; {$IFDEF SUPP_INL}inline;{$ENDIF}
function ChangeTo(S: string; Source: CharSet; Dest: Char): string; overload; {$IFDEF SUPP_INL}inline;{$ENDIF}
function ChangeTo(S: string; Source: Char; Dest: Char): string; overload; {$IFDEF SUPP_INL}inline;{$ENDIF}
function ChangeWord(N: Integer; const Wrd, S: string; WordDelims: CharSet {$IFNDEF CLR}= [',']{$ENDIF}): string; {$IFDEF SUPP_INL}inline;{$ENDIF}
function ChangeXChars(FindChar, DestChar: Char; const Source: string): string; {$IFDEF SUPP_INL}inline;{$ENDIF}
function CharStr(CH: Char; Len: Integer): string; {$IFDEF SUPP_INL}inline;{$ENDIF}
function CleanDOSFileName(FileName: string): string; {$IFDEF SUPP_INL}inline;{$ENDIF}
function ContractInterval(Value: string; const Delimiter: Char {$IFNDEF CLR}= ','{$ENDIF}): string; {$IFDEF SUPP_INL}inline;{$ENDIF}
function Count(CH: Char; const Dest: string; var Posic, Len: Integer): Boolean; {$IFDEF SUPP_INL}inline;{$ENDIF}
function DefaultExtension(const Name, Ext: string): string; {$IFDEF SUPP_INL}inline;{$ENDIF}
function Detab(const Sx: string; TabSize: Integer): string; {$IFDEF SUPP_INL}inline;{$ENDIF}
function Doc(L: LongInt; const Soustava: Byte): string; {$IFDEF SUPP_INL}inline;{$ENDIF}
function Entab(const Sx: string; TabSize: Integer): string; {$IFDEF SUPP_INL}inline;{$ENDIF}
function CheckIntervalIsSimple(const Value: string): Boolean; {$IFDEF SUPP_INL}inline;{$ENDIF}
function CheckIntervalLimit(const Value: string; var oLow, oHigh: Integer): Boolean; {$IFDEF SUPP_INL}inline;{$ENDIF}
function ExpandInterval(const Value: string; const Delimiter: Char {$IFNDEF CLR}= ','{$ENDIF}; const iLimit: Integer {$IFNDEF CLR}= 1024{$ENDIF}): string; {$IFDEF SUPP_INL}inline;{$ENDIF}
function SplitIntervals(const Value: string; Delimiter: Char {$IFNDEF CLR}= ','{$ENDIF}): string; {$IFDEF SUPP_INL}inline;{$ENDIF}
function ExtractAlphaNum(const S: string): string; {$IFDEF SUPP_INL}{$IFNDEF CLR}inline;{$ENDIF}{$ENDIF}
function ExtractAlphas(const S: string): string; {$IFDEF SUPP_INL}inline;{$ENDIF}
function ExtractChars(const S: string; Chars: CharSet): string; {$IFDEF SUPP_INL}inline;{$ENDIF}
function ExtractNumber(const S: string): string; {$IFDEF SUPP_INL}inline;{$ENDIF}
function ExtractQuoteStr(QuoteStr: string; QuoteChar: Char {$IFNDEF CLR}= '"'{$ENDIF}): string; {$IFDEF SUPP_INL}inline;{$ENDIF}
function ExtractWord(N: Integer; S: string; WordDelims: CharSet {$IFNDEF CLR}= [',']{$ENDIF}): string; overload;
function ExtractWord(N: Integer; S: string; WordDelims: CharSet; Operation: TWordOperations): string; overload;
function ExtractNameOfFileOnly(const PathName: string): string;
function FindWord(what, S: string; WordDelims: CharSet {$IFNDEF CLR}= [',']{$ENDIF}): Boolean; {$IFDEF SUPP_INL}inline;{$ENDIF}
function ForceExtension(const Name, Ext: string): string; {$IFDEF SUPP_INL}inline;{$ENDIF}
function Form(const Mask: string; R: Double; const MaskZipChar: Char = 'X'): string; {$IFDEF VER6UP}deprecated;{$ENDIF}
function FormatStr4CGI(str: string): string; {$IFDEF SUPP_INL}inline;{$ENDIF}
function GetEnd(B: Integer; S: string; WordDelims: CharSet {$IFNDEF CLR}= [',']{$ENDIF}): Integer;
function GetFirstWord(const S: string; WordDelims: CharSet {$IFNDEF CLR}= [',']{$ENDIF}): string; {$IFDEF SUPP_INL}inline;{$ENDIF}
function GetLastWord(const S: string; WordDelims: CharSet {$IFNDEF CLR}= [',']{$ENDIF}): string; {$IFDEF SUPP_INL}inline;{$ENDIF}
function GetPos(B: Integer; S: string; WordDelims: CharSet {$IFNDEF CLR}= [',']{$ENDIF}): Integer;
function HasExtension(const Name: string; var DotPos: Word): Boolean; {$IFDEF SUPP_INL}inline;{$ENDIF}
function Hash(const S: string): LongInt; {$IFDEF SUPP_INL}inline;{$ENDIF}
function htmlSrcEmail(const S: string): string; {$IFNDEF CLR}{$IFDEF SUPP_INL}inline;{$ENDIF}{$ENDIF}
function InsWord(iWord, cWord, cString: string): string; {$IFDEF SUPP_INL}inline;{$ENDIF}
function Int2Roman(Value: Longint): string; {$IFDEF SUPP_INL}inline;{$ENDIF}
function IsNumber(S: string): Boolean; {$IFDEF SUPP_INL}inline;{$ENDIF}
function IsValidEmail(const Value: string): Boolean;
function JoinBoth(PreStr, Delim, PostStr: string): string; {$IFDEF SUPP_INL}inline;{$ENDIF}
function JoinTo(PreStr, Delim, PostStr: string): string; {$IFDEF SUPP_INL}inline;{$ENDIF}
function JustExtension(const PathName: string): string;
function JustFilename(const PathName: string): string;
function JustPathname(const PathName: string): string;
function LeftPad(const S: string; Len: Integer): string; {$IFDEF SUPP_INL}inline;{$ENDIF}
function LeftPadCh(I: integer; Len: Integer): string; overload; {$IFDEF SUPP_INL}inline;{$ENDIF}
function LeftPadCh(S: string; CH: Char; Len: Integer): string; overload; {$IFDEF SUPP_INL}inline;{$ENDIF}
function LoCase(CH: Char): Char;
function Long2Str(L: LongInt): string; {$IFDEF VER6UP}deprecated; {$ENDIF}{alias only} {$IFDEF SUPP_INL}inline;{$ENDIF}
function MakeStr(const S: string; B: Integer): string; {$IFDEF VER6UP}deprecated;{$ENDIF}{alias charstr} {$IFDEF SUPP_INL}inline;{$ENDIF}
function Mask(CharOfMask: Char; const StrMask, Matrice: string; var NextPosic: Integer): string; {$IFDEF SUPP_INL}inline;{$ENDIF}
function Mult(const S: string): Integer; {$IFDEF SUPP_INL}inline;{$ENDIF}
function Num(const S: string; Soustava: Byte): LongInt; {$IFDEF SUPP_INL}inline;{$ENDIF}
{$IFDEF CLR}
{$ELSE}
{$IFDEF UNICODE}
{$ELSE}
{only for non-Unicode and not for .NET}
function PackNum(const S: AnsiString): AnsiString; {$IFDEF SUPP_INL}inline;{$ENDIF}
function UnpackNum(const S: AnsiString): AnsiString; {$IFDEF SUPP_INL}inline;{$ENDIF}
{$ENDIF}
{$ENDIF}
function Pad(const S: string; Len: Integer): string; {$IFDEF SUPP_INL}inline;{$ENDIF}
function PadCh(S: string; CH: Char; Len: Integer): string; {$IFDEF SUPP_INL}inline;{$ENDIF}
function PopWord(B: Integer; var S: string; WordDelims: CharSet {$IFNDEF CLR}= [',']{$ENDIF}): string;
function PosEx(const SubStr, S: string; Offset: Cardinal): Integer; {$IFDEF SUPP_INL}inline;{$ENDIF}
function PosN(Substring, Mainstring: string; occurrence: Integer): Integer; {$IFDEF SUPP_INL}inline;{$ENDIF}
function Push(const Substr: string; PosicInStr: Integer; const Str: string): string; overload; {$IFDEF SUPP_INL}inline;{$ENDIF}
function Push(Posic: Integer; const Source, Dest: string): string; overload;{$IFDEF SUPP_INL}inline; deprecated;{$ENDIF}
function Real2Str(R: Real; Width, Places: Byte): string; {$IFDEF SUPP_INL}inline;{$ENDIF}
function Reduce(const S: string; AboutSize: Integer): string; {$IFDEF SUPP_INL}inline;{$ENDIF}
function RemLastChar(const DirName: string): string; {$IFDEF SUPP_INL}inline;{$ENDIF}
function RemoveCSPDiacritics(S: string): string; {$IFDEF SUPP_INL}inline;{$ENDIF}
function Replace(const NewStr, InputString, OldStr: string): string; {$IFDEF SUPP_INL}inline;{$ENDIF}
function Roman2Int(const S: string): LongInt;
function ShortDirName(Len: Integer; const PName: string): string;
function ShortFileName(Len: Integer; const FName: string): string; {$IFDEF SUPP_INL}inline;{$ENDIF}
function Smash(C: Char; const S: string): string; overload; {$IFDEF SUPP_INL}inline;{$ENDIF}
function Smash(CH: CharSet; const S: string): string; overload; {$IFDEF SUPP_INL}inline;{$ENDIF}
function Space(B: Integer): string; //{$IFDEF VER6UP}deprecated;{$ENDIF} {$IFDEF SUPP_INL}inline;{$ENDIF}
function Split(input: string; splitChar: Char; s: Integer): string; {$IFDEF SUPP_INL}inline;{$ENDIF}
function Str2Int(const S: string; var I: SmallInt): Boolean; {$IFDEF SUPP_INL}inline;{$ENDIF}
function Str2Long(const S: string; var I: LongInt): Boolean; {$IFDEF SUPP_INL}inline;{$ENDIF}
function Str2Real(const S: string; var R: Real): Boolean; {$IFDEF SUPP_INL}inline;{$ENDIF}
function Str2Single(const S: string; var SS: Single): Boolean; {$IFDEF SUPP_INL}inline;{$ENDIF}
function Str2Double(const S: string; var D: Double): Boolean; {$IFDEF SUPP_INL}inline;{$ENDIF}
function Str2Word(const S: string; var I: Word): Boolean; {$IFDEF SUPP_INL}inline;{$ENDIF}
function Str3Long(const S: string): LongInt; {$IFDEF SUPP_INL}inline;{$ENDIF}
function Strip(const Mask, Source: string; MaskZipChar: Char = 'X'): string; {$IFDEF SUPP_INL}inline;{$ENDIF}
function StripChars(S: string; ch: CharSet): string; {$IFDEF SUPP_INL}inline;{$ENDIF}
function StrLoCase(S: string): string; {$IFDEF SUPP_INL}inline;{$ENDIF}
function StrStr(const S: string; krat: Integer): string; {$IFDEF SUPP_INL}inline;{$ENDIF}
function StrUpCase(S: string): string; {$IFDEF SUPP_INL}inline;{$ENDIF}
function StrUpCaseNoCs(S: string): string;
function SwitchIntTo(co: Integer; arg: array of Integer): Integer;
function SwitchTo(co: string; arg: array of string): string;
function TestBeginTo(S: string; arg: array of string): Boolean;
function TestEndTo(const S: string; arg: array of string): Boolean;
function TestFileName(FName: string): Boolean; {$IFDEF SUPP_INL}inline;{$ENDIF}
function TestIntTo(N: Integer; arg: array of Integer): Boolean;
function TestTo(S: string; SArr: array of string; CaseSensitive: Boolean {$IFNDEF CLR}= False{$ENDIF}): Boolean;
function TestToReplace(const S: string; SArr: array of string; const NewValue: string): string;
function Trim(const S: string): string; {$IFDEF SUPP_INL}inline;{$ENDIF}
function TrimLead(const S: string): string; {$IFDEF SUPP_INL}inline;{$ENDIF}
function TrimTrail(const S: string): string; {$IFDEF SUPP_INL}inline;{$ENDIF}
function Turn(const iStr: string): string; {$IFDEF SUPP_INL}inline;{$ENDIF}
function UpCase(CH: Char): Char;
function WordCount(S: string; WordDelims: CharSet {$IFNDEF CLR}= [',']{$ENDIF}): Integer; {$IFDEF SUPP_INL}inline;{$ENDIF}
function WrapQuoteStr(Str: string; QuoteChar: Char = '"'): string; {$IFDEF SUPP_INL}inline;{$ENDIF}
function YesOrNoEx(const B, BoolYes, BoolNo: Boolean): Boolean; overload; {$IFDEF SUPP_INL}inline;{$ENDIF}
function YesOrNoEx(B: Boolean; const IntYes, IntNo: Integer): Integer; overload; {$IFDEF SUPP_INL}inline;{$ENDIF}
function YesOrNoEx(B: Boolean; const IntYes, IntNo: Int64): Int64; overload; {$IFDEF SUPP_INL}inline;{$ENDIF}
function YesOrNoEx(B: Boolean; const StrYes: string = '1'; const StrNo: string = '0'): string; overload; {$IFDEF SUPP_INL}inline;{$ENDIF}
function ZeroClip(const S: string): string; {$IFDEF SUPP_INL}inline;{$ENDIF}
function Zip(const Mask, Source: string; MaskZipChar: Char = 'X'): string; {$IFDEF SUPP_INL}inline;{$ENDIF}
{swap two variables in one operation}
procedure Flop(var S1, S2: string); overload; {$IFDEF SUPP_INL}inline;{$ENDIF}
procedure Flop(var I1, I2: Integer); overload; {$IFDEF SUPP_INL}inline;{$ENDIF}
procedure Flop(var W1, W2: Word); overload; {$IFDEF SUPP_INL}inline;{$ENDIF}
procedure Flop(var B1, B2: Byte); overload; {$IFDEF SUPP_INL}inline;{$ENDIF}
procedure Flop(var B1, B2: SmallInt); overload; {$IFDEF SUPP_INL}inline;{$ENDIF}
procedure Flop(var B1, B2: ShortInt); overload; {$IFDEF SUPP_INL}inline;{$ENDIF}
procedure Flop(var D1, D2: Double); overload; {$IFDEF SUPP_INL}inline;{$ENDIF}
procedure Null(var S: string); {$IFDEF VER6UP}deprecated;{$ENDIF} {$IFDEF SUPP_INL}inline;{$ENDIF}
procedure WordWrap(InSt: string; var OutSt, Overlap: string; Margin: Integer; PadToMargin: Boolean); {$IFDEF SUPP_INL}inline;{$ENDIF}

const
  ccCrLf = #13#10;
  cc_Cr = #10;

{$IFNDEF WINUC}
function CharInSet(C: Char; const CharSet: TSysCharSet): Boolean; {$IFDEF SUPP_INL}inline;{$ENDIF}
{$ENDIF}

function GetAnsiControlData(S: string): string;
function SetAnsiControlData(S: string): string;

{helper function for simple build xml/html file}
{sestaveni parametru}
function xmlPP2Str(iparam, ivalue: string): string; overload; {$IFDEF SUPP_INL}inline;{$ENDIF}
function xmlPP2Str(iparam: string; ivalue: Integer): string; overload; {$IFDEF SUPP_INL}inline;{$ENDIF}
function xmlPP2Str(iparam: string; ivalue: Boolean): string; overload; {$IFDEF SUPP_INL}inline;{$ENDIF}
{tag vcetne parametru}
function xmlDP2T(iname, iparam, tagcode: string; breaklines: Boolean = False): string; {$IFDEF SUPP_INL}inline;{$ENDIF}
{cisty tag bez parametru}
function xmlD2T(iname, tagcode: string; breaklines: Boolean = False): string; {$IFDEF SUPP_INL}inline;{$ENDIF}
{komentar}
function xmlCC(comment: string): string; {$IFDEF SUPP_INL}inline;{$ENDIF}

const
  xmlHeader = '<?xml version="1.0" encoding="windows-1250"?>';
  xmlCrLf = #13#10;

implementation

{$DEFINE REAPIR_WORDS_DELIMITED}
{there is defined all Czech and Slovak diacritics as standard for selective}
{repacement with non-diacritics, used to in some special cases like by}
{database column name and other}
const
  LoCharCS: string = '¸ÈÔ‰ËÏÂæûÙˆ˘˝ù·ÌÛ˙Úö¯‡';
  HiCharCS: string = '‹…œƒ»Ã≈ºé‘÷Ÿ›ç¡Õ”⁄“äÿ¿';
  NoCharCS: string = 'UEDACELLZOOUYTAIOUNSRR';

{$IFNDEF WINUC}
{replacement function for non-unicode versions of Delphi}
function CharInSet(C: Char; const CharSet: TSysCharSet): Boolean; {$IFDEF SUPP_INL}inline;{$ENDIF}
begin
  Result := AnsiChar(C) in CharSet;
end;
{$ENDIF}

function GetAnsiControlData(S: string): string;
var  //^IToto je text se ridicimi znaky^M^J
  I: Integer;
begin
  for I := 1 to Length(S) do
    if S[I] < ' ' then
    begin
      if S[I] <> #0 then
        Result := Result + '^' + Char(Ord(S[I])+ Ord('A') - 1)
    end
    else
      Result := Result + S[I];
end;

function SetAnsiControlData(S: string): string;
var  //^IToto je text se ridicimi znaky^M^J
  C: Char;
begin
  Result := S;
  for C := 'A' to 'Z' do
    if Pos('^' + C, Result) > 0 then
      Result := StringReplace(Result, '^' + C, Char(Ord(C) - Ord('A') + 1), [rfReplaceAll]);
end;

function RemoveCSPDiacritics(S: string): string;
  {-odstrani CSP diakritiku CASE SENSITIVE}
  {selective remove czech diacritics from text}
const
  dcsCSP = 'äçéèöùûüÒÚÔº≈æÂ¡ƒ»∆…ÃÕœ“—”‘÷ÿŸ⁄‹›·‰ËÊÈÏÌÔÚÛÙˆı¯‡˘˙¸˚˝';
  rdcCSP = 'STZZstzznndLLllAACCEEIDNNOOORUUUYaacceeidnoooorruuuuy';
var
  I, E: Integer;
begin
  Result := S;
  if Result = '' then Exit;
  for I := 1 to Length(Result) do
  begin
    E := Pos(Result[I], dcsCSP);
    if E > 0 then
      Result[I] := Char(rdcCSP[E]);
  end;
end;

function UpCase(CH: Char): Char;
  {-prevede mala pismena na velka, pouze kod Latin2}
  {selective upcase}
begin
  {$ifdef CLR}
  Result := System.Char.ToUpper(Ch);
  {$else}
  {$IFDEF UNICODE}
  Result := CH;
  if IsLetter(CH) then
    Result := ToUpper(Ch);
  {$ELSE}
  if Pos(CH, LoCharCS) <> 0 then
  begin
    Result := HiCharCS[Pos(CH, LoCharCS)];
    Exit
  end;
  if CharInSet(CH, ['a'..'z']) then
    Result := Char(Byte(CH) and $DF)
  else
    Result := CH;
  {$ENDIF}
  {$endif}
end;

function Reduce(const S: string; AboutSize: Integer): string;
 {-zkrati retezec o urcitou delku}
 {-truncate string to about size}
begin
  Result := Copy(S, 1, Length(S) - AboutSize)
end;

function JoinTo(PreStr, Delim, PostStr: string): string;
 {-spoji 2 retezce pomoci oddelovace}
 {join two strings with define delimiter}
begin
  if PreStr = '' then Result := PostStr
  else
    if PostStr = '' then Result := PreStr
    else Result := PreStr + Delim + PostStr;
end;

function JoinBoth(PreStr, Delim, PostStr: string): string;
 {-spoji 2 retezce pomoci oddelovace, pokud jsou oba naplnene}
 {join two strings with define delimiter, both must be unempty}
begin
  Result := '';
  if (PreStr <> '') and (PostStr <> '') then
    Result := PreStr + Delim + PostStr;
end;

function Alter(Str, AlterStr: string): string;
 {-alternativni plneni retezce}
 {alternative filing of result}
begin
  if Str = '' then Result := AlterStr
  else Result := Str;
end;

function AlterTo(Str, CondicStr, AlterStr: string): string;
  {-alternativni plneni retezce s podminkou}
 {alternative filing of result with conditional}
begin
  if Str = CondicStr then Result := AlterStr
  else Result := Str;
end;

function LoCase(CH: Char): Char;
  {-prevede velka pismena na mala}
  {selective lower case}
begin
  {$ifdef CLR}
  Result := System.Char.ToLower(Ch);
  {$else}
  {$IFDEF UNICODE}
  Result := CH;
  if IsLetter(CH) then
    Result := ToLower(Ch);
  {$ELSE}
  if Pos(CH, HiCharCS) <> 0 then
  begin
    LoCase := LoCharCS[Pos(CH, HiCharCS)];
    Exit
  end;
  if CharInSet(CH, ['A'..'Z']) then
    Result := Char(Byte(CH) or $20)
  else
    Result := CH;
  {$ENDIF}
  {$endif}
end;

function StrLoCase(S: string): string;
  {-v celem retezci prevede velka pismena na mala}
begin
  Result := S;
  if Result <> '' then
    Result := sysutils.AnsiLowerCase(S);
end;

function StrUpCase(S: string): string;
  {-v celem retezci prevede mala pismena na velka}
begin
  Result := S;
  if Result <> '' then
    Result := sysutils.AnsiUpperCase(S);
end;

function StrUpCaseNoCs(S: string): string;
  {-v celem retezci prevede mala pismena na velka a odstrani ceske znaky}
var I: Integer;
begin
  Result := '';
  if S = '' then Exit;
  for I := 1 to Length(S) do
  begin
  {$IFDEF WINUC}
    S[I] := ToUpper(S[I]);
  {$ELSE}
    S[I] := UpCase(S[I]);
    if Pos(S[I], HiCharCs) <> 0 then S[I] := Char(NoCharCs[Pos(S[I], HiCharCs)]);
  {$ENDIF}
  end;
  Result := S;
end;

function CharStr(CH: Char; Len: Integer): string;
  {-vyrobi novy retezec vyplneny znaky C}
var
  I: Integer;
begin
  Result := '';
  if Len > 0 then
    for I := 1 to Len do
      Result := Result + CH;
end;

function StrStr(const S: string; krat: Integer): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to krat do
    Result := Result + S;
end;

function PadCh(S: string; CH: Char; Len: Integer): string;
  {-vraci zprava znakem ch zarovnany retezec v delce len}
var
  I: Integer;
begin
  Result := S;
  if Length(S) < Len then
    for I := Length(S) + 1 to Len do
      Result := Result + CH
end;

function Pad(const S: string; Len: Integer): string;
  {-vraci zprava mezerami zarovnany retezec v delce len}
begin
  Result := PadCh(S, ' ', Len);
end;

function LeftPadCh(S: string; CH: Char; Len: Integer): string;
  {-vraci zleva znakem ch zarovnany retezec v delce len}
var
  I: Integer;
begin
  Result := S;
  if Length(S) < Len then
    for I := Length(S) + 1 to Len do
      Result := CH + Result
end;

function LeftPadCh(I: Integer; Len: Integer): string;
  {-vraci zleva nulou zarovnany integer v delce len}
begin
  Result := SysUtils.Format('%.*d', [Len, I]) ;
end;

function LeftPad(const S: string; Len: Integer): string;
  {-vraci zleva mezerami zarovnany retezec v delce len}
begin
  Result := LeftPadCh(S, ' ', Len);
end;

procedure Null(var S: string);
  {-vyrobi prazdny retezec}
begin
  S := '';
end;

function Hash(const S: string): LongInt;
  {-secte ordinalni hodnoty vsech prvku retezce}
var I: LongInt;
begin
  Result := 0;
  if S <> '' then
    for I := 1 to Length(S) do
      Result := Result + Ord(S[I]);
end;

function Space(B: Integer): string;
  {- vyrobi retezec vyplneny mezerami}
begin
  Result := CharStr(' ', B);
end;

function MakeStr(const S: string; B: Integer): string; {alias charstr}
  {-vyrobi novy retezec vyplneny znaky C}
begin
  Result := StrStr(S, B);
end;

function TrimLead(const S: string): string;
  {-vraci zleva orezany retezec}
begin
  Result := S;
  while (Length(Result) > 0) and (Result[1] <= ' ') do
    Delete(Result, 1, 1);
end;

function TrimTrail(const S: string): string;
  {-vraci zprava orezany retezec}
begin
  Result := S;
  while (Length(Result) > 0) and (Result[Length(Result)] <= ' ') do
    Delete(Result, Length(Result), 1);
end;

function Trim(const S: string): string;
  {-vraci z obou stran orezany retezec}
begin
  Result := TrimLead(TrimTrail(S));
end;

function ZeroClip(const S: string): string;
  {-odrizne zleva nuly v cisle}
var
  I: Word;
begin
  Result := TrimLead(S);
  if Result = '' then Exit; {29.11.1999 J.B.}
  if Result[1] <> '0' then Exit;
  if Mult(Result) = Length(Result) then
  begin
    ZeroClip := '0';
    Exit;
  end;
  I := 1;
  while (I <= Length(Result)) and (Result[I] = '0') do
    Inc(I);
  Dec(I);
  if I > 0 then
    Delete(Result, 1, I);
end;

function CapitalizeWord(const S: string): string;
 {-kazde slovo v retezci bude mit zvetseno prvni pismeno}
var
  I: Integer;
  CapitalizeNextLetter: Boolean;
begin
  Result := {$IFNDEF CLR}S{$ELSE}AnsiUpperCase(S){$ENDIF};
  CapitalizeNextLetter := True;
  for I := 1 to Length(Result) do
  begin
    {$IFNDEF CLR}
    if CapitalizeNextLetter and ({$IFDEF UNICODE}IsLower(Result[I]){$ELSE}(Result[I] in ['a'..'z']){$ENDIF} or (Pos(Result[I], LoCharCS) > 0))
    then
      Result[I] := UpCase(Result[I]);
    {$ELSE}
    if not CapitalizeNextLetter then
    begin
      Result[I] := S[I];
    end;
    {$ENDIF}
    CapitalizeNextLetter := Result[I] = ' ';
  end;
end;

function CenterCh(const S: string; CH: Char; Width: Integer): string;
  {-vrati znaky ch vycentrovany retezec v sirce width}
begin
  Result := S;
  if Length(S) < Width then
  begin
    Result := CharStr(CH, Width - Length(S));
    Insert(S, Result, (Length(Result) div 2) + 1);
  end;
end;

function Center(const S: string; Width: Integer): string;
  {-vrati mezerami vycentrovany retezec v sirce width}
begin
  Center := CenterCh(S, ' ', Width);
end;

function AppendWord(Wrd, S: string; WordDelim: Char = ','): string;
begin
  if S = '' then
    Result := Wrd
  else
    Result := S + WordDelim + Wrd;
end;

{$IFDEF REAPIR_WORDS_DELIMITED}
function WordCount(S: string; WordDelims: CharSet): Integer;
  {-vrati pocet slov oddelenych WordDelims}
var
  ch: Char;
  {$IFNDEF VER9UP}I: Integer;{$ENDIF}
begin
  Result := 0;
  if S = '' then Exit;
  {$IFNDEF VER9UP}
  for I := 1 to Length(S) do
  begin
    ch := S[I];
  {$ELSE}
  for ch in S do
  begin
  {$ENDIF}
    if CharInSet(ch, WordDelims) then
      Inc(Result);
  end;
  Inc(Result);
end;

function ExtractWord(N: Integer; S: string; WordDelims: CharSet): string;
  {-zkopiruje na vystup N-te slovo oddelene WordDelims}
var
  SL: Integer;
  ch: Char;
  substr: string;
  {$IFNDEF VER9UP}I: Integer;{$ENDIF}
begin
  Result := '';
  if S = '' then Exit;
  if N <= 0 then Exit;
  if N > WordCount(S, WordDelims) then
    Exit;
  SL := 1; substr := '';
  try
    {$IFNDEF VER9UP}
    for I := 1 to Length(S) do
    begin
      ch := S[I];
    {$ELSE}
    for ch in S do
    begin
    {$ENDIF}
      if CharInSet(ch, WordDelims) then
      begin
        if SL = N then Exit;
        Inc(SL);
        substr := '';
      end
      else
        substr := substr + ch;
    end;
  finally
    Result := substr;
  end;
end;

function ExtractWord(N: Integer; S: string; WordDelims: CharSet; Operation: TWordOperations): string;
begin
  Result := ExtractWord(N, S, WordDelims);
  if woTrim in Operation then
    Result := Trim(Result)
  else
    if woLeftTrim in Operation then
      Result := TrimLeft(Result)
    else
      if woRightTrim in Operation then
        Result := TrimRight(Result);
  if woUpper in Operation then
    Result := AnsiUpperCase(Result)
  else
    if woLower in Operation then
      Result := AnsiLowerCase(Result)
    else
      if woCapitalize in Operation then
        Result := CapitalizeWord(Result)
end;

function GetStartAndEndWord(N: Integer; S: string; WordDelims: CharSet; var St, En: Integer): Integer;
  {-nalezne zacatek a konec slova v indexu, a vrati delku nalezeneho slova}
var
  SL: Integer;
  ch: Char;
  substr: string;
  I: Integer;
begin
  Result := -1;
  if S = '' then Exit;
  if N <= 0 then Exit;
  if N > WordCount(S, WordDelims) then Exit;
  En := 1;
  SL := 1;
  substr := '';
  St := 1;
  for I := 1 to Length(S) do
  begin
    ch := S[I];
    if CharInSet(ch, WordDelims) then
    begin
      if SL = N then {slovo je nalezeno}
      begin
        En := I - 1;
        Break;
      end
      else
        St := I + 1;
      Inc(SL);
      substr := '';
    end
    else
    begin
      substr := substr + ch;
      Inc(En);
    end;
  end;
  Result := Length(substr);
end;

function PopWord(B: Integer; var S: string; WordDelims: CharSet): string;
{-vyrizne b-te slovo z retezce}
var
  St, En: Integer;
begin
  GetStartAndEndWord(B, S, WordDelims, St, En);
  if St > 0 then
  begin
    Result := Copy(S, St, En - St + 1);
    Delete(S, St, En - St + 1);
  end;
end;

function GetPos(B: Integer; S: string; WordDelims: CharSet): Integer;
{-vrati pocatecni pozici slova}
var
  En: Integer;
begin
{-vraci pocatecni pozici b-teho slova}
  GetStartAndEndWord(B, S, WordDelims, Result, En);
end;

function GetEnd(B: Integer; S: string; WordDelims: CharSet): Integer;
{-vraci koncovou pozici b-teho slova}
var
  St: Integer;
begin
  GetStartAndEndWord(B, S, WordDelims, St, Result);
end;

function GetFirstWord(const S: string; WordDelims: CharSet): string;
{-poda na vystup prvni slovo retezce}
begin
  Result := '';
  if WordCount(S, WordDelims) > 0 then
    Result := ExtractWord(1, S, WordDelims)
end;

function GetLastWord(const S: string; WordDelims: CharSet): string;
{-poda na vystup posledni slovo retezce}
begin
  Result := '';
  if WordCount(S, WordDelims) > 0 then
    Result := ExtractWord(WordCount(S, WordDelims), S, WordDelims)
end;

function ChangeWord(N: Integer; const Wrd, S: string; WordDelims: CharSet): string;
{-vymeni slovo uvozene oddelovaci na pozici za jine slovo}
var X: Integer;
begin
  Result := S;
  X := GetPos(N, Result, WordDelims);
  PopWord(N, Result, WordDelims);
  Insert(Wrd, Result, X);
end;
{$ELSE}
function WordCount(S: string; WordDelims: CharSet): Integer;
  {-vrati pocet slov oddelenych WordDelims}
var
  I: Integer;
begin
  Result := 0;
  I := 1;
  while I <= Length(S) do
  begin
    {preskoc oddelovace}
    while (I <= Length(S)) and CharInSet(S[I], WordDelims) do
      Inc(I);
    {dokud neni konec retezce, skakej po slovech}
    if I <= Length(S) then Inc(Result);
    {a zde je konec slova}
    while (I <= Length(S)) and not CharInSet(S[I], WordDelims) do
      Inc(I);
  end;
end;

function ExtractWord(N: Integer; S: string; WordDelims: CharSet): string;
  {-zkopiruje na vystup N-te slovo oddelene WordDelims}
var
  I, J: Word;
  Count: Integer;
  SLen: Integer;
begin
  Count := 0;
  I := 1;
  Result := '';
  SLen := Length(S);
  while I <= SLen do
  begin
    {preskoc oddelovace}
    while (I <= SLen) and CharInSet(S[I], WordDelims) do
      Inc(I);
    {neni-li na konci retezce, bude nalezen zacatek slova}
    if I <= SLen then Inc(Count);
    J := I;
    {a zde je konec slova}
    while (J <= SLen) and not CharInSet(S[J], WordDelims) do
      Inc(J);
    {je-li toto n-te slovo, vloz ho na vystup}
    if Count = N then
    begin
      Result := Copy(S, I, J - I);
      Exit
    end;
    I := J;
  end; {while}
end;

procedure GetStartAndEndWord(N: Integer; S: string; WordDelims: CharSet; var St, En: Integer);
 {-nalezne zacatek a konec slova v indexu}
var
  I, J, Count: Integer;
  SLen: Integer;
begin
  Count := 0;
  I := 1;
  St := 0; En := 0;
  SLen := Length(S);
  while I <= SLen do
  begin
    {preskoc oddelovace}
    while (I <= SLen) and CharInSet(S[I], WordDelims) do Inc(I);
    {neni-li na konci retezce, bude nalezen zacatek slova}
    if I <= SLen then Inc(Count);
    J := I;
    {a zde je konec slova}
    while (J <= SLen) and not CharInSet(S[J], WordDelims) do Inc(J);
    {je-li toto n-te slovo, vloz ho na vystup}
    if Count = N then
    begin
      St := I;
      En := J - 1;
      Exit
    end;
    I := J;
  end; {while}
end;

function PopWord(B: Integer; var S: string; WordDelims: CharSet): string;
  {-vyrizne b-te slovo z retezce}
var
  St, En: Integer;
begin
  GetStartAndEndWord(B, S, WordDelims, St, En);

  if St > 0 then
  begin
    Result := Copy(S, St, En - St + 1);
    Delete(S, St, En - St + 1);
  end;
end;

function GetPos(B: Integer; S: string; WordDelims: CharSet): Integer;
 {-vrati pocatecni pozici slova}
var
  En: Integer;
begin
  {-vraci pocatecni pozici b-teho slova}
  GetStartAndEndWord(B, S, WordDelims, Result, En);
end;

function GetEnd(B: Integer; S: string; WordDelims: CharSet): Integer;
  {-vraci koncovou pozici b-teho slova}
var
  St: Integer;
begin
  GetStartAndEndWord(B, S, WordDelims, St, Result);
end;

function GetFirstWord(const S: string; WordDelims: CharSet): string;
 {-poda na vystup prvni slovo retezce}
begin
  Result := '';
  if WordCount(S, WordDelims) > 0 then
    Result := ExtractWord(1, S, WordDelims)
end;

function GetLastWord(const S: string; WordDelims: CharSet): string;
 {-poda na vystup posledni slovo retezce}
begin
  Result := '';
  if WordCount(S, WordDelims) > 0 then
    Result := ExtractWord(WordCount(S, WordDelims), S, WordDelims)
end;

function ChangeWord(N: Integer; const Wrd, S: string; WordDelims: CharSet): string;
 {-vymeni slovo uvozene oddelovaci na pozici za jine slovo}
var X: Integer;
begin
  Result := S;
  X := GetPos(N, Result, WordDelims);
  PopWord(N, Result, WordDelims);
  Insert(Wrd, Result, X);
end;
{$ENDIF}

function CountOfMembers(const ASet: CharSet): Integer;
var
  C: AnsiChar;
begin
  Result := 0;
  if ASet = [] then Exit;
  for C := Low(AnsiChar) to High(AnsiChar) do
    if C in ASet then
      Inc(Result);
end;

function FindWord(what, S: string; WordDelims: CharSet): Boolean;
 {-nalezne slovo what v seznamu slov oddelenych WordDelims}
var
  I, J: Integer;
  C: AnsiChar;
begin
  Result := False;
  if WordDelims = [] then Exit;
  I := 0;
  for C := Low(AnsiChar) to High(AnsiChar) do
    if CharInSet(C, WordDelims) then
      Inc(I);
  if I = 1 then
    if CharInSet(',', WordDelims) then
      with TStringList.Create do
      try
        CommaText := S;
        Result := IndexOf(what) <> -1;
        Exit;
      finally
        Free
      end;

  I := WordCount(S, WordDelims);
  if I > 0 then
    for J := 1 to I do
      if AnsiCompareText(what, ExtractWord(J, S, WordDelims)) = 0 then
      begin
        Result := True;
        Exit;
      end;
end;

procedure WordWrap(InSt: string; var OutSt, Overlap: string;
  Margin: Integer; PadToMargin: Boolean);
  {-Seskladani slov do pozadovane delky radku}
var
  InStLen, OutStLen, OvrLen, EndPos, BegPos: Integer;
  I: Integer;
begin
  InStLen := Length(InSt);
  {hledani konce radku}
  if InStLen > Margin then
  begin
    {nalezeni konce slova na okraji je-li to potreba}
    EndPos := Margin;
    while (EndPos <= InStLen) and (InSt[EndPos] <> ' ') do
      Inc(EndPos);
    if EndPos > InStLen then
      EndPos := InStLen;
    {odstran okrajove mezery}
    while (InSt[EndPos] = ' ') and (EndPos > 0) do
      Dec(EndPos);
    if EndPos > Margin then
    begin
      {nepradchazeji-li slovu mezery}
      while (EndPos > 0) and (InSt[EndPos] <> ' ') do
        Dec(EndPos);
      {je-li EndPos = 0 potom to muzes zabalit}
      if EndPos = 0 then
        EndPos := Margin
      else {zarizni prazdne znaky}
        while (InSt[EndPos] = ' ') and (EndPos > 0) do
          Dec(EndPos);
    end;
  end
  else
    EndPos := InStLen;

  {kopiruj nezabalene casti radku}
  OutStLen := EndPos;

  for I := 1 to OutStLen do
    OutSt := OutSt + InSt[I];

  {nalezni pocatek pristiho slova v radku}
  BegPos := EndPos + 1;
  while (BegPos <= InStLen) and (InSt[BegPos] = ' ') do
    Inc(BegPos);

  if BegPos <= InStLen then
  begin
    {kopiruj od pocatku pristiho slova ke konci radku}
    OvrLen := Succ(InStLen - BegPos);

    for I := 0 to OvrLen - 1 do
      Overlap := Overlap + InSt[BegPos + I];

  end;

  {je-li zadano zarovnej z prava retezec}
  if PadToMargin and (OutStLen < Margin) then
  begin
    for I := 1 to Margin - OutStLen do
      OutSt := OutSt + ' ';
  end;
end;

function InsWord(iWord, cWord, cString: string): string;
  {-na pozici iWord vlozi jine slovo}
var
  cc: Integer;
begin
  cc := Pos(iWord, cString);
  if cc <> 0 then
  begin
    Delete(cString, cc, Length(iWord));
    Insert(cWord, cString, cc);
  end;
  Result := cString;
end;

function Push(const Substr: string; PosicInStr: Integer; const Str: string): string;
  {-do retezce vlozi znaky jineho retezce od prislusne pozice}
begin
  Result := Str; {je vlozeno za retezcem}
  if (PosicInStr < 1) or (Substr = '') then Exit;
  if Length(Result) < (PosicInStr - 1) then
    Result := Pad(Result, PosicInStr - 1) + Substr
  else
  begin
    Delete(Result, PosicInStr, Length(Substr));
    Insert(Substr, Result, PosicInStr);
  end;
end;

function Push(Posic: Integer; const Source, Dest: string): string;
  {-do retezce vlozi znaky jineho retezce od prislusne pozice}
begin
  Result := Dest; {je vlozeno za retezcem}
  if Posic > Length(Result) then Result := Pad(Result, Posic) + Source
  else begin
    {$IFDEF CLR}
    Insert(Source, Result, Posic);
    {$ELSE}
    if (Posic + Length(Source)) > Length(Result) then
      Result := Pad(Result, Posic + Length(Source));
    Move(Source[1], Result[Posic], Length(Source));
    {$ENDIF}
  end;
end;

function Smash(C: Char; const S: string): string;
  {-vypusti znak C z retezce S}
var I: Integer;
begin
  Result := '';
  if S <> '' then
    for I := 1 to Length(S) do
      if S[I] <> C then
        Result := Result + S[I];
end;

function Smash(CH: CharSet; const S: string): string;
  {-vypusti znaky deklarovane v CH z retezce S}
var I: Integer;
begin
  Result := '';
  if S <> '' then
    for I := 1 to Length(S) do
      if not CharInset(S[I], CH) then
        Result := Result + S[I];
end;

function Mask(CharOfMask: Char; const StrMask, Matrice: string;
  var NextPosic: Integer): string;
  {-vstupem je znak masky CharOfMask, ktery je hledan v masce StrMask a to
   od prvni pozice. Kdyz je nalezen, jsou vraceny funkci znaky z Matrice,
   odpovidajici pozici vuci masce a NextPosic ukazuje na dalsi znak za
   vracenym podretezcem; Podminka: Length(StrMask)=Length(Matrice)}
var
  O: Integer;
begin
  Result := '';
  if (StrMask = '') or (Length(StrMask) <> Length(Matrice)) then Exit;
  if NextPosic = 0 then NextPosic := 1; {jen kdyz je 0 pak od zacatku}
  while (NextPosic <= Length(StrMask)) and (StrMask[NextPosic] <> CharOfMask) do
    Inc(NextPosic);
  O := NextPosic;
  while (O <= Length(StrMask)) and (StrMask[O] = CharOfMask) do Inc(O);
  Result := Copy(Matrice, NextPosic, O - NextPosic);
end;

function Count(CH: Char; const Dest: string; var Posic, Len: Integer): Boolean;
  {-nacita od posic len stejnych znaku ch}
var SS: string;
  I: Integer;
begin
  SS := Copy(Dest, Posic, 255); {od urcite pozice}
  Posic := 0;
  Len := 0;
  I := Pos(CH, SS);
  if I <> 0 then
  begin
    Posic := I;
    while SS[I + Len] = CH do Inc(Len);
    if Length(SS) <> Length(Dest) then
      Posic := Length(Dest) - Length(SS) + Posic;
  end; {neni nic}
  REsult := Posic <> 0;
end;

procedure Flop(var S1, S2: string);
  {-prohodi obsahy dvou retezcu}
var SS: string;
begin
  SS := S1;
  S1 := S2;
  S2 := SS
end;

procedure Flop(var I1, I2: Integer);
var
  II: Integer;
begin
  II := I1;
  I1 := I2;
  I2 := II;
end;

procedure Flop(var W1, W2: Word);
var
  WW: Word;
begin
  WW := W1;
  W1 := W2;
  W2 := WW;
end;

procedure Flop(var B1, B2: Byte);
var
  BB: Byte;
begin
  BB := B1;
  B1 := B2;
  B2 := BB;
end;

procedure Flop(var B1, B2: SmallInt);
var
  BB: SmallInt;
begin
  BB := B1;
  B1 := B2;
  B2 := BB;
end;

procedure Flop(var B1, B2: ShortInt);
var
  BB: ShortInt;
begin
  BB := B1;
  B1 := B2;
  B2 := BB;
end;

procedure Flop(var D1, D2: Double);
var
  DD: Double;
begin
  DD := D1;
  D1 := D2;
  D2 := DD;
end;

function Strip(const Mask, Source: string; MaskZipChar: Char = 'X'): string;
   {-nastavuje dle masky novy retezec}
var I: Integer;
  S, SS: string;
begin
  Result := Source;
  if (Source = '') or (Mask = '') then Exit;
  S := '';
  SS := Pad(Source, Length(Mask));
  for I := 1 to Length(Mask) do
    if Mask[I] = MaskZipChar then
      S := S + SS[I]; {J.B. 12.12.95}
  Result := S;
end;

function Change(S: string; Source, Dest: Char): string;
  {-zmeni znaky dest za source}
var I: Integer;
begin
  Result := S;
  if Result = '' then Exit;
  for I := 1 to Length(Result) do
    if Result[I] = Source then
      Result[I] := Dest;
end;

function Replace(const NewStr, InputString, OldStr: string): string;
  {-nahradi vsechny vyskyty podretezce OldStr v retezci S za podretezec NewStr}
{$IFNDEF VER5UP}
var
  L: LongInt;
begin
  Result := InputString;
  L := Pos(OldStr, Result);
  while L > 0 do
  begin
    Delete(Result, L, Length(OldStr));
    Insert(NewStr, Result, L);
    L := Pos(OldStr, Result);
  end;
{$ELSE}
begin
  Result := StringReplace(InputString, OldStr, NewStr, [rfReplaceAll, rfIgnoreCase]);
{$ENDIF}
end;

function ChangeTo(S: string; Source: CharSet; Dest: Char): string;
  {-zmeni n znaku dest za source}
var I: Integer;
begin
  Result := S;
  if Result = '' then Exit;
  for I := 1 to Length(Result) do
    if CharInSet(Result[I], Source) then
      Result[I] := Dest;
end;

function ChangeTo(S: string; Source: Char; Dest: Char): string;
  {-zmeni n znaku dest za source}
var I: Integer;
begin
  Result := S;
  if Result = '' then Exit;
  for I := 1 to Length(Result) do
    if Result[I] = Source then
      Result[I] := Dest;
end;

function Zip(const Mask, Source: string; MaskZipChar: Char = 'X'): string;
  {-zaformatuje retezec podle masky}
var I, J: Integer;
  S: string;
begin
  Result := Source;
  if Mask = '' then Exit;
  Result := '';
  S := '';
  if Source = '' then
    Result := Change(Mask, MaskZipChar, ' ')
  else
  begin
    J := 1;
    for I := 1 to Length(Mask) do
      if Mask[I] = MaskZipChar then
      begin
        S := S + Source[J];
        if J < Length(Source) then Inc(J)
        else Break;
      end
      else S := S + Mask[I];
      Result := S;
  end;
end;

function Turn(const iStr: string): string;
 {-otoci retezec}
var
  I: Integer;
begin
  Result := '';
  if iStr <> '' then
    for I := 1 to Length(iStr) do
      Result := iStr[I] + Result;
end;

function Entab(const Sx: string; TabSize: Integer): string;
  {-nahradi vsechny mezery v dane delce jednim tabelatorem}
var
  First: Integer;
  S: string;
begin
  S := Sx;
  while Pos(CharStr(' ', TabSize), S) <> 0 do
  begin
    First := Pos(CharStr(' ', TabSize), S);
    Delete(S, first, Tabsize);
    Insert(#9, S, first);
  end;
  Result := S;
end;

function Detab(const Sx: string; TabSize: Integer): string;
 {-odstrani vsechny znaky tabelatoru}
var
  first: Integer;
  S: string;
begin
  S := Sx;
  while Pos(#9, S) <> 0 do
  begin
    first := Pos(#9, S);
    Delete(S, first, 1);
    Insert(CharStr(' ', TabSize), S, first);
  end;
  Result := S;
end;

function CheckIntervalIsSimple(const Value: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  I := Pos('..', Value);
  if I = 0 then Exit;
  //test "number..number"
  Result := IsNumber(Copy(Value, 1, I - 1)) and IsNumber(Copy(Value, I + 2, Length(Value)));
end;

function CheckIntervalLimit(const Value: string; var oLow, oHigh: Integer): Boolean;
label
  __fin;
const
  ccIntrvl = '.';
var
  strLow, strHig: string;
  intLow, intHig: LongInt;
  iValue: string;
begin
  //pouze pro jednoduche intervaly typu [low..hig]
  Result := False;
  {neni zadny interval - nic}
  if not CheckIntervalIsSimple(Value) then Exit;
  iValue := Value;
  iValue := StringReplace(iValue, '..', ccIntrvl, [rfReplaceAll]);
  {je "." tj. je "x.." nebo "..x"}
  if WordCount(iValue, [ccIntrvl]) = 1 then
  begin
    strLow := ExtractWord(1, iValue, [ccIntrvl]);
    strHig := strLow;
    goto __fin;
  end;
  strLow := ExtractWord(1, iValue, [ccIntrvl]);
  strHig := ExtractWord(2, iValue, [ccIntrvl]);
  __fin:
  if strLow > strHig then Flop(strLow, strHig);
  if not Str2Long(strLow, intLow) then Exit;
  if not Str2Long(strHig, intHig) then Exit;
  oLow := intLow;
  oHigh := intHig;
  Result := True;
end;

function ExpandInterval(const Value: string; const Delimiter: Char {$IFNDEF CLR}= ','{$ENDIF}; const iLimit: Integer {$IFNDEF CLR}= 1024{$ENDIF}): string;
  {zadany interval od..do rozloz na polozky a1,a2..an}
const
  ccIntrvl = '.';
var
  oResult, strLow, strHig: string;
  I, intLow, intHig: LongInt;
  iValue: string;
begin
  Result := Value;
  {neni zadny interval - nic}
  if Pos('..', Value) = 0 then Exit;
  iValue := Value;
  iValue := StringReplace(iValue, '..', ccIntrvl, [rfReplaceAll]);
  {je "." tj. je "x.." nebo "..x"}
  if WordCount(iValue, [ccIntrvl]) = 1 then
  begin
    Result := ExtractWord(1, iValue, [ccIntrvl]);
    Exit;
  end;
  strLow := ExtractWord(1, iValue, [ccIntrvl]);
  strHig := ExtractWord(2, iValue, [ccIntrvl]);
  if strLow > strHig then Flop(strLow, strHig);
  if not Str2Long(strLow, intLow) then Exit;
  if not Str2Long(strHig, intHig) then Exit;
  {pokud je prvku v intervalu vic jak iLimit, tak nerozkladej a ven}
  if (intHig - intLow) > iLimit then Exit;
  oResult := '';
  for I := intLow to intHig do
    if {$IFNDEF CLR}System.{$ENDIF}Copy(strLow, 1, 1) = '0' then
      oResult := JoinTo(oResult, Delimiter, LeftPadCh(IntToStr(I), '0', Length(strLow)))
    else
      oResult := JoinTo(oResult, Delimiter, IntToStr(I));
  Result := oResult;
end;

function SplitIntervals(const Value: string; Delimiter: Char {$IFNDEF CLR}= ','{$ENDIF}): string;
{z parametru oddelenych carkami vybere intervaly a rozlozi je}
var
  iValue, strPart: string;
  I: Integer;
begin
  iValue := '';
  {je pouze jediny interval "lo..hi"}
  if WordCount(Value, [AnsiChar(Delimiter)]) = 0 then
  begin
    if Pos('..', Value) <> 0 then
      iValue := ExpandInterval(Value, Delimiter {$IFDEF CLR}, 500{$ENDIF})
    else
      iValue := Value; {vrat puvodni}
  end
  else
    {intervalu je vice "lo1..hi1, n1, lo2..hi2, ..."}
    for I := 1 to WordCount(Value, [AnsiChar(Delimiter)]) do
    begin
      {nabirej je postupne}
      strPart := ExtractWord(I, Value, [AnsiChar(Delimiter)]);
      {je-li intrval, rozloz ho}
      if Pos('..', strPart) <> 0 then
        strPart := ExpandInterval(strPart, Delimiter {$IFDEF CLR}, 500{$ENDIF});
      { a pridavej}
      iValue := JoinTo(iValue, Delimiter, strPart);
    end;
  Result := iValue;
end;

function ContractInterval(Value: string; const Delimiter: Char {$IFNDEF CLR}= ','{$ENDIF}): string;
  {z parametru oddelenych carkami sestavi intervaly, musi byt vzestupne setrideny}
  {omezeni jen na cisla}
  {only numbers on input limited}
label goAgain;
var
  iPos, oPos: Integer;
  iPrime, S, oPrime, oResult: string;
begin
  Result := Value;
  oResult := '';
  iPos := WordCount(Result, [AnsiChar(Delimiter)]);
  iPrime := '';
  S := iPrime;
  if iPos > 0 then
  begin
    oPos := 1;
    {hledej meze v retezci}
   goAgain:
    S := ExtractWord(oPos, Result, [Ansichar(Delimiter)]);
    oPrime := S;
    while (oPos <= iPos) do
    begin
      if (oPos + 1) <= iPos then
      begin
        iPrime := ExtractWord(oPos + 1, Result, [Ansichar(Delimiter)]);
        if (Str3Long(oPrime) = (Str3Long(iPrime) - 1)) then oPrime := iPrime
        else
        begin
          if Length(oResult) > 0 then oResult := oResult + ',';
          if S <> oPrime then
            oResult := oResult + S + '..' + oPrime
          else
            oResult := oResult + oPrime;
          Inc(oPos);
          goto goAgain;
        end
      end
      else
      begin
        if Length(oResult) > 0 then oResult := oResult + ',';
        if S <> oPrime then
          oResult := oResult + S + '..' + oPrime
        else
          oResult := oResult + S
      end;
      Inc(oPos);
    end
  end;
  Result := oResult;
end;

function HasExtension(const Name: string; var DotPos: Word): Boolean;
  {-kdyz existuje, vrati pozici separatoru extenze jmena}
{$IFNDEF VER5UP}
var
  I: Integer;
begin
  DotPos := 0;
  for I := Length(Name) downto 1 do
    if (Name[I] = '.') and (DotPos = 0) then
      DotPos := I;
  Result := (DotPos > 0) and (Pos('\', Copy(Name, Succ(DotPos), 64)) = 0);
end;
{$ELSE}
var
  s: string;
begin
  S := ExtractFileExt(Name);
  Result := Length(ExtractFileExt(Name)) <> 0;
  if Result then
    DotPos := Pos(S, Name);
end;
{$ENDIF}

function DefaultExtension(const Name, Ext: string): string;
  {-kdyz extenze existuje, vrati nezmeneno jinak extenzi doplni}
{$IFNDEF VER5UP}
var
  DotPos: Word;
begin
  if HasExtension(Name, DotPos) then Result := Name
  else Result := Name + '.' + Ext;
end;
{$ELSE}
begin
  if ExtractFileExt(Name) = '' then
    Result := ChangeFileExt(Name, '.' + Ext);
end;
{$ENDIF}

function ForceExtension(const Name, Ext: string): string;
  {-nahradi extenzi jinou extenzi}
  {extension without . DOT}
{$IFNDEF VER5UP}
var
  DotPos: Word;
begin
  if HasExtension(Name, DotPos) then
    Result := Copy(Name, 1, DotPos) + Ext
  else
    Result := Name + '.' + Ext;
{$ELSE}
begin
  Result := ChangeFileExt(Name, '.' + Ext)
{$ENDIF}
end;

function JustExtension(const PathName: string): string;
  {-vraci pouze extenzi souboru}
{$IFNDEF VER5UP}
var
  DotPos: Word;
begin
  if HasExtension(PathName, DotPos) then
    Result := Copy(PathName, Succ(DotPos), 3)
  else
    Result := '';
{$ELSE}
begin
  Result := ExtractFileExt(PathName);
{$ENDIF}
end;

function JustFilename(const PathName: string): string;
  {-vraci pouze cele jmeno souboru tj .jmeno a extenzi}
{$IFDEF VER5UP}
begin
  Result := ExtractFileName(PathName);
end;
{$ELSE}
var
  SS: string;
  I: Integer;
begin
  SS := Turn(PathName);
  I := Pos({$IFDEF VER6UP}Sysutils.PathDelim{$ELSE}'\'{$ENDIF}, SS); {pr. c:\rwewe\kokol.txt}
  if I = 0 then I := Pos(':', SS);
  {neobsahuje-li ani \ ani : pak to muze byt jmeno}
  if I = 0 then Result := PathName
  else Result := Turn(Copy(SS, 1, I - 1));
end;
{$ENDIF}

function JustPathName(const PathName: string): string;
  {-vraci pouze cestu ze jmena souboru}
{$IFDEF VER5UP}
begin
  Result := ExtractFilePath(PathName);
end;
{$ELSE}
var
  SS: string;
  I: Integer;
begin
  SS := Turn(PathName);
  I := Pos({$IFDEF VER6UP}Sysutils.PathDelim{$ELSE}'\'{$ENDIF}, SS); {pr. c:\rwewe\kokol.txt}
  if I = 0 then I := Pos(':', SS);
  if I = 0 then Result := '' {not path}
  else Result := Turn(Copy(SS, I + 1, 255));
end;
{$ENDIF}

function AddLastChar( const DirName: string; C: Char): string;
  {-prida \ ke jmenu adresare}
begin
  if (DirName = '') or (DirName[Length(DirName)] = C) then Result := DirName
  else Result := DirName + C;
end;

function RemLastChar(const DirName: string): string;
  {-ubere \ ke jmenu adresare}
begin
  Result := DirName;
  if Length(DirName) > 1 then Result := Copy(Result, 1, Length(Result) - 1);
end;

function CleanDOSFileName(FileName: string): string;
  {-vraci jmeno souboru max 8 znaku a 3 znaky pro extenzi}
var
  S, Dir, Name, Ext: string;
begin
  S := Turn(FileName);
  if Pos('.', S) = 0 then Ext := '.'
  else
  begin
    Ext := Turn(Copy(S, 1, Pos('.', S)));
    Delete(S, 1, Pos('.', S));
  end;
  if Pos({$IFDEF VER6UP}Sysutils.PathDelim{$ELSE}'\'{$ENDIF}, S) = 0 then
    if Pos(':', S) > 1 then
    begin
      Name := Turn(Copy(S, 1, Pos(':', S) - 1));
      Delete(S, 1, Pos(':', S) - 1);
      Dir := Turn(S);
    end
    else
    begin
      Name := Turn(S);
      Dir := '';
    end
  else
  begin
    Name := Turn(Copy(S, 1, Pos({$IFDEF VER6UP}Sysutils.PathDelim{$ELSE}'\'{$ENDIF}, S) - 1));
    Delete(S, 1, Pos({$IFDEF VER6UP}Sysutils.PathDelim{$ELSE}'\'{$ENDIF}, S) - 1);
    Dir := Turn(S);
  end;
  {FSplit(FileName,Dir,Name,Ext);}
  Result := Concat(Copy(Name, 1, 8), Copy(Ext, 1, 4));
end;

function TestFileName(FName: string): Boolean;
  {-testuje dosovske jmeno na nepovolene znaky}
  {-vraci false obsahuje-li jmeno souboru nepovolene znaky}
const
  InExt = ['''', '/', '\', '[', ']', ':', ';', '+', '=', ',', '*', '?', '|'];
  InPath = ['''', '/', '[', ']', ';', '+', '=', ',', '*', '?', '|'];
  InName = ['''', '/', '\', '[', ']', ':', ';', '+', '=', ',', '*', '?', '|'];
var
  I: Integer;
  Path: string;
  Name: string;
  Ext: string;
begin
  Result := False;
  Path := JustPathName(FName);
  Name := ExtractNameOfFileOnly(FName);
  Ext := JustExtension(FName);
  if Name = '' then Exit; {jmeno nemuze byt prazdne}
  for I := 1 to Length(Path) do if CharInSet(Path[I], InPath) then Exit;
  for I := 1 to Length(Name) do if CharInSet(Name[I], InName) then Exit;
  for I := 1 to Length(Ext) do if CharInSet(Ext[I], InExt) then Exit;
  Result := True;
end;

function ShortDirName(Len: Integer; const PName: string): string;
  {-vraci retezec DOS jmena bez strednich slov}
  function FindBackSlash(Posic: Byte; S: string): Byte;
  var
    I: Byte;
  begin
    Result := 0;
    if Length(S) = 0 then Exit;
    repeat
      I := Pos({$IFDEF VER6UP}Sysutils.PathDelim{$ELSE}'\'{$ENDIF}, S);
    until I >= Posic;
    Result := I
  end;
var
  Q, S: string;
  I, L, C: Integer;
begin
  Q := AddLastChar(PName); {opraveno 30.11.2000 L.Taborsky}
  Result := Q;
  L := Length(Q);
  if L <= Len then Exit;
  C := 1;
  if Q[1] <> {$IFDEF VER6UP}Sysutils.PathDelim{$ELSE}'\'{$ENDIF} then
  begin
    S := Copy(Q, 1, 3);
    Delete(Q, 1, 3);
  end
  else
  begin
    S := {$IFDEF VER6UP}Sysutils.PathDelim{$ELSE}'\'{$ENDIF};
    Delete(Q, 1, 1);
  end;
  repeat
    I := FindBackSlash(C, Q);
    Delete(Q, 1, I);
  until Length(S + '..' + {$IFDEF VER6UP}Sysutils.PathDelim{$ELSE}'\'{$ENDIF} + Q) <= Len;
  Result := S + '..' + {$IFDEF VER6UP}Sysutils.PathDelim{$ELSE}'\'{$ENDIF} + Q
end;

function ShortFileName(Len: Integer; const FName: string): string;
 {-zkrati priliz dlouhe jmeno souboru}
begin
  Result := AddLastChar(ShortDirName(Len - Length(JustFileName(FName)) - 1,
    JustPathName(FName))) + JustFileName(FName);
end;

function ExtractNameOfFileOnly(const PathName: string): string;
  {-vrat pouze jmeno bez extense a cesty}
{$IFNDEF VER5UP}
var
  SS: string;
begin
  SS := JustFileName(PathName);
  if Pos('.', SS) <> 0 then Result := Copy(SS, 1, Pos('.', SS) - 1)
  else Result := SS
{$ELSE}
begin
  Result := ExtractFileName(PathName);
  Result := Reduce(Result, Length(ExtractFileExt(Result)))
{$ENDIF}
end;

function Mult(const S: string): Integer;
 {-}
var N: Integer;
begin
  N := 0;
  while (S[Succ(N)] = S[1]) and (N < Length(S)) do Inc(N);
  Result := N;
end;

function Num(const S: string; Soustava: Byte): LongInt;
 {-prevede cislo ze soustavy 2..36 na desitkove}
var
  I: Integer;
  N: LongInt;
begin
  N := 0;
  if Soustava in [2..36] then
    for I := 1 to Length(S) do
      if {$IFDEF WINUC}IsLetter(S[I]){$ELSE}CharInSet(UpCase(S[I]), ['A'..'Z']){$ENDIF} then
        N := N * Soustava + Ord(UpCase(S[I])) - Ord('A') + 10
      else
        if {$IFDEF WINUC}IsNumber(S[I]){$ELSE}CharInSet(UpCase(S[I]), ['0'..'9']){$ENDIF} then
          N := N * Soustava + Ord(S[I]) - Ord('0');
  Result := N
end;

function Doc(L: LongInt; const Soustava: Byte): string;
 {-prevede desitkove cislo na cislo ze soustavy 2..36}
var S: string;
  I: Integer;
begin
  S := '';
  if Soustava in [2..36] then
    repeat
      I := L mod Soustava;
      if I in [0..9] then S := Chr(Ord('0') + I) + S
      else S := Chr(I - 10 + Ord('A')) + S;
      L := L div Soustava;
    until L = 0;
  Result := S
end;

{$IFDEF CLR}
{$ELSE}
{$IFDEF UNICODE}
{$ELSE}
function PackNum(const S: AnsiString): AnsiString;
 {-jednoduche zapakovani cisla}
var
  I: Byte;
  SS: AnsiString;
begin
  Result := ''; {vystupni retezec}
  if S = '' then Exit; {kdyz je vstup prazdny pak ven}
  SS := ''; {nuluj pomocne retezce}
  for I := 1 to Length(S) do
  begin
    if Odd(I) {je liche} then
      SS := SS + Chr(16 * (Ord(S[I]) - Ord('0')) + $F)
    else
      Byte(SS[Length(SS)]) := 16 * (Ord(SS[Length(SS)]) shr 4) + (Ord(S[I]) - Ord('0'));
  end;
  Result := SS;
end;

function UnpackNum(const S: AnsiString): AnsiString;
 {-jednoduche rozpakovani cisla}
var
  I, X: Byte;
  SS: AnsiString;
begin
  Result := '';
  if S = '' then Exit;
  SS := '';
  for I := 1 to Length(S) do
  begin
    X := Ord(S[I]);
    if (X shr 4) <> $F then SS := SS + Chr((X shr 4) + Ord('0'));
    if (X and $F) <> $F then SS := SS + Chr((X and $F) + Ord('0'));
  end;
  Result := SS
end;
{$ENDIF}
{$ENDIF}

function Str3Long(const S: string): LongInt;
 {-nacteni cisla ze retezce do prvniho neciselneho znaku}
var
  SS: string;
  I: Byte;
  L: LongInt;
  code: Integer;
begin
  Result := 0;
  SS := Trim(S);
  if SS = '' then Exit; {fixed 19.12.2001}
  I := 1;
  if CharInSet(SS[I], ['+', '-']) then
    Inc(I);
  while {$IFDEF CLR}IsNumber(SS[I]){$ELSE}{$IFDEF UNICODE}IsNumber(SS[I]){$ELSE}CharInSet(SS[I], ['0'..'9']){$ENDIF}{$ENDIF} do
    Inc(I);
  Val(Copy(SS, 1, I - 1), L, code);
  if code = 0 then Result := L;
end;

function Str2Long(const S: string; var I: LongInt): Boolean;
  {-prevede string na longint, true kdyz ok}
{$IFNDEF CONDITIONALEXPRESSIONS}
var
  code: Integer;
{$ENDIF}
begin
  {$IFDEF CONDITIONALEXPRESSIONS}
  Result := TryStrToInt(S, I);
  {$ELSE}
  Val(Trim(S), I, code);
  Result := code = 0;
  {$ENDIF}
end;

function Str2Word(const S: string; var I: Word): Boolean;
  {-prevede string na word, true kdyz ok}
var
  code: Integer;
begin
  Val(Trim(S), I, code);
  Result := code = 0
end;

function Str2Int(const S: string; var I: SmallInt): Boolean;
  {-prevede string na integer, true kdyz ok}
var
  code: Integer;
begin
  Val(Trim(S), I, code);
  Result := code = 0
end;

function Str2Real(const S: string; var R: Real): Boolean;
  {-prevede string na real, true kdyz ok}
var
  code: Integer;
begin
  Val(Trim(S), R, code);
  Result := code = 0
end;

function Str2Single(const S: string; var SS: Single): Boolean;
begin
  {$IFDEF CONDITIONALEXPRESSIONS}
  Result := TryStrToFloat(S, SS);
  {$ELSE}
  try
    SS := StrToFloat(S);
    Result := True;
  except
    Result := False;
  end;
  {$ENDIF}
end;

function Str2Double(const S: string; var D: Double): Boolean;
begin
  {$IFDEF CONDITIONALEXPRESSIONS}
  Result := TryStrToFloat(S, D);
  {$ELSE}
  try
    D := StrToFloat(S);
    Result := True;
  except
    Result := False;
  end;
  {$ENDIF}
end;

function Long2Str(L: LongInt): string;
  {-prevede long/word/integer/byte/shortint na retezec}
begin
  Result := IntToStr(L);
end;

function Real2Str(R: Real; Width, Places: Byte): string;
  {-prevede real na retezec}
begin
  {$IFDEF UNICODE}
  Result := Format('%' + IntToStr(Width) + '.' + IntToStr(Places) +'f', [R]);
  {$ELSE}
  Str(R: Width: Places, Result);
  {$ENDIF}
end;

function Form(const Mask: string; R: Double; const MaskZipChar: Char = 'X'): string;
 {-nove formatovani realneho cisla dle masky - old school method}
  function PW(Zaklad, Na: Integer): Extended; {$IFDEF SUPP_INL}inline;{$ENDIF}
  var I: Integer;
  begin
    Result := 0;
    if (Na = 0) or (Zaklad = 0) then Exit;
    Result := Zaklad;
    for I := 1 to Na - 1 do
      Result := Result * Zaklad
  end;
var
  Tecka: Integer;
  Vysledek, Pred, Za: string;
  Cela, Zlom: string;
  E: Extended;
begin
  if Mask = '' then
  begin
    {$IFDEF UNICODE}
    Vysledek := Real2Str(R, 20, 7);
    {$ELSE}
    Str(R: 20: 7, Vysledek);
    {$ENDIF}
    if Vysledek = '' then Exit;
    Vysledek := Turn(Trim(Vysledek));
    if Vysledek[1] = '0' then
      Vysledek := Copy(Vysledek, Mult(Vysledek), 255);
    Vysledek := Turn(Trim(Vysledek));
    Result := Vysledek;
    Exit;
  end;
  Tecka := Pos({$IFDEF VER15UP}FormatSettings.{$ENDIF}DecimalSeparator, Mask);
  Za := '';
  {maska, jen vyznamne cislice}
  if Tecka <> 0 then
  begin
    Pred := Copy(Mask, 1, Tecka - 1);
    Pred := StripChars(Pred, [AnsiChar(MaskZipChar)]);
    Za := Copy(Mask, Tecka + 1, 255);
    Za := StripChars(Za, [AnsiChar(MaskZipChar)]);
  end
  else
    Pred := StripChars(Mask, [AnsiChar(MaskZipChar)]);
  {$IFDEF UNICODE}
  Vysledek := Real2Str(R, 20, 8);
  {$ELSE}
  Str(R: 20: 8, Vysledek);
  {$ENDIF}
  Cela := Trim(Copy(Vysledek, 1, Pos({$IFDEF VER15UP}FormatSettings.{$ENDIF}DecimalSeparator, Vysledek) - 1));
  Zlom := Trim(Copy(Vysledek, Pos({$IFDEF VER15UP}FormatSettings.{$ENDIF}DecimalSeparator, Vysledek) + 1, 255));
  if Zlom[Length(Zlom)] = '0' then
  begin {odstrani koncove nuly}
    Zlom := Copy(Zlom, 1, Length(Zlom) - Mult(Turn(Zlom)));
    if Zlom = '' then Zlom := '0'; {15.7.1998}
  end;
  {------------------------------------------------------------ CELE CISLO ----}
  if Tecka = 0 then
  begin {celociselne}
    Vysledek := Cela;
    E := Frac(R);
    if E >= 0.5 then E := R + 1 else E := R;
    Vysledek := IntToStr(Trunc(E));
    if Length(Pred) < Length(Vysledek) then
      Vysledek := Change(Mask, MaskZipChar, '*')
    else
      Vysledek := LeftPad(Vysledek, Length(Pred));
    {zaformatuje napr. XXX XXX => 999 999}
    Vysledek := Turn(Zip(Turn(Mask), Turn(Vysledek)));
    Result := Vysledek;
    Exit;
  end;
  {---------------------------------------------------------- REALNE CISLO ----}
  if Length(Cela) > Length(Pred) then
    Vysledek := Change(Mask, MaskZipChar, '*') {preteceni cele casti}
  else
  begin
    Vysledek := Zlom;
    if Za <> '' then
    begin {je-li nejaky}
      if Length(Za) < Length(Vysledek) then
      begin
        E := Frac(R);
        {vynasob}
        E := E * PW(10, Length(Za));
        {zaokrouhli}
        if Frac(E) >= 0.5 then E := E + 1;
        Vysledek := IntToStr(Trunc(E)); //Str(Trunc(E): 20, Vysledek);
        Vysledek := LeftPadCh(TrimLead(Vysledek), '0', 20);
      end
    end;
    Zlom := Trim(Vysledek);
    if Zlom[Length(Zlom)] = '0' then
    begin {odstrani koncove nuly}
      Zlom := Copy(Zlom, 1, Length(Zlom) - Mult(Turn(Zlom)));
      if Zlom = '' then Zlom := '0' {15.7.1998}
    end;
    Vysledek := Turn(Zip(Turn(Copy(Mask, 1, Tecka - 1)), Turn(Cela))) + {$IFDEF VER15UP}FormatSettings.{$ENDIF}DecimalSeparator;
    Cela := Copy(Mask, Tecka + 1, 255);
    Vysledek := Vysledek + Zip(Cela, Copy(Zlom, Length(Zlom) - Length(Cela) + 1, 20));
  end;
  Result := Vysledek;
end;

function Roman2Int(const S: string): LongInt;
 {-rimska cislice do int}
const
  RomanChars = ['C', 'D', 'I', 'L', 'M', 'V', 'X'];
  RomanValues: array['C'..'X'] of Word = (100, 500, 0, 0, 0, 0, 1, 0, 0, 50, 1000, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 10);
var
  Index, Next: Char;
  I: Integer;
  Negative: Boolean;
begin
  Result := 0;
  I := 0;
  Negative := (Length(S) > 0) and (S[1] = '-');
  if Negative then
    Inc(I);
  while (I < Length(S)) do
  begin
    Inc(I);
    Index := UpCase(S[I]);
    if CharInSet(Index, RomanChars) then
    begin
      if Succ(I) <= Length(S) then Next := UpCase(S[I + 1])
      else Next := #0;
      if CharInSet(Next, RomanChars) and (RomanValues[Index] < RomanValues[Next]) then
      begin
        Inc(Result, RomanValues[Next]);
        Dec(Result, RomanValues[Index]);
        Inc(I);
      end
      else
        Inc(Result, RomanValues[Index]);
    end
    else
    begin
      Result := 0;
      Exit;
    end;
  end;
  if Negative then
    Result := -Result;
end;

function Int2Roman(Value: Longint): string;
 {-int na rimskou cislici}
label
  A500, A400, A100, A90, A50, A40, A10, A9, A5, A4, A1;
begin
  Result := '';
  while Value >= 1000 do
  begin
    Dec(Value, 1000); Result := Result + 'M';
  end;
  if Value < 900 then goto A500
  else
  begin
    Dec(Value, 900); Result := Result + 'CM';
  end;
  goto A90;
  A400:
  if Value < 400 then goto A100
  else
  begin
    Dec(Value, 400); Result := Result + 'CD';
  end;
  goto A90;
  A500:
  if Value < 500 then goto A400
  else
  begin
    Dec(Value, 500); Result := Result + 'D';
  end;
  A100:
  while Value >= 100 do
  begin
    Dec(Value, 100); Result := Result + 'C';
  end;
  A90:
  if Value < 90 then goto A50
  else
  begin
    Dec(Value, 90); Result := Result + 'XC';
  end;
  goto A9;
  A40:
  if Value < 40 then goto A10
  else
  begin
    Dec(Value, 40); Result := Result + 'XL';
  end;
  goto A9;
  A50:
  if Value < 50 then goto A40
  else
  begin
    Dec(Value, 50); Result := Result + 'L';
  end;
  A10:
  while Value >= 10 do
  begin
    Dec(Value, 10); Result := Result + 'X';
  end;
  A9:
  if Value < 9 then goto A5
  else
  begin
    Result := Result + 'IX';
  end;
  Exit;
  A4:
  if Value < 4 then goto A1
  else
  begin
    Result := Result + 'IV';
  end;
  Exit;
  A5:
  if Value < 5 then goto A4
  else
  begin
    Dec(Value, 5); Result := Result + 'V';
  end;
  goto A1;
  A1:
  while Value >= 1 do
  begin
    Dec(Value); Result := Result + 'I';
  end;
end;

function ExtractNumber(const S: string): string;
  {-vytahni z retezce pouze cisla}
var I: Integer;
begin
  Result := '';
  if Trim(S) = '' then Exit;
  for I := 1 to Length(S) do
    if {$IFDEF CLR}IsNumber(S[I]){$ELSE}{$IFDEF UNICODE}IsNumber(S[I]){$ELSE}CharInSet(S[I], ['0'..'9']){$ENDIF}{$ENDIF} then
      Result := Result + S[I];
end;

function ExtractAlphaNum(const S: string): string;
  {-vytahni z retezce pouze cisla a znaky}
var I: Integer;
begin
  Result := '';
  if Trim(S) = '' then Exit;
  for I := 1 to Length(S) do
    if CharInSet(S[I], ['0'..'9', 'a'..'z', 'A'..'Z']) then
      Result := Result + S[I];
end;

function ExtractChars(const S: string; chars: CharSet): string;
var I: Integer;
begin
  Result := '';
  if Trim(S) = '' then Exit;
  for I := 1 to Length(S) do
    if CharInSet(S[I], chars) then
      Result := Result + S[I];
end;

function ExtractAlphas(const S: string): string;
  {-vytahni z retezce pouze znaky}
var I: Integer;
begin
  Result := '';
  if Trim(S) = '' then Exit;
  for I := 1 to Length(S) do
    if CharInSet(S[I], ['a'..'z', 'A'..'Z']) then
      Result := Result + S[I];
end;

function StripChars(S: string; ch: CharSet): string;
 {-vytahne jen pozadovane znaky z retezce}
var I: Integer;
begin
  Result := '';
  for I := 1 to Length(S) do
  begin
    if CharInSet(S[I], ch) then
      Result := Result + S[I]
  end;
end;

function htmlSrcEmail(const S: string): string;
 {- search e-mail form source on string}
 {* hledej e-mail adresu v retezci}
const
  PSEM = ['A'..'Z', 'a'..'z', '0'..'9', '_', '-', '.', '@'];
var
  I, N: Integer;
  E: string;
begin
  Result := '';
  I := Pos('@', S);
  if I > 1 then N := I - 1 else N := 1;
  while CharInSet(S[N], PSEM) and (N > 1) do Dec(N);
  if not CharInSet(S[N], PSEM) then Inc(N);
  E := Copy(S, N, 255);
  I := Pos('@', E);
  while CharInSet(E[I], PSEM) and (I < Length(E)) do Inc(I);
  if not CharInSet(E[I], PSEM) then Dec(I);
  E := Copy(E, 1, I);
  if (Length(Copy(E, 1, Pos('@', E) - 1)) > 0)
    and (Length(Copy(E, Pos('@', E) + 1, 255)) > 0) then
    Result := E;
end;

function BitSet(Num, B: Byte): Byte;
begin
  Result := B or (1 shl Num);
end;

function BitIsSet(Num, B: Byte): Boolean;
begin
  Result := (B and (1 shl Num)) <> 0;
end;

function BitReSet(Num, B: Byte): Byte;
begin
  Result := B and ((1 shl Num) xor $FF);
end;

function BitSetToggle(Num, B: Byte): Byte;
begin
  Result := B xor (1 shl Num);
end;

function ChangeXChars(FindChar, DestChar: Char; const Source: string): string;
 {-for change table with spaces to one delimitiers}
 {-pro prevod tabulky s mezerami na jeden oddelovac}
var
  I, N: Integer;
  Q: string;
begin
  Result := '';
  if Source = '' then Exit;
  I := 1;
  while I <= Length(Source) do
  begin
    if Source[I] = FindChar then
    begin
      Q := Copy(Source, I, Length(Source));
      N := Mult(Q);
      if N > 1 then
      begin
        Inc(I, N - 1);
        Result := Result + DestChar;
      end;
    end
    else
      Result := Result + Source[I];
    Inc(I);
  end;
end;

function YesOrNoEx(const B, BoolYes, BoolNo: Boolean): Boolean;
begin
  if B then Result := BoolYes
  else Result := BoolNo;
end;

function YesOrNoEx(B: Boolean; const StrYes: string; const StrNo: string): string;
  {-for convert boolean value to string -> ccYes and ccNo may be redefined}
 {-pro booleanovskou hodnotu mozne pojmenovani}
begin
  if B then Result := StrYes
  else Result := StrNo;
end;

function YesOrNoEx(B: Boolean; const IntYes, IntNo: Integer): Integer;
 {-pro booleanovskou hodnotu mozne pojmenovani}
begin
  if B then Result := IntYes
  else Result := IntNo;
end;

function YesOrNoEx(B: Boolean; const IntYes, IntNo: Int64): Int64;
begin
  if B then Result := IntYes
  else Result := IntNo;
end;

function TestTo(S: string; SArr: array of string; CaseSensitive: Boolean): Boolean;
 {-provede test zda nejaky ze sady argumentu je stejny jako vstupni retezec}
var I: Integer;
begin
  Result := True;
  if CaseSensitive then
  begin
    for I := Low(SArr) to High(SArr) do
      if AnsiCompareStr(S, SArr[I]) = 0 then Exit;
  end
  else
    for I := Low(SArr) to High(SArr) do
      if AnsiCompareText(S, SArr[I]) = 0 then Exit;
  Result := False;
end;

function TestToReplace(const S: string; SArr: array of string; const NewValue: string): string;
 {-provede test na retezec a pak zamenu, bez ohledu na velikost pismen}
var
  i: Integer;
begin
  Result := S;
  if S = '' then Exit;
  if Length(SArr) = 0 then Exit;
  for i := Low(SArr) to High(SArr) do
    if Pos(AnsiLowerCase(SArr[i]), AnsiLowerCase(S)) > 0 then
      Result := StringReplace(Result, SArr[i], NewValue, [rfReplaceAll, rfIgnoreCase]);
end;

function TestIntTo(N: Integer; arg: array of Integer): Boolean;
 {-provede test zda nejaky ze sady argumentu je stejny jako vstupni cislo}
var I: Integer;
begin
  Result := True;
  for I := Low(arg) to High(arg) do
    if N = arg[I] then Exit;
  Result := False;
end;

function TestBeginTo(S: string; arg: array of string): Boolean;
 {-provede test zda nejaky ze sady argumentu je stejny jako prvnich n-znaky vstupniho retezce}
var
  i: Integer;
begin
  Result := False;
  if S = '' then Exit;
  if Length(arg) = 0 then Exit;

  for I := Low(arg) to High(arg) do
  begin
    if Pos(AnsiLowerCase(arg[i]), AnsiLowerCase(S)) = 1 then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function TestEndTo(const S: string; arg: array of string): Boolean;
 {-provede test zda nejaky ze sady argumentu je stejny jako poslednich n-znaku vstupniho retezce}
var
  i, idx: Integer;
begin
  Result := False;
  if S = '' then Exit;
  if Length(arg) = 0 then Exit;

  for i := Low(arg) to High(arg) do
  begin
    idx := Pos(AnsiLowerCase(arg[i]), AnsiLowerCase(S));
    if idx > 0 then
      if idx = (Length(S) - Length(arg[I]) + 1) then
      begin
        Result := True;
        Exit;
      end;
  end;
end;

function SwitchTo(co: string; arg: array of string): string;
  {-provede case s AnsiStringem}
  {-podobne DECODE()}
var
  i, j: Integer;
  pol, res: string;
begin
  for i := Low(arg) to High(arg) div 2 do
  begin
    j := i * 2;
    pol := arg[j];
    if (j + 1) > High(arg) then Break;
    res := arg[j + 1];
    if AnsiCompareText(co, pol) = 0 then
    begin
      Result := res;
      Exit;
    end;
  end;
  {part else}
  if (high(arg) mod 2) = 0 then
    Result := arg[high(arg)];
end;

function SwitchIntTo(co: Integer; arg: array of Integer): Integer;
  {-provede case s cislem}
  {-podobne DECODE()}
var i, j, pol, res: Integer;
begin
  Result := 0;
  for i := Low(arg) to High(arg) div 2 do
  begin
    j := i * 2;
    pol := arg[j];
    if (j + 1) > High(arg) then Break;
    res := arg[j + 1];
    if co = pol then
    begin
      Result := res;
      Exit;
    end;
  end;
  {part else}
  if (high(arg) mod 2) = 0 then
    Result := arg[high(arg)];
end;

function PosN(Substring, Mainstring: string; occurrence: integer): integer;
{
Function PosN get recursive - the "occurrence" the position of "Substring" in
"Mainstring". Does the Mainstring not contain Substring the result
is 0. Works with chars and AnsiStrings.

Examples :
  i:=PosN('s','swissdelphicenter.ch',2);
  Result -> i=4
  i:=posn('x','swissdelphicenter.ch',1);
  Result -> i=0
  i:=posn('delphi','swissdelphicenter.ch',1);
  Result -> i=6
}
begin
  if Pos(substring, mainstring) = 0 then
  begin
    Result := 0;
    Exit;
  end
  else
  begin
    if occurrence = 1 then
      Result := Pos(substring, mainstring)
    else
    begin
      Result := Pos(substring, mainstring)
        + PosN(substring, Copy(mainstring, (Pos(substring, mainstring) + 1), Length(mainstring)), occurrence - 1);
    end;
  end;
end;

//tips here

{
from Thomas Mueller
Sometimes you probably have written something like this:
s := Format('Hello %s, your name is %s %s', [FirstName, FirstName, LastName]);
(an admittedly stupid example ;-) )
And if you do, you probably found it annoying that you need to specify the FirstName parameter
twice, in particular if there are lots of similar lines.
But this isn't necessary because you can specify the parameter position to use for the placeholder
in the format string like this:
s := Format('Hello %0:s, your name is %0:s %1:s', [FirstName, LastName]);
Just one more example from a code generator I am currently writing:
TableName := 'Customer';
...
s := Format(' f%0:sTableAuto := T%0:sTableAuto.Create(f%0:Table);', [TableName]);
which results in
s := ' fCustomerTableAuto := TCustomerTableAuto.Create(fCustmerTable);';
}

// from Heiko Schrˆder
// Get the Position of a string, starting at the end
// R¸ckw‰rtiges Vorkommen einer Zeichenkette innerhalb eines AnsiStrings, Position von hinten

function LastPos(SearchStr, Str: string): Integer;
var
  i: Integer;
  TempStr: string;
begin
  Result := Pos(SearchStr, Str);
  if Result = 0 then Exit;
  if (Length(Str) > 0) and (Length(SearchStr) > 0) then
  begin
    for i := Length(Str) + Length(SearchStr) - 1 downto Result do
    begin
      TempStr := Copy(Str, i, Length(Str));
      if Pos(SearchStr, TempStr) > 0 then
      begin
        Result := i;
        break;
      end;
    end;
  end;
end;

// Search for the next occurence of a string from a certain Position
// N‰chstes Vorkommen einer Zeichenkette ab einer frei definierbaren Stelle im string

function NextPos(SearchStr, Str: string; Position: Integer): Integer;
begin
  Delete(Str, 1, Position - 1);
  Result := Pos(SearchStr, StrUpCase(Str));
  if Result = 0 then Exit;
  if (Length(Str) > 0) and (Length(SearchStr) > 0) then
    Result := Result + Position + 1;
end;

// Get the number of characters from a certain Position to the string to be searched
// Anzahl der Zeichen von einer definierbaren Position zur gesuchten Zeichenkette

function NextPosRel(SearchStr, Str: string; Position: Integer): Integer;
begin
  Delete(Str, 1, Position - 1);
  Result := Pos(SearchStr, StrUpCase(Str)) - 1;
end;

// simple replacement for AnsiStrings
// einfaches Ersetzen von Zeichenketten

function ReplaceStr(Str, SearchStr, ReplaceStr: string): string;
begin
  while Pos(SearchStr, Str) <> 0 do
  begin
    Insert(ReplaceStr, Str, Pos(SearchStr, Str));
    Delete(Str, Pos(SearchStr, Str), Length(SearchStr));
  end;
  Result := Str;
end;

//from Ollo Heﬂ  (Ollo Hess)

function MailURLMayBeInvalid(const s: string): Boolean;
const
  ccInvalidsC = [' ', '‰', 'ˆ', '¸', 'ﬂ', '[', ']', '(', ')', ':'];
var
  i: Integer;
  SY: string;
  SX: string;
  {$IFDEF VER9UP}Z: Char;{$ENDIF}
begin // ' ', ‰, ˆ, ¸, ﬂ, [, ], (, ), : in EMail-Address
  Result := True; {wrong}
  SX := Trim(StrLoCase(s));
  if SX = '' then Exit;
  if not Result then
  {$IFDEF VER9UP}
    for Z in SX do
      if CharInSet(Z, ccInvalidsC) then Exit;
  {$ELSE}
    for i := 1 to Length(SX) do
      if CharInSet(SX[I], ccInvalidsC) then Exit;
  {$ENDIF}
  { @ not in EMail-Address }
  i := Pos('@', SX);
  if i in [0, 1, Length(SX)] then Exit;
  if (Pos('@', Copy(SX, i + 1, Length(SX) - 1)) > 0) then Exit; // Domain <= 1
  SY := Copy(SX, i + 1, Length(SX));
  if Length(SY) <= 1 then Exit;
  Result := Pos('.', SY) in [0, 1, Length(SY)];
end;

function IsValidEmail(const Value: string): Boolean;
  function CheckAllowed(const s: string): Boolean;
  var
    i: integer;
  begin
    Result := False;
    for i := 1 to Length(s) do
    begin
      // illegal char in s -> no valid address
      if not CharInSet(s[i], ['a'..'z', 'A'..'Z', '0'..'9', '_', '-', '.']) then
        Exit;
    end;
    Result := True;
  end;
var
  i: Integer;
  NamePart, ServerPart, s: string;
begin
  Result := False;
  if Trim(Value) = '' then Exit;
  i := Pos('@', Value);
  if (i = 0) or (pos('..', Value) > 0) then Exit;

  NamePart := Copy(Value, 1, i - 1);
  if Length(NamePart) = 0 then Exit;
  if (NamePart[1] = '.') or (NamePart[Length(NamePart)] = '.') then Exit;
  //test __@a.aa
  if Length(StripChars(NamePart, ['_'])) = Length(NamePart) then Exit;
  ServerPart := Copy(Value, i + 1, Length(Value));
  if Length(ServerPart) = 0 then Exit;
  if (ServerPart[1] = '.') or (ServerPart[Length(serverPart)] = '.') then Exit;
  //a@a.com or a@a.cz
  if Length(ServerPart) < 4 then Exit;
  i := Pos('.', ServerPart);
  //a@dddd or a@aaa.a  or a@a.aa.
  if (i = 0) or (i > (Length(ServerPart) - 2)) then Exit;
  //test a@a.__
  s := Turn(ServerPart);
  s := Turn(Copy(s, 1, Pos('.', s) - 1));
  if Length(StripChars(s, ['_'])) = Length(s) then Exit;
  //test a@__.cz
  s := Turn(ServerPart);
  s := Turn(Copy(s, Pos('.', s) + 1, Length(s)));
  if Length(StripChars(s, ['_'])) = Length(s) then Exit;
  if IsNumber(s) then Exit; //no numbers in high domain
  if Pos('-.', Value) > 0 then Exit;
  if Pos('.-', Value) > 0 then Exit;
  if Copy(NamePart, 1, 1) = '-' then Exit;
  if Copy(NamePart, Length(NamePart), 1) = '-' then Exit;
  if Copy(ServerPart, 1, 1) = '-' then Exit;
  if Copy(ServerPart, Length(ServerPart), 1) = '-' then Exit;
  Result := CheckAllowed(NamePart) and CheckAllowed(ServerPart);
end;

//Can be use for Delphi 3 well

function PosEx(const SubStr, S: string; Offset: Cardinal): Integer;
var
  I, X: Integer;
  Len, LenSubStr: Integer;
begin
  if Offset = 1 then
    Result := Pos(SubStr, S)
  else
  begin
    I := Offset;
    LenSubStr := Length(SubStr);
    Len := Length(S) - LenSubStr + 1;
    while I <= Len do
    begin
      if S[I] = SubStr[1] then
      begin
        X := 1;
        while (X < LenSubStr) and (S[I + X] = SubStr[X + 1]) do
          Inc(X);
        if (X = LenSubStr) then
        begin
          Result := I;
          Exit;
        end;
      end;
      Inc(I);
    end;
    Result := 0;
  end;
end;

{
 VERY fast split function
 this function returns part of a string based on
 constant defineable delimiters, such as ";". So
 SPLIT('this is a test ',' ',3) = 'is' or
 SPLIT('data;another;yet;again;more;',';',4) = 'yet'

 Split function shifts index integer by two to
 be compatible with commonly used PD split function
 gpl 2004 / Juhani Suhonen
}

function Split(input: string; splitChar: Char; s: Integer): string;
var
  c: array of Integer;
  b, t: Integer;
begin
  Dec(s, 2);  // for compatibility with very old & slow split function
  t := 0;     // variable T needs to be initialized...
  SetLength(c, Length(input));
  for b := 0 to pred(High(c)) do
  begin
    c[b + 1] := PosEx(splitChar, input, succ(c[b]));
    // BREAK LOOP if posex looped (position before previous)
    // or wanted position reached..
    if (c[b + 1] < c[b]) or (s < t) then break
    else
      Inc(t);
  end;
  Result := Copy(input, succ(c[s]), pred(c[s + 1] - c[s]));
end;

function IsNumber(S: string): Boolean;
var R: Real;
begin
  Result := Str2Real(S, R)
end;

function FormatStr4CGI(str: string): string;
var
  i: integer;
begin
  for i := 1 to Length(str) do
  begin
    if CharInSet(str[i], ['a'..'z', 'A'..'Z', '0', '1'..'9']) then
      Result := Result + Str[i]
    else
      if Str[i] = ' ' then
        Result := Result + '+'
      else
        Result := Result + '%' + IntToHex(Byte(Str[i]), 2);
  end;
end;

function ChangeFilename(Filename: string; const prefix, postfix: string): string;
var
  p, e, n: string;
begin
  p := ExtractFilePath(Filename);
  n := ExtractFileName(Filename);
  e := ExtractFileExt(Filename);
  {$IFNDEF CLR}System.{$ENDIF}Delete(n, Length(n) - Length(e) + 1, Length(e));
  Result := p + prefix + n + postfix + e;
end;

function ExtractQuoteStr(QuoteStr: string; QuoteChar: Char {$IFNDEF CLR}= '"'{$ENDIF}): string;
{$IFNDEF CLR}
var
  P: PChar;
{$ENDIF}
begin
  {$IFDEF CLR}
  Result := DequotedStr(QuoteStr, QuoteChar);
  {$ELSE}
  P := PChar(QuoteStr);
  Result := AnsiExtractQuotedStr(P, QuoteChar);
  if Result = '' then
    Result := QuoteStr; //neni ""
  {$ENDIF}
end;

function WrapQuoteStr(Str: string; QuoteChar: Char = '"'): string;
begin
  {$IFDEF CLR}
  Result := QuotedStr(Str, QuoteChar);
  {$ELSE}
  Result := AnsiQuotedStr(Str, QuoteChar);
  {$ENDIF}
end;

function xmlCC(comment: string): string;
{komentar}
begin
  Result := '<!-- ' + comment + ' -->';
end;

function xmlPP2Str(iparam, ivalue: string): string;
{pouze formatovaci funkce}
{parametry}
{iparam ... nazev parametru}
{ivalue ... hodnota parametru, uzavrena do dvojitych uvozovek}
begin
  if ivalue = '' then
    Result := ''
  else
    Result := Format('%s="%s" ', [iparam, ivalue]);
end;

function xmlPP2Str(iparam: string; ivalue: Integer): string;
begin
  Result := Format('%s="%d" ', [iparam, ivalue]);
end;

function xmlPP2Str(iparam: string; ivalue: Boolean): string;
begin
  Result := Format('%s="%s" ', [iparam, YesOrNoEx(ivalue,'true', 'false')]);
end;

function xmlDP2T(iname, iparam, tagcode: string; breaklines: Boolean = False): string;
//<iname iparam>
//  tagcode
//</iname>
{pouze formatovaci funkce}
{parametry}
{iname ... jmeno tagu}
{iparam .. zakladni parametry jako str}
{tagcode . zanoreny tag (slozenina, cokoli atp...)}
const
  ctag = '<%s%s>' + xmlCrLf + '%s' + xmlCrLf + '</%s>' + xmlCrLf;
  c_tag = '<%s%s>%s</%s>';
  stag = '<%s%s/>' + xmlCrLf;
  s_tag = '<%s%s/>';
var
  S: string;
begin
  S := '';
  if iparam <> '' then S := ' ' + Trim(iparam); //to je jen kvuli nadbytecne mezere
  if tagcode = '' then //pokud je vlozeny kod prazdny tak ho hned uzavri
    if breaklines then
      Result := Format(stag, [iname, S]) //pokud to bude nekdy nutne je mozne vracet '' a tag vubec nevytvaret
    else
      Result := Format(s_tag, [iname, S])
  else
    if breaklines then
      Result := Format(ctag, [iname, S, tagcode, iname])
    else
      Result := Format(c_tag, [iname, S, tagcode, iname]);
end;

function xmlD2T(iname, tagcode: string; breaklines: Boolean = False): string;
//<iname>
//  tagcode
//</iname>
{pouze zjednodusena formatovaci funkce}
{parametry}
{iname ... jmeno tagu}
{tagcode . zanoreny tag (slozenina, cokoli atp...)}
begin
  Result := xmlDP2T(iname, '', tagcode, breaklines);
end;

end.