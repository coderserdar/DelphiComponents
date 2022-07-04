unit API_strings;

//------------------------------------------------------------------------------
// component free to use and modify, subject to following restinctions:
// 1. do not mispresent the origin
// 2. altered revisions must be clearly marked as modified from the original
// 3. do not remove this notice from the source code
// 4. send email about bugs, features needed and features you would like
// * if you like this very much, feel free to donate and support supporting
// and developing the package at www.paypal.com - ari pikivirta@kolumbus.fi
//------------------------------------------------------------------------------
// string related functions collection;
//------------------------------------------------------------------------------
//
// 08102009, ari pikivirta
//  * fixed bug on GetNextToken function
//
// 02102009, ari pikivirta
//  * bug fix for stringtolist function
//
// 02072009, ari pikivirta
//  * added ListToString and StringToList functions
//  * added optional parameter NoCase to Match function
//
// 30062009, ari pikivirta
//  * added QuoteIfSpaces function
//
// 29062009, ari pikivirta
//  * added Sort function
//
// 5.6.2009, ari pikivirta
//  * added GetNextToken function (and changed split strings to use this same)
//  * fixed pos when using startfrom result -1 added (heck all recode using pos after this change)
//
// 1.6.2009, ari pikivirta
//  * added StringToTime(const S: String; var T: TDateTime) function
//  * added LINEFEED to SplitString function default separators list
//  * added CompareStringsInPercent function,  result = 0-100%
//  * added procedure SwapStrings
//
// 28052009, ari pikivirta
//  * fixed bugs on StreamReadLn and StreamWriteLn functions (is used by API_Workbook)
//  * added splitstring function with some of the basic separtors
//
// 19052009, ari pikivirta
//  * added StringBeginsWith
//  * added StringEndsWith
//  * added AddBackslash
//  * added RemoveBackslash
//
// 17042009, ari pikivirta
//  * added function PosFromEnd
//
// 14042009, ari pikivirta
//  * added allowretry const to paremeter of openstrings and savestrings to/from file
//  * clearing strings in any case on opening strings from file
//
// 05012009, ari pikivirta
//  * added functions to read and write strings as lines with streams
//
// 01012009, ari pikivirta
//  * new function ShortenString added
//
// 31122008, ari pikivirta
//  * added new function GeneratePass
//
// 30122008, ari pikivirta
//  * added support for embedded tags to FindTagText function(s)
//  * new functions to manage strings directory to/from file
//
// 19112008, ari pikivirta
//  * added open file to strings and save strings to file functions optimized
//
// 07102008, ari pikivirta
//  * fixed bug in pos function with start position
//
// 15092008, ari pikivirta
//  * added function TimeToString to show time over 24h like hhhhh:mm:ss
//  * added StringExists function to check if string exists in another string
//
// 03092008, ari pikivirta
//  * added SameString function with case option
//  * removed ReplaceAll function (stringreplace will do exactly same thing)
//
// 02092008, ari pikivirta
//  * fixed bug in stringreplace function
//  * added new function FindTagText
//
// 31082008, ari pikivirta
//  * added no case option to the stringreplace (default true)
//  * added Pos function to replace one in sysutils with "no case" option, default false
//    extended with posex functionality as an option
//  * inttostring renamed to inttostr with default leading zeros set to 0
//
// 27082008, ari pikivirta
//  * added charreplace function
//  * added stringreplace function (maby needs some optimization still..)
//  * added custom lower- and uppercase function to have åäöü included
//
// 02072008, ari pikivirta
//  * added URLEncode and URLDecode functions (use existing functions)
//  * Added BinToString and StringtoBin functions
//
// 16062008, ari pikivirta
//  * added RFC112 date encode & decode to string function
//  * added stringtohtml and htmltostring functions
//  * added IntToString function with leading zeros parameter
//

//{$define DEBUGSTRINGS}

interface

uses
  Classes, Sysutils,
  {$ifdef DEBUGSTRINGS}
  Dialogs,
  {$endif}
  Graphics;

const
  RETRYCOUNTONREADINGORWRITINGFILE = 10;  // 10*50ms = 500ms at maximum

function  Pos(const LocateThis, FromThisString: Ansistring; const NoCase: Boolean = FALSE; const StartFrom: Integer = 1): integer; // pos replacement
function  PosFromEnd(const LocateThis, FromThisString: AnsiString; const NoCase: boolean = FALSE): integer; // pos from end

procedure CharReplace(var InputString: AnsiString; const OldCh, NewCh: AnsiChar); // replace one character
procedure CharsReplace(var InputString: AnsiString; const OldChs, NewChs: AnsiString); // replcae multiple characters
procedure StringReplace(var InputString: Ansistring; const OldS, NewS: AnsiString; NoCase: Boolean = TRUE); // replace strings (sysutils replacemenet)
function  ReplaceFirst(const SourceStr, FindStr, ReplaceStr: Ansistring): AnsiString;

function  LowerCase(s: Ansistring): Ansistring;
function  UpperCase(s: Ansistring): Ansistring;
function  IntToStr(I: int64; LeadingZeros: Integer = 0): string;
function  BytesToStr(const Size: Int64): string;
function  TimeToString(T: TDateTime): string;
function  StringToTime(const S: String; var T: TDateTime): boolean;
function  RFC1123ToDateTime(Date: string): TDateTime;
function  DateTimeToRFC1123(aDate: TDateTime): string;
function  BinToString(stream: tstream): string;
function  StringToBin(s: string; stream: tstream): boolean; // true if succesfull
function  StrToPchar(var S: String): PAnsiChar;
function  ShortenString(Const S: AnsiString; Const MaxLength: Integer = 128; ReplaceCRWith: AnsiString = '^'; ReplaceLFWith: AnsiString = ''): AnsiString;
procedure SwapStrings(var s1, s2: string);
function  QuoteIfSpaces(Const InputString: AnsiString): AnsiString;

function  GetNextToken(Var SourceString, TokenFound: String; Const Separators: AnsiString = ' ;:,.!"£%&/()[]{}=?\*'+#13#10): boolean;
procedure SplitString(const S: String; var List: TStringlist; Const Separators: AnsiString = ' ;:,.!"£%&/()[]{}=?\*'+#13#10);
function  CompareStringsInPercent(Const Str1, Str2: Ansistring): Byte;
procedure Sort(var Strings: TStrings; Const Start: Integer = 0; Const Stop: Integer = 0);

function  ListToString(Const List: TStrings; Const Separator: AnsiChar = ';'): AnsiString; overload;
procedure ListToString(Const List: TStrings; var OutputString: AnsiString; Const Separator: AnsiChar = ';'); overload;
procedure StringToList(Const InputString: AnsiString; List: Tstrings; Const Separator: AnsiChar = ';');

function  OpenFileToStrings(const Filename: String; Strings: tstrings; const AllowRetry: Boolean = TRUE): boolean;
function  OpenFileToString(const FileName: string; var S: String; const AllowRetry: Boolean = TRUE): boolean;
function  SaveStringsToFile(const Filename: String; Strings: tstrings; const AllowRetry: Boolean = TRUE): boolean;
function  SaveStringToFile(const Filename, S: string; const AllowRetry: Boolean = TRUE): boolean;
function  AppendStringsToFile(const Filename: String; Strings: tstrings ): boolean;
function  AppendStringToFile(const Filename, S: String): boolean;

function  StreamWrite(const Str: string; Stream: TStream): Int64;
function  StreamWriteLn(Const S: String; Stream: TStream; Const Append: Boolean = TRUE; Const EOLN: String = #13#10): Int64;
function  StreamReadLn(Stream: TStream; Const EOLN: String = #13#10): String;

Function  Match(Const N1, N2: AnsiString; Const NoCase: Boolean = TRUE): Boolean;
function  SameString(Const s1, s2: AnsiString; Const Nocase: Boolean = TRUE): boolean;
function  StringExists(const LookFor, FromThisString: AnsiString; const NoCase: Boolean = TRUE): boolean;
function  StringBeginsWith(const LookFor, FromThisString: AnsiString; const NoCase: Boolean = TRUE): boolean;
function  StringEndsWith(const LookFor, FromThisString: AnsiString; const NoCase: Boolean = TRUE): boolean;
function  AddBackslash(const AFilename: AnsiString): AnsiString;
function  RemoveBackslash(const AFilename: AnsiString): AnsiString;

function  FindTagText(Const OrigText: AnsiString; StartFrom: Integer; var Position: integer; var TagContentStart: integer; var TagLength: integer; Const StartTag: AnsiString = '<'; Const EndTag: AnsiString = '>'): AnsiString; overload;
function  FindTagText(Const OrigText: Ansistring; StartFrom: integer; StartTag: AnsiString = '<'; EndTag: AnsiString = '>'): Ansistring; overload;
function  FindTagText(Const OrigText: Ansistring; StartFrom: integer; var Position: integer; StartTag: AnsiString = '<'; EndTag: AnsiString = '>'): AnsiString; overload;
function  HtmlParamsToString(S: Ansistring ): Ansistring;
function  URLDecode(Value: Ansistring): Ansistring; // same as HtmlParamsToString
function  StringToHtmlParams(Str: Ansistring): Ansistring;
function  URLEncode(Value: Ansistring): Ansistring; // same as StringToHtmlParams

function  BitmapToRTF(pict: TBitmap): string;
function  Encode64(S: Ansistring): AnsiString;
function  Decode64(S: Ansistring): AnsiString;
function  Encrypt(const InString: Ansistring; StartKey, MultKey, AddKey: Integer): Ansistring;
function  Decrypt(const InString: Ansistring; StartKey, MultKey, AddKey: Integer): Ansistring;
function  Hash(Const text: AnsiString): Integer;
function  GeneratePass(syllables, numbers: Byte): string;

implementation

{$include '..\API_source\inc\CompilerVersions.INC'}
{$WARN UNSAFE_CODE OFF}

uses
  dialogs;

const
  Codes64 = ansistring('0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz+/');

//------------------------------------------------------------------------------
function ListToString(Const List: TStrings; Const Separator: AnsiChar = ';'): AnsiString;
var
  i: integer;
begin
  Result:= ''; // clear result
  for i:=0 to list.count-1 do
  begin
    Result:= Result + ansistring(List[i]) + Separator;
  end;
end;

procedure ListToString(Const List: TStrings; var OutputString: AnsiString; Const Separator: AnsiChar = ';');
begin
  OutputString:= ListToString(List, Separator);
end;

procedure StringToList(Const InputString: AnsiString; List: Tstrings; Const Separator: AnsiChar = ';');
var
  i, SF: integer;
  s: ansistring;
begin
  List.Clear;
  //
  SF:= 1;
  repeat
    i:= api_strings.pos(Separator, InputString, FALSE, SF);
    if (i>0) then
    begin
      s:= copy(InputString, sf, i-sf); // copy finding
      List.Add(string(s)); // even if it's empty since 02102009
      sf:= i+1; // look for next separtor
      (* debug:
      showmessage(inttostr(sf)+'-'+inttostr(i)+'='+s);
      :gubed *)
    end;
  until (i<1);
end;

//------------------------------------------------------------------------------
function QuoteIfSpaces(Const InputString: AnsiString): AnsiString;
begin
  if pos(' ', InputString)>0 then
  begin
    result:= '"'+InputString+'"';
  end else
    result:= InputString; // no quotes needed
end;

//------------------------------------------------------------------------------
procedure Sort(var Strings: TStrings; Const Start: Integer = 0; Const Stop: Integer = 0);
var
  Left, Right, Mid, AStop, AStart: Integer;
  Pivot, Temp: string;
begin
  Astart:= Start;
  AStop:= Stop;
  if AStop<1 then AStop:= Strings.Count; // set to last
  if Start>AStop then
  begin
    Mid:= AStop;
    AStart:= AStop;
    AStop:= Mid; // swap
  end;
  Left:= AStart;
  Right:= AStop;
  Mid:= (AStart + AStop) div 2;
  Pivot:= Strings[mid];
  repeat
    while (Strings[Left]<Pivot) do Inc(Left);
    while (Pivot<Strings[Right]) do Dec(Right);
    if (Left<=Right) then
    begin
      Temp:= Strings[Left];
      Strings[Left]:= Strings[Right]; // Swops the two Strings
      Strings[Right]:= Temp;
      Left:= Left + 1;
      Right:= Right - 1;
    end;
  until (Left>Right);
  if (AStart<Right) then Sort(Strings, AStart, Right); // Uses
  if (Left<AStop) then Sort(Strings, Left, AStop); // Recursion
end;

//------------------------------------------------------------------------------
function GetNextToken(Var SourceString, TokenFound: String; Const Separators: AnsiString = ' ;:,.!"£%&/()[]{}=?\*'+#13#10): boolean;
// function will look for next string from given source string and
// return it as result, sourcestring is cut to the finding.
var
  i, p, t: integer;
  s1, s2: ansistring;
begin
  result:= FALSE; // assume no tokens will be found
  s1:= ansistring(trim(string(SourceString))); // get rid of spaces at end and beginning
  s2:= '';
  //
  if length(sourcestring)<1 then exit; // exit if nothing was left after trim
  if length(separators)<1 then exit; // exit if no separators given as param
  //
  // scan for separators
  p:= length(s1);
  for i:=1 to length(separators) do // go trough all separators
  begin
    t:= pos(separators[i], s1); // look for separator position
    if (t>0) and (t<p) then p:= t; // closer than previous finding
  end;
  if (p<length(s1)) then // if some separator was found
  begin
    TokenFound:= string(copy(s1, 1, p-1)); // give next token as result
    delete(s1, 1, p); // delete finding from source string
    SourceString:= string(s1); // assign source string back
    result:= TRUE;
  end else
    TokenFound:= sourcestring; // no separators found, give source as result
end;

//------------------------------------------------------------------------------
procedure SplitString(const S: String; var List: TStringlist; Const Separators: AnsiString = ' ;:,.!"£%&/()[]{}=?\*'+#13#10);
var
  found: boolean;
  temps: String;
  res: String;
begin
  temps:= s; // copy source to temp string
  list.clear;
  repeat
    found:= GetNextToken(temps, res, Separators);
    list.add(res);
  until (not found);
end;

//------------------------------------------------------------------------------
// Write a string to the stream
function StreamWrite(const Str: string; Stream: TStream): Int64;
begin
  if not assigned(Stream) then raise exception.Create('No stream assigned');
  StreamWrite:= Stream.Write(Pointer(@Str[1])^, Length(Str));
end;

function StreamWriteLn(Const S: String; Stream: TStream; Const Append: Boolean = TRUE; Const EOLN: String = #13#10): Int64;
var
  tmp: string;
begin
  if not assigned(Stream) then raise exception.Create('No stream assigned');
  if Append then Stream.Seek(int64(0), soFromEnd);
  tmp:= s + EOLN;
  result:= StreamWrite(tmp, Stream);
end;

//------------------------------------------------------------------------------
function StreamReadLn(Stream: TStream; Const EOLN: String = #13#10): String;
var
  tmp: String;
  i: integer;
  buf: array[0..1023] of byte;
  br, cp: int64;
begin
  if not assigned(Stream) then raise exception.Create('No stream assigned');
  tmp:= '';
  while (stream.Position<stream.size) do
  begin
    cp:= stream.Position; // get current stream position
    br:= Stream.Read(buf, sizeof(buf));
    if (br>0) then
    begin
      for i:=0 to br-1 do tmp:= tmp + chr(buf[i]); // copy from buffer to string
      i:= system.pos(EOLN, tmp);
      if (i>0) then
      begin
        result:= copy(tmp, 1, i-1); // copy resulting string
        stream.seek(int64(cp+i+length(EOLN)-1), soFromBeginning);
        {$ifdef DEBUGSTRINGS}
        showmessage(result+' (len='+inttostr(length(result))+', '+inttostr(cp)+'-'+inttostr(stream.position)+')');
        {$endif}
        exit; // exit already here with result filled
      end;
    end else
      raise exception.Create('Stream read error.');
  end;
end;

//------------------------------------------------------------------------------
function ShortenString(Const S: AnsiString; Const MaxLength: Integer = 128; ReplaceCRWith: AnsiString = '^'; ReplaceLFWith: AnsiString = ''): AnsiString;
begin
  result:= s;
  StringReplace(result, #13, ReplaceCRWith);
  StringReplace(result, #10, ReplaceLFWith);
  if (length(result)>3) and (maxlength>3) then
  begin
    result:= copy(result, 1, maxlength-2);
    if (length(result)=maxlength-2) then result:=result+'..';
  end;
end;

//------------------------------------------------------------------------------
// Generate passowrd function
//------------------------------------------------------------------------------
function GeneratePass(syllables, numbers: Byte): string;
  //
  function Replicate(Caracter: string; Quant: Integer): string;
  var
    I: Integer;
  begin
    Result := '';
    for I := 1 to Quant do
      Result := Result + Caracter;
  end;
  //
const
  charmap: array [0..24] of AnsiChar = ('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'v', 'w', 'x', 'y', 'z');
var
  i: integer;
  si, sf: Longint;
  n: string;
begin
  Result := '';
  Randomize;
  if syllables <> 0 then
    for i:= 0 to syllables-1 do
      if random(2)=0 then Result := Result + string(charmap[random(24)])
        else result:= result + string(UpperCase(charmap[random(24)]));
  if numbers = 1 then Result := Result + IntToStr(Random(9))
  else if numbers >= 2 then
  begin
    if numbers > 9 then numbers := 9;
    si     := StrToInt('1' + Replicate('0', numbers - 1));
    sf     := StrToInt(Replicate('9', numbers));
    n      := FloatToStr(si + Random(sf));
    Result := Result + Copy(n, 0,numbers);
  end;
end;

//------------------------------------------------------------------------------
// open file and read contents to stream, deny no others to read
// nor write to the file same time
//------------------------------------------------------------------------------
// 14042009api; clearing strings even file doesn't exist
function OpenFileToStrings(const Filename: String; Strings: tstrings; const AllowRetry: Boolean = TRUE): boolean;
var
  fs: tfilestream;
  retrycount: integer;
begin
  result:= FALSE;
  Strings.Clear;
  if fileexists(Filename) then
  begin
    retrycount:= 0;
    while (retrycount<RETRYCOUNTONREADINGORWRITINGFILE) and (not result) do
    try
      fs:= tfilestream.Create(Filename, fmOpenRead or fmShareDenyNone);
      try
        fs.Seek(int64(0), soFromBeginning);
        strings.LoadFromStream(fs);
        result:= TRUE;
      finally
        freeandnil(fs);
      end;
    except
      retrycount:= retrycount + 1;
      sleep(50);
    end;
  end;
end;

function OpenFileToString(const FileName: string; var S: String; const AllowRetry: Boolean = TRUE): boolean;
var
  sl: tstringlist;
begin
  sl:= tstringlist.create;
  try
    result:= OpenFileToStrings(FileName, sl, AllowRetry);
    s:= sl.Text;
  finally
    freeandnil(sl);
  end;
end;

//------------------------------------------------------------------------------
// save to file, allow other threads to read from the same file,
// but don't let them to write into this file simultaneously
function SaveStringsToFile(const Filename: String; Strings: tstrings; const AllowRetry: Boolean = TRUE): boolean;
var
  fs: tfilestream;
  retrycount: integer;
begin
  result:= FALSE;
  retrycount:= 0;
  while (retrycount<RETRYCOUNTONREADINGORWRITINGFILE) and (not result) do
  try
    if not fileexists(FileName) then
      fs:= tfilestream.Create(FileName, FmCreate or fmShareDenyWrite) // create new file
      else fs:= tfilestream.Create(FileName, FmOpenReadWrite or fmShareDenyWrite); // open for modifying
    try
      fs.seek(int64(0), soFromBeginning);
      strings.SaveToStream(fs);
      fs.Size:= fs.Position; // cut rest of the file!
      result:= TRUE;
    finally
      freeandnil(fs);
    end;
  except
    retrycount:= retrycount + 1;
    sleep(50);
  end;
end;

function SaveStringToFile(const Filename, S: string; const AllowRetry: Boolean = TRUE): boolean;
var
  sl: tstringlist;
begin
  sl:= tstringlist.create;
  try
    sl.text:= s;
    result:= SaveStringsToFile(filename, sl);
  finally
    freeandnil(sl);
  end;
end;

//------------------------------------------------------------------------------
function AppendStringsToFile(const Filename: String; Strings: tstrings ): boolean;
var
  fs: tfilestream;
begin
  result:= false;
  if fileexists(Filename) then // file must exist
  try
    fs:= tfilestream.Create(Filename, fmOpenReadWrite or fmShareDenyWrite);
    try
      fs.Seek(int64(0), soFromEnd); // go to last byte of file
      strings.SaveToStream(fs); // save strings to file
      result:= true;
    finally
      freeandnil(fs);
    end;
  except
  end;
end;

function AppendStringToFile(const Filename, S: String): boolean;
var
  sl: tstringlist;
begin
  sl:= tstringlist.create;
  try
    sl.text:= s;
    result:= appendStringstoFile(Filename, sl);
  finally
    freeandnil(sl);
  end;
end;

//------------------------------------------------------------------------------
function StrToPchar(var S: String): PAnsiChar;
begin
  S:= S + #0;
  result:= PAnsiChar(S[1]);
end;

//------------------------------------------------------------------------------
function BytesToStr(const Size: Int64): string;
const
  i64GB = 1024 * 1024 * 1024;
  i64MB = 1024 * 1024;
  i64KB = 1024;
begin
  if Size div i64GB > 0 then Result:= Format('%.2f GB', [Size / i64GB])
    else if Size div i64MB > 0 then Result:= Format('%.2f MB', [Size / i64MB])
    else if Size div i64KB > 0 then Result:= Format('%.2f KB', [Size / i64KB])
    else Result:= IntToStr(Size) + ' Byte(s)';
end;

//------------------------------------------------------------------------------
function TimeToString(T: TDateTime): string;
var
  secs: int64;
begin
  if T<>0 then
  begin
    secs:= trunc(T*24*3600);
    result:=
      inttostr(secs div 3600)+':'+              // hours
      inttostr(secs div 60 mod 60, 2)+':'+      // minutes
      inttostr(secs mod 60, 2)                  // seconds
  end else
    result:= '0:00:00';                         // nothing (just show as would be formatted)
end;

//------------------------------------------------------------------------------
function StringToTime(const S: String; var T: TDateTime): boolean;
// Descr.: Function that allows convert over 24hours to tdatetime
// unlike the function in sysutils. Returns FALSE in case conversion fails.
// also accepts units maked below second (hudrets etc.) - formatting
// should ignore separators, but requires 3 or 5 fields as follows:
// [hour]:[minutes]:[seconds].[below]
var
  tmpS: string;
  s1, s2, s3: string;
  v1, v2, v3: integer;
begin
  T:= 0;
  result:= FALSE;
  if s<>'' then
  begin
    tmpS:= S;
    if not getnexttoken(tmpS, s1) then exit; // hours
    if not getnexttoken(tmpS, s2) then exit; // minutes
    getnexttoken(tmpS, s3); // seconds (might not end to : anymore)
    trystrtoint(S1, v1); // hours
    trystrtoint(S2, v2); // minutes
    trystrtoint(S3, v3); // seconds
    if (v1>0) then T:= T + v1 * 1/24; // hours
    if (v2>0) then T:= T + v2 * 1/1440; // minutes (24*60)
    if (v3>0) then T:= T + v3 * 1/86400; // seconds (24*60*60)
    result:= TRUE;
  end;
end;

//------------------------------------------------------------------------------
function  Pos(
  const LocateThis, FromThisString: AnsiString;
  const NoCase: Boolean = FALSE;
  const StartFrom: Integer = 1): integer; // pos replacement
var
  sf: integer;
  tmpS: ansistring;
begin
  sf:= 0;

  // moved location (only applied in case defined from 2nd char)
  if (StartFrom>1) then
  begin
    if (Startfrom<=Length(FromThisString)) then
    begin
      tmpS:= copy(FromThisString, startfrom, length(fromthisstring));
      sf:= startfrom - 1; // 05062009api
    end else
      tmpS:= '';
  end else
    tmpS:= FromThisString;

  // system pos with case if needed
  if NoCase then
    result:= system.Pos(lowercase(LocateThis), lowercase(tmpS))
    else result:= system.Pos(LocateThis, tmpS); // case intensitive

  // apply start from in case something was found
  if result>0 then result:= result + sf;
end;

//------------------------------------------------------------------------------
function PosFromEnd(const LocateThis, FromThisString: AnsiString; const NoCase: boolean = FALSE): integer; // pos from end
var
  p1, p2: integer;
begin
  // locate from end..
  p2:= 0;
  repeat
    p1:= pos(locatethis, fromthisstring, nocase, p2+1);
    if (p1>0) then p2:= p1;
  until (p1<1); // not found anymore
  // apply result
  result:= p2;
end;

//------------------------------------------------------------------------------
procedure CharReplace(var InputString: AnsiString; const OldCh, NewCh: AnsiChar);
var
  p: integer;
begin
  (*
  repeat
    p:= pos(OldCh, InputString);
    if p>0 then InputString[p]:= NewCh;
  until (p<1);
  *)

  // below faster than above??
  for p:=1 to length(inputstring) do
    if (InputString[p] = OldCh) then
      InputString[p]:= NewCh;
end;

//------------------------------------------------------------------------------
function StringExists(const LookFor, FromThisString: AnsiString; const NoCase: Boolean = TRUE): boolean;
begin
  result:= (pos(lookfor, fromthisstring, nocase)>0);
end;

function StringBeginsWith(const LookFor, FromThisString: AnsiString; const NoCase: Boolean = TRUE): boolean;
begin
  result:= SameString( LookFor, copy(FromThisString, 1, length(LookFor)), NoCase );
end;

function StringEndsWith(const LookFor, FromThisString: AnsiString; const NoCase: Boolean = TRUE): boolean;
var
  l1, l2: integer;
begin
  l1:= length(LookFor);
  l2:= length(FromThisString);
  if l1>l2 then
  begin
    result:= SameString( LookFor, copy(FromThisString, (l2-l1), l1), NoCase );
  end else
    result:= FALSE;
end;

//------------------------------------------------------------------------------
function AddBackslash(const AFilename: AnsiString): AnsiString;
begin
  result:= AFilename;
  if result<>'' then
    if result[length(result)]<>'\' then
      result:= result + '\';
end;

function RemoveBackslash(const AFilename: AnsiString): AnsiString;
begin
  if (AFilename<>'') and (AFilename[length(AFilename)]='\') then
    result:= copy(AFilename, 1, length(AFilename)-1)
    else result:= AFilename;
end;

//------------------------------------------------------------------------------
procedure CharsReplace(var InputString: AnsiString; const OldChs, NewChs: AnsiString);
var
  p, i: integer;
begin
  if (length(OldChs)<>length(NewChs)) then exit;
  for p:=1 to length(inputstring) do
    for i:=1 to length(OldChs) do
      if inputstring[p] = OldChs[i] then
        inputstring[p]:= NewChs[i];
end;

//------------------------------------------------------------------------------
procedure StringReplace(var InputString: AnsiString; const OldS, NewS: AnsiString; NoCase: Boolean = TRUE);
(*
var
  p, l, osl, nsl: integer;
*)
begin
  if (olds='') (* removed 30122008 or (news='') *) or (OldS=NewS) then exit;

  if NoCase then
    InputString:= ansistring(sysUtils.StringReplace(string(inputstring), string(olds), string(news), [rfReplaceAll, rfIgnoreCase]))
    else InputString:= ansistring(sysUtils.StringReplace(string(inputstring), string(olds), string(news), [rfReplaceAll]))

  (*
  l:= length(InputString);
  osl:= length(olds);
  nsl:= length(news);
  p:= 0;
  repeat
    p:= pos(olds, InputString, noCase, p);
    if (p>0) then
    begin
      InputString:= // make new input string
        copy(InputString, 1, p-1) + // keep beginning this far
        NewS + // add new string
        copy(InputString, p+osl, l); // append with rest of input string
      p:= p + nsl;
      {$ifdef DEBUGSTRINGS}
      showmessage(InputString);
      {$endif}
    end;
  until (p<1); // redo until no olds found anymore
  *)
end;

//------------------------------------------------------------------------------
function LowerCase(s: Ansistring): Ansistring;
begin
  result:= ansistring(sysutils.LowerCase(string(s)));
  // special characters included
  CharReplace(result, 'Å', 'å');
  CharReplace(result, 'Ä', 'ä');
  CharReplace(result, 'Ö', 'ö');
  CharReplace(result, 'Ü', 'ü');
end;

//------------------------------------------------------------------------------
function UpperCase(s: Ansistring): ansistring;
begin
  result:= ansistring(sysutils.UpperCase(string(s)));
  // special characters included
  CharReplace(result, 'å', 'Å');
  CharReplace(result, 'ä', 'Ä');
  CharReplace(result, 'ö', 'Ö');
  CharReplace(result, 'ü', 'Ü');
end;

//------------------------------------------------------------------------------
(*
procedure FindTaggedText(
  Const OrigText: String;
  Const StartTag, EndTag: string;
  var Start: Integer;
  var OrigTextPart: String;
  var TaggedText: String);
var
  p1, p2: integer;
  ls, le: integer;
begin
  ls:= length(starttag);
  le:= length(endtag);
  p1:= pos(starttag, origtext, TRUE, start);      // locate start tag
  if (p1>0) then
  begin
    start:= p1+ls+1;                              // new starting point
    p2:= pos(endtag, origtext, TRUE, start);      // locate end tag
    if (p2>p1) then
    begin
      OrigTextPart:= copy(origtext, p1, p2+le-p1);   // get original text to replace
      TaggedText:= copy(OrigTextPart, ls+1, length(OrigTextPart)-ls-le);
    end;
  end else
    start:= 0;
end;
*)

function FindTagText(
  Const OrigText: AnsiString;
  StartFrom: Integer;
  var Position: integer;
  var TagContentStart: integer;
  var TagLength: integer;
  Const StartTag: AnsiString = '<';
  Const EndTag: AnsiString = '>'): Ansistring;
  // 05062009api * fixed alongst the pos bug fix (-1 offset)
  // 30122008api * added support for embedded tags
var
  p1, p2: integer; // normal tags start and end position
  p11, p22, c, cp: integer; // sub tags search
begin
  p1:= pos(starttag, origtext, false, startfrom);
  if (p1>0) then
  begin
    c:= 1;
    cp:= p1+1;
    repeat
      p11:= pos(starttag, origtext, false, cp);
      p22:= pos(endtag, origtext, false, cp);
      if (p11<1) then
      begin
        if (p22<1) then p22:= length(origtext);
        break;
      end else
      if (p22<p11) then // end tag found before start tag
      begin
        dec(c);
        cp:= p22 + 1;
      end else
      if (p11<p22) then // start tag found before end tag
      begin
        inc(c);
        cp:= p11 + 1;
      end;
    until (c<1); // or ((p11<1) and (p22<1)); // 20042009 added p11 and p22 as exit case
    p2:= p22; // actual end tag position or -1 if end was not found
  end else
    p2:= 0;
  // make result
  if (p1>0) and (p2>p1) then
  begin
    Position:= p1+1;
    TagContentStart:= p1+length(starttag);
    TagLength:= (p2-p1-length(starttag));
    result:= copy(origtext, TagContentStart, Taglength);
  end else
  begin
    Position:= 0;
    TagContentStart:= 0;
    TagLength:= 0;
    result:= '';
  end;
end;

function FindTagText(
  Const OrigText: AnsiString;
  StartFrom: integer;
  var Position: integer;
  StartTag: AnsiString = '<';
  EndTag: AnsiString = '>'): AnsiString;
var
  i1, i2: integer;
begin
  result:= FindTagText(origtext, startfrom, position, i1, i2, starttag, endtag);
end;

function FindTagText(
  Const OrigText: Ansistring;
  StartFrom: integer;
  StartTag: AnsiString = '<';
  EndTag: AnsiString = '>'): Ansistring;
var
  p: integer;
begin
  result:= FindTagText(origtext, startfrom, p, starttag, endtag);
end;

//------------------------------------------------------------------------------
function SameString(Const s1, s2: AnsiString; Const Nocase: Boolean = TRUE): boolean;
begin
  if NoCAse then
    result:= (lowercase(s1) = lowercase(s2))
    else result:= (s1 = s2);
end;

//------------------------------------------------------------------------------
function BinToString(stream: tstream): string;
// inputted stream will be stored into string
// into it's hex presentation (00A02FDE..)
var
  bytes: array[0..127] of byte; // use 128 char(s) buffer
  count: integer;
  total: int64;
  i: integer;
begin
  stream.seek(0, soFromBeginning);
  result:= '';
  total:= 0;
  repeat
    count:= stream.Read(bytes, sizeof(bytes));
    for i:=0 to count-1 do
      result:= result+inttohex(bytes[i],2);
    total:= total+count;
  until (count=0) or (total = stream.size);
end;

//------------------------------------------------------------------------------
function StringToBin(s: string; stream: tstream): boolean;
// inputted hex formatted string is converted to stream
var
  bytes: array[0..127] of byte;
  p: integer; //, spos: integer;
  byteswritten: integer;
  tempstring: string;
begin
  result:= falsE;
  stream.Size:= 0; // clear?
  stream.Position:= 0; // to the beginning anyway

  repeat
    // get string into the bytes buffer
    p:= 0;
    while (p = length(s)) or (p = 127) do
    try
      tempstring:= '$'+s[p]+s[p+1];
      bytes[p]:= strtoint(tempstring);
      p:= p + 2;
    except
      exit; // with false result
    end;
    //spos:= spos+pos;
    // write to stream
    byteswritten:= stream.Write(bytes, p);
    // error check on writing
    if (byteswritten<p) then
      exit; // with false result
  until (stream.Size = length(s));
  result:= true;
end;

//------------------------------------------------------------------------------
procedure SwapStrings(var s1, s2: string);
var
   tmp : string;
begin
   tmp := s1;
   s1 := s2;
   s2 := tmp;
end;

//------------------------------------------------------------------------------
function  IntToStr(I: int64; LeadingZeros: Integer = 0): string;
begin
  if LeadingZeros>0 then result:= Format('%.*d', [LeadingZeros, I])
    else result:= sysutils.inttostr(int64(i));
end;

//------------------------------------------------------------------------------
function StringToHtmlParams(str: Ansistring): Ansistring;
var
  i: integer;
begin
  for i := 1 to Length(str) do
  begin
    {$ifdef DELPHI2009UP}
    if (charinset( str[i], ['a'..'z', 'A'..'Z', '0', '1'..'9'] )) then
    {$else}
    if str[i] in ['a'..'z', 'A'..'Z', '0', '1'..'9'] then
    {$endif}
      Result := Result + Str[i]
    else if Str[i] = ' ' then
      Result := Result + '+'
    else
      Result := Result + '%' + ansistring(IntToHex(Byte(Str[i]), 2));
  end;
end;

//------------------------------------------------------------------------------
function HtmlParamsToString( s: Ansistring ): AnsiString;
var
  hexstr: ansistring;
  orig: ansistring;
  i: integer;
  p: integer;
begin
  CharReplace(s, '+', ' ');

  repeat
    p:= pos('%', s);
    if p>0 then
    begin
      hexstr:= copy( s, p, 3 );
      orig:= hexstr;
      hexstr[1]:= '$';
      try
        i:= strtoint( string(hexstr) );
        stringreplace( s, orig, ansichar(i));
      except
        p:= -1;
      end;
    end;
  until (p<1);

  result:= s;
end;

//------------------------------------------------------------------------------
function URLEncode(Value: Ansistring): Ansistring; // same as StringToHtmlParams
begin
  result:= stringtohtmlparams(value);
end;

//------------------------------------------------------------------------------
function URLDecode(Value: Ansistring): Ansistring; // same as HtmlParamsToString
begin
  result:= htmlparamstostring(value);
end;

//------------------------------------------------------------------------------
function RFC1123ToDateTime(Date: string): TDateTime;
var
  day, month, year: Integer;
  strMonth: string;
  Hour, Minute, Second: Integer;
begin
  if date<>'' then
  try
    day      := StrToInt(Copy(Date, 6, 2));
    strMonth := Copy(Date, 9, 3);
    if strMonth = 'Feb' then month := 2
    else if strMonth = 'Mar' then month := 3
    else if strMonth = 'Apr' then month := 4
    else if strMonth = 'May' then month := 5
    else if strMonth = 'Jun' then month := 6
    else if strMonth = 'Jul' then month := 7
    else if strMonth = 'Aug' then month := 8
    else if strMonth = 'Sep' then month := 9
    else if strMonth = 'Oct' then month := 10
    else if strMonth = 'Nov' then month := 11
    else if strMonth = 'Dec' then month := 12
    else month := 1;
    year   := StrToInt(Copy(Date, 13, 4));
    hour   := StrToInt(Copy(Date, 18, 2));
    minute := StrToInt(Copy(Date, 21, 2));
    second := StrToInt(Copy(Date, 24, 2));
    Result := EncodeTime(hour, minute, second, 0);
    Result := Result + EncodeDate(year, month, day);
  except
    Result := 0;
  end else
    result:= 0;
end;

//------------------------------------------------------------------------------
function DateTimeToRFC1123(aDate: TDateTime): string;
const
  StrWeekDay: string = 'MonTueWedThuFriSatSun';
  StrMonth: string = 'JanFebMarAprMayJunJulAugSepOctNovDec';
var
  Year, Month, Day: Word;
  Hour, Min, Sec, MSec: Word;
  DayOfWeek: Word;
begin
  DecodeDate(aDate, Year, Month, Day);
  DecodeTime(aDate, Hour, Min, Sec, MSec);
  DayOfWeek := ((Trunc(aDate) - 2) mod 7);
  Result    := Copy(StrWeekDay, 1 + DayOfWeek * 3, 3) + ', ' +
    Format('%2.2d %s %4.4d %2.2d:%2.2d:%2.2d',
    [Day, Copy(StrMonth, 1 + 3 * (Month - 1), 3),
    Year, Hour, Min, Sec]);
end;

//------------------------------------------------------------------------------
function BitmapToRTF(pict: TBitmap): string;
var
  bi, bb, rtf: ansistring;
  bis, bbs: Cardinal;
  //achar: ShortString;
  achar: ansistring;
  hexpict: ansistring;
  I: Integer;
begin
  GetDIBSizes(pict.Handle, bis, bbs);
  SetLength(bi, bis);
  SetLength(bb, bbs);
  GetDIB(pict.Handle, pict.Palette, PAnsiChar(bi)^, PAnsiChar(bb)^);
  rtf := '{\rtf1 {\pict\dibitmap0 ';
  SetLength(hexpict, (Length(bb) + Length(bi)) * 2);
  I := 2;
  for bis := 1 to Length(bi) do
  begin
    achar := ansistring(IntToHex(Integer(bi[bis]), 2));
    hexpict[I - 1] := achar[1];
    hexpict[I] := achar[2];
    Inc(I, 2);
  end;
  for bbs := 1 to Length(bb) do
  begin
    achar := ansistring(IntToHex(Integer(bb[bbs]), 2));
    hexpict[I - 1] := achar[1];
    hexpict[I] := achar[2];
    Inc(I, 2);
  end;
  rtf := rtf + hexpict + ansistring(' }}');
  Result := string(rtf);
end;

//------------------------------------------------------------------------------
function Encode64(S: Ansistring): Ansistring;
var
  i: Integer;
  a: Integer;
  x: Integer;
  b: Integer;
begin
  Result := '';
  a := 0;
  b := 0;
  for i := 1 to Length(s) do
  begin
    x := Ord(s[i]);
    b := b * 256 + x;
    a := a + 8;
    while a >= 6 do
    begin
      a := a - 6;
      x := b div (1 shl a);
      b := b mod (1 shl a);
      Result := Result + Codes64[x + 1];
    end;
  end;
  if a > 0 then
  begin
    x := b shl (6 - a);
    Result := Result + Codes64[x + 1];
  end;
end;

//------------------------------------------------------------------------------
function Decode64(S: Ansistring): Ansistring;
var
  i: Integer;
  a: Integer;
  x: Integer;
  b: Integer;
begin
  Result := '';
  a := 0;
  b := 0;
  for i := 1 to Length(s) do
  begin
    x := Pos(s[i], codes64) - 1;
    if x >= 0 then
    begin
      b := b * 64 + x;
      a := a + 6;
      if a >= 8 then
      begin
        a := a - 8;
        x := b shr a;
        b := b mod (1 shl a);
        x := x mod 256;
        Result := Result + ansichar(x);
      end;
    end
    else
      Exit;
  end;
end;

//------------------------------------------------------------------------------
Function Match(Const N1, N2: Ansistring; Const NoCase: Boolean = TRUE): Boolean;
Var
  P1, P2: Byte;
  M: Boolean;
  tmpN1, tmpN2: AnsiString;
Begin
  if NoCase then
  begin
    tmpN1:= uppercase(N1);
    tmpN2:= uppercase(N2);
  end else
  begin
    tmpN1:= N1;
    tmpN2:= N2;
  end;

  P1:= 1;
  P2:= 1;
  M:= TRUE;

  If (Length(tmpN1) = 0) And (Length(tmpN2) = 0) Then M:= True
    Else If Length(tmpN1) = 0 Then If tmpN2[1] = '*' Then M:= True
        Else M:= False
          Else If Length(tmpN2) = 0 Then
            If tmpN1[1] = '*' Then M:= True
              Else M:= False;

  While (M = True) And (P1 <= Length(tmpN1)) And (P2 <= Length(tmpN2)) Do
    If (tmpN1[P1] = '?') Or (tmpN2[P2] = '?') Then
    Begin
      Inc(P1);
      Inc(P2);
    End Else
      If tmpN1[P1] = '*' Then
      Begin
        Inc(P1);
        If P1 <= Length(tmpN1) Then
        Begin
          While (P2 <= Length(tmpN2)) And Not Match(Copy(tmpN1,P1,Length(tmpN1)-P1+1),Copy(tmpN2,P2,Length(tmpN2)-P2+1)) Do Inc(P2);
          If P2 > Length(tmpN2) Then M:= False
            Else Begin
              P1 := Succ(Length(tmpN1));
              P2 := Succ(Length(tmpN2));
            End;
          End Else
            P2 := Succ(Length(tmpN2));
      End Else
        If tmpN2[P2] = '*' Then
        Begin
          Inc(P2);
          If P2 <= Length(tmpN2) Then
          Begin
            While (P1 <= Length(tmpN1)) And Not Match(Copy(tmpN1,P1,Length(tmpN1)-P1+1),Copy(tmpN2,P2,Length(tmpN2)-P2+1)) Do Inc(P1);
            If P1 > Length(tmpN1) Then M:= False
            Else
            Begin
              P1 := Succ(Length(tmpN1));
              P2 := Succ(Length(tmpN2));
            End;
          End Else
            P1 := Succ(Length(tmpN1));
        End Else
          If tmpN1[P1] = tmpN2[P2] Then
          Begin
            Inc(P1);
            Inc(P2);
          End Else
            M:= False;
        If P1 > Length(tmpN1) Then
        Begin
          While (P2 <= Length(tmpN2)) And (tmpN2[P2] = '*') Do Inc(P2);
          If P2 <= Length(tmpN2) Then M:= FALSE;
        End;
        If P2 > Length(tmpN2) Then
        Begin
          While (P1 <= Length(tmpN1)) And (tmpN1[P1] = '*') Do Inc(P1);
          If P1 <= Length(tmpN1) Then M:= FALSE;
        End;

  Match := M;
End;

//------------------------------------------------------------------------------
function ReplaceFirst(const SourceStr, FindStr, ReplaceStr: Ansistring): AnsiString;
var
  P: Integer;
begin
  Result := SourceStr;
  P := Pos(FindStr, SourceStr, false, 1);
  if P <> 0 then
    Result := Copy(SourceStr, 1, P - 1) + ReplaceStr + Copy(SourceStr, P + Length(FindStr), Length(SourceStr));
end;

//------------------------------------------------------------------------------
(*
removed 03092008 because this method will not work
in cases from text "Edit3" "di" is replaced with "editable" for example
-> will cause infinite loop and application to stuck

function ReplaceAll(text, origstr, newstr: string): string;
var
  nPos, nLenLookFor: integer;
begin
  result:= text;
  nPos:= Pos(origstr, result);
  nLenLookFor:= Length(origstr);
  while(nPos > 0)do
  begin
    Delete(result, nPos, nLenLookFor);
    Insert(newstr, result, nPos);
    nPos:= Pos(origstr, result);
  end;
end;
*)

//------------------------------------------------------------------------------
function CompareStringsInPercent(Const Str1, Str2: Ansistring): Byte;
type
  TLink = array[0..1] of Byte;
var
  tmpPattern: TLink;
  PatternA, PatternB: array of TLink;
  IndexA, IndexB, LengthStr: Integer;
  function Max(Const value1, value2: integer): integer;
  begin
    if (value1>value2) then result:= value1 else result:= value2;
  end;
  function Min(Const value1, value2: integer): integer;
  begin
    if (value1<value2) then result:= value1 else result:= value2;
  end;
begin
  Result := 100;
  // Building pattern tables
  LengthStr := Max(Length(Str1), Length(Str2));
  for IndexA := 1 to LengthStr do
  begin
    if Length(Str1) >= IndexA then
    begin
      SetLength(PatternA, (Length(PatternA) + 1));
      PatternA[Length(PatternA) - 1][0] := Byte(Str1[IndexA]);
      PatternA[Length(PatternA) - 1][1] := IndexA;
    end;
    if Length(Str2) >= IndexA then
    begin
      SetLength(PatternB, (Length(PatternB) + 1));
      PatternB[Length(PatternB) - 1][0] := Byte(Str2[IndexA]);
      PatternB[Length(PatternB) - 1][1] := IndexA;
    end;
  end;
  // Quick Sort of pattern tables
  IndexA := 0;
  IndexB := 0;
  while ((IndexA < (Length(PatternA) - 1)) and (IndexB < (Length(PatternB) - 1))) do
  begin
    if Length(PatternA) > IndexA then
    begin
      if PatternA[IndexA][0] < PatternA[IndexA + 1][0] then
      begin
        tmpPattern[0]           := PatternA[IndexA][0];
        tmpPattern[1]           := PatternA[IndexA][1];
        PatternA[IndexA][0]     := PatternA[IndexA + 1][0];
        PatternA[IndexA][1]     := PatternA[IndexA + 1][1];
        PatternA[IndexA + 1][0] := tmpPattern[0];
        PatternA[IndexA + 1][1] := tmpPattern[1];
        if IndexA > 0 then Dec(IndexA);
      end
      else 
        Inc(IndexA);
    end;
    if Length(PatternB) > IndexB then 
    begin
      if PatternB[IndexB][0] < PatternB[IndexB + 1][0] then 
      begin
        tmpPattern[0]           := PatternB[IndexB][0];
        tmpPattern[1]           := PatternB[IndexB][1];
        PatternB[IndexB][0]     := PatternB[IndexB + 1][0];
        PatternB[IndexB][1]     := PatternB[IndexB + 1][1];
        PatternB[IndexB + 1][0] := tmpPattern[0];
        PatternB[IndexB + 1][1] := tmpPattern[1];
        if IndexB > 0 then Dec(IndexB);
      end
      else 
        Inc(IndexB);
    end;
  end;
  // Calculating simularity percentage
  LengthStr := Min(Length(PatternA), Length(PatternB));
  for IndexA := 0 to (LengthStr - 1) do 
  begin
    if PatternA[IndexA][0] = PatternB[IndexA][0] then 
    begin
      if Max(PatternA[IndexA][1], PatternB[IndexA][1]) - Min(PatternA[IndexA][1],
        PatternB[IndexA][1]) > 0 then Dec(Result,
        ((100 div LengthStr) div (Max(PatternA[IndexA][1], PatternB[IndexA][1]) -
          Min(PatternA[IndexA][1], PatternB[IndexA][1]))))
      else if Result < 100 then Inc(Result);
    end
    else 
      Dec(Result, (100 div LengthStr))
  end;
  SetLength(PatternA, 0);
  SetLength(PatternB, 0);
end;

{*******************************************************
 * Standard Encryption algorithm - Copied from Borland *
 *******************************************************}

function Encrypt(const InString: Ansistring; StartKey, MultKey, AddKey: Integer): Ansistring;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(InString) do
  begin
    Result := Result + AnsiChar(Byte(InString[I]) xor (StartKey shr 8));
    StartKey := (Byte(Result[I]) + StartKey) * MultKey + AddKey;
  end;
end;
{*******************************************************
 * Standard Decryption algorithm - Copied from Borland *
 *******************************************************}

function Decrypt(const InString: Ansistring; StartKey, MultKey, AddKey: Integer): Ansistring;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(InString) do
  begin
    Result := Result + AnsiChar(Byte(InString[I]) xor (StartKey shr 8));
    StartKey := (Byte(InString[I]) + StartKey) * MultKey + AddKey;
  end;
end;

//------------------------------------------------------------------------------
function hash(Const text: Ansistring): Integer;
var
  i: integer;
begin
  if text<>'' then
  begin
    result:= ord(text[1]);
    for i:=2 to length(text) do
      result:= result*ord(text[i]) xor result;
  end else
    result:= 0;
end;

end.
