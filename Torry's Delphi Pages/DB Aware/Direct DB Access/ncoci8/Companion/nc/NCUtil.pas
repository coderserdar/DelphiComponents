{***********************************************}
{File:      NCUtil.PAS                          }
{Revision:  3.06 / 10.06.99                     }
{Comment:   Generic utilities                   }
{Copyright: (c) 1997-2000, Dmitry Arefiev       }
{Author:    Dmitry Arefiev, dmitrya@inthink.com }
{***********************************************}
{$I NCOciDef.inc}

unit NCUtil;

interface

uses
    SysUtils, Classes, Graphics, TypInfo
{$IFDEF OCI_D6}
    , Variants
{$ENDIF}
    ;

type
    PInteger = ^Integer;
    PSmallInt = ^SmallInt;
    PDouble = ^Double;
    PExtended = ^Extended;
    PBoolean = ^Boolean;
    PDateTime = ^TDateTime;
    TChars = set of char;

Const
    SpaceChars: TChars = [#32, #13, #10, #9, #0];

var
    OneCharMask: Char;
    ManyCharsMask: Char;

{Errors generation}
procedure GenError(const err: String);
procedure GenErrorFmt(const err: String; const args: array of const);
procedure GenErrorRes(resID: Word);
procedure GenErrorResFmt(resID: Word; const args: array of const);
{Min/Max for Integer and Double}
function iMax(i1, i2: Integer): Integer;
function iMin(i1, i2: Integer): Integer;
function iRange(i, i1, i2: Integer): Integer;
function dMax(d1, d2: Double): Double;
function dMin(d1, d2: Double): Double;
function dRange(d, d1, d2: Double): Double;

{Append spaces to cString up to length nLen}
function PadR(const cString: string ; nLen: integer): string;
{Add spaces to begin of cString up to length nLen}
function PadL(const cString: string ; nLen: integer): string;
{Add spaces to begin and end of cString up to length nLen}
function PadC(const cStrPar: string ; nLen: integer): string;
{Returns string, which consist from nLen times of S}
function Replicate(const S: String; nLen: integer): string;
{Returns string, which consist from nLen spaces}
function Space(nLen: integer): string;
{Detects is string empty or not}
function Empty(const cString: string): boolean;
{Like a Pos, but start search from fromI position in S}
function iPos(const Sub: string; const S: string; fromI: Integer): Integer;
{Like Pos, but searches from right}
function RPos(const Sub: string; const S: string): Integer;
{Like RPos, but searches from fromI position in S}
function iRPos(const Sub: string; const S: string; fromI: Integer): Integer;
{Returns left nLen characters from cString}
function Left(const cString: string; nLen: integer): string;
{Like a Copy}
function Substr(const cString: string; nFrom: integer; nChars: integer): string;
{Returns right nLen characters from cString}
function Right(const cString: string; nLen: integer): string;
{From position nFrom deletes nLen characters and inserts cS}
function Stuff(const cString: string; nFrom, nLen: Integer; cS: String): String;
{Array of strings --> str0 #13#10 str1 ...}
function Strs2Text(AStrs: array of String): String;
{Array of strings --> <APrefix> str0 <ADelim> str1 <ADelim> .... strN <ASuffix>}
function Strs2Str(AStrs: array of String; ASkipEmpty: Boolean; const APrefix, ADelim, ASuffix: String): String;
{Convert cString to upper case}
function Upper(const cString: string ): string;
{Convert cString to lower case}
function Lower(const cString: string ): string;
{Replace all cOld with cNew}
{StrTran('12334', '23', '2') -> '1234'}
function StrTran(const cString, cOld, cNew: string): string;
{Removes repetition of AChars from cString}
function StrSqueeze(const cString: string; AChars: TChars; All: Boolean): string;
{Returns substrings from cString, starting from iFrom and up to first
character from AChars or up to end of cString}
function StrToken(const cString: string; AChars: TChars; var iFrom: Integer): string;
function NotChars(AChars: TChars): TChars;

{Compares string AStr with template AMask}
function StrLike(const AStr, AMask: String; noCase: Boolean): Boolean;
{C/C++ SScanf}
function ScanStr(buff, format: PChar; arglist: array of Pointer; pChCount: PInteger): Integer;
function Scan(const buff, format: String; arglist: array of Pointer): Integer;
function iScan(const buff, format: String; arglist: array of Pointer; var fromI: Integer): Integer;

{$IFDEF NCHOME}
    {Breaking long string into parts}
    function Perenos(const cString: string; Num: Integer): TStrings;
    function PerenosGraph(const cString: string; AWidths: array of Integer;
        ACanvas: TCanvas; AFont: TFont): TStrings;
{$ELSE}
    function StrCharBreak(const cString: string; Num: Integer): TStrings;
    function StrGraphBreak(const cString: string; AWidths: array of Integer;
        ACanvas: TCanvas; AFont: TFont): TStrings;
{$ENDIF}

{Copy properties from Src object into Dest object, using tkFlt filter}
procedure CloneProperties(src, dest: TObject; tkFlt: TTypeKinds; skipDef: Boolean);
{Creates object of the same class, as Src object}
function DuplicateComponent(src: TComponent; AOwner: TComponent): TComponent;
{Get/Set value of published property}
procedure AssignObjProp(obj: TObject; const propName: String; propVal: TPersistent);
procedure AssignOrdProp(obj: TObject; const propName: String; propVal: LongInt);
procedure AssignStrProp(obj: TObject; const propName: String; const propVal: String);
function ReadObjProp(obj: TObject; const propName: String): TPersistent;
function ReadOrdProp(obj: TObject; const propName: String): LongInt;
function ReadStrProp(obj: TObject; const propName: String): String;

{Variant --> String }
function Var2Text(const V: Variant): String;
{! empty && ! null}
function VarIsDefined(const V: Variant): Boolean;
function VarIsAllDefined(const V: Variant): Boolean;
{ ? V1 == V2 }
function VarEQ(const V1, V2: Variant): Boolean;
{Smart comparision of Val1 and Val2. For strings with/without Case and full/portional }
function VarCmp(const Val1, Val2: Variant; APartialCmp, ANoCase: Boolean): Boolean;
{Variant manipulation}
function VarFormat(AFormat: String; const AValue: Variant; AErrNote: String): String;
function VarExtHighBound(const AValue: Variant): Integer;
function VarExtType(const AValue: Variant; i: Integer): Integer;
function VarExtValue(const AValue: Variant; i: Integer; const APadValue: Variant): Variant;
function VarArraySlice(const AValue: Variant; fromI, Count: Integer): Variant;
function VarNullOf(const AValue: Variant): Variant;
function NVL(const AValue, AResult: Variant): Variant;

{ ------------------------------------------------------------------------------ }
{ ------------------------------------------------------------------------------ }

implementation

Uses NCStrs;

{ ------------------------------------------------------------------------------ }
{ ------------------------------------------------------------------------------ }

procedure GenError(const err: String);
begin
    raise Exception.Create(err);
end;

procedure GenErrorFmt(const err: String; const args: array of const);
begin
    raise Exception.CreateFmt(err, args);
end;

procedure GenErrorRes(resID: Word);
begin
    raise Exception.CreateRes(resID);
end;

procedure GenErrorResFmt(resID: Word; const args: array of const);
begin
    raise Exception.CreateResFmt(resID, args);
end;

{ ------------------------------------------------------------------------------ }
{ ------------------------------------------------------------------------------ }

function iMax(i1, i2: Integer): Integer;
begin
    if i1 > i2 then
        Result := i1
    else
        Result := i2;
end;

function iMin(i1, i2: Integer): Integer;
begin
    if i1 > i2 then
        Result := i2
    else
        Result := i1;
end;

function iRange(i, i1, i2: Integer): Integer;
begin
    Result := i;
    if i < i1 then
        Result := i1
    else if i > i2 then
        Result := i2;
end;

function dMax(d1, d2: Double): Double;
begin
    if d1 > d2 then
        Result := d1
    else
        Result := d2;
end;

function dMin(d1, d2: Double): Double;
begin
    if d1 > d2 then
        Result := d2
    else
        Result := d1;
end;

function dRange(d, d1, d2: Double): Double;
begin
    Result := d;
    if d < d1 then
        Result := d1
    else if d > d2 then
        Result := d2;
end;

{ ------------------------------------------------------------------------------ }
{ ------------------------------------------------------------------------------ }
{ Pad }
function PadR(const cString: string; nLen: integer): string;
begin
    if (Length(cString) <> nLen) then
        if Length(cString) > nLen then
            Result := Copy(cString,1,nLen)
        else
            Result := cString + Space(nLen-Length(cString))
    else
        Result := cString;
end;

function PadL(const cString: string; nLen: integer): string;
begin
    if (Length(cString) <> nLen) then
        if Length(cString) > nLen then
            Result := Copy(cString,1,nLen)
        else
            Result := Space(nLen-Length(cString)) + cString
    else
        Result := cString;
end;

function PadC(const cStrPar: string; nLen: integer): string;
var
    nSpace : Longint;
    cString: String;
begin
    cString := Trim(cStrPar);
    if (Length(cString) <> nLen) then
        if Length(cString) > nLen then
            Result := Copy(cString,1,nLen)
        else begin
            nSpace := (nLen-Length(cString)) div 2;
            Result := Space(nSpace) + cString + Space(nLen-Length(cString)-nSpace);
        end
    else
        Result := cString;
end;

{ ------------------------------------------------------------------------------ }
{ ------------------------------------------------------------------------------ }
{ String manipulating }

function Left(const cString: string; nLen: integer): string;
begin
    if nLen < Length(cString) then
        Result := Copy(cString, 1, nLen)
    else
        Result := cString;
end;

function SubStr(const cString: string; nFrom: integer; nChars: integer): string;
begin
    Result := Copy(cString, nFrom, nChars);
end;

function Right(const cString: string; nLen: integer): string;
begin
    Result := Copy(cString,Length(cString)-nLen+1,nLen);
end;

function Stuff(const cString: string; nFrom, nLen: Integer; cS: String): String;
begin
    Result := cString;
    if nLen <> 0 then
        Delete(Result, nFrom, nLen);
    if cS <> '' then
        Insert(cS, Result, nFrom);
end;

function Space(nLen: integer): string;
begin
    Result := Replicate(' ', nLen);
end;

function Replicate(const S: String; nLen: integer): string;
var
    L, i: Integer;
begin
    if nLen <= 0 then
        Result := ''
    else begin
        L := Length(S);
        SetLength(Result, L * nLen);
        for i := 1 to nLen do
            Move(S[1], Result[1 + L * (i - 1)], L)
    end;
end;

function Empty(const cString: string): boolean;
begin
    Result := (cString = '') or (StrSqueeze(cString, SpaceChars, True) = '');
end;

function iPos(const Sub: string; const S: string; fromI: Integer): Integer;
var
    P: PChar;
begin
    if fromI < 1 then
        fromI := 1;
    if fromI > Length(S) then
        Result := 0
    else begin
        P := StrPos(PChar(S) + (fromI - 1), PChar(Sub));
        if P = nil then
            Result := 0
        else
            Result := P - PChar(S) + 1;
    end;
end;

function RPos(const Sub: string; const S: string): Integer;
begin
    Result := iRPos(Sub, S, Length(S));
end;

function iRPos(const Sub: string; const S: string; fromI: Integer): Integer;
var
    p1, p2, pBSub, pESub, pBS, pES: PChar;
begin
    Result := 0;
    if fromI > Length(S) then
        fromI := Length(S);
    if (fromI < 1) or (Length(Sub) > fromI) or (Sub = '') then
        Exit;
    pBSub := PChar(Sub);
    pESub := @Sub[Length(Sub)];
    pBS := PChar(S) + Length(Sub) - 1;
    pES := @S[fromI];
    while pES >= pBS do begin
        if pES^ = pESub^ then begin
            p1 := pES;
            p2 := pESub;
            while (p2 >= pBSub) and (p1^ = p2^) do begin
                Dec(p1);
                Dec(p2);
            end;
            if p2 < pBSub then begin
                Result := p1 - PChar(S) + 2;
                Exit;
            end;
        end;
        Dec(pES);
    end;
end;

function Strs2Text(AStrs: array of String): String;
begin
    Result := Strs2Str(AStrs, False, '', #13#10, '');
end;

function Strs2Str(AStrs: array of String; ASkipEmpty: Boolean; const APrefix, ADelim, ASuffix: String): String;
var
    i, l, ld: Integer;
    pResult, pDelim: PChar;
begin
    ld := Length(ADelim);
    l := -ld;
    for i := Low(AStrs) to High(AStrs) do
        if AStrs[i] <> '' then
            Inc(l, Length(AStrs[i]) + ld);
    if l <= 0 then
        Exit;
    pDelim := PChar(ADelim);
    l := l + Length(APrefix) + Length(ASuffix) + 1;
    SetLength(Result, l);
    pResult := PChar(Result);
    if APrefix <> '' then
        pResult := StrECopy(pResult, PChar(APrefix));
    for i := Low(AStrs) to High(AStrs) do
        if not ASkipEmpty or (AStrs[i] <> '') then begin
            pResult := StrECopy(pResult, PChar(AStrs[i]));
            if i <> High(AStrs) then
                pResult := StrECopy(pResult, pDelim);
        end;
    if ASuffix <> '' then
        StrECopy(pResult, PChar(ASuffix));
    SetLength(Result, l - 1);
end;

function StrTran(const cString, cOld, cNew: string): string;
var
    i: Integer;
begin
    Result := cString;
    i := Pos(cOld, Result);
    while i <> 0 do begin
        Result := Stuff(Result, i, Length(cOld), cNew);
        i := iPos(cOld, Result, i);
    end;
end;

function StrSqueeze(const cString: string; AChars: TChars; All: Boolean): string;
var
    prevCh: Char;
    pRes, pS, pResFrom: PChar;
begin
    SetLength(Result, Length(cString));
    pRes := PChar(Result);
    pResFrom := pRes;
    pS := PChar(cString);
    prevCh := #0;
    while pS^ <> #0 do begin
        if not ((pS^ in AChars) and (All or (prevCh = pS^))) then begin
            pRes^ := pS^;
            Inc(pRes);
        end;
        prevCh := pS^;
        Inc(pS);
    end;
    SetLength(Result, pRes - pResFrom);
end;

function StrToken(const cString: string; AChars: TChars; var iFrom: Integer): string;
var
    pS, pFrom: PChar;
begin
    if iFrom <= Length(cString) then begin
        pS := PChar(cString) + iFrom - 1;
        pFrom := pS;
        while (pS^ <> #0) and not (pS^ in AChars) do
            Inc(pS);
        Inc(iFrom, pS - pFrom + 1);
        SetString(Result, pFrom, pS - pFrom);
    end
    else
        Result := '';
end;

function NotChars(AChars: TChars): TChars;
var
    i: Char;
begin
    Result := [];
    for i := #0 to #255 do
        if not (i in AChars) then
            Include(Result, i);
end;

function Upper(const cString: string ): string;
begin
    Result := AnsiUpperCase(cString);
end;

function Lower(const cString: string ): string;
begin
    Result := AnsiLowerCase(cString);
end;

{ ----------------------------------------------------------------------------- }
{ ----------------------------------------------------------------------------- }

function StrLike(const AStr, AMask: String; noCase: Boolean): Boolean;

    function InternalScan(pStr, pMask: PChar): Boolean;
    begin
        Result := True;
        while ((pStr^ = pMask^) and (pMask^ <> ManyCharsMask) or (pMask^ = OneCharMask)) and
              (pStr^ <> #0) and (pMask^ <> #0) do begin
            Inc(pMask);
            Inc(pStr);
        end;
        if pMask^ = ManyCharsMask then begin
            while (pMask^ = ManyCharsMask) or (pMask^ = OneCharMask) do begin
                if pMask^ = OneCharMask then
                    if pStr^ = #0 then begin
                        Result := False;
                        Exit;
                    end
                    else
                        Inc(pStr);
                Inc(pMask);
            end;
            if pMask^ <> #0 then begin
                while (pStr^ <> #0) and ((pMask^ <> pStr^) or not InternalScan(pStr, pMask)) do
                    Inc(pStr);
                Result := pStr^ <> #0;
            end;
        end
        else
            Result := (pMask^ = #0) and (pStr^ = #0);
    end;

begin
    if AMask = ManyCharsMask then
        Result := True
    else if (AMask = '') or (AStr = '') then
        Result := False
    else begin
        if noCase then
            Result := InternalScan(PChar(AnsiUpperCase(AStr)), PChar(AnsiUpperCase(AMask)))
        else
            Result := InternalScan(PChar(AStr), PChar(AMask));
    end;
end;

{ ------------------------------------------------------------------------------ }
{ ------------------------------------------------------------------------------ }
{C/C++ SScanf}
{Return:
    > 0 - count of assigned fields
      0 - nothing assigned
     -1 - nothing to scan ((format = nil) or (buff = nil))
     -2 - field quantity > High(arglist) + 1
}

function iScan(const buff, format: String; arglist: array of Pointer; var fromI: Integer): Integer;
var
    n: Integer;
begin
    if fromI > Length(buff) then
        Result := -1
    else begin
        Result := ScanStr(PChar(buff) + fromI - 1, PChar(format), arglist, @n);
        fromI := fromI + n;
    end;
end;

function Scan(const buff, format: String; arglist: array of Pointer): Integer;
begin
    Result := ScanStr(PChar(buff), PChar(format), arglist, nil);
end;

{$WARNINGS OFF}
function ScanStr(buff, format: PChar; arglist: array of Pointer; pChCount: PInteger): Integer;
label
    error_return;

const
    LEFT_BRACKET = Chr(Ord('[') or (Ord('a') - Ord('A')));
    ASCII = 32;           { # of bytes needed to hold 256 bits }
    SCAN_SHORT = 0;       { also for FLOAT }
    SCAN_LONG = 1;        { also for DOUBLE }
    SCAN_L_DOUBLE = 2;    { only for LONG DOUBLE }
    sbrackset = ' '#9'-'#13']'; { use range-style list }
    cbrackset = ']';
    rej2int: array[Boolean] of Byte = (0, 255);
    EOF = #255;
    CVTBUFSIZE = 50;

var
    tmpBuff: array[0..2047] of Char;           { buffer for Pascal string scanning }
    table: array[0..ASCII-1] of Byte;          { which chars allowed for %[], %s   }
    floatstring: array[0..CVTBUFSIZE] of Char; { ASCII buffer for floats           }

    number: Word;                              { temp hold-value                   }
    ptr: Pointer;                              { points to user data receptacle    }
    start: Pointer;                            { indicate non-empty string         }

    scanptr: PChar;                            { for building "table" data         }
    ch: Char;

    charcount: Integer;                      { total number of chars read        }
    comchr: Char;                            { holds designator type             }
    count: Integer;                          { return value.  # of assignments   }

    started: Boolean;                        { indicate good number              }
    width: Integer;                          { width of field                    }
    widthset: Boolean;                       { user has specified width          }

    done_flag: Boolean;                     { general purpose loop monitor      }
    longone: Integer;                       { 0 = SHORT, 1 = LONG, 2 = L_DOUBLE }
    reject: Boolean;                        { %[^ABC] instead of %[ABC]         }
    negative: Boolean;                      { flag for '-' detected             }
    suppress: Boolean;                      { don't assign anything             }
    match: Integer;                         { flag: !0 if any fields matched    }
    iArgList: Integer;
    iArgListSave: Integer;                  { save arglist value                }

    rngch: Char;                            { used while scanning range         }
    last: Char;                             { also for %[a-z]                   }
    prevchar: Char;                         { for %[a-z]                        }

    i: Integer;
    E: Extended;

    function GETCH: Char;
    begin
        Inc(charcount);
        Result := buff^;
        if buff^ = #0 then
            Result := EOF
        else
            Inc(buff);
    end;

    procedure UN_GETCH(c: Char);
    begin
        Dec(charcount);
        if (EOF <> c) then begin
            Dec(buff);
            buff^ := c;
        end;
    end;

    function SKIPWS: Char;
    var
        ch: Char;
    begin
        repeat
            ch := GETCH;
        until not (ch in SpaceChars);
        Result := ch;
    end;

begin
    if (format = nil) or (buff = nil) then begin
        Result := -1;
        Exit;
    end;

    {
    count = # fields assigned
    charcount = # chars read
    match = flag indicating if any fields were matched

    [Note that we need both count and match.  For example, a field
    may match a format but have assignments suppressed.  In this case,
    match will get set, but 'count' will still equal 0.  We need to
    distinguish 'match vs no-match' when terminating due to EOF.]
    }

    count := 0;
    charcount := 0;
    match := 0;
    iArgList := 0;

    while format^ <> #0 do begin

        if format^ in SpaceChars then begin

            UN_GETCH(SKIPWS()); { put first non-space char back }

            repeat
                Inc(format);
            until not (format^ in SpaceChars);

        end;

        if '%' = format^ then begin

            number := 0;
            prevchar := #0;
            width := 0;
            widthset := False;
            started := False;
            done_flag := False;
            suppress := False;
            negative := False;
            reject := False;

            longone := 1;

            while not done_flag do begin

                Inc(format);
                comchr := format^;
                if comchr in ['0'..'9'] then begin
                    widthset := True;
                    width := width * 10 + (Ord(comchr) - Ord('0'));
                end
                else
                    case comchr of
                    'h':{ set longone to 0 }
                        Dec(longone);

                    'L':{  ++longone;  }
                        Inc(longone);

                    'l':
                        begin
                            Inc(longone);
                            suppress := True;
                        end;

                    '*':
                        suppress := True;

                    else
                        done_flag := True;
                    end;
            end;

            if not suppress then begin
                if iArgList = High(arglist) + 1 then begin
                    Result := -2;
                    Exit;
                end;
                iArgListSave := iArgList;
                ptr := arglist[iArgList];
                Inc(iArgList);
            end;

            done_flag := False;

            { switch to lowercase to allow %E,%G, and to
               keep the switch table small }

            comchr := Char(Ord(format^) or (Ord('a') - Ord('A')));

            if 'n' <> comchr then
                if ('c' <> comchr) and (LEFT_BRACKET <> comchr) then
                    ch := SKIPWS()
                else
                    ch := GETCH();


            if not widthset or (width <> 0) then begin

                case comchr of

                'c',
            {   'C',  }
                's',
            {   'S',  }
                LEFT_BRACKET :   { scanset }
                    begin
                        if comchr = 'c' then begin
                            if not widthset then begin
                                widthset := True;
                                Inc(width);
                            end;
                            scanptr := PChar(cbrackset);
                            reject := True;
                        end
                        else if comchr = 's' then begin
                            scanptr := PChar(sbrackset);
                            reject := True;
                        end
                        else if comchr = LEFT_BRACKET then begin
                            Inc(format);
                            scanptr := format;

                            if '^' = scanptr^ then begin
                                Inc(scanptr);
                                reject := True;
                            end;
                        end;

                        FillChar(table, ASCII, #0);

                        if LEFT_BRACKET = comchr then
                            if ']' = scanptr^ then begin
                                prevchar := ']';
                                Inc(scanptr);

                                table[Ord(']') shr 3] := 1 shl (Ord(']') and 7);

                            end;

                        while (scanptr^ <> #0) and (scanptr^ <> ']') do begin

                            rngch := scanptr^;
                            Inc(scanptr);

                            if ('-' <> rngch) or
                                (prevchar = #0) or          { first char }
                                (']' = scanptr^) then begin { last char }

                                prevchar := rngch;
                                i := Ord(rngch) shr 3;
                                table[i] := table[i] or (1 shl (Ord(rngch) and 7));

                            end
                            else begin  { handle a-z type set }

                                rngch := scanptr^; { get end of range }
                                Inc(scanptr);

                                if prevchar < rngch then  { %[a-z] }
                                    last := rngch
                                else begin              { %[z-a] }
                                    last := prevchar;
                                    prevchar := rngch;
                                end;
                                rngch := prevchar;
                                while rngch <= last do begin
                                    i := Ord(rngch) shr 3;
                                    table[i] := table[i] or (1 shl (Ord(rngch) and 7));
                                    rngch := Chr(Ord(rngch) + 1);
                                end;
                                prevchar := #0;

                            end;
                        end;

                        if scanptr^ = #0 then
                            goto error_return;      { trunc'd format string }

                        { scanset completed.  Now read string }

                        if LEFT_BRACKET = comchr then
                            format := scanptr;

                        if 'c' <> comchr then
                            start := @tmpBuff[0]
                        else
                            start := ptr;

                        {
                        * execute the format directive. that is, scan input
                        * characters until the directive is fulfilled, eof
                        * is reached, or a non-matching character is
                        * encountered.
                        *
                        * it is important not to get the next character
                        * unless that character needs to be tested! other-
                        * wise, reads from line-buffered devices (e.g.,
                        * scanf()) would require an extra, spurious, newline
                        * if the first newline completes the current format
                        * directive.
                        }
                        UN_GETCH(ch);

                        while True do begin
                            if widthset then begin
                                Dec(width);
                                if (width + 1) = 0 then
                                    Break;
                            end;
                            ch := GETCH();
                            if (EOF <> ch) and
                                ((table[Ord(ch) shr 3] xor rej2int[reject]) and (1 shl (Ord(ch) and 7)) <> 0) then begin
                                if not suppress then
                                    PChar(start)^ := ch;
                                start := PChar(start) + 1;
                            end
                            else begin
                                UN_GETCH(ch);
                                break;
                            end;
                        end;

                        { make sure something has been matched and, if
                          assignment is not suppressed, Pascal
                          output string if comchr != c }

                        if ('c' <> comchr) and (start <> tmpBuff) or
                           ('c' = comchr) and (start <> ptr) then
                            if not suppress then begin
                                Inc(count);
                                { Pascal string - zero terminate buffer,
                                  and assign it to Pascal String }
                                if 'c' <> comchr then begin
                                    PChar(start)^ := #0;
                                    PString(ptr)^ := PChar(@tmpBuff[0]);
                                end
                            end
                                else {NULL}
                        else
                            goto error_return;
                    end;

                'i',      { could be d, o, or x }
                'x',
                'p',
                'o',
                'u',
                'd':
                    begin
                        if comchr in ['i', 'x'] then begin
                            if comchr = 'i' then
                                comchr := 'd'; { use as default }

                            if ch in ['-', '+'] then begin
                                negative := (ch = '-');
                                Dec(width);
                                if (width = 0) and widthset then
                                    done_flag := True
                                else
                                    ch := GETCH();
                            end;

                            if '0' = ch then begin

                                ch := GETCH();
                                if ('x' = ch) or ('X' = ch) then begin
                                    ch := GETCH();
                                    comchr := 'x';
                                end
                                else begin
                                    started := True;
                                    if 'x' <> comchr then
                                        comchr := 'o'
                                    else begin
                                        { scanning a hex number that starts }
                                        { with a 0. push back the character }
                                        { currently in ch and restore the 0 }
                                        UN_GETCH(ch);
                                        ch := '0';
                                    end;
                                end;
                            end;
                        end
                        else if comchr in ['p', 'o', 'u', 'd'] then begin

                            if comchr = 'p' then
                                longone := 1; { force %hp to be treated as %p }

                            if ch in ['-', '+'] then begin
                                negative := (ch = '-');
                                Dec(width);
                                if (width = 0) and widthset then
                                    done_flag := True
                                else
                                    ch := GETCH();
                            end;
                        end;

                        while not done_flag do begin

                            if ('x' = comchr) or ('p' = comchr) then

                                if ch in ['0'..'9', 'a'..'f', 'A'..'F'] then begin
                                    number := number shl 4;
                                    if not (ch in ['0'..'9']) then
                                        ch := Chr((Ord(ch) and not
                                            (Ord('a') - Ord('A'))) -
                                            Ord('A') + 10 + Ord('0'));
                                end
                                else
                                    done_flag := True

                            else if ch in ['0'..'9'] then

                                if 'o' = comchr then
                                    if '8' > ch then
                                        number := number shl 3
                                    else
                                        done_flag := True
                                else { 'd' == comchr }
                                    number := number * 10

                            else
                                done_flag := True;

                            if not done_flag then begin
                                started := True;
                                Inc(number, Ord(ch) - Ord('0'));

                                if widthset then begin
                                    Dec(width);
                                    done_flag := (width = 0);
                                end
                                else
                                    ch := GETCH();
                            end
                            else
                                UN_GETCH(ch);

                        end; { end of WHILE loop }

                        if negative then
                            number := Cardinal(- Integer(number));

                        if 'F' = comchr then { expected ':' in long pointer }
                            started := False;

                        if started then
                            if not suppress then begin
                                Inc(count);
                                if longone > 0 then
                                    PInteger(ptr)^ := Cardinal(number)
                                else
                                    PSmallInt(ptr)^ := Word(number);
                            end
                                else {NULL}
                        else
                            goto error_return;

                    end;

                'n':      { char count, don't inc return value }
                    begin
                        number := charcount;
                        if not suppress then
                            if longone > 0 then
                                PInteger(ptr)^ := Cardinal(number)
                            else
                                PSmallInt(ptr)^ := Word(number);
                    end;


                'e',
                {  'E', }
                'g', { scan a float }
                {  'G', }
                'm',
                'f':
                    begin
                        scanptr := floatstring;

                        if ch in ['-', '+'] then begin
                            if ch = '-' then begin
                                scanptr^ := '-';
                                Inc(scanptr);
                            end;
                            Dec(width);
                            ch := GETCH();
                        end;
                                           { must watch width }
                        if not widthset or (width > CVTBUFSIZE) then
                            width := CVTBUFSIZE;

                        { now get integral part
                          If 'm' format then ThousandSeparator is valid char }

                        while (ch in ['0'..'9', ThousandSeparator]) do begin
                            if (ch = ThousandSeparator) then
                                if (comchr <> 'm') then
                                    Break
                                else begin
                                    ch := GETCH();
                                    Continue;
                                end;
                            Dec(width);
                            if width + 1 = 0 then
                                Break;
                            started := True;
                            scanptr^ := ch;
                            Inc(scanptr);
                            ch := GETCH();
                        end;

                        { now check for decimal }

                        if (DecimalSeparator = ch) then begin
                            Dec(width);
                            if width + 1 <> 0 then begin
                                ch := GETCH();
                                scanptr^ := DecimalSeparator;
                                Inc(scanptr);
                                while ch in ['0'..'9'] do begin
                                    Dec(width);
                                    if width + 1 = 0 then
                                        Break;
                                    started := True;
                                    scanptr^ := ch;
                                    Inc(scanptr);
                                    ch := GETCH();
                                end;
                            end;
                        end;

                        { now check for exponent }

                        if started and (('e' = ch) or ('E' = ch)) then begin
                            Dec(width);
                            if width + 1 <> 0 then begin
                                scanptr^ := 'e';
                                Inc(scanptr);

                                ch := GETCH();
                                if ch in ['-', '+'] then begin
                                    if ch = '-' then begin
                                        scanptr^ := '-';
                                        Inc(scanptr);
                                    end;

                                    Dec(width);
                                    if width + 1 = 0 then
                                        Inc(width)
                                    else
                                        ch := GETCH();
                                end;

                                while ch in ['0'..'9'] do begin
                                    Dec(width);
                                    if width + 1 = 0 then
                                        Break;
                                    started := True;
                                    scanptr^ := ch;
                                    Inc(scanptr);
                                    ch := GETCH();
                                end;
                            end;

                        end;

                        UN_GETCH(ch);

                        if started then
                            if not suppress then begin
                                Inc(count);
                                scanptr^ := #0;
                                case longone - 1 of
                                0:
                                    begin
                                        TextToFloat(floatstring, E, fvExtended);
                                        PDouble(ptr)^ := E;
                                    end;
                                1:
                                    TextToFloat(floatstring, PExtended(ptr)^, fvExtended);
                                end;
                            end
                                else {NULL }
                        else
                            goto error_return;
                    end;

                else    { either found '%' or something else }
                    begin
                        if (format^ <> ch) then begin
                            UN_GETCH(ch);
                            goto error_return;
                        end
                        else
                            Dec(match); { % found, compensate for inc below }

                        if not suppress then
                            iArgList := iArgListSave;
                    end;

                end; { case }

                Inc(match);        { matched a format field - set flag }

            end { IF (width) }

            else begin  { zero-width field in format string }
                UN_GETCH(ch);  { check for input error }
                goto error_return;
            end;

            Inc(format);  { skip to next char }

        end
        else begin {  ('%' != *format) }

            ch := GETCH();
            Inc(format);
            if (format - 1)^ <> ch then begin
                UN_GETCH(ch);
                goto error_return;
            end;
        end;

        if (EOF = ch) and ((format^ <> '%') or ((format + 1)^ <> 'n')) then
            break;

    end;  { WHILE (*format) }

error_return:

    if EOF = ch then
        { If any fields were matched or assigned, return count }
        if (count > 0) or (match > 0) then
            Result := count
        else
            Result := 0
    else
        Result := count;
    if pChCount <> nil then
        pChCount^ := charcount;
end;
{$WARNINGS ON}

{ ------------------------------------------------------------------------------ }
{ ------------------------------------------------------------------------------ }
{ String break }

var
    _IntSList: TStringList;


function {$IFDEF NCHOME}Perenos{$ELSE}StrCharBreak{$ENDIF}
    (const cString: string; Num: Integer): TStrings;
begin
    Result := {$IFDEF NCHOME}PerenosGraph{$ELSE}StrGraphBreak{$ENDIF}
        (cString, [Num], nil, nil);
end;


function {$IFDEF NCHOME}PerenosGraph{$ELSE}StrGraphBreak{$ENDIF}
    (const cString: string; AWidths: array of Integer;
    ACanvas: TCanvas; AFont: TFont): TStrings;
var
    vhod, tt, i : integer;
    ob_str: string;
    curWidth, Num: integer;
    tmpFont: TFont;

    function GetDelta: Integer;
    begin
        Result := ACanvas.TextWidth(Copy(cString, vhod, Num)) - curWidth;
    end;

begin
    Result := _IntSList;
    Result.Clear;
    vhod := 1;
    tt := 0;
    tmpFont := nil;
    try
        if (ACanvas <> nil) and (AFont <> nil) then begin
           tmpFont := TFont.Create;
           tmpFont.Assign(ACanvas.Font);
           ACanvas.Font := AFont;
        end;
        while vhod < length(cString) do begin
           if tt > High(AWidths) then
              curWidth := AWidths[High(AWidths)]
           else
              curWidth := AWidths[tt];
           Result.Add('');
           if ACanvas = nil then
              Num := curWidth
           else begin
              Num := Length(cString) - vhod + 1;
              if GetDelta > 0 then begin
                 Dec(Num, GetDelta div ACanvas.TextWidth('W'));
                 if GetDelta > 0 then
                    while (Num > 0) and (GetDelta >= 0) do
                       Dec(Num)
                 else begin
                    while (Num + vhod - 1 <= Length(cString)) and (GetDelta < 0) do
                       Inc(Num);
                    Dec(Num);
                 end;
              end;
           end;
           ob_str := Copy(cString, vhod, num);
           If ob_str[Length(ob_str)] in SpaceChars then begin
              Result[tt] := ob_str;
              Inc(vhod, num);
           end
           else if cString[vhod+num] in SpaceChars then begin
              Result[tt] := ob_str;
              Inc(vhod, num + 1);
           end
           else if cString[vhod+num-2] in SpaceChars then begin
              Result[tt] := Copy(cString, vhod, num-1);
              Inc(vhod, num - 1);
           end
           else if (Length(cString) < vhod+num+1) or (cString[vhod+num+1] in SpaceChars) then begin
              Result[tt] := Copy(cString, vhod, num+1);
              Inc(vhod, num + 1);
           end
           else if ob_str[Length(ob_str)] in ['0'..'9'] then begin
              i := 1;
              while not (ob_str[Length(ob_str) - i + 1] in SpaceChars) do begin
                 If i = num - 1 then begin
                    result.Clear;
                    GenError(STooLongNum);
                 end;
                 Inc(i);
              end;
              Result[tt] := Copy(cString, vhod, num-i);
              Inc(vhod, num - i);
           end
           else begin
              if vhod + num < length(cString) then
                ob_str := ob_str + '-';
              Result[tt] := ob_str;
              Inc(vhod, num);
           end;
           inc(tt);
        end;
        for tt := 0 to Result.Count - 1 do
            Result[tt] := TrimLeft(Result[tt]);
    finally
        if tmpFont <> nil then begin
            ACanvas.Font := tmpFont;
            tmpFont.Free;
        end;
    end;
end;

{ ----------------------------------------------------------------------------- }
{ ----------------------------------------------------------------------------- }
{ Work with RTTI }
{ TypInfo extension }

function DuplicateComponent(src: TComponent; AOwner: TComponent): TComponent;
begin
    Result := TComponentClass(src.ClassType).Create(AOwner);
end;

procedure CopyProperty(src, dest: TObject; srcPropInfo, destPropInfo: PPropInfo);
begin
    case srcPropInfo^.PropType^.Kind of
    tkInteger, tkChar, tkEnumeration, tkSet, tkClass, tkWChar:
        SetOrdProp(dest, destPropInfo, GetOrdProp(src, srcPropInfo));
    tkFloat:
        SetFloatProp(dest, destPropInfo, GetFloatProp(src, srcPropInfo));
    tkString, tkLString, {$IFDEF D2}tkLWString{$ELSE}tkWString{$ENDIF}:
        SetStrProp(dest, destPropInfo, GetStrProp(src, srcPropInfo));
    tkMethod:
        SetMethodProp(dest, destPropInfo, GetMethodProp(src, srcPropInfo));
    tkVariant:
        SetVariantProp(dest, destPropInfo, GetVariantProp(src, srcPropInfo));
    end;
end;

procedure CloneProperties(src, dest: TObject; tkFlt: TTypeKinds; skipDef: Boolean);
var
    i, PropCount: Integer;
    PSrcPropsList: PPropList;
    PInfoDest: PPropInfo;
begin
    PropCount := GetPropList(src.ClassInfo, tkFlt, nil);
    PSrcPropsList := nil;
    try
        GetMem(PSrcPropsList, SizeOf(Pointer) * PropCount);
        GetPropList(src.ClassInfo, tkFlt, PSrcPropsList);
        for i := 0 to PropCount - 1 do begin
            PInfoDest := GetPropInfo(dest.ClassInfo, PSrcPropsList^[i]^.Name);
            if (PInfoDest <> nil) and (PInfoDest^.PropType^.Kind in tkFlt) and
               (PInfoDest^.SetProc <> nil) and 
               (not skipDef or IsStoredProp(src, PSrcPropsList^[i])) then
                CopyProperty(src, dest, PSrcPropsList^[i], PInfoDest);
        end;
    finally
        if PSrcPropsList <> nil then
            FreeMem(PSrcPropsList, SizeOf(Pointer) * PropCount);
    end;
end;

procedure AssignObjProp(obj: TObject; const propName: String; propVal: TPersistent);
var
    pProp: PPropInfo;
begin
    pProp := GetPropInfo(obj.ClassInfo, propName);
    if (pProp <> nil) and (pProp.SetProc <> nil) then
        TPersistent(GetOrdProp(obj, pProp)).Assign(propVal);
end;

procedure AssignOrdProp(obj: TObject; const propName: String; propVal: LongInt);
var
    pProp: PPropInfo;
begin
    pProp := GetPropInfo(obj.ClassInfo, propName);
    if (pProp <> nil) and (pProp.SetProc <> nil) then
        SetOrdProp(obj, pProp, propVal);
end;

procedure AssignStrProp(obj: TObject; const propName: String; const propVal: String);
var
    pProp: PPropInfo;
begin
    pProp := GetPropInfo(obj.ClassInfo, propName);
    if (pProp <> nil) and (pProp.SetProc <> nil) then
        SetStrProp(obj, pProp, propVal);
end;

function ReadObjProp(obj: TObject; const propName: String): TPersistent;
var
    pProp: PPropInfo;
begin
    pProp := GetPropInfo(obj.ClassInfo, propName);
    Result := nil;
    if (pProp <> nil) and (pProp.GetProc <> nil) then
        Result := TPersistent(GetOrdProp(obj, pProp));
end;

function ReadOrdProp(obj: TObject; const propName: String): LongInt;
var
    pProp: PPropInfo;
begin
    pProp := GetPropInfo(obj.ClassInfo, propName);
    Result := 0;
    if (pProp <> nil) and (pProp.GetProc <> nil) then
        Result := GetOrdProp(obj, pProp);
end;

function ReadStrProp(obj: TObject; const propName: String): String;
var
    pProp: PPropInfo;
begin
    pProp := GetPropInfo(obj.ClassInfo, propName);
    Result := '';
    if (pProp <> nil) and (pProp.GetProc <> nil) then
        Result := GetStrProp(obj, pProp);
end;

{ ----------------------------------------------------------------------------- }
{ ----------------------------------------------------------------------------- }
{ Work with variant }

function Var2Text(const V: Variant): String;
var
    tmpV: Variant;
begin
    Result := '';
    if VarIsArray(V) then begin
        tmpV := v[0];
        Result := Var2Text(tmpV);
        tmpV := Unassigned;
        Exit;
    end;
    if not ((TVarData(V).Vtype and varTypeMask) in [varEmpty, varNull, varDispatch,
            varError, varVariant, varUnknown]) then
    try
            Result := V;
    except
    end;
end;

function IntVarIsDefined(const V: Variant; AAll: Boolean): Boolean;
var
    n, i: Integer;
    v1: Variant;
begin
    n := TVarData(V).VType;
    if (n and varArray) = 0 then
        Result := (n <> varEmpty) and (n <> varNull)
    else begin
        n := VarArrayHighBound(V, 1);
        Result := False;
        for i := 0 to n do begin
            v1 := v[i];
            if AAll then
                Result := Result and IntVarIsDefined(v1, AAll)
            else
                Result := Result or IntVarIsDefined(v1, AAll);
            v1 := Unassigned;
            if Result then
                Break;
        end;
    end;
end;

function VarIsDefined(const V: Variant): Boolean;
begin
    Result := IntVarIsDefined(V, False);
end;

function VarIsAllDefined(const V: Variant): Boolean;
begin
    Result := IntVarIsDefined(V, True);
end;

function VarEQ(const V1, V2: Variant): Boolean;
begin
    Result := False;
    if not ((TVarData(V1).Vtype and varTypeMask) in [varEmpty, varDispatch,
            varError, varVariant, varUnknown, varByte]) and
       not ((TVarData(V2).Vtype and varTypeMask) in [varEmpty, varDispatch,
            varError, varVariant, varUnknown, varByte]) then
    try
        Result := V1 = V2;
    except
    end;
end;

function VarCmp(const Val1, Val2: Variant; APartialCmp, ANoCase: Boolean): Boolean;
var
    n1, i: Integer;
    v1, v2: Variant;

    function OneCmp(Val1, Val2: Variant): Boolean;
    var
        s1, s2: String;
        n: Integer;
    begin
            if VarIsNull(Val1) and VarIsNull(Val2) then
                Result := True
            else if VarIsNull(Val1) or VarIsNull(Val2) then
                Result := APartialCmp
            else if (ANoCase or APartialCmp) and
                    (VarType(Val1) and ((varString or varOleStr)) <> 0) then begin
                s1 := val1;
                s2 := val2;
                if ANoCase then begin
                    s1 := AnsiUpperCase(s1);
                    s2 := AnsiUpperCase(s2);
                end;
                if APartialCmp then begin
                    n := Length(s1);
                    if n > Length(s2) then
                        n := Length(s2);
                    s1 := Copy(s1, 1, n);
                    s2 := Copy(s2, 1, n);
                end;
                Result := s1 = s2;
            end
            else
                try
                    Result := val1 = val2;
                except
                    Result := False;
                end;
    end;

begin
    if VarIsArray(Val1) then begin
        n1 := VarArrayHighBound(Val1, 1);
        i := 0;
        while (i <= n1) do begin
            v1 := Val1[i];
            v2 := Val2[i];
            if not OneCmp(v1, v2) then
                Break;
            v1 := Unassigned;
            v2 := Unassigned;
            Inc(i);
        end;
        v1 := Unassigned;
        v2 := Unassigned;
        Result := i > n1;
    end
    else
        Result := OneCmp(Val1, Val2);
end;

function VarFormat(AFormat: String; const AValue: Variant; AErrNote: String): String;
var
    highBound, n: Integer;
    pFmt, pPrev, pResult: PChar;
    ResultMem: array[0..4095] of char;
    s: String;
    v: Variant;
begin
    if AFormat = '' then begin
        Result := Var2Text(AValue);
        exit;
    end;
    pFmt := PChar(AFormat);
    pPrev := pFmt;
    pResult := ResultMem;
    highBound := VarExtHighBound(AValue);
    while pFmt^ <> #0 do begin
        if (pFmt^ = '%') and ((pFmt + 1)^ <> #0) then begin
            while pPrev < pFmt do begin
                pResult^ := pPrev^;
                Inc(pResult);
                Inc(pPrev);
            end;
            Inc(pFmt);
            pPrev := pFmt + 1;
            if pFmt^ <> '%' then begin
                n := Ord(pFmt^) - Ord('0');
                if VarIsDefined(AValue) and (n > highBound) then
                    GenErrorFmt(SDisplayFormatError, [AFormat, n, AErrNote])
                else if highBound > 0 then begin
                    v := AValue[n];
                    s := Var2Text(v);
                    v := Unassigned;
                end
                else
                    s := Var2Text(AValue);
            end
            else
                s := '%';
            StrMove(pResult, PChar(s), Length(s));
            Inc(pResult, Length(s));
        end;
        Inc(pFmt);
    end;
    while pPrev < pFmt do begin
        pResult^ := pPrev^;
        Inc(pResult);
        Inc(pPrev);
    end;
    SetString(Result, ResultMem, pResult - ResultMem);
end;

function VarExtHighBound(const AValue: Variant): Integer;
begin
    if VarIsArray(AValue) then
        Result := VarArrayHighBound(AValue, 1)
    else
        Result := 0;
end;

function VarExtType(const AValue: Variant; i: Integer): Integer;
begin
    Result := varEmpty;
    if VarIsArray(AValue) then begin
        if i <= VarArrayHighBound(AValue, 1) then
            Result := VarType(AValue[i]) and varTypeMask;
    end
    else begin
        if i = 0 then
            Result := VarType(AValue) and varTypeMask;
    end;
end;

function VarExtValue(const AValue: Variant; i: Integer; const APadValue: Variant): Variant;
begin
    if VarIsArray(AValue) then
        if i <= VarArrayHighBound(AValue, 1) then
            Result := AValue[i]
        else
            Result := APadValue
    else
        if i = 0 then
            Result := AValue
        else
            Result := APadValue
end;

function VarArraySlice(const AValue: Variant; fromI, Count: Integer): Variant;
var
    i: Integer;
begin
    if fromI = -1 then
        fromI := VarArrayLowBound(AValue, 1);
    if Count = -1 then
        Count := VarArrayHighBound(AValue, 1) - fromI + 1;
    if Count - 1 = 0 then
        Result := AValue[fromI]
    else begin
        Result := VarArrayCreate([0, Count - 1], VarType(AValue) and varTypeMask);
        for i := 0 to Count - 1 do
            Result[i] := AValue[fromI + i];
    end;
end;

function VarNullOf(const AValue: Variant): Variant;
var
    i, n: Integer;
begin
    if VarIsArray(AValue) then begin
        n := VarArrayHighBound(AValue, 1);
        Result := VarArrayCreate([0, n], varVariant);
        for i := 0 to n do
            Result[i] := Null;
    end
    else
        Result := Null;
end;

function NVL(const AValue, AResult: Variant): Variant;
begin
    if VarIsDefined(AValue) then
        Result := AValue
    else
        Result := AResult;
end;

initialization
    _IntSList := TStringList.Create;
    OneCharMask := '_';
    ManyCharsMask := '%';
finalization
    _IntSList.Free;
end.
