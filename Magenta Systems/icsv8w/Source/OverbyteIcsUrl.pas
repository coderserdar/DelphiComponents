{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     Aug 08, 2004 (extracted from various ICS components)
Version:      8.67
Description:  This unit contain support routines for URL handling.
EMail:        francois.piette@overbyte.be         http://www.overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 1997-2021 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.
              <francois.piette@overbyte.be>

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.

Overview
--------

TRestParams
-----------
Defines a collection of REST parameters and allows them to be saved as
URL encoded, Json, XML or comma separated values, including arrays.  Many
options for varied REST schemes.  String, numeric, boolean and null types
are supported.


TRestParamsSrv
--------------
Allows a SQL database dataset to be converted into Json for REST servers,
including arrays, also handles Json error responses.



History:
Mar 26, 2006 V6.00 New version 6 started
Sep 28, 2008 V6.01 A. Garrels modified UrlEncode() and UrlDecode() to support
             UTF-8 encoding. Moved IsDigit, IsXDigit, XDigit, htoi2 and htoin
             to OverbyteIcsUtils.
Apr 17, 2009 V6.02 A. Garrels added argument CodePage to functions
             UrlEncode() and UrlDecode.
Dec 19, 2009 V6.03 A. Garrels added UrlEncodeToA().
Aug 07, 2010 V6.04 Bjørnar Nielsen suggested to add an overloaded UrlDecode()
                   that takes a RawByteString URL.
Jan 20, 2012 V6.05 RTT changed ParseUrl() to support URLs starting with "//".
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory
Mar 10, 2020 V8.64 Added IcsBuildURL, IcsURLtoASCII and IcsURLtoUnicode to
                     support International Domain Names, note these are primarily
                     for display purposes, ICS now handles IDNs internally.
Oct 17, 2020 V8.65 For UrlEncode RFC3986 section 2.1 says four unreserved chars
                      (- . _ -) should not be percent encoded, so added RfcStrict
                      option to ensure this.
                   IcsUrlEncode uses AnsiString and RfcStrict.
                   UrlEncodeEx always uses RfcStrict.
Sep 21, 2021 V8.67 Moved TRestParama here from OverbyteIcsSslHttpRest to
                     ease circular references.
                   RestParams has a new method AddItemNULL to add a null,
                     in Json this will be unquoted.
                   Added TRestParamsSrv component which provides methods for
                     creating REST server Json responses from a SQL database
                     resultset, one or more rows, also error responses. Note
                     this is only compiled if DATABASE is defined in OverbyteIcsDefs.inc
                     to avoid bringing in database units that are not available
                     on all Delphi editions.  There is a REST server sample
                      OverbyteIcsDDWebService.dpr that illustrates SQL lookups.



 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsUrl;

interface

{$B-}             { Enable partial boolean evaluation   }
{$T-}             { Untyped pointers                    }
{$X+}             { Enable extended syntax              }
{$I Include\OverbyteIcsDefs.inc}
{$IFDEF DELPHI6_UP}
    {$WARN SYMBOL_PLATFORM   OFF}
    {$WARN SYMBOL_LIBRARY    OFF}
    {$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}
{$IFNDEF VER80}   { Not for Delphi 1                    }
    {$H+}         { Use long strings                    }
    {$J+}         { Allow typed constant to be modified }
{$ENDIF}
{$IFDEF BCB3_UP}
    {$ObjExportAll On}
{$ENDIF}

uses
{$IFDEF MSWINDOWS}
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
{$ENDIF}
    {$IFDEF RTL_NAMESPACES}System.Classes{$ELSE}Classes{$ENDIF},      { V8.67 }
    {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},    { V8.67 }
{$IFDEF DATABASE}
    {$IFDEF RTL_NAMESPACES}Data.DB{$ELSE}DB{$ENDIF},   { V8.67 }
{$ENDIF}
    OverbyteIcsSuperObject,    { V8.67 }
    OverbyteIcsUtils;

const
    IcsUrlVersion        = 867;
    CopyRight : String   = ' TIcsURL (c) 1997-2021 F. Piette V8.67 ';

{ Syntax of an URL: protocol://[user[:password]@]server[:port]/path }
procedure ParseURL(const URL : String;
                   var Proto, User, Pass, Host, Port, Path : String);
function  Posn(const s, t : String; count : Integer) : Integer;
{ V8.64 build a URL without changing any encoding }
function IcsBuildURL(const Proto, User, Pass, Host, Port, Path: string): string ;
{ V8.64 convert the Unicode domain host name in a URL to A-Label (Punycode ASCII) and vice versa }
function IcsURLtoASCII(const Input: string): string ;
function IcsURLtoUnicode(const Input: string): string ;

{ following functions are not for host domain names, but Unicode paths and queries in URLs }
function UrlEncode(const S: String; DstCodePage: LongWord = CP_UTF8;
                                     RfcStrict: Boolean = False): String;          { V8.65 }
function UrlDecode(const S     : String;
                   SrcCodePage : LongWord = CP_ACP;
                   DetectUtf8  : Boolean = TRUE) : String;
{$IFDEF COMPILER12_UP}
                   overload;
function UrlDecode(const S     : RawByteString;
                   SrcCodePage : LongWord = CP_ACP;
                   DetectUtf8  : Boolean = TRUE) : UnicodeString; overload;
{$ENDIF}
function IcsUrlEncode(const AStr: AnsiString; RfcStrict: Boolean = False): AnsiString;  { V8.65 }
function UrlEncodeToA(const S: String; DstCodePage: LongWord = CP_UTF8;
                                           RfcStrict: Boolean = False): AnsiString;   { V8.65 }
function UrlEncodeEx(const S: String): String;                                        { V8.65 }


{ V8.67 moved from OverbyteIcsSslHttpRest to ease circular references }
type
  TPContent = (PContUrlencoded, PContJson, PContXML, PContBodyUrlEn, PContBodyJson, PContBodyXML,  { V8.64 added Body versions and XML }
                  PContCommaList); { V8.66 added CommaList for OAuth1 }

  TRParamType = (RPTypeStr, RPTypeInt, RPTypeDate, RPTypeFloat, RPTypeBool, RPTypeObj, RPTypeArray, RPTypeNull); { V8.65, V8.67 added Null }

const
  RParamTypeLits: array[TRParamType] of string =
        ('RPTypeStr','RPTypeInt','RPTypeDate','RPTypeFloat','RPTypeBool','RPTypeObj','RPTypeArray', 'RPTypeNull');  { V8.65 }

{ TRestParam is one REST parameter }
type
  TRestParam = class(TCollectionItem)
  private
    FPName: String;
    FPValue: String;
    FPRaw: Boolean;               { V8.65 gone }
    FRParamType: TRParamType;     { V8.65 }
    FPValObj: ISuperObject;       { V8.65 }
    FPValArray: TStrings;         { V8.65 }
  protected
    function GetDisplayName: string; override;
  published
    constructor Create (Collection: TCollection); Override ;
    destructor Destroy; Override ;                                              { V8.65 }
    property PName: String                read  FPName
                                          write FPName;
    property PValue : String              read  FPValue
                                          write FPValue;
    property PRaw : boolean               read  FPRaw
                                          write FPRaw;
    property RParamType: TRParamType      read  FRParamType
                                          write FRParamType;
    property PValObj: ISuperObject        read  FPValObj
                                          write FPValObj;
    property PValArray: TStrings          read  FPValArray
                                          write FPValArray;
  end;

{ TRestParams defines a collection of  REST parameters }
  TRestParams = class(TCollection)
  private
    FOwner: TPersistent;
    FPContent: TPContent;
    FSortList: TStringList;    { V8.65 }
    FRfcStrict: Boolean;       { V8.65 RFC3986 strict urlencoding }
    function GetItem(Index: Integer): TRestParam;
    procedure SetItem(Index: Integer; Value: TRestParam);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(Owner: TPersistent);
    destructor Destroy; Override;
//    procedure Clear; Override;                                                { V8.65 }
    procedure RebuildSortList;                                                  { V8.65 }
    function GetParameters(Sorted: Boolean = False): AnsiString;  { V8.65 added sorted }
    function IndexOf(const aName: String): Integer;
    procedure AddItem(const aName, aValue: String; aRaw: Boolean = False); overload;
    procedure AddItem(const aName, aValue: String; RParamType: TRParamType); overload;   { V8.65 }
    procedure AddItemA(const aName: String; const aValue: AnsiString; aRaw: Boolean = False);      { V8.67 }
    procedure AddItem(const aName: String; aValue: Integer); overload;          { V8.65 }
    procedure AddItem(const aName: String; aValue: Double); overload;           { V8.65 }
    procedure AddItem(const aName: String; aValue: Boolean); overload;          { V8.65 }
    procedure AddItemSO(const aName: String; aValue: ISuperObject);             { V8.65 }
    procedure AddItemAR(const aName: String; aValue: TStrings);                 { V8.65 }
    procedure AddItemDT(const aName: String; aValue: TDateTime);                { V8.65 }
    procedure AddItemNULL(const aName: String);                                 { V8.67 }
    procedure RemoveItem(const aName: String);                                  { V8.65 }
    property Items[Index: Integer]: TRestParam      read GetItem
                                                    write SetItem; default;
  published
    property PContent: TPContent                    read  FPContent
                                                    write FPContent;
    property RfcStrict: Boolean                     read  FRfcStrict            { V8.65 }
                                                    write FRfcStrict;
  end;

{ V8.67 TRestParamsSrv extends TRestParams for database REST servers }
{$IFDEF DATABASE}
type
  TRestErr = (RestErrUnknown,       // 0
              RestErrInternal,      // 1
              RestErrUnsupported,   // 2
              RestErrConvert,       // 3
              RestErrNoRecords,     // 4
              RestErrNoFields,      // 5
              RestErrGeneral,       // 6
              RestErrQuota,         // 7
              RestErrDatabase,      // 8
              RestErrAuthz);        // 9

const
  RestErrLits: array[TRestErr] of string = (
        'Unknown Error',
        'Internal Error',
        'Unsupported Request',
        'Failed to convert SQL resultset to JSON',
        'No Records Found',
        'No Fields Found',
        'General Error',   { normally use custom text }
        'Quota Exceeded',
        'Database is currently unavailable',
        'Authorisation Failed' );

type
  TRestParamsSrv = class(TObject)
  private
    FJsonRecord: TRestParams;
    FJsonResult: TRestParams;
    FJsonArray: TIcsStringBuild;
  public
    constructor  Create;
    destructor   Destroy; override;
    function     JsonErr(RestErr: TRestErr; const ErrDesc: String = ''): AnsiString;
    function     ResultSet2Json(DataSet: TDataSet; var JsonStr: AnsiString; MaxRecs: Integer = 0): Boolean;
  end;
{$ENDIF}

function IcsEscapeJson(const AStr: AnsiString): AnsiString;  { V8.66 renamed and made public }

implementation

type
    TCharSet = set of AnsiChar;
const
    UriProtocolSchemeAllowedChars : TCharSet = ['a'..'z','0'..'9','+','-','.'];


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Find the count'th occurence of the s string in the t string.              }
{ If count < 0 then look from the back                                      }
function Posn(const s , t : String; Count : Integer) : Integer;
var
    i, h, Last : Integer;
    u          : String;
begin
    u := t;
    if Count > 0 then begin
        Result := Length(t);
        for i := 1 to Count do begin
            h := Pos(s, u);
            if h > 0 then
                u := Copy(u, h + 1, Length(u))
            else begin
                u := '';
                Inc(Result);
            end;
        end;
        Result := Result - Length(u);
    end
    else if Count < 0 then begin
        Last := 0;
        for i := Length(t) downto 1 do begin
            u := Copy(t, i, Length(t));
            h := Pos(s, u);
            if (h <> 0) and ((h + i) <> Last) then begin
                Last := h + i - 1;
                Inc(count);
                if Count = 0 then
                    break;
            end;
        end;
        if Count = 0 then
            Result := Last
        else
            Result := 0;
    end
    else
        Result := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Syntax of an URL: protocol://[user[:password]@]server[:port]/path         }
procedure ParseURL(
    const url : String;
    var Proto, User, Pass, Host, Port, Path : String);
var
    p, q, i : Integer;
    s       : String;
    CurPath : String;
begin
    CurPath := Path;
    proto   := '';
    User    := '';
    Pass    := '';
    Host    := '';
    Port    := '';
    Path    := '';

    if Length(url) < 1 then
        Exit;

    { Handle path beginning with "./" or "../".          }
    { This code handle only simple cases !               }
    { Handle path relative to current document directory }
    if (Copy(url, 1, 2) = './') then begin
        p := Posn('/', CurPath, -1);
        if p > Length(CurPath) then
            p := 0;
        if p = 0 then
            CurPath := '/'
        else
            CurPath := Copy(CurPath, 1, p);
        Path := CurPath + Copy(url, 3, Length(url));
        Exit;
    end
    { Handle path relative to current document parent directory }
    else if (Copy(url, 1, 3) = '../') then begin
        p := Posn('/', CurPath, -1);
        if p > Length(CurPath) then
            p := 0;
        if p = 0 then
            CurPath := '/'
        else
            CurPath := Copy(CurPath, 1, p);

        s := Copy(url, 4, Length(url));
        { We could have several levels }
        while TRUE do begin
            CurPath := Copy(CurPath, 1, p-1);
            p := Posn('/', CurPath, -1);
            if p > Length(CurPath) then
                p := 0;
            if p = 0 then
                CurPath := '/'
            else
                CurPath := Copy(CurPath, 1, p);
            if (Copy(s, 1, 3) <> '../') then
                break;
            s := Copy(s, 4, Length(s));
        end;

        Path := CurPath + Copy(s, 1, Length(s));
        Exit;
    end;

    p := pos('://', url);
    q := p;
    if p <> 0 then begin
        S := IcsLowerCase(Copy(url, 1, p - 1));
        for i := 1 to Length(S) do begin
            if not (AnsiChar(S[i]) in UriProtocolSchemeAllowedChars) then begin
                q := i;
                Break;
            end;
        end;
        if q < p then begin
            p     := 0;
            proto := 'http';
        end;
    end;
    if p = 0 then begin
        if (url[1] = '/') then begin
            { Relative path without protocol specified }
            proto := 'http';
            //p     := 1;     { V6.05 }
            if (Length(url) > 1) then begin
                if (url[2] <> '/') then begin
                    { Relative path }
                    Path := Copy(url, 1, Length(url));
                    Exit;
                end
                else
                    p := 2;   { V6.05 }
            end
            else begin        { V6.05 }
                Path := '/';  { V6.05 }
                Exit;         { V6.05 }
            end;
        end
        else if IcsLowerCase(Copy(url, 1, 5)) = 'http:' then begin
            proto := 'http';
            p     := 6;
            if (Length(url) > 6) and (url[7] <> '/') then begin
                { Relative path }
                Path := Copy(url, 6, Length(url));
                Exit;
            end;
        end
        else if IcsLowerCase(Copy(url, 1, 7)) = 'mailto:' then begin
            proto := 'mailto';
            p := pos(':', url);
        end;
    end
    else begin
        proto := IcsLowerCase(Copy(url, 1, p - 1));
        inc(p, 2);
    end;
    s := Copy(url, p + 1, Length(url));

    p := pos('/', s);
    q := pos('?', s);
    if (q > 0) and ((q < p) or (p = 0)) then
        p := q;
    if p = 0 then
        p := Length(s) + 1;
    Path := Copy(s, p, Length(s));
    s    := Copy(s, 1, p-1);

    { IPv6 URL notation, for instance "[2001:db8::3]" }
    p := Pos('[', s);
    q := Pos(']', s);
    if (p = 1) and (q > 1) then
    begin
        Host := Copy(s, 2, q - 2);
        s := Copy(s, q + 1, Length(s));
    end;

    p := Posn(':', s, -1);
    if p > Length(s) then
        p := 0;
    q := Posn('@', s, -1);
    if q > Length(s) then
        q := 0;
    if (p = 0) and (q = 0) then begin   { no user, password or port }
        if Host = '' then
            Host := s;
        Exit;
    end
    else if q < p then begin  { a port given }
        Port := Copy(s, p + 1, Length(s));
        if Host = '' then
            Host := Copy(s, q + 1, p - q - 1);
        if q = 0 then
            Exit; { no user, password }
        s := Copy(s, 1, q - 1);
    end
    else begin
        if Host = '' then
            Host := Copy(s, q + 1, Length(s));
        s := Copy(s, 1, q - 1);
    end;
    p := pos(':', s);
    if p = 0 then
        User := s
    else begin
        User := Copy(s, 1, p - 1);
        Pass := Copy(s, p + 1, Length(s));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.64 build a URL without changing any encoding }
function IcsBuildURL (const Proto, User, Pass, Host, Port, Path: string): string ;
begin
    Result := Proto + '://' ;
    if User <> '' then begin
        Result := Result + User ;
        if Pass <> '' then Result := Result + ':' + Pass ;
        Result := Result + '@' ;
    end ;
    Result := Result + Host ;
    if Port <> '' then Result := Result + ':' + Port ;
    if Path <> '' then begin
        if Path[1] <> '/' then Result := Result + '/' ;
        Result := Result + Path ;
    end;
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.64 convert the Unicode domain host name in a URL to A-Label (Punycode ASCII) }
function IcsURLtoASCII (const Input: string): string ;
var
    Proto, User, Pass, Host, Port, Path: string;
begin
    ParseURL(Input, Proto, User, Pass, Host, Port, Path);
    Result := IcsBuildURL(Proto, User, Pass, IcsIDNAToASCII(Host), Port, Path);
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.64 convert the A-Label (Punycode ASCII) domain host name in a URL to Unicode }
{ not does not change path }
function IcsURLtoUnicode (const Input: string): string ;
var
    Proto, User, Pass, Host, Port, Path: string;
begin
    ParseURL(Input, Proto, User, Pass, Host, Port, Path);
    Result := IcsBuildURL(Proto, User, Pass, IcsIDNAToUnicode(Host), Port, Path);
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.65 RFC3986 section 2.1 says four unreserved chars (- . _ -) should not
  be percent encoded, so added RfcStrict option to ensure this, AnsiStrings }
function IcsUrlEncode(const AStr: AnsiString; RfcStrict: Boolean = False): AnsiString;
var
    I, J   : Integer;
    RStr   : AnsiString;
    HexStr : String[2];
    ACh    : AnsiChar;
begin
    SetLength(RStr, Length(AStr) * 3);
    J := 0;
    for I := 1 to Length(AStr) do begin
        ACh := AStr[I];
        if ((ACh >= '0') and (ACh <= '9')) or
              ((ACh >= 'a') and (ACh <= 'z')) or
                  ((ACh >= 'A') and (ACh <= 'Z')) then begin
            Inc(J);
            RStr[J] := ACh;
        end
        else if RfcStrict and ((ACh = '.') or (ACh = '-') or
                         (ACh = '_')  or (ACh = '~')) then begin
            Inc(J);
            RStr[J] := ACh;
        end
        else begin
            Inc(J);
            RStr[J] := '%';
            HexStr  := IcsIntToHexA(Ord(ACh), 2);
            Inc(J);
            RStr[J] := HexStr[1];
            Inc(J);
            RStr[J] := HexStr[2];
        end;
    end;
    SetLength(RStr, J);
    Result := RStr;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.65 RFC3986 section 2.1 says four unreserved chars (- . _ -) should not
  be percent encoded, so added RfcStrict option to ensure this, Strings }
function UrlEncodeToA(const S: String; DstCodePage: LongWord = CP_UTF8;
                                            RfcStrict: Boolean = False): AnsiString;
var
    AStr   : AnsiString;
begin
{$IFDEF COMPILER12_UP}
    AStr := UnicodeToAnsi(S, DstCodePage);
{$ELSE}
    if DstCodePage = CP_UTF8 then
        AStr := StringToUtf8(S)
    else
        AStr := S;
{$ENDIF}
    Result := IcsUrlEncode(AStr, RfcStrict);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function UrlEncode(const S: String; DstCodePage: LongWord = CP_UTF8;
                                             RfcStrict: Boolean = False): String; { V8.65 added RfcStrict }
begin
    Result := String(UrlEncodeToA(S, DstCodePage, RfcStrict));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function UrlEncodeEx(const S: String): String;                                   { V8.65 }
begin
    Result := String(UrlEncodeToA(S, CP_UTF8, True));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function UrlDecode(const S : String; SrcCodePage: LongWord = CP_ACP;
  DetectUtf8: Boolean = TRUE) : String;
var
    I, J, L : Integer;
    U8Str   : AnsiString;
    Ch      : AnsiChar;
begin
    L := Length(S);
    SetLength(U8Str, L);
    I := 1;
    J := 0;
    while (I <= L) and (S[I] <> '&') do begin
        Ch := AnsiChar(S[I]);
        if Ch = '%' then begin
            Ch := AnsiChar(htoi2(PChar(@S[I + 1])));
            Inc(I, 2);
        end
        else if Ch = '+' then
            Ch := ' ';
        Inc(J);
        U8Str[J] := Ch;
        Inc(I);
    end;
    SetLength(U8Str, J);
    if (SrcCodePage = CP_UTF8) or (DetectUtf8 and IsUtf8Valid(U8Str)) then
{$IFDEF COMPILER12_UP}
        Result := Utf8ToStringW(U8Str)
    else
        Result := AnsiToUnicode(U8Str, SrcCodePage);
{$ELSE}
        Result := Utf8ToStringA(U8Str)
    else
        Result := U8Str;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF COMPILER12_UP}
function UrlDecode(const S: RawByteString; SrcCodePage: LongWord = CP_ACP;
  DetectUtf8: Boolean = TRUE): UnicodeString;
var
    I, J, L : Integer;
    U8Str   : AnsiString;
    Ch      : AnsiChar;
begin
    L := Length(S);
    SetLength(U8Str, L);
    I := 1;
    J := 0;
    while (I <= L) and (S[I] <> '&') do begin
        Ch := AnsiChar(S[I]);
        if Ch = '%' then begin
            Ch := AnsiChar(htoi2(PAnsiChar(@S[I + 1])));
            Inc(I, 2);
        end
        else if Ch = '+' then
            Ch := ' ';
        Inc(J);
        U8Str[J] := Ch;
        Inc(I);
    end;
    SetLength(U8Str, J);
    if (SrcCodePage = CP_UTF8) or (DetectUtf8 and IsUtf8Valid(U8Str)) then
        Result := Utf8ToStringW(U8Str)
    else
        Result := AnsiToUnicode(U8Str, SrcCodePage);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TRestParam }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TRestParam.Create(Collection: TCollection);
begin
    inherited;
    FPRaw := False;
    FRParamType := RPTypeStr;  { V8.65 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TRestParam.Destroy;                                                  { V8.65 }
begin
    FreeAndNil(FPValArray);
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TRestParam.GetDisplayName: string;
begin
    if FPName <> '' then
        Result := FPName + '=' + FPValue
    else
        Result := Inherited GetDisplayName
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TRestParams }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TRestParams.Create(Owner: TPersistent);
begin
    FOwner := Owner;
    inherited Create(TRestParam);
    FPContent := PContUrlencoded;
    FRfcStrict := false;  { V8.65 trur means strict RFC URL encoding }
    FSortList := TStringList.Create;
    FSortList.Sorted := True;
    FSortList.CaseSensitive := True;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TRestParams.Destroy;
begin
    FreeAndNil(FSortList);
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TRestParams.GetItem(Index: Integer): TRestParam;
begin
  Result := TRestParam(inherited GetItem(Index));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TRestParams.SetItem(Index: Integer; Value: TRestParam);
begin
  inherited SetItem(Index, TCollectionItem(Value));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TRestParams.GetOwner: TPersistent;
begin
  Result := FOwner;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TRestParams.IndexOf(const aName: string): Integer;
var
    I: Integer;
begin
    Result := -1;
    if Count = 0 then Exit;
    for I := 0 to Count - 1 do begin
        if Items[I].PName = aName then begin
            Result := I;
            Exit;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TRestParams.AddItem(const aName, aValue: string; aRaw: Boolean = False);
var
    Index: Integer;
begin
    Index := IndexOf(aName);
    if Index < 0 then begin
        Index := Count;
        Add;
    end;
    Items[Index].PName := aName;
    Items[Index].PValue := aValue;
    Items[Index].PRaw := aRaw;
    if aRaw then
        Items[Index].RParamType := RPTypeObj  { V8.65 }
    else
        Items[Index].RParamType := RPTypeStr;  { V8.65 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TRestParams.AddItem(const aName, aValue: String; RParamType: TRParamType);   { V8.65 }
var
    Index: Integer;
    MyInt: Integer;
    MyFloat: Double;
    MyStr: String;
begin
    Index := IndexOf(aName);
    if Index < 0 then begin
        Index := Count;
        Add;
    end;
    Items[Index].PName := aName;
    Items[Index].PValue := aValue;

{ check non-string types are acceptable to Json, otherwise make them strings }
    if RParamType = RPTypeInt then begin
        if NOT TryStrToInt(aValue, MyInt) then
            RParamType := RPTypeStr;
    end;
    if RParamType = RPTypeFloat then begin
        if NOT TryStrToFloat(aValue, MyFloat) then
            RParamType := RPTypeStr;
    end;
  { boolean made lower case, also accepts Y/N }
    if RParamType = RPTypeBool then begin
        MyStr := IcsLowercase(aValue);
        if Pos('y', MyStr) = 1 then MyStr := 'true';
        if Pos('n', MyStr) = 1 then MyStr := 'false';
        if (MyStr <> 'true') and (MyStr <> 'false') then
            RParamType := RPTypeStr
        else
            Items[Index].PValue := MyStr;
    end;
    if RParamType = RPTypeObj then begin
        MyInt := Length(aValue);
        if (MyInt < 2) then
            RParamType := RPTypeStr
        else begin
           if NOT (((aValue[1]='{') and (aValue[MyInt]='}')) or
                   ((aValue[1]='[') and (aValue[MyInt]=']'))) then RParamType := RPTypeStr;
        end;
    end;
    if RParamType = RPTypeArray then begin
        if NOT Assigned(Items[Index].PValArray) then
            Items[Index].PValArray := TStringList.Create;
        Items[Index].PValArray.CommaText := aValue;
    end;
    Items[Index].RParamType := RParamType;
    Items[Index].PRaw := (RParamType in [RPTypeInt, RPTypeFloat, RPTypeBool, RPTypeObj]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TRestParams.AddItemA(const aName: String; const aValue: AnsiString; aRaw: Boolean = False);      { V8.67 }
begin
    AddItem(aName, String(aValue), aRaw);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TRestParams.AddItem(const aName: String; aValue: Integer);        { V8.65 }
var
    Index: Integer;
begin
    Index := IndexOf(aName);
    if Index < 0 then begin
        Index := Count;
        Add;
    end;
    Items[Index].PName := aName;
    Items[Index].PValue := IntToStr(aValue);
    Items[Index].RParamType := RPTypeInt;
    Items[Index].PRaw := True;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TRestParams.AddItem(const aName: String; aValue: Double);           { V8.65 }
var
    Index: Integer;
begin
    Index := IndexOf(aName);
    if Index < 0 then begin
        Index := Count;
        Add;
    end;
    Items[Index].PName := aName;
    Items[Index].PValue := FloatToStr(aValue);
    Items[Index].RParamType := RPTypeFloat;
    Items[Index].PRaw := False;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TRestParams.AddItem(const aName: String; aValue: Boolean);         { V8.65 }
var
    Index: Integer;
begin
    Index := IndexOf(aName);
    if Index < 0 then begin
        Index := Count;
        Add;
    end;
    Items[Index].PName := aName;
    if aValue then
        Items[Index].PValue := 'true'
    else
        Items[Index].PValue := 'false';
    Items[Index].RParamType := RPTypeBool;
    Items[Index].PRaw := True;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TRestParams.AddItemSO(const aName: String; aValue: ISuperObject);     { V8.65 }
var
    Index: Integer;
begin
    Index := IndexOf(aName);
    if Index < 0 then begin
        Index := Count;
        Add;
    end;
    Items[Index].PName := aName;
    Items[Index].PValObj := aValue;
    if Assigned(aValue) then
        Items[Index].PValue := aValue.AsJson(false,false)  { no indent, no escape }
    else
        Items[Index].PValue := '';
    Items[Index].RParamType := RPTypeObj;
    Items[Index].PRaw := True;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TRestParams.AddItemAR(const aName: String; aValue: TStrings);     { V8.65 }
var
    Index: Integer;
begin
    Index := IndexOf(aName);
    if Index < 0 then begin
        Index := Count;
        Add;
    end;
    Items[Index].PName := aName;
    if NOT Assigned(Items[Index].PValArray) then
        Items[Index].PValArray := TStringList.Create;
    Items[Index].PValArray.Assign(aValue);
    Items[Index].PValue := aValue.CommaText;
    Items[Index].RParamType := RPTypeArray;
    Items[Index].PRaw := False;  // assume strings for now
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TRestParams.AddItemDT(const aName: String; aValue: TDateTime);    { V8.65 }
var
    Index: Integer;
begin
    Index := IndexOf(aName);
    if Index < 0 then begin
        Index := Count;
        Add;
    end;
    Items[Index].PName := aName;
//    Items[Index].PValDate := aValue;
    Items[Index].PValue := RFC3339_DateToStr(aValue);
    Items[Index].RParamType := RPTypeDate;
    Items[Index].PRaw := False;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TRestParams.AddItemNULL(const aName: String);                                 { V8.67 }
var
    Index: Integer;
begin
    Index := IndexOf(aName);
    if Index < 0 then begin
        Index := Count;
        Add;
    end;
    Items[Index].PName := aName;
    Items[Index].PValue := 'null';
    Items[Index].RParamType := RPTypeNull;
    Items[Index].PRaw := False;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TRestParams.RemoveItem(const aName: string);                      { V8.65 }
var
    Index: Integer;
begin
    Index := IndexOf(aName);
    if Index >= 0 then Delete(Index);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TRestParams.RebuildSortList;                       { V8.65 build sorted list }
var
    Index: Integer;
begin
    FSortList.Clear;
    if Count = 0 then Exit;
    for Index := 0 to Count - 1 do
        FSortList.AddObject(Items[Index].PName, TObject(Index));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsEscapeJson(const AStr: AnsiString): AnsiString;  { V8.66 renamed and made public }
var
    I, outoff, inlen: integer;
    Ch: PAnsiChar;

    procedure AddEsc(NewCh: AnsiChar);
    begin
        Result[outoff] := '\';
        Inc(outoff);
        Result[outoff] := NewCh;
    end;

begin
    Result := '';
    outoff := 1;
    inlen := Length(AStr);
    if inlen = 0 then Exit;
    SetLength(Result, inlen * 2);
    Ch := Pointer(AStr);
    for I := 1 to inlen do begin
        if Ch^ = '\'  then
            AddEsc('\')
        else if Ch^ = '/' then
            AddEsc('/')
        else if Ch^ = '"' then
            AddEsc('"')
        else if Ch^ = IcsCR then
            AddEsc('r')
        else if Ch^ = IcsLF then
            AddEsc('n')
        else if Ch^ = IcsBACKSPACE  then
            AddEsc('b')
        else if Ch^ = IcsTab  then
            AddEsc('t')
        else
            Result[outoff] := Ch^;
        Inc(Ch);
        Inc(outoff);
    end;
    SetLength(Result, outoff - 1);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function EscapeXML(const AStr: AnsiString): AnsiString;  { V8.64 }
var
    I, outoff, inlen: integer;
    Ch: PAnsiChar;

    procedure AddEntity(NewStr: AnsiString);
    var
        J: Integer;
    begin
        Result[outoff] := '&';
        Inc(outoff);
        for J := 1 to Length(NewStr) do begin
            Result[outoff] := NewStr[J];
            Inc(outoff);
        end;
        Result[outoff] := ';';
    end;

begin
    Result := '';
    outoff := 1;
    inlen := Length(AStr);
    if inlen = 0 then Exit;
    SetLength(Result, inlen * 2);
    Ch := Pointer(AStr);
    for I := 1 to inlen do begin
        if Ch^ = '&'  then
            AddEntity('amp')
        else if Ch^ = '''' then
            AddEntity('apos')
        else if Ch^ = '"' then
            AddEntity('quot')
        else if Ch^ = '<' then
            AddEntity('lt')
        else if Ch^ = '>' then
            AddEntity('gt')
        else
            Result[outoff] := Ch^;
        Inc(Ch);
        Inc(outoff);
    end;
    SetLength(Result, outoff - 1);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$WARN NO_RETVAL OFF}
{ Delphi 2007 gives a false 'Return value undefined', stop the warning }
function TRestParams.GetParameters(Sorted: Boolean = False): AnsiString;  { V8.65 added sorted }
var
    I, J, K, ArrayLen, Len: integer;
    PN, PV: AnsiString;
    JFlag: Boolean; { V8.62 }
begin
    if Sorted then RebuildSortList; { V8.65 do we need to build sort list }
    if FPContent in [PContUrlencoded, PContBodyUrlen] then begin  { V8.64 added Body version }
        Result := '';
        if Count > 0 then begin
            for J := 0 to Count - 1 do begin
                if Sorted then
                    I := Integer(FSortList.Objects[J])
                else
                    I := J;
                PN := StringToUtf8(Trim(Items[I].PName));
                if PN <> '' then begin
                    K := 0;
                    ArrayLen := 0;
                { V8.65 array is added as multiple identical name=value pairs }
                { beware this may not always be supported, but works for Google APIs }
                    if (Items[I].RParamType = RPTypeArray) and Assigned(Items[I].FPValArray) then
                        ArrayLen := Items[I].FPValArray.Count;
                    while true do begin
                        if ArrayLen > 0 then
                            PV := StringToUtf8(Trim(Items[I].FPValArray[K]))
                        else
                            PV := StringToUtf8(Trim(Items[I].PValue));
                        if Result <> '' then
                            Result := Result + '&';
                        Result := Result + PN + '=';
                        if Items[I].PRaw then
                            Result := Result + PV
                        else
                            Result := Result + IcsUrlEncode(PV, FRfcStrict);  { V8.65 added strict }
                        inc(K);
                        if K >= ArrayLen then break;
                    end;
                end;
            end;
        end;
    end
    else if FPContent in [PContJson, PContBodyJson] then begin  { V8.64 added Body version }
        Result := '{';
        if Count > 0 then begin
            for J := 0 to Count - 1 do begin
                if Sorted then
                    I := Integer(FSortList.Objects[J])
                else
                    I := J;
                PN := StringToUtf8(Trim(Items[I].PName));
                if PN <> '' then begin
                    ArrayLen := 0;
                    JFlag := False;
                 { V8.65 see if building Json array from StringList }
                    if (Items[I].RParamType = RPTypeArray) and Assigned(Items[I].FPValArray) then
                        ArrayLen := Items[I].FPValArray.Count;
                    if ArrayLen > 0 then begin
                        PV := '[';
                        for K := 0 to ArrayLen - 1 do begin
                            if K >= 1 then PV := PV + ',';
                            PV := PV + '"' + IcsEscapeJson(StringToUtf8(Trim(Items[I].FPValArray[K]))) + '"';
                        end;
                        PV := PV + ']';
                        JFlag := True;
                    end
                    else begin
                        PV := StringToUtf8(Trim(Items[I].PValue));
                        if NOT Items[I].PRaw then
                            PV := IcsEscapeJson(PV);
                    end;
                    Len := Length(PV);
                  { V8.62 check if adding Json, don't quote it }
                    if Len >= 2 then
                            JFlag := ((PV[1]='{') and (PV[Len]='}')) or
                                            ((PV[1]='[') and (PV[Len]=']'));
                  { V8.65 data types that don't need quotes }
                    if Items[I].RParamType in [RPTypeInt, RPTypeFloat, RPTypeBool, RPTypeObj, RPTypeNull] then  { V8.67 added Null }
                        JFlag := True;
                    if Length(Result) > 1 then Result := Result + ',';
                    Result := Result + '"' + PN + '":';
                    if NOT JFlag then Result := Result + '"';
                    Result := Result + PV;
                    if NOT JFlag then Result := Result + '"';
                end;
            end;
        end;
        Result := Result + '}'
    end
    else if FPContent in [PContXml, PContBodyXml] then begin  { V8.64 new }
        Result := '<?xml version="1.0" encoding="UTF-8"><ICS>';
        if Count > 0 then begin
            for J := 0 to Count - 1 do begin
            { V8.65 XML does not supports arrays so use string instead }
                if Sorted then
                    I := Integer(FSortList.Objects[J])
                else
                    I := J;
                PN := StringToUtf8(Trim(Items[I].PName));
                if PN <> '' then begin
                    PV := StringToUtf8(Trim(Items[I].PValue));
                    Result := Result + '<' + EscapeXML(PN) + '>';
                    if Items[I].PRaw then
                        Result := Result + PV
                    else
                        Result := Result + EscapeXML(PV);
                    Result := Result + '</' + EscapeXML(PN) + '>';
                end;
            end;
        end;
        Result := Result + '</ICS>';
    end
  { V8.65 comma separate quoted values for OAuth1 Authhorize: header }
    else if FPContent = PContCommaList then begin
        Result := '';
        if Count > 0 then begin
            for J := 0 to Count - 1 do begin
            { V8.65 OAuth1 does not need arrays so use string instead }
                if Sorted then
                    I := Integer(FSortList.Objects[J])
                else
                    I := J;
                PN := StringToUtf8(Trim(Items[I].PName));
                if PN <> '' then begin
                    PV := StringToUtf8(Trim(Items[I].PValue));
                    if Result <> '' then Result := Result + ', ';
                    Result := Result + AnsiString(PN) + '="';
                    if Items[I].PRaw then
                        Result := Result + PV
                    else
                        Result := Result + IcsUrlEncode(PV, FRfcStrict);
                    Result := Result + '"';
                end;
            end;
        end;
    end
    else
        Result := '';
end;
{$WARN NO_RETVAL ON}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TRestParamsSrv }
{ V8.67 TRestParamsSrv extends TRestParams for database REST servers }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF DATABASE}
constructor TRestParamsSrv.Create;
begin
    inherited Create;
    FJsonRecord := TRestParams.Create(Nil);
    FJsonRecord.PContent := PContJson;
    FJsonResult := TRestParams.Create(Nil);
    FJsonResult.PContent := PContJson;
    FJsonArray := TIcsStringBuild.Create;
    FJsonArray.CharSize := 1;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TRestParamsSrv.Destroy;
begin
    FreeAndNil(FJsonRecord);
    FreeAndNil(FJsonRecord);
    FreeAndNil(FJsonArray);
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{
  "success": false,
  "reccount":0,
  "errno":x,
  "errdesc":"xxxxxx"
  }
function TRestParamsSrv.JsonErr(RestErr: TRestErr; const ErrDesc: String = ''): AnsiString;
begin
    FJsonResult.Clear;
    FJsonResult.AddItem('success', false);
    FJsonResult.AddItem('reccount', 0);
    FJsonResult.AddItem('errno', Ord(RestErr));
    if ErrDesc <> '' then
        FJsonResult.AddItem('errdesc', ErrDesc)
    else
        FJsonResult.AddItem('errdesc', RestErrLits[RestErr]);
    Result := FJsonResult.GetParameters;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// convert database resultset into Json array
{
  "success": true,
  "reccount":1,
  "records":[]
  }
function TRestParamsSrv.ResultSet2Json(DataSet: TDataSet; var JsonStr: AnsiString; MaxRecs: Integer = 0): Boolean;
var
    Recs, Flds, J: Integer;
    FName: String;
begin
    JsonStr := '';
    Result := False;
    Recs := 0;
    if NOT Assigned(DataSet) then begin
        JsonStr := JsonErr(RestErrInternal);
        Exit;
    end;
    try
        FJsonRecord.Clear;
        FJsonResult.Clear;
        FJsonArray.Clear;
        FJsonArray.CharSize := 1; // ansi buffer
        DataSet.First ;    // don't use RecCount, not always set
        FJsonArray.AppendBufA('[');
        while NOT DataSet.EOF do begin
            Recs := Recs + 1;
            FJsonRecord.Clear;
            Flds := DataSet.FieldCount ;
            for J := 0 to Flds - 1 do begin
                FName := DataSet.FieldDefs[J].DisplayName;
                if DataSet.Fields.Fields[J].IsNull then
                    FJsonRecord.AddItemNULL(FName)
                else if DataSet.Fields.Fields[J].DataType = ftBoolean then
                    FJsonRecord.AddItem(FName, DataSet.Fields.Fields[J].AsBoolean)
                else if DataSet.Fields.Fields[J].DataType in [ftInteger, ftSmallint, ftWord, ftLargeint] then
                    FJsonRecord.AddItem(FName, DataSet.Fields.Fields[J].AsInteger)
                else
                    FJsonRecord.AddItem(FName, DataSet.Fields.Fields[J].AsString);
            end;
            if Recs > 1 then
                FJsonArray.AppendBufA(',');
            FJsonArray.AppendBufA(FJsonRecord.GetParameters);
            if (MaxRecs > 0) and (Recs > MaxRecs) then break;
            DataSet.Next ;
        end;
        FJsonArray.AppendBufA(']');
        if Recs <= 0 then begin
            JsonStr := JsonErr(RestErrNoRecords);
            Exit;
        end;
        FJsonResult.AddItem('success', true);
        FJsonResult.AddItem('reccount', Recs);
        FJsonResult.AddItem('records', String(FJsonArray.GetAString), True);
        JsonStr := FJsonResult.GetParameters;
        FJsonRecord.Clear;
        FJsonResult.Clear;
        FJsonArray.Clear;
        Result := True;
    except
       JsonStr := JsonErr(RestErrGeneral, 'ResultSet2Json - ' + IcsGetExceptMess (ExceptObject)) ;
    end;
end;

{$ENDIF}
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}


end.
