{$INCLUDE ..\cDefines.inc}
unit cInternetUtils;

{                                                                              }
{                           Internet Utilities 3.03                            }
{                                                                              }
{      This unit is copyright � 2000-2003 by David Butler (david@e.co.za)      }
{                                                                              }
{                  This unit is part of Delphi Fundamentals.                   }
{                Its original file name is cInternetUtils.pas                  }
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
{ Notes:                                                                       }
{   See RFC 2045 (MIME)                                                        }
{                                                                              }
{ Revision history:                                                            }
{   17/10/2000  1.01  Unit cInternetStandards.                                 }
{   22/12/2001  1.02  Unit cMIME.                                              }
{   12/12/2002  3.03  Created unit cInternetUtils.                             }
{                     Revised for Fundamentals 3.                              }
{                                                                              }

interface

uses
  { Delphi }
  SysUtils,

  { Fundamentals }
  cUtils,
  cStrings,
  cReaders,
  cUnicode;



{                                                                              }
{ Constants                                                                    }
{                                                                              }
const
  SPACE = csAsciiCtl + [asciiSP];



{                                                                              }
{ MIME Content Types                                                           }
{                                                                              }
const
  ctHTML         = 'text/html';
  ctText         = 'text/plain';
  ctXML          = 'text/xml';
  ctJPG          = 'image/jpeg';
  ctGIF          = 'image/gif';
  ctBMP          = 'image/bmp';
  ctPNG          = 'image/png';
  ctTIFF         = 'image/tiff';
  ctMPG          = 'video/mpeg';
  ctAVI          = 'video/avi';
  ctQT           = 'video/quicktime';
  ctBinary       = 'application/binary';
  ctPDF          = 'application/pdf';
  ctPostscript   = 'application/postscript';
  ctBasicAudio   = 'audio/basic';
  ctMP3          = 'audio/mpeg';
  ctRA           = 'audio/x-realaudio';
  ctURLEncoded   = 'application/x-www-form-urlencoded';
  ctZIP          = 'application/zip';
  ctJavaScript   = 'application/javascript';
  ctPascal       = 'text/x-source-pascal';
  ctCPP          = 'text/x-source-cpp';
  ctINI          = 'text/x-windows-ini';
  ctBAT          = 'text/x-windows-bat';

function  MIMEContentTypeFromExtention(const Extention: String): String;



{                                                                              }
{ URL                                                                          }
{                                                                              }
const
  protoHTTP   = 'http';
  protoNNTP   = 'news';
  protoFTP    = 'ftp';
  protoGopher = 'gopher';
  protoEMail  = 'mailto';
  protoHTTPS  = 'https';
  protoIRC    = 'irc';
  protoFile   = 'file';
  protoTelnet = 'telnet';

procedure DecodeURL(const URL: String; var Protocol, Host, Path: String);
function  EncodeURL(const Protocol, Host, Path: String): String;
procedure DecodeHost(const Address: String; var Host, Port: String;
          const DefaultPort: String = '');



{                                                                              }
{ HTML                                                                         }
{                                                                              }
function  htmlCharRef(const CharVal: LongWord; const UseHex: Boolean = False): String;
function  htmlSafeAsciiText(const S: String): String;
procedure htmlSafeWideText(var S: WideString);
function  htmlSafeQuotedText(const S: String): String;



{                                                                              }
{ Header field                                                                 }
{                                                                              }
function  EncodeHeaderField(const Name, Body: String): String;
function  DecodeHeaderField(const S: String; var Name, Body: String): Boolean;



{                                                                              }
{ E-mail address                                                               }
{                                                                              }
procedure DecodeEMailAddress(const S: String; var User, Domain: String);
procedure DecodeEMailField(const S: String; var EMailAddress, Name: String);



{                                                                              }
{ Date                                                                         }
{                                                                              }
function  DateFieldBody: String;
function  DateField: String;



{                                                                              }
{ Other header fields                                                          }
{                                                                              }
function  MessageIDFieldBody(const ID: String = ''; const Host: String = ''): String;



{                                                                              }
{ THeader class                                                                }
{                                                                              }
type
  { AHeaderField                                                               }
  AHeaderField = class
  protected
    function  GetName: String; virtual; abstract;
    procedure SetName(const Name: String); virtual; abstract;
    function  GetBody: String; virtual; abstract;
    procedure SetBody(const Body: String); virtual; abstract;
    function  GetBodyAsInteger: Int64; virtual;
    procedure SetBodyAsInteger(const Value: Int64); virtual;
    function  GetBodyAsFloat: Extended; virtual;
    procedure SetBodyAsFloat(const Value: Extended); virtual;
    function  GetAsString: String; virtual;
    procedure SetAsString(const Value: String); virtual;

  public
    constructor Create(const Body: String); reintroduce; overload;
    constructor Create(const Name, Body: String); reintroduce; overload;

    function  Duplicate: AHeaderField; virtual;
    procedure Prepare; virtual;

    property  Name: String read GetName write SetName;
    property  Body: String read GetBody write SetBody;
    property  BodyAsInteger: Int64 read GetBodyAsInteger write SetBodyAsInteger;
    property  BodyAsFloat: Extended read GetBodyAsFloat write SetBodyAsFloat;
    property  AsString: String read GetAsString write SetAsString;
  end;
  AHeaderFieldClass = class of AHeaderField;
  AHeaderFieldArray = Array of AHeaderField;

  { THeader                                                                    }
  THeader = class
  protected
    FFields : AHeaderFieldArray;

    function  GetFieldCount: Integer;
    function  GetFieldByIndex(const Idx: Integer): AHeaderField;
    procedure SetFieldByIndex(const Idx: Integer; const Field: AHeaderField);
    function  GetField(const Name: String): AHeaderField;
    procedure SetField(const Name: String; const Field: AHeaderField);
    function  GetFieldBody(const Name: String): String;
    procedure SetFieldBody(const Name: String; const Body: String);
    function  GetAsString: String; virtual;
    procedure SetAsString(const Header: String); virtual;

  public
    constructor Create(const Header: String); reintroduce; overload;
    constructor Create(const HeaderReader: AReaderEx;
                const Tolerant: Boolean = True;
                const MaxHeaderSize: Integer = -1); reintroduce; overload;
    destructor Destroy; override;

    function  Duplicate: THeader; virtual;

    procedure ReadFromStream(const HeaderReader: AReaderEx;
              const Tolerant: Boolean = True;
              const MaxHeaderSize: Integer = -1); virtual;
    procedure Prepare; virtual;
    property  AsString: String read GetAsString write SetAsString;

    property  FieldCount: Integer read GetFieldCount;
    property  FieldByIndex[const Idx: Integer]: AHeaderField
              read GetFieldByIndex write SetFieldByIndex;
    function  GetFieldIndex(const Name: String): Integer;
    function  HasField(const Name: String): Boolean;
    property  Field[const Name: String]: AHeaderField
              read GetField write SetField; default;
    property  FieldBody[const Name: String]: String
              read GetFieldBody write SetFieldBody;
    function  GetFieldNames: StringArray; virtual;

    procedure Clear; virtual;
    procedure DeleteFieldByIndex(const Idx: Integer);
    function  DeleteField(const Name: String): Boolean;

    procedure AddField(const Field: AHeaderField); overload;
    procedure AddField(const Name, Body: String); overload; virtual;
    procedure AddField(const FieldLine: String); overload; virtual;
  end;
  THeaderClass = class of THeader;



{                                                                              }
{ AHeaderField implementations                                                 }
{                                                                              }
type
  { THeaderField                                                               }
  THeaderField = class(AHeaderField)
  protected
    FName : String;
    FBody : String;

    function  GetName: String; override;
    procedure SetName(const Name: String); override;
    function  GetBody: String; override;
    procedure SetBody(const Body: String); override;

  public
    function  Duplicate: AHeaderField; override;
  end;

  { TInvalidField                                                              }
  TInvalidField = class(AHeaderField)
  protected
    FRawLine : String;

    function  GetName: String; override;
    procedure SetName(const Name: String); override;
    function  GetBody: String; override;
    procedure SetBody(const Body: String); override;
    function  GetAsString: String; override;
    procedure SetAsString(const Value: String); override;

  public
    constructor Create(const RawLine: String);

    function  Duplicate: AHeaderField; override;
  end;

  { TDateField                                                                 }
  TDateField = class(AHeaderField)
  protected
    FGMTDateTime : TDateTime;

    function  GetName: String; override;
    procedure SetName(const Name: String); override;
    function  GetBody: String; override;
    procedure SetBody(const Body: String); override;

  public
    constructor CreateNow;
    constructor Create(const LocalTime: TDateTime); reintroduce; overload;

    function  Duplicate: AHeaderField; override;

    property  GMTDateTime: TDateTime read FGMTDateTime write FGMTDateTime;
  end;

  { TEMailField                                                                }
  TEMailField = class(THeaderField)
  protected
    FAddress  : String;
    FName     : String;

    function  GetBody: String; override;
    procedure SetBody(const Body: String); override;

  public
    property  Address: String read FAddress;
    property  Name: String read FName;
  end;



{                                                                              }
{ Self-testing code                                                            }
{                                                                              }
procedure SelfTest;



implementation

uses
  { Delphi }
  Windows,

  { Fundamentals }
  cFileUtils,
  cRandom,
  cDateTime,
  cWindows;



{                                                                              }
{ MIME Content Type                                                            }
{                                                                              }
type
  TContentTypeDef = record
    Ext : String;
    CT  : String;
  end;

const
  PredefinedContentTypes = 28;
  PredefinedContentType: Array[1..PredefinedContentTypes] of TContentTypeDef = (
      (Ext: '.htm';  CT: ctHTML),         (Ext: '.html'; CT: ctHTML),
      (Ext: '.jpg';  CT: ctJPG),          (Ext: '.jpeg'; CT: ctJPG),
      (Ext: '.gif';  CT: ctGIF),          (Ext: '.png';  CT: ctPNG),
      (Ext: '.bmp';  CT: ctBMP),          (Ext: '.txt';  CT: ctText),
      (Ext: '.mpeg'; CT: ctMPG),          (Ext: '.mpg';  CT: ctMPG),
      (Ext: '.pdf';  CT: ctPDF),          (Ext: '.exe';  CT: ctBinary),
      (Ext: '.ps';   CT: ctPostScript),   (Ext: '.avi';  CT: ctAVI),
      (Ext: '.qt';   CT: ctQT),           (Ext: '.mov';  CT: ctQT),
      (Ext: '.au';   CT: ctBasicAudio),   (Ext: '.wav';  CT: ctBasicAudio),
      (Ext: '.mp3';  CT: ctMP3),          (Ext: '.mp2';  CT: ctMP3),
      (Ext: '.pdf';  CT: ctPDF),          (Ext: '.ra';   CT: ctRA),
      (Ext: '.tif';  CT: ctTIFF),         (Ext: '.tiff'; CT: ctTIFF),
      (Ext: '.zip';  CT: ctZIP),          (Ext: '.ini';  CT: ctINI),
      (Ext: '.bat';  CT: ctBAT),          (Ext: '.js';   CT: ctJavaScript));

function MIMEContentTypeFromExtention(const Extention: String): String;
var I : Integer;
begin
  // pre-defined types
  For I := 1 to PredefinedContentTypes do
    if StrEqualNoCase(Extention, PredefinedContentType[I].Ext) then
      begin
        Result := PredefinedContentType[I].CT;
        exit;
      end;
  // check OS
  Result := ContentTypeFromExtention(Extention);
end;



{                                                                              }
{ URL                                                                          }
{                                                                              }
function urlDecodeHTTP(const S: String; var Protocol, Host, Path: String): Boolean;
var I, J: Integer;
begin
  Protocol := '';
  Host := '';
  Path := '';
  if StrMatchLeft('http:', S, False) then
    Protocol := protoHTTP else
  if StrMatchLeft('https:', S, False) then
    Protocol := protoHTTPS;
  Result := Protocol <> '';
  if not Result then
    exit;
  I := PosChar(':', S);
  Assert(I > 0, 'I > 0');
  if StrMatch('//', S, I + 1) then
    Inc(I, 2);
  J := PosChar('/', S, I + 1);
  if J = 0 then
    Host := CopyFrom(S, I + 1) else
    begin
      Host := CopyRange(S, I + 1, J - 1);
      Path := CopyFrom(S, J);
    end;
end;

function urlDecodeEMail(const S: String; var Protocol, Host, Path: String): Boolean;
begin
  Protocol := '';
  Host := '';
  Path := '';
  if StrMatchLeft('mailto:', S, False) then
    begin
      Protocol := protoEMail;
      Host := CopyFrom(S, 8);
    end else
  if (PosChar([':', '/', '\'], S) = 0) and
     (PosChar('@', S) > 1) then
    begin
      Protocol := protoEMail;
      Host := S;
    end;
  Result := Protocol <> '';
  if not Result then
    exit;
  TrimInPlace(Host, SPACE);
end;

function urlDecodeFile(const S: String; var Protocol, Host, Path: String): Boolean;
begin
  Protocol := '';
  Host := '';
  Path := '';
  if S <> '' then
    if StrMatchLeft('file:', S, False) then
      begin
        Protocol := protoFile;
        Path := CopyFrom(S, 6);
      end else
    if (PChar(S)^ = '\') or
       (PathHasDriveLetter(S) and StrMatch('\', S, 3))  then
      begin
        Protocol := protoFile;
        Path := S;
      end;
  Result := Protocol <> '';
end;

function urlDecodeKnownProtocol(const S: String; var Protocol, Host, Path: String): Boolean;
begin
  Result := urlDecodeHTTP(S, Protocol, Host, Path);
  if Result then
    exit;
  Result := urlDecodeEMail(S, Protocol, Host, Path);
  if Result then
    exit;
  Result := urlDecodeFile(S, Protocol, Host, Path);
  if Result then
    exit;
end;

function urlDecodePath(const S: String; var Protocol, Host, Path: String): Boolean;
var I: Integer;
begin
  Protocol := '';
  Host := '';
  Path := '';
  Result := False;
  // special cases
  if (S = '') or (S = '*') or (S = '/') then
    begin
      Path := S;
      Result := True;
    end else
  // relative path
  if StrMatchLeft('../', S) or StrMatchLeft('./', S) then
    begin
      Path := S;
      Result := True;
    end else
  // "/" prefix
  if PChar(S)^ = '/' then
    begin
      if StrMatchLeft('//', S) then
        begin
          // "//"host["/"path]
          I := PosChar('/', S, 3);
          if I = 0 then
            // "//"host
            Host := CopyFrom(S, 3) else
            begin
              // "//"host"/"path
              Host := CopyRange(S, 3, I - 1);
              Path := CopyFrom(S, I);
            end;
        end else
        // "/"path
        Path := S;
      Result := True;
    end;
end;

procedure urlDecodeGeneral(const S: String; var Protocol, Host, Path: String);
var I, J : Integer;
    T    : String;
begin
  Protocol := '';
  Host := '';
  Path := '';
  I := PosStr('://', S);
  J := PosChar('/', S);
  if (I > 0) and (J = I + 1) then
    begin
      // protocol"://"
      Protocol := Trim(CopyLeft(S, I - 1), SPACE);
      J := PosChar('/', S, I + 3);
      if J = 0 then
        begin
          Host := Trim(CopyFrom(S, I + 3), SPACE);
          Path := '';
        end else
        begin
          Host := Trim(CopyRange(S, I + 3, J - 1), SPACE);
          Path := Trim(CopyFrom(S, J), SPACE);
        end;
      exit;
    end;
  I := PosChar(':', S);
  if (I = 0) or ((I > 0) and (J > 0) and (J < I)) then
    begin
      // no protocol
      Path := S;
      exit;
    end;
  // check text between ":" and "/"
  if J > 0 then
    T := CopyRange(S, I + 1, J - 1) else
    T := CopyFrom(S, I + 1);
  if StrIsNumeric(T) then
    begin
      // address":"port/path
      if J = 0 then
        Host := S else
        begin
          Host := CopyLeft(S, J - 1);
          Path := CopyFrom(S, J);
        end;
      exit;
    end;
  // protocol":"host"/"path
  Protocol := Trim(CopyLeft(S, I - 1), SPACE);
  if J = 0 then
    Host := CopyFrom(S, I + 1) else
    begin
      Host := CopyRange(S, I + 1, J - 1);
      Path := CopyFrom(S, J);
    end;
end;

procedure DecodeURL(const URL: String; var Protocol, Host, Path: String);
const KnownProtocols = 3;
      KnownProtocol: Array [1..KnownProtocols] of String = (protoEMail,
                    protoNNTP, protoFile);
var S : String;
begin
  Protocol := '';
  Host := '';
  Path := '';
  // clean URL
  S := Trim(URL, SPACE);
  if S = '' then
    exit;
  // check if url is a path only
  if urlDecodePath(S, Protocol, Host, Path) then
    exit;
  // check known protocol
  if urlDecodeKnownProtocol(S, Protocol, Host, Path) then
    exit;
  // check general format
  urlDecodeGeneral(S, Protocol, Host, Path);
end;

procedure DecodeHost(const Address: String; var Host, Port: String; const DefaultPort: String);
begin
  if not StrSplitAtChar(Address, ':', Host, Port) then
    begin
      Host := Address;
      Port := '';
    end;
  TrimInPlace(Host, SPACE);
  TrimInPlace(Port, SPACE);
  if Port = '' then
    Port := DefaultPort;
end;

function EncodeURL(const Protocol, Host, Path: String): String;
begin
  Result := '';
  if Protocol <> '' then
    if StrEqualNoCase(protoHTTP, Protocol) or
       StrEqualNoCase(protoHTTPS, Protocol) then
      Result := Protocol + '://' else
      Result := Protocol + ':';
  Result := Result + Host;
  if Path <> '' then
    if not (Path [1] in [':', '/', '\', '@', ',']) then
      Result := Result + '/' + Path else
      Result := Result + Path;
end;



{                                                                              }
{ HTML                                                                         }
{                                                                              }
function htmlCharRef(const CharVal: LongWord; const UseHex: Boolean): String;
begin
  if UseHex then
    if CharVal <= $FF then
      Result := '#x' + LongWordToHex(CharVal, 2) + ';' else
    if CharVal <= $FFFF then
      Result := '#x' + LongWordToHex(CharVal, 4) + ';' else
      Result := '#x' + LongWordToHex(CharVal, 6) + ';'
  else
    Result := '#' + LongWordToStr(CharVal) + ';';
end;

function htmlSafeAsciiText(const S: String): String;
begin
  Result := S;
  // Check for unsafe characters
  if PosChar(['&', '<', '>', '"', #39], S) = 0 then
    exit;
  // Replace unsafe characters with entity references
  Result := StrReplace('&', '&amp;',  Result);
  Result := StrReplace('<', '&lt;',   Result);
  Result := StrReplace('>', '&gt;',   Result);
  Result := StrReplace('"', '&quot;', Result);
  Result := StrReplace(#39, '&apos;', Result);
end;

procedure htmlSafeWideText(var S: WideString);
begin
  WideReplaceChar('&', '&amp;',  S);
  WideReplaceChar('<', '&lt;',   S);
  WideReplaceChar('>', '&gt;',   S);
  WideReplaceChar('"', '&quot;', S);
  WideReplaceChar(#39, '&apos;', S);
end;

function htmlSafeQuotedText(const S: String): String;
begin
  Result := '"' + htmlSafeAsciiText(S) + '"';
end;



{                                                                              }
{ Header fields                                                                }
{                                                                              }
function EncodeHeaderField(const Name, Body: String): String;
begin
  Result := Trim(Name, SPACE) + ': ' + Trim(Body, SPACE);
end;

function DecodeHeaderField(const S: String;
    var Name, Body: String): Boolean;
begin
  Result := StrSplitAtChar(S, ':', Name, Body);
  if not Result then
    exit;
  TrimInPlace(Name, SPACE);
  TrimInPlace(Body, SPACE);
end;



{                                                                              }
{ E-mail address                                                               }
{                                                                              }
procedure DecodeEMailAddress(const S: String; var User, Domain: String);
begin
  if not StrSplitAtChar(S, '@', User, Domain) then
    begin
      User := S;
      Domain := '';
    end;
  TrimInPlace(User, SPACE);
  TrimInPlace(Domain, SPACE);
end;

{ Recognised e-mail formats:                                                   }
{   fundamentals@eternallines.com (Fundamentals Project)                       }
{   Fundamentals Project <fundamentals@eternallines.com>                       }
{   <fundamentals@eternallines.com> Fundamentals Project                       }
{   "Fundamentals Project" fundamentals@eternallines.com                       }
{   fundamentals@eternallines.com "Fundamentals Project"                       }
procedure DecodeEMailField(const S: String; var EMailAddress, Name: String);
var T, U : String;
    I    : Integer;
begin
  EMailAddress := '';
  Name := '';
  T := Trim(S, SPACE);
  if T = '' then
    exit;
  EMailAddress := Trim(StrRemoveCharDelimited(T, '<', '>'), SPACE);
  Name := Trim(StrRemoveCharDelimited(T, '"', '"'), SPACE);
  U := Trim(StrRemoveCharDelimited(T, '(', ')'), SPACE);
  if (U <> '') and (Name = '') then
    Name := U;
  TrimInPlace(T, SPACE);
  I := PosChar([';', ':', ',', '"', '(', '<'], T);
  if I > 0 then
    begin
      SetLength(T, I - 1);
      TrimInPlace(T, SPACE);
    end;
  if T <> '' then
    if EMailAddress = '' then
      EMailAddress := T else
    if Name = '' then
      Name := T;
end;



{                                                                              }
{ Date                                                                         }
{                                                                              }
function DateFieldBody: String;
begin
  Result := NowAsRFCDateTime;
end;

function DateField: String;
begin
  Result := EncodeHeaderField('Date', DateFieldBody);
end;



{                                                                              }
{ Miscellaneous                                                                }
{                                                                              }
function MessageIDFieldBody(const ID: String; const Host: String): String;
var S, T: String;
begin
  if ID = '' then
    S := RandomHex(24) + LongWordToHex(GetTickCount, 8) else
    S := ID;
  ConvertLower(S);
  if Host = '' then
    T := RandomPseudoWord(12) else
    T := Host;
  ConvertLower(T);
  Result := '<' + S + '@' + T + '>';
end;



{                                                                              }
{ AHeaderField                                                                 }
{                                                                              }
constructor AHeaderField.Create(const Body: String);
begin
  inherited Create;
  SetBody(Body);
end;

constructor AHeaderField.Create(const Name, Body: String);
begin
  inherited Create;
  SetName(Name);
  SetBody(Body);
end;

function AHeaderField.Duplicate: AHeaderField;
begin
  Result := AHeaderFieldClass(ClassType).Create(GetName, GetBody);
end;

procedure AHeaderField.Prepare;
begin
end;

function AHeaderField.GetBodyAsInteger: Int64;
begin
  Result := StrToInt64Def(GetBody, -1);
end;

procedure AHeaderField.SetBodyAsInteger(const Value: Int64);
begin
  SetBody(IntToStr(Value));
end;

function AHeaderField.GetBodyAsFloat: Extended;
begin
  Result := StrToFloatDef(GetBody, -1.0);
end;

procedure AHeaderField.SetBodyAsFloat(const Value: Extended);
begin
  SetBody(FloatToStr(Value));
end;

function AHeaderField.GetAsString: String;
begin
  Result := EncodeHeaderField(GetName, GetBody);
end;

procedure AHeaderField.SetAsString(const Value: String);
var N, B: String;
begin
  DecodeHeaderField(Value, N, B);
  SetName(N);
  SetBody(B);
end;



{                                                                              }
{ THeader                                                                      }
{                                                                              }
constructor THeader.Create(const Header: String);
begin
  inherited Create;
  SetAsString(Header);
end;

constructor THeader.Create(const HeaderReader: AReaderEx;
    const Tolerant: Boolean; const MaxHeaderSize: Integer);
begin
  inherited Create;
  ReadFromStream(HeaderReader, Tolerant, MaxHeaderSize);
end;

procedure THeader.ReadFromStream(const HeaderReader: AReaderEx;
    const Tolerant: Boolean; const MaxHeaderSize: Integer);
var I : Integer;
    S : String;
begin
  I := HeaderReader.LocateStr(CRLF, MaxHeaderSize);
  if I = 0 then
    begin
      HeaderReader.Skip(2);
      SetAsString('');
      exit;
    end;
  if Tolerant then
    Repeat
      S := HeaderReader.ExtractLine(MaxHeaderSize);
      if S <> '' then
        AddField(S);
    Until S = ''
  else
    begin
      I := HeaderReader.LocateStr(CRLF + CRLF, MaxHeaderSize);
      if I = -1 then
        exit;
      SetAsString(HeaderReader.ReadStr(I));
      HeaderReader.Skip(4);
    end;
end;

function THeader.Duplicate: THeader;
var I : Integer;
begin
  Result := THeaderClass(ClassType).Create;
  For I := 0 to Length(FFields) - 1 do
    Result.AddField(FFields[I].Duplicate);
end;

function THeader.GetFieldNames: StringArray;
var I, L : Integer;
begin
  L := Length(FFields);
  SetLength(Result, L);
  For I := 0 to L - 1 do
    Result[I] := FFields[I].Name;
end;

function THeader.GetFieldIndex(const Name: String): Integer;
var I : Integer;
    S : String;
begin
  S := Trim(Name, SPACE);
  For I := 0 to Length(FFields) - 1 do
    if StrEqualNoCase(S, FFields[I].Name) then
      begin
        Result := I;
        exit;
      end;
  Result := -1;
end;

function THeader.DeleteField(const Name: String): Boolean;
var I : Integer;
begin
  Result := False;
  Repeat
    I := GetFieldIndex(Name);
    if I < 0 then
      break;
    Result := True;
    DeleteFieldByIndex(I);
  Until False;
end;

procedure THeader.Clear;
var L : Integer;
Begin
  Repeat
    L := Length(FFields);
    if L = 0 then
      break;
    DeleteFieldByIndex(L - 1);
  Until False;
End;

function THeader.GetField(const Name: String): AHeaderField;
var I : Integer;
begin
  I := GetFieldIndex(Name);
  if I = -1 then
    Result := nil else
    Result := GetFieldByIndex(I);
end;

procedure THeader.SetField(const Name: String; const Field: AHeaderField);
var I : Integer;
begin
  if Assigned(Field) and not StrEqualNoCase(Field.Name, Name) then
    Field.Name := Name;
  I := GetFieldIndex(Name);
  if I = -1 then
    begin
      if Assigned(Field) then
        AddField(Field);
    end else
    if not Assigned(Field) then
      DeleteFieldByIndex(I) else
      FieldByIndex[I] := Field;
end;

function THeader.GetFieldBody(const Name: String): String;
var F : AHeaderField;
begin
  F := GetField(Name);
  if not Assigned(F) then
    Result := '' else
    Result := F.Body;
end;

procedure THeader.SetFieldBody(const Name: String; const Body: String);
var F : AHeaderField;
begin
  F := GetField(Name);
  if not Assigned(F) then
    AddField(Name, Body) else
    F.Body := Body;
end;

function THeader.HasField(const Name: String): Boolean;
begin
  Result := GetFieldIndex(Name) >= 0;
end;

procedure THeader.AddField(const FieldLine: String);
var N, B: String;
begin
  if DecodeHeaderField(FieldLine, N, B) then
    AddField(N, B) else
    AddField(TInvalidField.Create(FieldLine));
end;

function THeader.GetAsString: String;
var I : Integer;
begin
  Result := '';
  For I := 0 to FieldCount - 1 do
    Result := Result + FieldByIndex[I].GetAsString + CRLF;
  Result := Result + CRLF;
end;

procedure THeader.SetAsString(const Header: String);
var S     : StringArray;
    I, J  : Integer;
    Name  : String;
    Body  : String;
begin
  S := StrSplit(Header, CRLF);
  Name := '';
  For I := 0 to Length(S) - 1 do
    begin
      J := Pos(':', S[I]);
      if J > 0 then // header-field
        begin
          if Name <> '' then
            AddField(Name, Body);
          Name := Trim(CopyLeft(S[I], J - 1), SPACE);
          Body := Trim(CopyFrom(S[I], J + 1), SPACE);
        end else
        if (Name <> '') and (S[I] <> '') and (S[I][1] in SPACE) then // header-line continuation
          Body := Body + S[I] else
          begin
            if Name <> '' then
              AddField(Name, Body);
            Name := '';
            if S[I] <> '' then
              AddField(TInvalidField.Create(S[I]));
          end;
    end;
  if Name <> '' then
    AddField(Name, Body);
end;

procedure THeader.Prepare;
var I : Integer;
begin
  For I := 0 to Length(FFields) - 1 do
    FFields[I].Prepare;
end;

destructor THeader.Destroy;
var I : Integer;
begin
  For I := Length(FFields) - 1 downto 0 do
    FreeAndNil(FFields[I]);
  FFields := nil;
  inherited Destroy;
end;

procedure THeader.AddField(const Field: AHeaderField);
begin
  Append(ObjectArray(FFields), Field);
end;

procedure THeader.AddField(const Name, Body: String);
begin
  AddField(THeaderField.Create(Name, Body));
end;

function THeader.GetFieldCount: Integer;
begin
  Result := Length(FFields);
end;

function THeader.GetFieldByIndex(const Idx: Integer): AHeaderField;
begin
  Result := FFields[Idx];
end;

procedure THeader.SetFieldByIndex(const Idx: Integer; const Field: AHeaderField);
begin
  FreeAndNil(FFields[Idx]);
  FFields[Idx] := Field;
end;

procedure THeader.DeleteFieldByIndex(const Idx: Integer);
begin
  Remove(ObjectArray(FFields), Idx, 1, True);
end;



{                                                                              }
{ THeaderField                                                                 }
{                                                                              }
function THeaderField.GetName: String;
begin
  Result := FName;
end;

procedure THeaderField.SetName(const Name: String);
begin
  FName := Name;
end;

function THeaderField.GetBody: String;
begin
  Result := FBody;
end;

procedure THeaderField.SetBody(const Body: String);
begin
  FBody := Body;
end;

function THeaderField.Duplicate: AHeaderField;
begin
  Result := THeaderField.Create(FName, FBody);
end;



{                                                                              }
{ TInvalidField                                                                }
{                                                                              }
constructor TInvalidField.Create(const RawLine: String);
begin
  inherited Create;
  FRawLine := RawLine;
end;

function TInvalidField.Duplicate: AHeaderField;
begin
  Result := TInvalidField.Create(FRawLine);
end;

function TInvalidField.GetName: String;
begin
  Result := '';
end;

function TInvalidField.GetBody: String;
begin
  Result := '';
end;

procedure TInvalidField.SetBody(const Body: String);
begin
end;

procedure TInvalidField.SetName(const Name: String);
begin
end;

function TInvalidField.GetAsString: String;
begin
  Result := FRawLine;
end;

procedure TInvalidField.SetAsString(const Value: String);
begin
  FRawLine := Value;
end;



{                                                                              }
{ TDateField                                                                   }
{                                                                              }
constructor TDateField.CreateNow;
begin
  inherited Create;
  GMTDateTime := LocalTimeToGMTTime(Now);
end;

constructor TDateField.Create(const LocalTime: TDateTime);
begin
  inherited Create;
  GMTDateTime := LocalTimeToGMTTime(LocalTime);
end;

function TDateField.Duplicate: AHeaderField;
begin
  Result := TDateField.Create;
  TDateField(Result).FGMTDateTime := FGMTDateTime;
end;

function TDateField.GetBody: String;
begin
  Result := GMTDateTimeToRFC1123DateTime(GMTDateTime);
end;

procedure TDateField.SetBody(const Body: String);
begin
  GMTDateTime := RFCDateTimeToGMTDateTime(Body);
end;

procedure TDateField.SetName(const Name: String);
begin
end;

function TDateField.GetName: String;
begin
  Result := 'Date';
end;



{                                                                              }
{ TEMailField                                                                  }
{                                                                              }
function TEMailField.GetBody: String;
begin
  if FName <> '' then
    Result := '<' + Address + '> (' + FName + ')' else
    Result := Address;
end;

procedure TEMailField.SetBody(const Body: String);
begin
  DecodeEMailField(Body, FAddress, FName);
end;



{                                                                              }
{ Self-testing code                                                            }
{                                                                              }
{$ASSERTIONS ON}
procedure SelfTest;
var A, B    : String;
    P, M, U : String;
begin
  { EncodeHeaderField                                                          }
  Assert(EncodeHeaderField('Xyz', 'Abcd') = 'Xyz: Abcd', 'EncodeHeaderField');
  DecodeHeaderField('Xyz: Abcd', A, B);
  Assert((A = 'Xyz') and (B = 'Abcd'), 'DecodeHeaderField');
  DecodeHeaderField(' X : A ', A, B);
  Assert((A = 'X') and (B = 'A'), 'DecodeHeaderField');

  { DecodeURL                                                                  }
  DecodeURL('http://abc.com/index.html', P, M, U);
  Assert((P = protoHTTP) and (M = 'abc.com') and (U = '/index.html'), 'DecodeURL');
  DecodeURL('a://b.c/1/2/3', P, M, U);
  Assert((P = 'a') and (M = 'b.c') and (U = '/1/2/3'), 'DecodeURL');
  DecodeURL('http://b:80/i.html', P, M, U);
  Assert((P = protoHTTP) and (M = 'b:80') and (U = '/i.html'), 'DecodeURL');
  DecodeURL('mailto:a@b', P, M, U);
  Assert((P = protoEMail) and (M = 'a@b') and (U = ''), 'DecodeURL');

  { EncodeURL                                                                  }
  Assert(EncodeURL('http', 'abc.com', '/') = 'http://abc.com/', 'EncodeURL');
  Assert(EncodeURL('news', 'a.b', '') = 'news:a.b', 'EncodeURL');
  Assert(EncodeURL('https', 'abc.com', '/') = 'https://abc.com/', 'EncodeURL');

  { DecodeEMailField                                                           }
  DecodeEMailField('a@b.com', A, B);
  Assert((A = 'a@b.com') and (B = ''), 'DecodeEmailField');
  DecodeEMailField('a@b.c "D"', A, B);
  Assert((A = 'a@b.c') and (B = 'D'), 'DecodeEmailField');
  DecodeEMailField('<a@b.c> Koos', A, B);
  Assert((A = 'a@b.c') and (B = 'Koos'), 'DecodeEmailField');
  DecodeEMailField('Koos <a>', A, B);
  Assert((A = 'a') and (B = 'Koos'), 'DecodeEmailField');
end;



end.

