{$INCLUDE ..\cDefines.inc}
unit cURL;

{                                                                              }
{                              URL Utilities 3.07                              }
{                                                                              }
{             This unit is copyright © 2000-2004 by David J Butler             }
{                                                                              }
{                  This unit is part of Delphi Fundamentals.                   }
{                     Its original file name is cURL.pas                       }
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
{ Revision history:                                                            }
{   17/10/2000  1.01  Unit cInternetStandards.                                 }
{   22/12/2001  1.02  Unit cMIME.                                              }
{   12/12/2002  3.03  Unit cInternetUtils.                                     }
{   21/02/2004  3.04  Added URL protocol base class.                           }
{   22/02/2004  3.05  Added URL File Protocol implementation class.            }
{   05/03/2004  3.06  Unit cURL.                                               }
{   12/03/2004  3.07  Added asynchronous URL content functions.                }
{                                                                              }

interface

uses
  { Delphi }
  SysUtils,

  { Fundamentals }
  cReaders,
  cThreads;



{                                                                              }
{ URL protocol                                                                 }
{   URL protocol implementations must use AURLProtocol as their base class.    }
{   URL protocol implementations must call RegisterURLProtocol to register     }
{   the implementation object.                                                 }
{                                                                              }
type
  AURLProtocol = class
  public
    { URL }
    function  DecodeURL(const URL: String; var Protocol, Host, Path: String): Boolean; virtual;

    { Content }
    function  IsContentSupported(const Protocol, Host, Path: String): Boolean; virtual;
    function  GetContentReader(const Protocol, Host, Path: String;
              var ContentType: String): AReaderEx; virtual;
    function  GetContentString(const Protocol, Host, Path: String;
              var Content, ContentType: String): Boolean; virtual;
  end;
  EURLProtocol = class(Exception);

procedure RegisterURLProtocol(const Handler: AURLProtocol);



{                                                                              }
{ URL string                                                                   }
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



{                                                                              }
{ URL content (blocking functions)                                             }
{                                                                              }
function  GetURLProtocolContentReader(const Protocol, Host, Path: String;
          var ContentType: String): AReaderEx;
function  GetURLProtocolContentString(const Protocol, Host, Path: String;
          var Content, ContentType: String): Boolean;

function  GetURLContentReader(const URL: String;
          var ContentType: String): AReaderEx;
function  GetURLContentString(const URL: String;
          var Content, ContentType: String): Boolean;

function  RequireURLProtocolContentReader(const Protocol, Host, Path: String;
          var ContentType: String): AReaderEx;
function  RequireURLProtocolContentString(const Protocol, Host, Path: String;
          var ContentType: String): String;

function  RequireURLContentReader(const URL: String; var ContentType: String): AReaderEx;
function  RequireURLContentString(const URL: String; var ContentType: String): String;



{                                                                              }
{ URL content (asynchronous functions)                                         }
{   Call GetURLContentAsync to retrieve URL content asynchronously.            }
{   Caller must free the returned TURLContentAsync object.                     }
{                                                                              }
type
  TURLContentAsync = class;
  TURLContentNotifyEvent = procedure (const URLContent: TURLContentAsync;
      const Data: Pointer) of object;
  TURLContentProgressEvent = procedure (const URLContent: TURLContentAsync;
      const Data: Pointer; const Buffer; const Size: Integer;
      var Abort: Boolean) of object;
  TURLContentMode = (
      ucGetContentString,        // Return content in ContentString property
      ucSaveContentFile,         // Save content to ContentFileName
      ucNotifyContentBlocks);    // Call OnProgress with content blocks
  TURLContentAsync = class(TThreadEx)
  private
    FProtocol        : String;
    FHost            : String;
    FPath            : String;
    FContentMode     : TURLContentMode;
    FContentFileName : String;
    FData            : Pointer;
    FOnProgress      : TURLContentProgressEvent;
    FOnFinished      : TURLContentNotifyEvent;
    FFinished        : Boolean;
    FSuccess         : Boolean;
    FErrorMessage    : String;
    FContentSize     : Integer;
    FContentProgress : Integer;
    FContentType     : String;
    FContentString   : String;

  protected
    procedure TriggerProgress(const Buffer; const Size: Integer;
              var Abort: Boolean); virtual;
    procedure TriggerFinished; virtual;
    
    procedure Execute; override;

  public
    constructor Create(
                const Protocol, Host, Path: String;
                const ContentMode: TURLContentMode = ucGetContentString;
                const ContentFileName: String = '';
                const Data: Pointer = nil;
                const OnProgress: TURLContentProgressEvent = nil;
                const OnFinished: TURLContentNotifyEvent = nil);

    property  Protocol: String read FProtocol;
    property  Host: String read FHost;
    property  Path: String read FPath;
    property  ContentMode: TURLContentMode read FContentMode;
    property  ContentFileName: String read FContentFileName;
    property  Data: Pointer read FData;

    property  Finished: Boolean read FFinished;
    property  Success: Boolean read FSuccess;
    property  ErrorMessage: String read FErrorMessage;

    property  ContentSize: Integer read FContentSize;
    property  ContentProgress: Integer read FContentProgress;
    property  ContentType: String read FContentType;
    property  ContentString: String read FContentString;
  end;

function  GetURLProtocolContentAsync(
          const Protocol, Host, Path: String;
          const ContentMode: TURLContentMode = ucGetContentString;
          const ContentFileName: String = '';
          const Data: Pointer = nil;
          const OnProgress: TURLContentProgressEvent = nil;
          const OnFinished: TURLContentNotifyEvent = nil): TURLContentAsync;
function  GetURLContentAsync(
          const URL: String;
          const ContentMode: TURLContentMode = ucGetContentString;
          const ContentFileName: String = '';
          const Data: Pointer = nil;
          const OnProgress: TURLContentProgressEvent = nil;
          const OnFinished: TURLContentNotifyEvent = nil): TURLContentAsync;



{                                                                              }
{ URL file protocol                                                            }
{                                                                              }
type
  TURLFileProtocol = class(AURLProtocol)
  public
    function  IsContentSupported(const Protocol, Host, Path: String): Boolean; override;
    function  GetContentReader(const Protocol, Host, Path: String;
              var ContentType: String): AReaderEx; override;
    function  GetContentString(const Protocol, Host, Path: String;
              var Content, ContentType: String): Boolean; override;
  end;

procedure RegisterURLFileProtocol;



{                                                                              }
{ Self-testing code                                                            }
{                                                                              }
procedure SelfTest;



implementation

uses
  { Fundamentals }
  cUtils,
  cStrings,
  cWriters,
  cStreams,
  cFileUtils,
  cInternetUtils;



{                                                                              }
{ AURLProtocol                                                                 }
{                                                                              }
function AURLProtocol.DecodeURL(const URL: String; var Protocol, Host, Path: String): Boolean;
begin
  Protocol := '';
  Host := '';
  Path := '';
  Result := False;
end;

function AURLProtocol.IsContentSupported(const Protocol, Host, Path: String): Boolean;
begin
  Result := False;
end;

function AURLProtocol.GetContentReader(const Protocol, Host, Path: String;
    var ContentType: String): AReaderEx;
begin
  ContentType := '';
  Result := nil;
end;

function AURLProtocol.GetContentString(const Protocol, Host, Path: String;
    var Content, ContentType: String): Boolean;
begin
  Content := '';
  ContentType := '';
  Result := False;
end;



{                                                                              }
{ URL Protocol implementations                                                 }
{                                                                              }
var
  URLProtocols : Array of AURLProtocol = nil;

procedure RegisterURLProtocol(const Handler: AURLProtocol);
begin
  if not Assigned(Handler) then
    raise EURLProtocol.Create('URL protocol handler required');
  Append(ObjectArray(URLProtocols), Handler);
end;



{                                                                              }
{ URL string                                                                   }
{                                                                              }
function urlDecodeHTTP(const S: String; var Protocol, Host, Path: String): Boolean;
var I, J: Integer;
begin
  Protocol := '';
  Host := '';
  Path := '';
  if StrMatchLeft(S, 'http:', False) then
    Protocol := protoHTTP else
  if StrMatchLeft(S, 'https:', False) then
    Protocol := protoHTTPS;
  Result := Protocol <> '';
  if not Result then
    exit;
  I := PosChar(':', S);
  Assert(I > 0, 'I > 0');
  if StrMatch(S, '//', I + 1) then
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
  if StrMatchLeft(S, 'mailto:', False) then
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
    if StrMatchLeft(S, 'file:', False) then
      begin
        Protocol := protoFile;
        Path := CopyFrom(S, 6);
      end else
    if (PChar(S)^ = '\') or
       (PathHasDriveLetter(S) and StrMatch(S, '\', 3))  then
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
  if StrMatchLeft(S, '../') or StrMatchLeft(S, './') then
    begin
      Path := S;
      Result := True;
    end else
  // "/" prefix
  if PChar(S)^ = '/' then
    begin
      if StrMatchLeft(S, '//') then
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
    I : Integer;
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
  // check url protocol handlers
  For I := 0 to Length(URLProtocols) - 1 do
    if URLProtocols[I].DecodeURL(URL, Protocol, Host, Path) then
      exit;
  // check known protocol
  if urlDecodeKnownProtocol(S, Protocol, Host, Path) then
    exit;
  // check general format
  urlDecodeGeneral(S, Protocol, Host, Path);
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
{ URL content (blocking functions)                                             }
{                                                                              }
function GetURLProtocolContentReader(const Protocol, Host, Path: String;
    var ContentType: String): AReaderEx;
var I : Integer;
    P : AURLProtocol;
    S : String;
begin
  For I := 0 to Length(URLProtocols) - 1 do
    begin
      P := URLProtocols[I];
      if P.IsContentSupported(Protocol, Host, Path) then
        begin
          Result := P.GetContentReader(Protocol, Host, Path, ContentType);
          if Assigned(Result) then
            exit;
          if P.GetContentString(Protocol, Host, Path, S, ContentType) then
            begin
              Result := TStringReader.Create(S);
              exit;
            end;
        end;
    end;
  ContentType := '';
  Result := nil;
end;

function GetURLProtocolContentString(const Protocol, Host, Path: String;
    var Content, ContentType: String): Boolean;
var I : Integer;
    P : AURLProtocol;
    R : AReaderEx;
begin
  For I := 0 to Length(URLProtocols) - 1 do
    begin
      P := URLProtocols[I];
      if P.IsContentSupported(Protocol, Host, Path) then
        begin
          Result := P.GetContentString(Protocol, Host, Path, Content, ContentType);
          if Result then
            exit;
          R := P.GetContentReader(Protocol, Host, Path, ContentType);
          if Assigned(R) then
            begin
              try
                Content := R.GetToEOF;
              finally
                R.Free;
              end;
              Result := True;
              exit;
            end;
        end;
    end;
  Content := '';
  ContentType := '';
  Result := False;
end;

function GetURLContentReader(const URL: String; var ContentType: String): AReaderEx;
var Protocol, Host, Path : String;
begin
  if URL = '' then
    raise EURLProtocol.Create('URL required');
  DecodeURL(URL, Protocol, Host, Path);
  Result := GetURLProtocolContentReader(Protocol, Host, Path, ContentType);
end;

function GetURLContentString(const URL: String;
    var Content, ContentType: String): Boolean;
var Protocol, Host, Path : String;
begin
  if URL = '' then
    raise EURLProtocol.Create('URL required');
  DecodeURL(URL, Protocol, Host, Path);
  Result := GetURLProtocolContentString(Protocol, Host, Path, Content, ContentType);
end;

function RequireURLProtocolContentReader(const Protocol, Host, Path: String;
    var ContentType: String): AReaderEx;
begin
  Result := GetURLProtocolContentReader(Protocol, Host, Path, ContentType);
  if not Assigned(Result) then
    raise EURLProtocol.Create('URL not supported');
end;

function RequireURLProtocolContentString(const Protocol, Host, Path: String;
    var ContentType: String): String;
begin
  if not GetURLProtocolContentString(Protocol, Host, Path, Result, ContentType) then
    raise EURLProtocol.Create('URL not supported');
end;

function RequireURLContentReader(const URL: String; var ContentType: String): AReaderEx;
begin
  Result := GetURLContentReader(URL, ContentType);
  if not Assigned(Result) then
    raise EURLProtocol.Create('URL not supported');
end;

function RequireURLContentString(const URL: String; var ContentType: String): String;
begin
  if not GetURLContentString(URL, Result, ContentType) then
    raise EURLProtocol.Create('URL not supported');
end;



{                                                                              }
{ URL content (asynchronous functions)                                         }
{                                                                              }
const
  ProgressBlockSize = 4096;

constructor TURLContentAsync.Create(
    const Protocol, Host, Path: String;
    const ContentMode: TURLContentMode;
    const ContentFileName: String;
    const Data: Pointer;
    const OnProgress: TURLContentProgressEvent;
    const OnFinished: TURLContentNotifyEvent);
begin
  FProtocol := Protocol;
  FHost := Host;
  FPath := Path;
  FContentMode := ContentMode;
  FContentFileName := ContentFileName;
  FData := Data;
  FOnProgress := OnProgress;
  FOnFinished := OnFinished;
  FreeOnTerminate := False;
  inherited Create(False);
end;

procedure TURLContentAsync.TriggerProgress(const Buffer; const Size: Integer;
    var Abort: Boolean);
begin
  if Assigned(FOnProgress) then
    FOnProgress(self, FData, Buffer, Size, Abort);
end;

procedure TURLContentAsync.TriggerFinished;
begin
  if Assigned(FOnFinished) then
    FOnFinished(self, FData);
end;

procedure TURLContentAsync.Execute;
var Reader : AReaderEx;
    Writer : TFileWriter;
    Buf    : Array[0..ProgressBlockSize - 1] of Byte;
    I      : Integer;
    A      : Boolean;
begin
  FErrorMessage := '';
  try try
    if FContentMode = ucGetContentString then
      begin
        FContentString := RequireURLProtocolContentString(FProtocol, FHost, FPath, FContentType);
        FContentSize := Length(FContentString);
        FSuccess := True;
      end else
    if FContentMode in [ucNotifyContentBlocks, ucSaveContentFile] then
      begin
        Reader := RequireURLProtocolContentReader(FProtocol, FHost, FPath, FContentType);
        try
          FContentSize := Reader.Size;
          if FContentMode = ucSaveContentFile then
            begin
              if FContentFileName = '' then
                raise EURLProtocol.Create('Filename required');
              Writer := TFileWriter.Create(FContentFileName, fwomCreate)
            end
          else
            Writer := nil;
          try
            A := False;
            While not Reader.EOF and not Terminated do
              begin
                I := Reader.Read(Buf[0], ProgressBlockSize);
                if (I = 0) and not Reader.EOF then
                  raise EURLProtocol.Create('Read error');
                Inc(FContentProgress, I);
                if Terminated then
                  exit;
                TriggerProgress(Buf[0], I, A);
                if A then
                  raise EURLProtocol.Create('Aborted');
                if Assigned(Writer) then
                  Writer.WriteBuffer(Buf[0], I);
              end;
          finally
            Writer.Free;
          end;
        finally
          Reader.Free;
        end;
        FContentSize := FContentProgress;
        FSuccess := True;
      end;
  except
    on E : Exception do
      FErrorMessage := E.Message;
  end;
  finally
    FFinished := True;
    TriggerFinished;
  end;
end;

function GetURLProtocolContentAsync(const Protocol, Host, Path: String;
    const ContentMode: TURLContentMode;
    const ContentFileName: String;
    const Data: Pointer;
    const OnProgress: TURLContentProgressEvent;
    const OnFinished: TURLContentNotifyEvent): TURLContentAsync;
begin
  Result := TURLContentAsync.Create(Protocol, Host, Path,
      ContentMode, ContentFileName, Data, OnProgress, OnFinished);
end;

function GetURLContentAsync(
    const URL: String;
    const ContentMode: TURLContentMode;
    const ContentFileName: String;
    const Data: Pointer;
    const OnProgress: TURLContentProgressEvent;
    const OnFinished: TURLContentNotifyEvent): TURLContentAsync;
var Protocol, Host, Path : String;
begin
  DecodeURL(URL, Protocol, Host, Path);
  Result := GetURLProtocolContentAsync(Protocol, Host, Path,
      ContentMode, ContentFileName, Data, OnProgress, OnFinished);
end;



{                                                                              }
{ URL File Protocol                                                            }
{                                                                              }
function TURLFileProtocol.IsContentSupported(const Protocol, Host, Path: String): Boolean;
begin
  Result := StrEqualNoCase(Protocol, protoFile) and (Host = '') and (Path <> '');
end;

function TURLFileProtocol.GetContentReader(const Protocol, Host, Path: String;
    var ContentType: String): AReaderEx;
begin
  ContentType := MIMEContentTypeFromExtention(ExtractFileExt(Path));
  Result := TFileReader.Create(Path);
end;

function TURLFileProtocol.GetContentString(const Protocol, Host, Path: String;
    var Content, ContentType: String): Boolean;
begin
  Content := ReadFileToStr(Path);
  ContentType := MIMEContentTypeFromExtention(ExtractFileExt(Path));
  Result := True;
end;

var
  URLFileProtocol : AURLProtocol = nil;

procedure RegisterURLFileProtocol;
begin
  if Assigned(URLFileProtocol) then
    exit;
  URLFileProtocol := TURLFileProtocol.Create;
  RegisterURLProtocol(URLFileProtocol);
end;



{                                                                              }
{ Self-testing code                                                            }
{                                                                              }
{$ASSERTIONS ON}
procedure SelfTest;
var P, M, U : String;
begin
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
end;



initialization
finalization
  FreeAndNil(URLFileProtocol);
end.

