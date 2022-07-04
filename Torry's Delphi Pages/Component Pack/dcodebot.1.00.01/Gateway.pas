
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit Gateway;

interface

{$I STD.INC}

uses
  Classes, SysUtils, Windows;

const
  MAX_ENVIRONMENT = 30;

{ TEnvironment class }

type
  TEnvironmentStringArray = array[0..MAX_ENVIRONMENT - 1] of string;

  TEnvironment = class
  private
    FStringArray: TEnvironmentStringArray;
    function GetString(Index: Integer): string;
  public
    constructor Create;
    function GetDebugString: string;
    property ContentLength: string index 0 read GetString;
    property ContentType: string index 1 read GetString;
    property Cookie: string index 2 read GetString;
    property GatewayInterface: string index 3 read GetString;
    property HTTPAccept: string index 4 read GetString;
    property HTTPAcceptLanguage: string index 5 read GetString;
    property HTTPConnection: string index 6 read GetString;
    property HTTPContentLength: string index 7 read GetString;
    property HTTPContentType: string index 8 read GetString;
    property HTTPFrom: string index 9 read GetString;
    property HTTPHost: string index 10 read GetString;
    property HTTPReferer: string index 11 read GetString;
    property HTTPUAPixels: string index 12 read GetString;
    property HTTPUAColor: string index 13 read GetString;
    property HTTPUAOS: string index 14 read GetString;
    property HTTPUACPU: string index 15 read GetString;
    property HTTPUserAgent: string index 16 read GetString;
    property HTTPPragma: string index 17 read GetString;
    property PathTranslated: string index 18 read GetString;
    property QueryString: string index 19 read GetString;
    property RemoteAddr: string index 20 read GetString;
    property RemoteHost: string index 21 read GetString;
    property RemoteUsername: string index 22 read GetString;
    property RequestMethod: string index 23 read GetString;
    property ScriptName: string index 24 read GetString;
    property ServerName: string index 25 read GetString;
    property ServerPort: string index 26 read GetString;
    property ServerPortSecure: string index 27 read GetString;
    property ServerProtocol: string index 28 read GetString;
    property ServerSoftware: string index 29 read GetString;
  end;

{ TGatewayParams class }

  TGatewayParams = class(TStringList)
  private
    function GetValues(Key: string): string;
  public
    property Values[Key: string]: string read GetValues; default;
  end;

{ TCookie class }

  TCookie = class
  private
    FExecuted: Boolean;
    FActive: Boolean;
    FDomain: string;
    FExpires: TDateTime;
    FName: string;
    FPath: string;
    FValue: string;
    FCookieList: TStringList;
    procedure SetDomain(const Value: string);
    procedure SetExpires(const Value: TDateTime);
    procedure SetName(const Value: string);
    procedure SetPath(const Value: string);
    procedure SetValue(AValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute;
    procedure Delete;
    property Active: Boolean read FActive;
    property Domain: string read FDomain write SetDomain;
    property Expires: TDateTime read FExpires write SetExpires;
    property Executed: Boolean read FExecuted;
    property Name: string read FName write SetName;
    property Path: string read FPath write SetPath;
    property Value: string read FValue write SetValue;
    property Items: TStringList read FCookieList;
  end;

{ TGateway class }

  TCGIOption = (giColor, giSound);
  TCGIOptionSet = set of TCGIOption;

  TGateway = class
  private
    FCookie: TCookie;
    FEnvironment: TEnvironment;
    FParams: TGatewayParams;
    FWebPage: TStrings;
    FTitle: string;
    FColor: LongInt;
    FSound: string;
    FOptions: TCGIOptionSet;
    procedure SetWebPage(Value: TStrings);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Append(const FileName: string);
    procedure Redirect(const Location: string);
    procedure Return(Formatted: Boolean = True);
    property Cookie: TCookie read FCookie;
    property Environment: TEnvironment read FEnvironment;
    property Params: TGatewayParams read FParams;
    property WebPage: TStrings read FWebPage write SetWebPage;
    property Title: string read FTitle write FTitle;
    property Color: LongInt read FColor write FColor;
    property Sound: string read FSound write FSound;
    property Options: TCGIOptionSet read FOptions write FOptions;
    procedure CreateErrorPage(const Value: string);
  end;

implementation

const
  SEnvironmentKeys: array[0..MAX_ENVIRONMENT - 1] of PChar = ('CONTENT_LENGTH',
   'CONTENT_TYPE', 'HTTP_COOKIE', 'GATEWAY_INTERFACE', 'HTTP_ACCEPT',
   'HTTP_ACCEPT_LANGUAGE', 'HTTP_CONNECTION', 'HTTP_CONTENT_LENGTH',
   'HTTP_CONTENT_TYPE', 'HTTP_FROM', 'HTTP_HOST', 'HTTP_REFERER',
   'HTTP_UA_PIXELS', 'HTTP_UA_COLOR', 'HTTP_UA_OS', 'HTTP_UA_CPU',
   'HTTP_USER_AGENT', 'HTTP_PRAGMA', 'PATH_TRANSLATED', 'QUERY_STRING',
   'REMOTE_ADDR', 'REMOTE_HOST', 'REMOTE_USERNAME', 'REQUEST_METHOD',
   'SCRIPT_NAME', 'SERVER_NAME', 'SERVER_PORT', 'SERVER_PORT_SECURE',
   'SERVER_PROTOCOL', 'SERVER_SOFTWARE');

{ TEnvironment }

constructor TEnvironment.Create;
var
  Strings: TStrings;
  EnvPtr, SavePtr: PChar;
  I: Integer;
begin
  inherited Create;
  Strings := TStringList.Create;
  try
    EnvPtr := GetEnvironmentStrings;
    SavePtr := EnvPtr;
    try
      repeat
        Strings.Add(StrPas(EnvPtr));
        Inc(EnvPtr, StrLen(EnvPtr) + 1);
      until EnvPtr^ = #0;
    finally
      FreeEnvironmentStrings(SavePtr);
    end;
    for I := Low(FStringArray) to High(FStringArray) do
      FStringArray[I] := Strings.Values[SEnvironmentKeys[I]];
  finally
    Strings.Free;
  end;
end;

function TEnvironment.GetDebugString: string;
begin
  Result := 'ContentLength: ' + ContentLength + '<P>';
  Result := Result + 'ContentType: ' + ContentType + '<P>';
  Result := Result + 'Cookie: ' + Cookie + '<P>';
  Result := Result + 'GatewayInterface: ' + GatewayInterface + '<P>';
  Result := Result + 'HTTPAccept: ' + HTTPAccept + '<P>';
  Result := Result + 'HTTPAcceptLanguage: ' + HTTPAcceptLanguage + '<P>';
  Result := Result + 'HTTPConnection: ' + HTTPConnection + '<P>';
  Result := Result + 'HTTPContentLength: ' + HTTPContentLength + '<P>';
  Result := Result + 'HTTPContentType: ' + HTTPContentType + '<P>';
  Result := Result + 'HTTPFrom: ' + HTTPFrom + '<P>';
  Result := Result + 'HTTPHost: ' + HTTPHost + '<P>';
  Result := Result + 'HTTPReferer: ' + HTTPReferer + '<P>';
  Result := Result + 'HTTPUAPixels: ' + HTTPUAPixels + '<P>';
  Result := Result + 'HTTPUAColor: ' + HTTPUAColor + '<P>';
  Result := Result + 'HTTPUAOS: ' + HTTPUAOS + '<P>';
  Result := Result + 'HTTPUACPU: ' + HTTPUACPU + '<P>';
  Result := Result + 'HTTPUserAgent: ' + HTTPUserAgent + '<P>';
  Result := Result + 'HTTPPragma: ' + HTTPPragma + '<P>';
  Result := Result + 'PathTranslated: ' + PathTranslated + '<P>';
  Result := Result + 'QueryString: ' + QueryString + '<P>';
  Result := Result + 'RemoteAddr: ' + RemoteAddr + '<P>';
  Result := Result + 'RemoteHost: ' + RemoteHost + '<P>';
  Result := Result + 'RemoteUsername: ' + RemoteUsername + '<P>';
  Result := Result + 'RequestMethod: ' + RequestMethod + '<P>';
  Result := Result + 'ScriptName: ' + ScriptName + '<P>';
  Result := Result + 'ServerName: ' + ServerName + '<P>';
  Result := Result + 'ServerPort: ' + ServerPort + '<P>';
  Result := Result + 'ServerPortSecure: ' + ServerPortSecure + '<P>';
  Result := Result + 'ServerProtocol: ' + ServerProtocol + '<P>';
  Result := Result + 'ServerSoftware: ' + ServerSoftware + '<P>';
end;

function TEnvironment.GetString(Index: Integer): string;
begin
  Result := FStringArray[Index];
end;

{ TGatewayParams }

function TGatewayParams.GetValues(Key: string): string;
var
 Head, Tail: string;
 I: Integer;
begin
  Head := '';
  Tail := inherited Values[Key];
  while Pos('+', Tail) > 0 do Tail[Pos('+', Tail)] := #32;
  I :=  Pos('%', Tail);
  while I > 0 do
  begin
    Head := Head + Copy(Tail, 1, I - 1) + Chr(StrToInt('$' + Copy(Tail, I + 1, 2)));
    Tail := Copy(Tail, I + 3, Length(Tail));
    I :=  Pos('%', Tail);
  end;
  Head := Head + Tail;
  Result := Head;
end;

{ TCookie }

constructor TCookie.Create;
begin
  inherited Create;
  FCookieList := TStringList.Create;
end;

destructor TCookie.Destroy;
begin
  FCookieList.Free;
  inherited Destroy;
end;

procedure TCookie.Execute;
begin
  FExecuted := True;
end;

procedure TCookie.Delete;
const
  ExpiresDate = 32874;
begin
  if FActive then
  begin
    FExecuted := True;
    FExpires := ExpiresDate;
  end;
end;

procedure TCookie.SetDomain(const Value: string);
begin
  if not FExecuted then
    FDomain := Value;
end;

procedure TCookie.SetExpires(const Value: TDateTime);
begin
  if not FExecuted then
    FExpires := Value;
end;

procedure TCookie.SetName(const Value: string);
begin
  if not FExecuted then
    FName := Value;
end;

procedure TCookie.SetPath(const Value: string);
begin
  if not FExecuted then
    FPath := Value;
end;

procedure TCookie.SetValue(AValue: string);
begin
  if not FExecuted then
    FValue := AValue;
end;

{ TGateway }

constructor TGateway.Create;

  procedure ReadPostParams;
  var
    S: string;
    I: Integer;
  begin
    S := '';
    if Trim(FEnvironment.ContentLength) <> '' then
    begin
      SetLength(S, StrToInt(FEnvironment.ContentLength));
      for I := 1 to Length(S) do
        Read(S[I]);
      S := StringReplace(S, '&', #13#10, [rfReplaceAll]);
    end;
    FParams.Text := S;
  end;

  procedure ReadQueryParams;
  var
    S: string;
  begin
    S := Trim(FEnvironment.QueryString);
    S := StringReplace(S, '+', ' ', [rfReplaceAll]);
    S := StringReplace(S, '&', #13#10, [rfReplaceAll]);
    FParams.Text := S;
  end;

var
  Cookies: string;
  I: Integer;
begin
  inherited Create;
  FCookie := TCookie.Create;
  FEnvironment := TEnvironment.Create;
  FParams := TGatewayParams.Create;
  FWebPage := TStringList.Create;
  if UpperCase(FEnvironment.RequestMethod) = 'POST' then
    ReadPostParams
  else
    ReadQueryParams;
  Cookies := FEnvironment.Cookie;
  if Cookies <> '' then
    with FCookie do
    begin
      FActive := True;
      I := Pos(';', Cookies);
      while I <> 0 do
      begin
        FCookieList.Add(Copy(Cookies, 1, I - 1));
        Cookies := Copy(Cookies, I + 2, 1024);
        I := Pos(';', Cookies);
      end;
      FCookieList.Add(Cookies);
    end;
  FOptions := [];
  FColor := $FFFFFF;
end;

destructor TGateway.Destroy;
begin
  FCookie.Free;
  FEnvironment.Free;
  FWebPage.Free;
  FParams.Free;
  inherited destroy;
end;

procedure TGateway.Append(const FileName: string);
var
  AppendFile: TextFile;
  S: string;
begin
  try
    AssignFile(AppendFile, FileName);
    Reset(AppendFile);
    while not EOF(AppendFile) do
    begin
      ReadLn(AppendFile, S);
      WebPage.Add(S);
    end;
  finally
    CloseFile(AppendFile);
  end;
end;

procedure TGateway.Redirect(const Location: string);
begin
  WriteLn('HTTP/1.0 302 Redirect');
  WriteLn('Location: ' + Location);
  WriteLn('');
end;

procedure TGateway.Return(Formatted: Boolean = True);
begin
  WriteLn('HTTP/1.0 200 OK');
  WriteLn('Content-Type: text/html');
  with Cookie do
    if Executed then
    begin
      FValue := 'Set-Cookie: ' + Cookie.Name + ' = ' + Cookie.Value + ';';
      if FExpires <> 0 then FValue := FValue + ' Expires = ' +
        FormatDateTime('ddd, dd-mmm-yy hh:mm:ss GMT', FExpires) + ';';
      if FPath <> '' then FValue := FValue + ' Path = ' + FPath + ';';
      if FDomain <> '' then FValue := FValue + ' Domain' + FPath + ';';
      WriteLn(FValue);
    end;
  WriteLn('');
  if Formatted then
  begin
    WriteLn('<HTML>');
    WriteLn('<HEAD>');
    WriteLn('<TITLE>' + Title + '</TITLE>');
    WriteLn('</HEAD>');
    if giColor in Options then
      WriteLn('<BODY BGCOLOR = "#' + IntToHex(FColor, 6) + '">')
    else
      WriteLn('<BODY BGCOLOR = "#FFFFFF">');
    WriteLn(FWebPage.Text);
    WriteLn('</BODY>');
    WriteLn('</HTML>');
  end
  else
    WriteLn(FWebPage.Text);
end;

procedure TGateway.SetWebPage(Value: TStrings);
begin
  FWebPage.Assign(Value);
end;

procedure TGateway.CreateErrorPage(const Value: string);
begin
  WriteLn('HTTP/1.0 200 OK');
  WriteLn('Content-Type: text/html');
  WriteLn('');
  WriteLn('<HTML>');
  WriteLn('<HEAD>');
  WriteLn('<TITLE>' + Title+ '</TITLE>');
  WriteLn('</HEAD>');
  WriteLn('<BODY BGCOLOR = "#FFFFFF">');
  WriteLn('<H2>Data Validation Error<H2>');
  WriteLn(Format('<H3>%S<H3>', [Value]));
  WriteLn('</BODY>');
  WriteLn('</HTML>');
end;

end.
