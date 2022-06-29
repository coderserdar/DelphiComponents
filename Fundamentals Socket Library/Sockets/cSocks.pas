{$INCLUDE ..\cDefines.inc}
unit cSocks;

interface

uses
  { Delphi }
  SysUtils,
  WinSock;



{                                                                              }
{                           SOCKS functions v0.02                              }
{                                                                              }
{       This unit is copyright © 2001-2002 by David Butler (david@e.co.za)     }
{                                                                              }
{                  This unit is part of Delphi Fundamentals.                   }
{                    Its original file name is cSockets.pas                    }
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
{ Description:                                                                 }
{   SOCKS was originally developed by David Koblas and subsequently modified   }
{   as SOCKS4 by Ying-Da Lee from NEC.                                         }
{   SOCKS5 is an internet standard as defined in RFC1928.                      }
{                                                                              }
{ Revision history:                                                            }
{   13/12/2001  v0.01  Added Socks5 support for TCP clients.                   }
{   21/09/2002  v0.02  Created cSocks unit.                                    }
{                      Added Socks4 functions.                                 }
{                                                                              }



{                                                                              }
{ SOCKS                                                                        }
{                                                                              }
const
  DefaultSocksListenPort    = 1080;
  DefaultSocksListenPortStr = '1080';



{                                                                              }
{ SOCKS4                                                                       }
{                                                                              }
const
  SOCKS4_MSG_VERSION = 4;

  SOCKS4_REQ_CODE_CONNECT = 1;
  SOCKS4_REQ_CODE_BIND    = 2;

  SOCKS4_RESP_CODE_GRANTED = 90;
  SOCKS4_RESP_CODE_FAILED  = 91;

// Message
type
  TSocks4Message = packed record
    Version  : Byte;       // 4
    Code     : Byte;
    DestPort : Word;
    DestIP   : LongWord;
  end;
  PSocks4Message = ^TSocks4Message;

procedure PopulateSocks4Message(var Msg: TSocks4Message;
          const Code: Byte; const IP: TInAddr; const Port: Word);

// Socks 4 Request
function  Socks4Request(const Code: Byte; const IP: TInAddr; const Port: Word;
          const UserID: String): String;
function  Socks4ConnectRequest(const IP: TInAddr; const Port: Word;
          const UserID: String = ''): String;
function  Socks4BindRequest(const IP: TInAddr; const Port: Word;
          const UserID: String = ''): String;

// Socks 4a Request
function  Socks4aRequest(const Code: Byte; const Domain: String; const Port: Word;
          const UserID: String): String;
function  Socks4aConnectRequest(const Domain: String; const Port: Word;
          const UserID: String = ''): String;

// Response
procedure PopulateSocks4ErrorResponse(var Msg : TSocks4Message; const Code: Byte);
function  IsGrantedSocks4ResponseCode(const Code: Byte): Boolean;



{                                                                              }
{ SOCKS5                                                                       }
{   From RFC 1928 (Socks5) and RFC 1929 (Socks5 User/Pass Authentication).     }
{                                                                              }
const
  SOCKS5_MSG_VERSION  = 5;
  SOCKS5_MAX_MSG_SIZE = 296;

type
  ESocks5 = class (Exception);

// Greeting
type
  TSocks5Greeting = packed record
    Version : Byte;
    Methods : Byte;
    Method1 : Byte;
  end;
  PSocks5Greeting = ^TSocks5Greeting;

const
  SOCKS5_METHOD_NOAUTH   = 0;
  SOCKS5_METHOD_GSSAPI   = 1;
  SOCKS5_METHOD_USERPASS = 2;
  SOCKS5_METHOD_PRIVATE0 = $80; // ..$FE
  SOCKS5_METHOD_INVALID  = $FF;

procedure PopulateSocks5Greeting(var Greeting: TSocks5Greeting;
          const UseUserPass: Boolean);

// Greeting Response
type
  TSocks5GreetingResponse = packed record
    Version : Byte;
    Method  : Byte;
  end;
  PSocks5GreetingResponse = ^TSocks5GreetingResponse;

procedure PopulateSocks5GreetingResponse(var Response: TSocks5GreetingResponse;
          const Method: Byte);
function  Socks5GreetingResponse(const Method: Byte): String;

const
  SOCKS5_GREETING_RESPONSE_NOAUTH   = Char (SOCKS5_MSG_VERSION) + Char (SOCKS5_METHOD_NOAUTH);
  SOCKS5_GREETING_RESPONSE_USERPASS = Char (SOCKS5_MSG_VERSION) + Char (SOCKS5_METHOD_USERPASS);
  SOCKS5_GREETING_RESPONSE_INVALID  = Char (SOCKS5_MSG_VERSION) + Char (SOCKS5_METHOD_INVALID);

// UserPass
const
  SOCKS5_USERPASS_VERSION = 1;

  SOCKS5_USERPASS_STATUS_OK   = 0;
  SOCKS5_USERPASS_STATUS_FAIL = 1;

  SOCKS5_USERPASS_RESPONSE_OK   = Char (SOCKS5_USERPASS_VERSION) + Char (SOCKS5_USERPASS_STATUS_OK);
  SOCKS5_USERPASS_RESPONSE_FAIL = Char (SOCKS5_USERPASS_VERSION) + Char (SOCKS5_USERPASS_STATUS_FAIL);

function  Socks5UserPassMessage(const Username, Password: String): String;

type
  TSocks5UserPassResponse = packed record
    Version : Byte;
    Status  : Byte;
  end;

procedure PopulateSocks5UserPassResponse(var Response: TSocks5UserPassResponse;
          const Status: Byte);

// Messages
const
  SOCKS5_REQ_CODE_CONNECT       = 1;
  SOCKS5_REQ_CODE_BIND          = 2;
  SOCKS5_REQ_CODE_UDP_ASSOCIATE = 3;

  SOCKS5_RESP_CODE_Success                 = 0;
  SOCKS5_RESP_CODE_GeneralServerFailure    = 1;
  SOCKS5_RESP_CODE_ConnectionNotAllowed    = 2;
  SOCKS5_RESP_CODE_NetworkUnreachable      = 3;
  SOCKS5_RESP_CODE_HostUnreachable         = 4;
  SOCKS5_RESP_CODE_ConnectionRefused       = 5;
  SOCKS5_RESP_CODE_TTLExpired              = 6;
  SOCKS5_RESP_CODE_CommandNotSupported     = 7;
  SOCKS5_RESP_CODE_AddressTypeNotSupported = 8;

  SOCKS5_ADDR_TYPE_IP4    = 1;
  SOCKS5_ADDR_TYPE_DOMAIN = 3;
  SOCKS5_ADDR_TYPE_IP6    = 4;

// IP4 Message
type
  TSocks5IP4Message = packed record
    Version  : Byte;
    Code     : Byte;
    Reserved : Byte;
    AddrType : Byte;
    IP4Addr  : TInAddr;
    Port     : Word;
  end;
  PSocks5IP4Message = ^TSocks5IP4Message;

procedure PopulateSocks5IP4Message(var Msg: TSocks5IP4Message;
          const Command: Byte; const Addr: TInAddr; const Port: Word);
procedure PopulateSocks5ErrorReply(var Msg: TSocks5IP4Message;
          const ResponseCode: Byte);

// Domain Message
type
  TSocks5DomainMessageHeader = packed record
    Version  : Byte;
    Code     : Byte;
    Reserved : Byte;
    AddrType : Byte;
    NameLen  : Byte;
  end;
  PSocks5DomainMessageHeader = ^TSocks5DomainMessageHeader;

procedure PopulateSocks5DomainMessageHeader(var MsgHdr: TSocks5DomainMessageHeader;
          const Command: Byte; const Domain: String);
function  Socks5DomainRequest(const Command: Byte; const Domain: String;
          const Port: Word): String;

// Response
type
  TSocks5ResponseHeader = packed record
    Version   : Byte;
    Code      : Byte;
    Reserved  : Byte;
    AddrType  : Byte;
    Addr1     : Byte;  // First byte of address (Length for saDomainName)
  end;
  PSocks5ResponseHeader = ^TSocks5ResponseHeader;

function  Socks5ResponseSize(const Header: PSocks5ResponseHeader): Integer;




implementation

uses
  { Fundamentals }
  cUtils;



{                                                                              }
{ SOCKS4                                                                       }
{                                                                              }
procedure PopulateSocks4Message(var Msg : TSocks4Message;
          const Code: Byte; const IP: TInAddr; const Port: Word);
  begin
    FillChar (Msg, Sizeof (Msg), #0);
    Msg.Version := SOCKS4_MSG_VERSION;
    Msg.Code := Code;
    Msg.DestPort := Port;
    Msg.DestIP := LongWord (IP.S_addr);
  end;

function Socks4Request(const Code: Byte; const IP: TInAddr; const Port: Word; const UserID: String): String;
var P : PChar;
    L : Integer;
  begin
    L := Length (UserID);
    SetLength (Result, Sizeof (TSocks4Message) + L + 1);
    P := Pointer (Result);
    PopulateSocks4Message (PSocks4Message (P)^, Code, IP, Port);
    Inc (P, Sizeof (TSocks4Message));
    if L > 0 then
      begin
        MoveMem (Pointer (UserID)^, P^, L);
        Inc (P, L);
      end;
    P^ := #0;
  end;

function Socks4ConnectRequest(const IP: TInAddr; const Port: Word; const UserID: String): String;
  begin
    Result := Socks4Request (SOCKS4_REQ_CODE_CONNECT, IP, Port, UserID);
  end;

function Socks4BindRequest(const IP: TInAddr; const Port: Word; const UserID: String): String;
  begin
    Result := Socks4Request (SOCKS4_REQ_CODE_BIND, IP, Port, UserID);
  end;

function Socks4aRequest(const Code: Byte; const Domain: String; const Port: Word; const UserID: String): String;
var IP   : TInAddr;
    L, M : Integer;
    P    : PChar;
  begin
    IP.S_addr := 0;
    IP.S_un_b.s_b4 := #$FF;
    Result := Socks4Request (Code, IP, Port, UserID);
    M := Length (Result);
    L := Length (Domain);
    SetLength (Result, M + L + 1);
    P := Pointer (Result);
    Inc (P, M);
    if L > 0 then
      begin
        MoveMem (Pointer (Domain)^, P^, L);
        Inc (P, L);
      end;
    P^ := #0;
  end;

function Socks4aConnectRequest(const Domain: String; const Port: Word; const UserID: String): String;
  begin
    Result := Socks4aRequest (SOCKS4_REQ_CODE_CONNECT, Domain, Port, UserID);
  end;

procedure PopulateSocks4ErrorResponse(var Msg : TSocks4Message; const Code: Byte);
var A : TInAddr;
  begin
    A.S_addr := 0;
    PopulateSocks4Message (Msg, Code, A, 0);
  end;

function IsGrantedSocks4ResponseCode(const Code: Byte): Boolean;
  begin
    Result := Code = SOCKS4_RESP_CODE_GRANTED;
  end;



{                                                                              }
{ SOCKS5                                                                       }
{                                                                              }
procedure PopulateSocks5Greeting(var Greeting : TSocks5Greeting; const UseUserPass: Boolean);
  begin
    FillChar (Greeting, Sizeof (TSocks5Greeting), #0);
    Greeting.Version := SOCKS5_MSG_VERSION;
    Greeting.Methods := 1;
    if UseUserPass then
      Greeting.Method1 := SOCKS5_METHOD_USERPASS else
      Greeting.Method1 := SOCKS5_METHOD_NOAUTH;
  end;

procedure PopulateSocks5GreetingResponse(var Response : TSocks5GreetingResponse; const Method: Byte);
  begin
    FillChar (Response, Sizeof (TSocks5GreetingResponse), #0);
    Response.Version := SOCKS5_MSG_VERSION;
    Response.Method := Method;
  end;

function Socks5GreetingResponse(const Method: Byte): String;
var P : PSocks5GreetingResponse;
  begin
    SetLength (Result, Sizeof (TSocks5GreetingResponse));
    P := Pointer (Result);
    PopulateSocks5GreetingResponse (P^, Method);
  end;

function Socks5UserPassMessage(const Username, Password: String): String;
var L, M : Integer;
    P    : PChar;
  begin
    L := Length (UserName);
    if L > 255 then
      raise ESocks5.Create ('Username too long for use with SOCKS');
    M := Length (Password);
    if M > 255 then
      raise ESocks5.Create ('Password too long for use with SOCKS');
    SetLength (Result, 3 + L + M);
    P := Pointer (Result);
    P [0] := Char (SOCKS5_USERPASS_VERSION);
    P [1] := Char (L);
    if L > 0 then
      Move (Pointer (UserName)^, P [2], L);
    P [2 + L] := Char (M);
    if M > 0 then
      Move (Pointer (Password)^, P [3 + L], M);
  end;

procedure PopulateSocks5UserPassResponse(var Response : TSocks5UserPassResponse; const Status: Byte);
  begin
    FillChar (Response, Sizeof (TSocks5UserPassResponse), #0);
    Response.Version := SOCKS5_USERPASS_VERSION;
    Response.Status := Status;
  end;

procedure PopulateSocks5IP4Message(var Msg : TSocks5IP4Message; const Command: Byte; const Addr: TInAddr; const Port: Word);
  begin
    FillChar (Msg, Sizeof (TSocks5IP4Message), #0);
    Msg.Version  := SOCKS5_MSG_VERSION;
    Msg.Code     := Command;
    Msg.Reserved := $00;
    Msg.AddrType := SOCKS5_ADDR_TYPE_IP4;
    Msg.IP4Addr  := Addr;
    Msg.Port     := Port;
  end;

procedure PopulateSocks5ErrorReply(var Msg : TSocks5IP4Message; const ResponseCode: Byte);
var A : TInAddr;
  begin
    A.S_addr := 0;
    PopulateSocks5IP4Message (Msg, ResponseCode, A, 0);
  end;

procedure PopulateSocks5DomainMessageHeader(var MsgHdr : TSocks5DomainMessageHeader; const Command: Byte; const Domain: String);
var L : Integer;
  begin
    L := Length (Domain);
    if L > 255 then
      raise ESocks5.Create ('Domain name too long for use with SOCKS5');
    FillChar (MsgHdr, Sizeof (TSocks5DomainMessageHeader), #0);
    MsgHdr.Version  := SOCKS5_MSG_VERSION;
    MsgHdr.Code     := Command;
    MsgHdr.Reserved := $00;
    MsgHdr.AddrType := SOCKS5_ADDR_TYPE_DOMAIN;
    MsgHdr.NameLen  := Byte (L);
  end;

function Socks5DomainRequest(const Command: Byte; const Domain: String; const Port: Word): String;
var L : Integer;
    P : PChar;
  begin
    L := Length (Domain);
    if L > 255 then
      raise ESocks5.Create ('Domain name too long for use with SOCKS');
    SetLength (Result, 7 + L);
    P := Pointer (Result);
    PopulateSocks5DomainMessageHeader (PSocks5DomainMessageHeader (P)^, Command, Domain);
    Inc (P, Sizeof (TSocks5DomainMessageHeader));
    if L > 0 then
      begin
        Move (Pointer (Domain)^, P^, L);
        Inc (P, L);
      end;
    Move (Port, P^, 2);
  end;

function Socks5ResponseSize(const Header: PSocks5ResponseHeader): Integer;
  begin
    Case Header^.AddrType of
      SOCKS5_ADDR_TYPE_IP4    : Result := 10;
      SOCKS5_ADDR_TYPE_IP6    : Result := 22;
      SOCKS5_ADDR_TYPE_DOMAIN : Result := 7 + Header^.Addr1;
    else
      raise ESocks5.Create ('Socks5 Address type #' + IntToStr (Header^.AddrType) + ' not supported');
    end;
  end;



end.

