{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  Component to query DNS records.
              Implement a subset of RFC 1035 (A and MX records).
Creation:     January 29, 1999
Version:      1.02
EMail:        http://users.swing.be/francois.piette  francois.piette@swing.be
              http://www.rtfm.be/fpiette             francois.piette@rtfm.be
              francois.piette@pophost.eunet.be
Support:      Use the mailing list twsocket@rtfm.be See website for details.
Legal issues: Copyright (C) 1999-2000 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@pophost.eunet.be>

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

History:
Feb 14, 1999 V0.02 Indirectly call winsock functions using wsocket because
             wsocket provide runtime dynamic link instead of loadtime link.
             This allows a program to use DnsQuery if it discover that winsock
             is installed and still run if winsock is not installed.
Feb 24, 1999 V1.00 Added code for reverse lookup (PTR record).
Mar 07, 1999 V1.01 Adapted for Delphi 1
Aug 20, 1999 V1.02 Revise compile time option. Adapted for BCB4


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit DnsQuery;

{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$R-}           { Disable range checking              }
{$IFNDEF VER80} { Not for Delphi 1                    }
    {$H+}       { Use long strings                    }
    {$J+}       { Allow typed constant to be modified }
{$ENDIF}
{$IFDEF VER110} { C++ Builder V3.0                    }
    {$ObjExportAll On}
{$ENDIF}
{$IFDEF VER125} { C++ Builder V4.0                    }
    {$ObjExportAll On}
{$ENDIF}

interface

uses
  WinTypes, WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Winsock, WSocket;

const
  DnsQueryVersion    = 102;
  CopyRight : String = ' TDnsQuery  (c) 1999-2000 F. Piette V1.02 ';

  { Maximum answers (responses) count }
  MAX_ANCOUNT     = 50;
  { Maximum number of MX records taken into account in responses }
  MAX_MX_RECORDS  = 50;
  MAX_A_RECORDS   = 50;
  MAX_PTR_RECORDS = 10;

  { DNS Classes }
  DnsClassIN      = 1;   { The internet                                      }
  DnsClassCS      = 2;   { The CSNET class (obsolete, used only for examples)}
  DnsClassCH      = 3;   { The CHAOS class                                   }
  DnsClassHS      = 4;   { Hesiod name service                               }
  DnsClassALL     = 255; { Any class                                         }

  { Type of query/response a DNS can handle }
  DnsQueryA       = 1;  { A     HostAddress                                  }
  DnsQueryNS      = 2;  { NS    Authoritative name server                    }
  DnsQueryMD      = 3;  { MD    MailDestination, obsolete, use Mail Exchange }
  DnsQueryMF      = 4;  { MF    MailForwarder, obsolete, use Mail Exchange   }
  DnsQueryCNAME   = 5;  { CNAME CanonicalName                                }
  DnsQuerySOA     = 6;  { SOA   Start of a Zone of Authority                 }
  DnsQueryMB      = 7;  { MB    MailBox, experimental                        }
  DnsQueryMG      = 8;  { MG    MailGroup, experimental                      }
  DnsQueryMR      = 9;  { MR    MailRename, experimental                     }
  DnsQueryNULL    = 10; { NULL  Experimental                                 }
  DnsQueryWKS     = 11; { WKS   Well Known Service Description               }
  DnsQueryPTR     = 12; { PTR   Domain Name Pointer                          }
  DnsQueryHINFO   = 13; { HINFO Host Information                             }
  DnsQueryMINFO   = 14; { MINFO Mailbox information                          }
  DnsQueryMX      = 15; { MX    Mail Exchange                                }
  DnsQueryTXT     = 16; { TXT   Text Strings                                 }

  { Some additional type only allowed in queries }
  DnsQueryAXFR    = 252; { Transfer for an entire zone                       }
  DnsQueryMAILB   = 253; { Mailbox related records (MB, MG or MR)            }
  DnsQueryMAILA   = 254; { MailAgent, obsolete, use MX instead               }
  DnsQueryALL     = 255; { Request ALL records                               }

  { Opcode field in query flags }                                            
  DnsOpCodeQUERY  = 0;
  DnsOpCodeIQUERY = 1;
  DnsOpCodeSTATUS = 2;

type
  TDnsAnswerNameArray   = packed array [0..MAX_ANCOUNT - 1]     of String;
  TDnsAnswerTypeArray   = packed array [0..MAX_ANCOUNT - 1]     of Integer;
  TDnsAnswerClassArray  = packed array [0..MAX_ANCOUNT - 1]     of Integer;
  TDnsAnswerTTLArray    = packed array [0..MAX_ANCOUNT - 1]     of LongInt;
  TDnsAnswerTagArray    = packed array [0..MAX_ANCOUNT - 1]     of Integer;
  TDnsMXPreferenceArray = packed array [0..MAX_MX_RECORDS - 1]  of Integer;
  TDnsMXExchangeArray   = packed array [0..MAX_MX_RECORDS - 1]  of String;
  TDnsAddressArray      = packed array [0..MAX_A_RECORDS - 1]   of TInAddr;
  TDnsHostnameArray     = packed array [0..MAX_PTR_RECORDS - 1] of String;

  TDnsRequestDoneEvent = procedure (Sender : TObject; Error : WORD) of Object;
  TDnsRequestHeader = packed record
      ID      : WORD;
      Flags   : WORD;
      QDCount : WORD;
      ANCount : WORD;
      NSCount : WORD;
      ARCount : WORD;
  end;
  PDnsRequestHeader = ^TDnsRequestHeader;

  TDnsQuery = class(TComponent)
  private
    { Déclarations privées }
  protected
    FWSocket                    : TWSocket;
    FPort                       : String;
    FAddr                       : String;
    FIDCount                    : WORD;
    FQueryBuf                   : array [0..511] of char;
    FQueryLen                   : Integer;
    FResponseBuf                : array [0..511] of char;
    FResponseLen                : Integer;
    FResponseID                 : Integer;
    FResponseCode               : Integer;
    FResponseOpCode             : Integer;
    FResponseAuthoritative      : Boolean;
    FResponseTruncation         : Boolean;
    FResponseRecursionAvailable : Boolean;
    FResponseQDCount            : Integer;
    FResponseANCount            : Integer;
    FResponseNSCount            : Integer;
    FResponseARCount            : Integer;
    FQuestionType               : Integer;
    FQuestionClass              : Integer;
    FQuestionName               : String;
    FAnswerNameArray            : TDnsAnswerNameArray;
    FAnswerTypeArray            : TDnsAnswerTypeArray;
    FAnswerClassArray           : TDnsAnswerClassArray;
    FAnswerTTLArray             : TDnsAnswerTTLArray;
    FAnswerTagArray             : TDnsAnswerTagArray;
    FMXRecordCount              : Integer;
    FMXPreferenceArray          : TDnsMXPreferenceArray; { For MX request  }
    FMXExchangeArray            : TDnsMXExchangeArray;   { For MX request  }
    FARecordCount               : Integer;
    FAddressArray               : TDnsAddressArray;      { For A request   }
    FPTRRecordCount             : Integer;
    FHostnameArray              : TDnsHostnameArray;     { For PTR request }
    FOnRequestDone              : TDnsRequestDoneEvent;
    function GetMXPreference(nIndex : Integer) : Integer;
    function GetMXExchange(nIndex : Integer)   : String;
    function GetAnswerName(nIndex : Integer)   : String;
    function GetAnswerType(nIndex : Integer)   : Integer;
    function GetAnswerClass(nIndex : Integer)  : Integer;
    function GetAnswerTTL(nIndex : Integer)    : LongInt;
    function GetAnswerTag(nIndex : Integer)    : Integer;
    function GetAddress(nIndex : Integer)      : TInAddr;
    function GetHostname(nIndex : Integer)     : String;
    procedure BuildRequestHeader(Dst       : PDnsRequestHeader;
                                 ID        : WORD;
                                 OPCode    : BYTE;
                                 Recursion : Boolean;
                                 QDCount   : WORD;
                                 ANCount   : WORD;
                                 NSCount   : WORD;
                                 ARCount   : WORD); virtual;
    function  BuildQuestionSection(Dst         : PChar;
                                   const QName : String;
                                   QType       : WORD;
                                   QClass      : WORD) : Integer; virtual;
    procedure WSocketDataAvailable(Sender: TObject; Error: WORD); virtual;
    procedure TriggerRequestDone(Error: WORD); virtual;
    function  GetResponseBuf : PChar;
    procedure SendQuery;
    function  ExtractName(Base       : PChar;
                          From       : PChar;
                          var Name   : String) : PChar;
    function  DecodeQuestion(Base       : PChar;
                             From       : PChar;
                             var Name   : String;
                             var QType  : Integer;
                             var QClass : Integer) : PChar;
    function DecodeAnswer(Base         : PChar;
                          From         : PChar;
                          var Name     : String;
                          var QType    : Integer;
                          var QClass   : Integer;
                          var TTL      : LongInt;
                          var RDataPtr : Pointer;
                          var RDataLen : Integer) : PChar;
    function DecodeMXData(Base           : PChar;
                          From           : PChar;
                          var Preference : Integer;
                          var Exchange   : String) : PChar;
    function DecodeAData(Base        : PChar;
                         From        : PChar;
                         var Address : TInAddr) : PChar;
    function DecodePTRData(Base         : PChar;
                           From         : PChar;
                           var Hostname : String) : PChar;
  public
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
    procedure   Notification(AComponent: TComponent; operation: TOperation); override;
    function    MXLookup(Domain : String) : Integer;
    function    ALookup(Host : String) : Integer;
    function    PTRLookup(IP : String) : Integer;
    property ResponseID                 : Integer read FResponseID;
    property ResponseCode               : Integer read FResponseCode;
    property ResponseOpCode             : Integer read FResponseOpCode;
    property ResponseAuthoritative      : Boolean read FResponseAuthoritative;
    property ResponseTruncation         : Boolean read FResponseTruncation;
    property ResponseRecursionAvailable : Boolean read FResponseRecursionAvailable;
    property ResponseQDCount            : Integer read FResponseQDCount;
    property ResponseANCount            : Integer read FResponseANCount;
    property ResponseNSCount            : Integer read FResponseNSCount;
    property ResponseARCount            : Integer read FResponseARCount;
    property ResponseBuf                : PChar   read GetResponseBuf;
    property ResponseLen                : Integer read FResponseLen;
    property QuestionType               : Integer read FQuestionType;
    property QuestionClass              : Integer read FQuestionClass;
    property QuestionName               : String  read FQuestionName;
    property AnswerName[nIndex : Integer]   : String  read GetAnswerName;
    property AnswerType[nIndex : Integer]   : Integer read GetAnswerType;
    property AnswerClass[nIndex : Integer]  : Integer read GetAnswerClass;
    property AnswerTTL[nIndex : Integer]    : LongInt read GetAnswerTTL;
    property AnswerTag[nIndex : Integer]    : Integer read GetAnswerTag;
    property MXPreference[nIndex : Integer] : Integer read GetMXPreference;
    property MXExchange[nIndex : Integer]   : String  read GetMXExchange;
    property Address[nIndex : Integer]      : TInAddr read GetAddress;
    property Hostname[nIndex : Integer]     : String  read GetHostname;
  published
    property Port    : String read  FPort write FPort;
    property Addr    : String read  FAddr write FAddr;
    property OnRequestDone : TDnsRequestDoneEvent read  FOnRequestDone
                                                  write FOnRequestDone;
  end;

function ReverseIP(const IP : String) : String;

procedure Register;

implementation

type
    PWORD  = ^WORD;
    PDWORD = ^DWORD;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function ReverseIP(const IP : String) : String;
var
    I, J : Integer;
begin
    Result := '';
    if Length(IP) = 0 then
        Exit;
    J      := Length(IP);
    I      := J;
    while I >= 0 do begin
        if (I = 0) or (IP[I] = '.') then begin
            Result := Result + '.' + Copy(IP, I + 1, J - I);
            J := I - 1;
        end;
        Dec(I);
    end;
    if Result[1] = '.' then
        Delete(Result, 1, 1);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure Register;
begin
    RegisterComponents('FPiette', [TDnsQuery]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TDnsQuery.Create(AOwner : TComponent);
begin
    inherited Create(AOwner);
    FWSocket := TWSocket.Create(nil);
    FPort    := '53';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TDnsQuery.Destroy;
begin
    if Assigned(FWSocket) then begin
        FWSocket.Destroy;
        FWSocket := nil;
    end;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDnsQuery.Notification(AComponent: TComponent; operation: TOperation);
begin
    inherited Notification(AComponent, operation);
    if operation = opRemove then begin
        if AComponent = FWSocket then
            FWSocket := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.GetMXPreference(nIndex : Integer) : Integer;
begin
    { Silently ignore index out of bounds error }
    if (nIndex < Low(FMXPreferenceArray)) or
       (nIndex > High(FMXPreferenceArray)) then
        Result := 0
    else
        Result := FMXPreferenceArray[nIndex];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.GetMXExchange(nIndex : Integer) : String;
begin
    { Silently ignore index out of bounds error }
    if (nIndex < Low(FMXExchangeArray)) or
       (nIndex > High(FMXExchangeArray)) then
        Result := ''
    else
        Result := FMXExchangeArray[nIndex];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.GetAnswerName(nIndex : Integer) : String;
begin
    { Silently ignore index out of bounds error }
    if (nIndex < Low(FAnswerNameArray)) or
       (nIndex > High(FAnswerNameArray)) then
        Result := ''
    else
        Result := FAnswerNameArray[nIndex];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.GetAnswerType(nIndex : Integer) : Integer;
begin
    { Silently ignore index out of bounds error }
    if (nIndex < Low(FAnswerTypeArray)) or
       (nIndex > High(FAnswerTypeArray)) then
        Result := 0
    else
        Result := FAnswerTypeArray[nIndex];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.GetAnswerClass(nIndex : Integer) : Integer;
begin
    { Silently ignore index out of bounds error }
    if (nIndex < Low(FAnswerClassArray)) or
       (nIndex > High(FAnswerClassArray)) then
        Result := 0
    else
        Result := FAnswerClassArray[nIndex];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.GetAnswerTTL(nIndex : Integer) : LongInt;
begin
    { Silently ignore index out of bounds error }
    if (nIndex < Low(FAnswerTTLArray)) or
       (nIndex > High(FAnswerTTLArray)) then
        Result := 0
    else
        Result := FAnswerTTLArray[nIndex];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.GetAnswerTag(nIndex : Integer) : Integer;
begin
    { Silently ignore index out of bounds error }
    if (nIndex < Low(FAnswerTagArray)) or
       (nIndex > High(FAnswerTagArray)) then
        Result := 0
    else
        Result := FAnswerTagArray[nIndex];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.GetAddress(nIndex : Integer) : TInAddr;
begin
    { Silently ignore index out of bounds error }
    if (nIndex < Low(FAddressArray)) or
       (nIndex > High(FAddressArray)) then
        Result.S_addr := 0
    else
        Result := FAddressArray[nIndex];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.GetHostname(nIndex : Integer) : String;
begin
    { Silently ignore index out of bounds error }
    if (nIndex < Low(FHostnameArray)) or
       (nIndex > High(FHostnameArray)) then
        Result := ''
    else
        Result := FHostnameArray[nIndex];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.GetResponseBuf : PChar;
begin
    Result := @FResponseBuf;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.MXLookup(Domain : String) : Integer;
begin
   Inc(FIDCount);
   BuildRequestHeader(PDnsRequestHeader(@FQueryBuf), FIDCount, DnsOpCodeQuery, TRUE, 1, 0, 0, 0);
   FQueryLen := BuildQuestionSection(@FQueryBuf[SizeOf(TDnsRequestHeader)], Domain, DnsQueryMX, DnsClassIN);
   FQueryLen := FQueryLen + SizeOf(TDnsRequestHeader);
   Result    := FIDCount;
   SendQuery;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.ALookup(Host : String) : Integer;
begin
   Inc(FIDCount);
   BuildRequestHeader(PDnsRequestHeader(@FQueryBuf), FIDCount, DnsOpCodeQuery, TRUE, 1, 0, 0, 0);
   FQueryLen := BuildQuestionSection(@FQueryBuf[SizeOf(TDnsRequestHeader)], Host, DnsQueryA, DnsClassIN);
   FQueryLen := FQueryLen + SizeOf(TDnsRequestHeader);
   Result    := FIDCount;
   SendQuery;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.PTRLookup(IP : String) : Integer;
begin
   Inc(FIDCount);
   BuildRequestHeader(PDnsRequestHeader(@FQueryBuf), FIDCount, DnsOpCodeQuery, TRUE, 1, 0, 0, 0);
   FQueryLen := BuildQuestionSection(@FQueryBuf[SizeOf(TDnsRequestHeader)],
                                     ReverseIP(IP) + '.in-addr.arpa',
                                     DnsQueryPTR, DnsClassIN);
   FQueryLen := FQueryLen + SizeOf(TDnsRequestHeader);
   Result    := FIDCount;
   SendQuery;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDnsQuery.SendQuery;
begin
   FResponseLen             := -1;
   FWSocket.OnDataAvailable := nil;
   FWSocket.Abort;
   FWSocket.OnDataAvailable := WSocketDataAvailable;
   FWSocket.Proto           := 'udp';
   FWSocket.Port            := FPort;
   FWSocket.Addr            := FAddr;
   FWSocket.Connect;
   FWSocket.Send(@FQueryBuf, FQueryLen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.BuildQuestionSection(
    Dst         : PChar;
    const QName : String;
    QType       : WORD;
    QClass      : WORD) : Integer;
var
    I   : Integer;
    p   : PChar;
    Ptr : PChar;
begin
    Ptr := Dst;
    if Ptr = nil then begin
        Result := 0;
        Exit;
    end;
    I := 1;
    while I <= Length(QName) do begin
        p := Ptr;
        Inc(Ptr);
        while (I <= Length(QName)) and (QName[I] <> '.') do begin
            Ptr^ := QName[I];
            Inc(Ptr);
            Inc(I);
        end;
        p^ := Chr(Ptr - p - 1);
        Inc(I);
    end;
    Ptr^ := #0;
    Inc(Ptr);
    PWORD(Ptr)^ := htons(QType);
    Inc(Ptr, 2);
    PWORD(Ptr)^ := htons(QClass);
    Inc(Ptr, 2);
    Result := Ptr - Dst;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDnsQuery.BuildRequestHeader(
    Dst       : PDnsRequestHeader;
    ID        : WORD;
    OPCode    : BYTE;
    Recursion : Boolean;
    QDCount   : WORD;
    ANCount   : WORD;
    NSCount   : WORD;
    ARCount   : WORD);
begin
    if Dst = nil then
        Exit;
    Dst^.ID      := htons(ID);
    Dst^.Flags   := htons((OpCode shl 11) + (Ord(Recursion) shl 8));
    Dst^.QDCount := htons(QDCount);
    Dst^.ANCount := htons(ANCount);
    Dst^.NSCount := htons(NSCount);
    Dst^.ARCount := htons(ARCount);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDnsQuery.TriggerRequestDone(Error: WORD);
begin
    if Assigned(FOnRequestDone) then
        FOnRequestDone(Self, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDnsQuery.WSocketDataAvailable(Sender: TObject; Error: WORD);
var
    Len    : Integer;
    Ans    : PDnsRequestHeader;
    Flags  : Integer;
    P      : PChar;
    RDataPtr : Pointer;
    RDataLen : Integer;
    I        : Integer;
begin
    Ans := PDnsRequestHeader(@FResponseBuf);
    Len := FWSocket.Receive(Ans, SizeOf(FResponseBuf));
    if Error <> 0 then begin
        TriggerRequestDone(Error);
        Exit;
    end;
    { Check for minimum response length }
    if Len < SizeOf(TDnsRequestHeader) then
        Exit;
    Flags := WSocket_ntohs(Ans^.Flags);
    { Check if we got a response }
    if (Flags and $8000) = 0 then
        Exit;
    FResponseLen := Len;
    { Decode response header }
    FResponseID                 := WSocket_ntohs(Ans^.ID);
    FResponseCode               := Flags and $000F;
    FResponseOpCode             := (Flags shr 11) and $000F;
    FResponseAuthoritative      := (Flags and $0400) = $0400;
    FResponseTruncation         := (Flags and $0200) = $0200;
    FResponseRecursionAvailable := (Flags and $0080) = $0080;
    FResponseQDCount            := WSocket_ntohs(Ans^.QDCount);
    FResponseANCount            := WSocket_ntohs(Ans^.ANCount);
    FResponseNSCount            := WSocket_ntohs(Ans^.NSCount);
    FResponseARCount            := WSocket_ntohs(Ans^.ARCount);

    P := @ResponseBuf[SizeOf(TDnsRequestHeader)];
    if FResponseQDCount = 0 then begin
        { I don't think we could receive 0 questions }
        FQuestionName  := '';
        FQuestionType  := 0;
        FQuestionClass := 0;
    end
    else begin
        { Should never be greater than 1 because we sent only one question }
        P := DecodeQuestion(@FResponseBuf, P,
                            FQuestionName, FQuestionType, FQuestionClass);
    end;
    if FResponseANCount = 0 then begin
        RDataPtr        := nil;
        RDataLen        := 0;
        FMXRecordCount  := 0;
        FARecordCount   := 0;
        FPTRRecordCount := 0;
    end
    else begin
        FMXRecordCount  := 0;
        FARecordCount   := 0;
        FPTRRecordCount := 0;
        for I := 0 to FResponseANCount - 1 do begin
            P := DecodeAnswer(@FResponseBuf,        P,
                              FAnswerNameArray[I],  FAnswerTypeArray[I],
                              FAnswerClassArray[I], FAnswerTTLArray[I],
                              RDataPtr,             RDataLen);
            FAnswerTagArray[I] := -1;
            case FAnswerTypeArray[I] of
            DnsQueryMX:
                begin
                    if FMXRecordCount <= High(FMXPreferenceArray) then begin
                        FAnswerTagArray[I] := FMXRecordCount;
                        DecodeMXData(@FResponseBuf, RDataPtr,
                                     FMXPreferenceArray[FMXRecordCount],
                                     FMXExchangeArray[FMXRecordCount]);
                        Inc(FMXRecordCount);
                    end;
                end;
            DnsQueryA:
                begin
                    if FARecordCount <= High(FAddressArray) then begin
                        FAnswerTagArray[I] := FARecordCount;
                        DecodeAData(@FResponseBuf, RDataPtr,
                                    FAddressArray[FARecordCount]);
                        Inc(FARecordCount);
                    end;
                end;
            DnsQueryPTR:
                begin
                    if FPTRRecordCount <= High(FHostnameArray) then begin
                        FAnswerTagArray[I] := FPTRRecordCount;
                        DecodePTRData(@FResponseBuf, RDataPtr,
                                      FHostnameArray[FPTRRecordCount]);
                        Inc(FPTRRecordCount);
                    end;
                end;
            end;
        end;
    end;
    TriggerRequestDone(0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.ExtractName(
    Base       : PChar;
    From       : PChar;
    var Name   : String) : PChar;
var
    N       : Integer;
    I       : Integer;
    P       : PChar;
    NameEnd : String;
begin
    P := From;
    if P^ = #0 then begin
        Name := '';
        Inc(P);
    end
    else begin
        Name := '';
        while TRUE do begin
            { Get name part length }
            N := Ord(P^);
            if (N and $C0) = $C0 then begin
                 { Message compression }
                 N := ((N and $3F) shl 8) + Ord(P[1]);
                 if Length(Name) = 0 then
                     Self.ExtractName(Base, Base + N, Name)
                 else begin
                     Self.ExtractName(Base, Base + N, NameEnd);
                     Name := Name + NameEnd;
                 end;
                 Inc(P, 2);
                 break;
            end;
            Inc(P);
            if N = 0 then
                break;
            { Copy name part }
            for I := 1 to N do begin
                Name := Name + P^;
                Inc(P);
            end;
            if P^ <> #0 then
                Name := Name + '.';
        end;
    end;
    Result := P;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.DecodeQuestion(
    Base       : PChar;
    From       : PChar;
    var Name   : String;
    var QType  : Integer;
    var QClass : Integer) : PChar;
var
    P : PChar;
begin
    P := ExtractName(Base, From, Name);
    QType  := WSocket_ntohs(PWORD(P)^);
    Inc(P, 2);
    QClass := WSocket_ntohs(PWORD(P)^);
    Inc(P, 2);
    Result := P;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.DecodeAnswer(
    Base         : PChar;
    From         : PChar;
    var Name     : String;
    var QType    : Integer;
    var QClass   : Integer;
    var TTL      : LongInt;
    var RDataPtr : Pointer;
    var RDataLen : Integer) : PChar;
var
    P : PChar;
begin
    P := ExtractName(Base, From, Name);
    QType  := WSocket_ntohs(PWORD(P)^);
    Inc(P, 2);
    QClass := WSocket_ntohs(PWORD(P)^);
    Inc(P, 2);
    TTL    := WSocket_ntohl(PDWORD(P)^);
    Inc(P, 4);
    RDataLen := WSocket_ntohs(PWORD(P)^);
    Inc(P, 2);
    RDataPtr := P;
    Result := P + RDataLen;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.DecodeMXData(
    Base           : PChar;
    From           : PChar;
    var Preference : Integer;
    var Exchange   : String) : PChar;
begin
    Result := From;
    Preference := WSocket_ntohs(PWORD(Result)^);
    Inc(Result, 2);
    Result := ExtractName(Base, Result, Exchange);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.DecodePTRData(
    Base         : PChar;
    From         : PChar;
    var Hostname : String) : PChar;
begin
    Result := ExtractName(Base, From, Hostname);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.DecodeAData(
    Base        : PChar;
    From        : PChar;
    var Address : TInAddr) : PChar;
begin
    Result := From;
    Address.S_addr := PDWORD(Result)^;
    Inc(Result, 4);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{
  <0><1><129><128><0><1><0><1><0><4><0><5><7>inp
  rise<3>com<0><0><15><0><1><192><12><0>
  <15><0><1><0><1>QV<0><10><0><10><5>drui
  d<192><12><192><12><0><2><0><1><0><1>Qc<0><6><3>
  ns1<192><12><192><12><0><2><0><1><0><1>Qc<0>
  <20><3>NS1<10>SPRINTLINK
  <3>NET<0><192><12><0><2><0><1><0><1>Qc<0>
  <6><3>NS2<192>U<192><12><0><2><0><1><0><1>Q
  c<0><6><3>NS3<192>U<192>+<0><1><0><1><0>
  <1>QV<0><4><143><186><11>F<192>?<0><1><0><1><0>
  <1>Qc<0><4><207>iS<30><192>Q<0><1><0><1><0>
  <2><144>i<0><4><204>u<214><10><192>q<0><1><0><1><0>
  <2><144>i<0><4><199><2><252><10><192><131><0><1><0><1><0>
  <2><142><182><0><4><204>a<212><10>
}
{
  <0><3><129><128><0><1><0><1><0><2><0><3><4>rtf
  m<2>be<0><0><15><0><1><192><12><0><15><0><1><0>
  <1>.b<0><9><0><10><4>mail<192><12><192><12>
  <0><2><0><1><0><1>.b<0><11><2>ns<3>dn
  s<2>be<0><192><12><0><2><0><1><0><1>.b<0>
  <5><2>ns<192><12><192>'<0><1><0><1><0><1>.b
  <0><4><195><0>d<253><192>:<0><1><0><1><0><1>QY
  <0><4><134>:J!<192>Q<0><1><0><1><0><1>.b
  <0><4><195><0>d<253>
}
{
  <0><7><133><128><0><1><0><1><0><2><0><2><3>www
  <4>rtfm<2>be<0><0><1><0><1><192><12><0>
  <1><0><1><0><1>Q<128><0><4><195><0>d<253><4>rt
  fm<2>be<0><0><2><0><1><0><1>Q<128><0><5>
  <2>ns<192>-<192>-<0><2><0><1><0><1>Q<128><0>
  <9><2>ns<3>dns<192>2<192>@<0><1><0><1>
  <0><1>Q<128><0><4><195><0>d<253><192>Q<0><1><0><1>
  <0><0><26><132><0><4><134>:J!
}
end.
