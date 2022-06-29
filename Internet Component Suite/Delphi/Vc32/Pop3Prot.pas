{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Object:       TPop3Cli class implements the POP3 protocol
              (RFC-1225, RFC-1939)
Creation:     03 october 1997
Version:      2.11
EMail:        http://www.overbyte.be       francois.piette@overbyte.be
              http://www.rtfm.be/fpiette   francois.piette@rtfm.be
              francois.piette@pophost.eunet.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1996-2001 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@pophost.eunet.be><francois.piette@overbyte.be>

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

Updates:
Sept 09, 1997 Modified TOP to be able to request 0 lines (bug reported by
              damien@jetman.demon.co.uk)
Oct 10, 1997  V1.10. Published ProtocolState property, made TOP command
              complies with RFC-1939 as suggested by damien@jetman.demon.co.uk
              Implemented the UIDL command.
Oct 11, 1997  V1.11 Implemented the APOP command, but not tested because no
              server available to test it.
              Made internal error message look like POP3 error messages (-ERR)
Oct 28, 1997  V1.12 Modified TWSocket to handle line buffer overflow and
              TPop3Client to handle that in GetMultiLine.
Jan 10, 1998  V1.13 Made FWSocket accessible with a read only property. This
              eases DNSLookup without a supplementary TWSocket.
              Added a Port property.
Apr 01, 1998  V1.14 Adapted for BCB V3
May 05, 1998  V1.15 Changed GetMultiLine to correctly handle double dots at
              line start.
Jun 01, 1998  V1.16 Ben Robinson <zeppelin@wwa.com> found that Last did'nt
              update MsgNum and MsgSize.
Aug 05, 1998  V2.00 New asynchronous version.
Sep 19, 1998  V2.01 Corrected WSocketDataAvailable to count for the added
              nul byte at the end of buffer.
Nov 28, 1998  V2.02 Corrected exception triggered using highlevel function
              when connection or DNS lookup failed (for example using Open).
Dec 03, 1998  V2.03 Added SetErrorMessage in WSocketSessionConnected.
Dec 22, 1998  V2.04 Handle exception when connecting (will be triggered when
              an invalid port has been given).
Feb 27, 1999  V2.05 Adde State property.
Mar 07, 1999  V2.06 Made public property Connected.
Aug 20, 1999  V2.07 Revised conditional compilation, adapted for BCB4, set
              compile options same as TWSocket.
Dec 26, 1999  V2.08 Makes OnRequestDone properly called after a QUIT command.
              Special thanks to roger.morton@dial.pipex.com for his work
              about that problem.
Jul 22, 2000  V2.09 Checked for buffer overflow in WSocketDataAvailable
              as suggested by Jeroen Stolting <stolting.em@ilco.nl>
Nov 11, 2000  V2.10 Made ClearErrorMessage public. Cleared ErrorMessage when
              connecting. Thanks to Jeroen Nijk <Nijk.em@ilco.nl> for pointing
              to problem.
Nov 25, 2000  V2.11 Converted MD5 digest to lower case before sending to the
              server. Thanks to Poessler Thomas <Thomas.Poessler@uta.at> who
              found the problem and fix.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit Pop3Prot;

interface

{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$X+}           { Enable extended syntax              }
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

uses
    WinTypes, WinProcs, SysUtils, Messages, Classes, Graphics, Controls,
    Forms, Dialogs, Menus, WSocket, WinSock, MD5;

const
    Pop3CliVersion     = 211;
    CopyRight : String = ' POP3 component (c) 1997-2000 F. Piette V2.11 ';
{$IFDEF VER80}
    { Delphi 1 has a 255 characters string limitation }
    POP3_RCV_BUF_SIZE = 255;
{$ELSE}
    POP3_RCV_BUF_SIZE = 4096;
{$ENDIF}
    WM_POP3_REQUEST_DONE = WM_USER + 1;

type
    Pop3Exception = class(Exception);
    TPop3Display  = procedure(Sender: TObject; Msg : String) of object;
    TPop3ProtocolState  = (pop3Disconnected,  pop3WaitingUser,
                           pop3WaitingPass,   pop3Transaction);
    TPop3State    = (pop3Ready,         pop3DnsLookup,       pop3Connecting,
                     pop3Connected,     pop3InternalReady,
                     pop3WaitingBanner, pop3WaitingResponse, pop3Abort);
    TPop3Request  = (pop3Connect, pop3User, pop3Pass, pop3RPop, pop3Quit,
                     pop3Stat,    pop3List, pop3Retr, pop3Top,  pop3Dele,
                     pop3Noop,    pop3Last, pop3RSet, pop3Uidl, pop3APop,
                     pop3Open,    pop3Custom);
    TPop3Fct      = (pop3FctNone,   pop3FctConnect, pop3FctUser, pop3FctPass,
                     pop3FctRPop,   pop3FctQuit,    pop3FctAPop, pop3FctStat,
                     pop3FctList, pop3FctUidl, pop3FctRetr, pop3FctTop,
                     pop3FctDele, pop3FctNoop, pop3FctRSet, pop3FctLast);
    TPop3FctSet   = set of TPop3Fct;
    TPop3NextProc = procedure of object;
    TPop3RequestDone        = procedure(Sender  : TObject;
                                        RqType    : TPop3Request;
                                        Error     : Word) of object;
    TPop3Method   = function : boolean of object;
    TCustomPop3Cli = class(TComponent)
    private
        FWSocket            : TWSocket;
        FWindowHandle       : HWND;
        FState              : TPop3State;
        FNextProtocolState  : TPop3ProtocolState;
        FProtocolState      : TPop3ProtocolState;
        FConnected          : Boolean;
        FRequestType        : TPop3Request;
        FRequestDoneFlag    : Boolean;
        FReceiveLen         : Integer;
        FRequestResult      : Integer;
        FStatusCode         : Integer;
        FReceiveBuffer      : array [0..POP3_RCV_BUF_SIZE - 1] of char;
        FNext               : TPop3NextProc;
        FWhenConnected      : TPop3NextProc;
        FFctSet             : TPop3FctSet;
        FFctPrv             : TPop3Fct;
        FHighLevelResult    : Integer;
        FHighLevelFlag      : Boolean;
        FNextRequest        : TPop3NextProc;
        FLastResponseSave   : String;
        FStatusCodeSave     : Integer;
        FRestartFlag        : Boolean;
        FDoneAsync          : TPop3NextProc;
        FMultiLineLine      : TNotifyEvent;
        FMultiLineEnd       : TNotifyEvent;
        FMultiLineProcess   : TNotifyEvent;
        FHost           : String;
        FPort           : String;
        FUserName       : String;
        FPassWord       : String;
        FLastResponse   : String;
        FErrorMessage   : String;
        FTimeStamp      : String;
        FMsgCount       : Integer;
        FMsgSize        : Integer;
        FMsgNum         : Integer;
        FMsgUidl        : String;
        FMsgLines       : Integer;
        FTag            : LongInt;
        FWaitingOnQuit  : Boolean;

        FOnDisplay      : TPop3Display;
        FOnMessageBegin : TNotifyEvent;
        FOnMessageEnd   : TNotifyEvent;
        FOnMessageLine  : TNotifyEvent;
        FOnListBegin    : TNotifyEvent;
        FOnListEnd      : TNotifyEvent;
        FOnListLine     : TNotifyEvent;
        FOnUidlBegin    : TNotifyEvent;
        FOnUidlEnd      : TNotifyEvent;
        FOnUidlLine     : TNotifyEvent;
        FOnStateChange      : TNotifyEvent;
        FOnRequestDone      : TPop3RequestDone;
        FOnResponse         : TPop3Display;
        FOnSessionConnected : TSessionConnected;
        FOnSessionClosed    : TSessionClosed;
    protected
        procedure   ExecAsync(RqType      : TPop3Request;
                              Cmd         : String;
                              NextState   : TPop3ProtocolState;
                              DoneAsync   : TPop3NextProc);
        procedure   NextExecAsync;
        procedure   StartTransaction(OpCode      : String;
                                     Params      : String;
                                     RqType      : TPop3Request;
                                     NextState   : TPop3ProtocolState;
                                     DoneTrans   : TPop3NextProc);
        procedure   StartMultiLine(aOnBegin : TNotifyEvent;
                                   aOnLine  : TNotifyEvent;
                                   aOnEnd   : TNotifyEvent;
                                   aProcess : TNotifyEvent);
        procedure   GetALine;
        procedure   StatDone;
        procedure   ListAllDone;
        procedure   ListSingleDone;
        procedure   UidlAllDone;
        procedure   UidlSingleDone;
        procedure   RetrDone;
        procedure   LastDone;
        procedure   WndProc(var MsgRec: TMessage); virtual;
        procedure   WMPop3RequestDone(var msg: TMessage);
                        message WM_POP3_REQUEST_DONE;
        procedure   WSocketDnsLookupDone(Sender: TObject; Error: Word);
        procedure   WSocketSessionConnected(Sender: TObject; Error: Word);
        procedure   WSocketDataAvailable(Sender: TObject; Error: Word);
        procedure   WSocketSessionClosed(Sender : TObject; Error : WORD);
        procedure   DisplayLastResponse;
        procedure   TriggerDisplay(Msg : String);
        procedure   TriggerSessionConnected(Error : Word); virtual;
        procedure   TriggerSessionClosed(Error : Word);
        procedure   TriggerResponse(Msg : String); virtual;
        procedure   TriggerStateChange; virtual;
        procedure   TriggerRequestDone(Error: Word); virtual;
        function    OkResponse : Boolean;
        procedure   StateChange(NewState : TPop3State);
        procedure   Notification(AComponent: TComponent; Operation: TOperation); override;
        procedure   SetErrorMessage;
        procedure   Display(Msg : String);
        procedure   SendCommand(Cmd : String);
        function    ExtractNumbers(var N1 : Integer; var N2 : Integer) : Boolean;
        function    ExtractUidl(var N1 : Integer; var N2 : String) : Boolean;
        procedure   ProcessUidl(Sender : TObject);
        procedure   ProcessList(Sender : TObject);
        procedure   CheckReady;
        procedure   DoHighLevelAsync;
    public
        constructor Create(AOwner : TComponent); override;
        destructor  Destroy; override;
        procedure   Connect; virtual;
        procedure   Open; virtual;
        procedure   User; virtual;
        procedure   Pass; virtual;
        procedure   RPop; virtual;
        procedure   APop; virtual;
        procedure   Quit; virtual;
        procedure   Stat; virtual;
        procedure   List; virtual;
        procedure   Retr; virtual;
        procedure   Top;  virtual;
        procedure   Dele; virtual;
        procedure   Noop; virtual;
        procedure   Last; virtual;
        procedure   RSet; virtual;
        procedure   Uidl; virtual;
        procedure   Abort; virtual;
        procedure   ClearErrorMessage;
        procedure   HighLevelAsync(RqType : TPop3Request; Fcts : TPop3FctSet);

        property WSocket : TWSocket                  read  FWSocket;
        property Host : String                       read  FHost
                                                     write FHost;
        property Port : String                       read  FPort
                                                     write FPort;
        property UserName : String                   read  FUserName
                                                     write FUserName;
        property PassWord : String                   read  FPassWord
                                                     write FPassWord;
        property ErrorMessage  : String              read  FErrorMessage;
        property LastResponse  : String              read  FLastResponse;
        property State         : TPop3State          read  FState;
        property Connected     : Boolean             read  FConnected;
        property ProtocolState : TPop3ProtocolState  read  FProtocolState;
        {:Updated by the Stat method with the number of
          messages in the maildrop }
        property MsgCount : Integer                  read  FMsgCount;
        {:Updated by the Stat method with the total size
          in byte for the messages in the maildrop }
        property MsgSize : Integer                   read  FMsgSize;
        {:This is the number of lines to display in the TOP command
          Set to zero if you wants the default value }
        property MsgLines : Integer                  read  FMsgLines
                                                     write FMsgLines;
        {:This is the message number which must be returned by the Retr
          method. It is also updated by the Last method }
        property MsgNum : Integer                    read  FMsgNum
                                                     write FMsgNum;
        property MsgUidl : String                    read  FMsgUidl;
        property Tag : LongInt                       read  FTag
                                                     write FTag;
        property Handle  : HWND                      read  FWindowHandle;

        property OnDisplay : TPop3Display            read  FOnDisplay
                                                     write FOnDisplay;
        property OnMessageBegin : TNotifyEvent       read  FOnMessageBegin
                                                     write FOnMessageBegin;
        property OnMessageEnd : TNotifyEvent         read  FOnMessageEnd
                                                     write FOnMessageEnd;
        property OnMessageLine : TNotifyEvent        read  FOnMessageLine
                                                     write FOnMessageLine;
        property OnListBegin : TNotifyEvent          read  FOnListBegin
                                                     write FOnListBegin;
        property OnListEnd : TNotifyEvent            read  FOnListEnd
                                                     write FOnListEnd;
        property OnListLine : TNotifyEvent           read  FOnListLine
                                                     write FOnListLine;
        property OnUidlBegin : TNotifyEvent          read  FOnUidlBegin
                                                     write FOnUidlBegin;
        property OnUidlEnd : TNotifyEvent            read  FOnUidlEnd
                                                     write FOnUidlEnd;
        property OnUidlLine : TNotifyEvent           read  FOnUidlLine
                                                     write FOnUidlLine;
        property OnStateChange : TNotifyEvent        read  FOnStateChange
                                                     write FOnStateChange;
        property OnRequestDone : TPop3RequestDone    read  FOnRequestDone
                                                     write FOnRequestDone;
        property OnResponse: TPop3Display            read  FOnResponse
                                                     write FOnResponse;
        property OnSessionConnected : TSessionConnected
                                                     read  FOnSessionConnected
                                                     write FOnSessionConnected;
        property OnSessionClosed : TSessionClosed
                                                     read  FOnSessionClosed
                                                     write FOnSessionClosed;
    end;

    TPop3Cli = class(TCustomPop3Cli)
    published
        property Host;
        property Port;
        property UserName;
        property PassWord;
        property ErrorMessage;
        property LastResponse;
        property ProtocolState;
        property MsgCount;
        property MsgSize;
        property MsgLines;
        property MsgNum;
        property MsgUidl;
        property Tag;
        property OnDisplay;
        property OnMessageBegin;
        property OnMessageEnd;
        property OnMessageLine;
        property OnListBegin;
        property OnListEnd;
        property OnListLine;
        property OnUidlBegin;
        property OnUidlEnd;
        property OnUidlLine;
        property OnStateChange;
        property OnRequestDone;
        property OnResponse;
        property OnSessionConnected;
        property OnSessionClosed;
    end;

    { TSyncPop3Cli add synchronous functions. You should avoid using this   }
    { component because synchronous function, apart from being easy, result }
    { in lower performance programs.                                        }
    TSyncPop3Cli = class(TPop3Cli)
    protected
        FTimeout       : Integer;                 { Given in seconds }
        FTimeStop      : LongInt;                 { Milli-seconds    }
        FMultiThreaded : Boolean;
        function WaitUntilReady : Boolean; virtual;
        function Synchronize(Proc : TPop3NextProc) : Boolean;
    public
        constructor Create(AOwner : TComponent); override;
        function    ConnectSync  : Boolean; virtual;
        function    OpenSync     : Boolean; virtual;
        function    UserSync     : Boolean; virtual;
        function    PassSync     : Boolean; virtual;
        function    RPopSync     : Boolean; virtual;
        function    APopSync     : Boolean; virtual;
        function    QuitSync     : Boolean; virtual;
        function    StatSync     : Boolean; virtual;
        function    ListSync     : Boolean; virtual;
        function    RetrSync     : Boolean; virtual;
        function    TopSync      : Boolean; virtual;
        function    DeleSync     : Boolean; virtual;
        function    NoopSync     : Boolean; virtual;
        function    LastSync     : Boolean; virtual;
        function    RSetSync     : Boolean; virtual;
        function    UidlSync     : Boolean; virtual;
        function    AbortSync    : Boolean; virtual;
    published
        property Timeout : Integer       read  FTimeout
                                         write FTimeout;
        property MultiThreaded : Boolean read  FMultiThreaded
                                         write FMultiThreaded;
    end;

procedure Register;

implementation


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF VER80}
procedure SetLength(var S: string; NewLength: Integer);
begin
    S[0] := chr(NewLength);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function RTrim(Str : String) : String;
var
    i : Integer;
begin
    i := Length(Str);
    while (i > 0) and (Str[i] = ' ') do
        i := i - 1;
    Result := Copy(Str, 1, i);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function LTrim(Str : String) : String;
var
    i : Integer;
begin
    if Str[1] <> ' ' then             { Petite optimisation: pas d'espace   }
        Result := Str
    else begin
        i := 1;
        while (i <= Length(Str)) and (Str[i] = ' ') do
            i := i + 1;
        Result := Copy(Str, i, Length(Str) - i + 1);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Trim(Str : String) : String;
begin
    Result := LTrim(Rtrim(Str));
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function stpblk(PValue : PChar) : PChar;
begin
    Result := PValue;
    while Result^ in [' ', #9, #10, #13] do
        Inc(Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function atoi(PValue : PChar) : Integer;
begin
    Result := 0;
    PValue := stpblk(PValue);
    while PValue^ in ['0'..'9'] do begin
        Result := Result * 10 + ord(PValue^) - ord('0');
        Inc(PValue);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TCustomPop3Cli.Create(AOwner : TComponent);
begin
    inherited Create(AOwner);
    FWindowHandle            := AllocateHWnd(WndProc);
    FWSocket                 := TWSocket.Create(nil);
    FWSocket.OnSessionClosed := WSocketSessionClosed;
    FProtocolState           := pop3Disconnected;
    FState                   := pop3Ready;
    FPort                    := 'pop3';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TCustomPop3Cli.Destroy;
begin
    if Assigned(FWSocket) then begin
        FWSocket.Destroy;
        FWSocket := nil;
    end;
    DeallocateHWnd(FWindowHandle);
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.WndProc(var MsgRec: TMessage);
begin
     with MsgRec do begin
         case Msg of
         WM_POP3_REQUEST_DONE : WMPop3RequestDone(MsgRec);
         else
             Result := DefWindowProc(Handle, Msg, wParam, lParam);
         end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.WMPop3RequestDone(var msg: TMessage);
begin
    if Assigned(FOnRequestDone) then
        FOnRequestDone(Self, FRequestType, Msg.LParam);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.Notification(AComponent: TComponent; Operation: TOperation);
begin
    inherited Notification(AComponent, Operation);
    if Operation = opRemove then begin
        if AComponent = FWSocket then
            FWSocket := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.WSocketDnsLookupDone(Sender: TObject; Error: Word);
begin
    if Error <> 0 then begin
        FLastResponse  := '-ERR ' + WSocketErrorDesc(Error) +
                          ' (Winsock error #' + IntToStr(Error) + ')';
        FStatusCode    := 500;
        FRequestResult := Error;      { V2.02 }
        SetErrorMessage;
        TriggerRequestDone(Error);
    end
    else begin
        FWSocket.Addr  := FWSocket.DnsResult;
        FWSocket.Proto := 'tcp';
        FWSocket.Port  := FPort;
        FWSocket.OnSessionConnected := WSocketSessionConnected;
        FWSocket.OnDataAvailable    := WSocketDataAvailable;
        StateChange(pop3Connecting);
        try
            FWSocket.Connect;
        except
            on E:Exception do begin
                FLastResponse  := '-ERR ' + E.ClassName + ': ' + E.Message;
                FStatusCode    := 500;
                FRequestResult := FStatusCode;
                SetErrorMessage;
                TriggerRequestDone(FStatusCode);
            end;
        end
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.WSocketSessionConnected(Sender: TObject; Error: Word);
begin
    { Do not trigger the client SessionConnected from here. We must wait }
    { to have received the server banner.                                }
    if Error <> 0 then begin
        FLastResponse  := '-ERR ' + WSocketErrorDesc(Error) +
                          ' (Winsock error #' + IntToStr(Error) + ')';
        FStatusCode    := 500;
        FConnected     := FALSE;
        FRequestResult := Error;      { V2.02 }
        SetErrorMessage;              { V2.03 }
        TriggerRequestDone(Error);
        FWSocket.Close;
        StateChange(pop3Ready);
    end
    else begin
        FConnected := TRUE;
        StateChange(pop3WaitingBanner);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.WSocketSessionClosed(Sender : TObject; Error : WORD);
begin
    FConnected := FALSE;
    TriggerSessionClosed(Error);
    TriggerRequestDone(WSAEINTR);
    FProtocolState := pop3Disconnected;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.WSocketDataAvailable(Sender: TObject; Error: Word);
var
    Len       : Integer;
    I, J      : Integer;
    Remaining : Integer;
begin
    { Compute remaining space in our buffer. Preserve 3 bytes for CR/LF   }
    { and nul terminating byte.                                           }
    Remaining := SizeOf(FReceiveBuffer) - FReceiveLen - 3;
    if Remaining <= 0 then begin
        { Received message has a line longer than our buffer. This is not }
        { acceptable ! We will add a CR/LF to enable processing, but this }
        { will ALTER received message and could cause strange results.    }
        { May be it is better to raise an exception ?                     }
        FReceiveBuffer[SizeOf(FReceiveBuffer) - 3] := #13;
        FReceiveBuffer[SizeOf(FReceiveBuffer) - 2] := #10;
        Len := 2;
    end
    else begin
        Len := FWSocket.Receive(@FReceiveBuffer[FReceiveLen], Remaining);
        if Len <= 0 then
            Exit;
    end;

    FReceiveBuffer[FReceiveLen + Len] := #0;
    FReceiveLen := FReceiveLen + Len;

    while FReceiveLen > 0 do begin
        { Search CRLF pair. We can't use Pos because it stops at first #0 }
        I := 1;
        while (I < FReceiveLen) and
              (FReceiveBuffer[I - 1] <> #13) and (FReceiveBuffer[I] <> #10) do
            Inc(I);
        if I >= FReceiveLen then
            break;                   { CRLF not found }

        { Found a CRLF. Extract data from buffer }
        FLastResponse := Copy(FReceiveBuffer, 1, I - 1);
        TriggerResponse(FLastResponse);

{$IFDEF DUMP}
        FDumpBuf := '>|';
        FDumpStream.WriteBuffer(FDumpBuf[1], Length(FDumpBuf));
        FDumpStream.WriteBuffer(FLastResponse[1], Length(FLastResponse));
        FDumpBuf := '|' + #13#10;
        FDumpStream.WriteBuffer(FDumpBuf[1], Length(FDumpBuf));
{$ENDIF}
{$IFDEF VER80}
        { Add a nul byte at the end of string for Delphi 1 }
        FLastResponse[Length(FLastResponse) + 1] := #0;
{$ENDIF}
        FReceiveLen := FReceiveLen - I - 1;
        if FReceiveLen > 0 then
            Move(FReceiveBuffer[I + 1], FReceiveBuffer[0], FReceiveLen + 1);

        if FState = pop3WaitingBanner then begin
            DisplayLastResponse;
            if not OkResponse then begin
                SetErrorMessage;
                FRequestResult := FStatusCode;
                FWSocket.Close;
                Exit;
            end;
            I := Pos('<', FLastResponse);
            J := Pos('>', Copy(FLastResponse, I, Length(FLastREsponse)));
            if (I > 0) and (J > 0) then
                FTimeStamp := Copy(FLastResponse, I, J);

            FProtocolState := pop3WaitingUser;
            StateChange(pop3Connected);
            TriggerSessionConnected(Error);

            if Assigned(FWhenConnected) then
                FWhenConnected
            else begin
                TriggerRequestDone(0);
            end;
        end
        else if FState = pop3WaitingResponse then begin
            if Assigned(FNext) then
                FNext
            else
                raise Pop3Exception.Create('Program error: FNext is nil');
        end
        else begin
            { Unexpected data received }
            DisplayLastResponse;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.TriggerResponse(Msg : String);
begin
    if Assigned(FOnResponse) then
        FOnResponse(Self, Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.TriggerStateChange;
begin
    if Assigned(FOnStateChange) then
        FOnStateChange(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.TriggerRequestDone(Error: Word);
begin
    { Special processing for Quit (Roger Morton 24-12-99) }
    if FRequestType = pop3Quit then begin
        if FWaitingOnQuit then
            { When the second RqDone arrives (from WSocketSessionClosed),   }
            { treat it as a normal event by setting a zero Error code       }
            Error := 0
        else begin
            { When the first RqDone arrives, set the FWaitingOnQuit flag so }
            { we're ready to handle a second RqDone.                        }
            { Take no other action (in particular, we don't advise the user }
            { that the first RqDone has happened)                           }
            FWaitingOnQuit := True;
      	    Exit;
        end;
        { Fall down here for all normal RqDone, and after the second RqDone }
	{ following a Quit                                                  }
        FWaitingOnQuit := False;
    end;

    if not FRequestDoneFlag then begin
        FRequestDoneFlag := TRUE;
        if Assigned(FNextRequest) then begin
            if FState <> pop3Abort then
                StateChange(pop3InternalReady);
            FNextRequest;
        end
        else begin
            StateChange(pop3Ready);
            { Restore the lastresponse saved before quit command }
            if FHighLevelFlag and (FStatusCodeSave >= 0) then begin
                 FLastResponse := FLastResponseSave;
                 FStatusCode   := FStatusCodeSave;
            end;
            FHighLevelFlag := FALSE;
            PostMessage(Handle, WM_POP3_REQUEST_DONE, 0, Error);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.TriggerDisplay(Msg : String);
begin
    if Assigned(FOnDisplay) then
        FOnDisplay(Self, Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.TriggerSessionConnected(Error : Word);
begin
    if Assigned(FOnSessionConnected) then
        FOnSessionConnected(Self, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.TriggerSessionClosed(Error : Word);
begin
    if Assigned(FOnSessionClosed) then
        FOnSessionClosed(Self, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.DoHighLevelAsync;
begin
{$IFDEF TRACE} TriggerDisplay('! HighLevelAsync ' + IntToStr(FRequestResult)); {$ENDIF}
    if FState = pop3Abort then begin
        {$IFDEF TRACE} TriggerDisplay('! Abort detected'); {$ENDIF}
        FFctSet := [];
        FHighLevelResult := 426;
        FErrorMessage    := '426 Operation aborted.';
    end;

    FNextRequest := DoHighLevelAsync;

    if FRequestResult <> 0 then begin
        { Previous command had errors }
        FHighLevelResult := FRequestResult;
        if (FFctPrv = pop3FctQuit) or (not (pop3FctQuit in FFctSet)) then
            FFctSet := []
        else
            FFctSet := [pop3FctQuit];
    end;

    if pop3FctConnect in FFctSet then begin
        FFctPrv := pop3FctConnect;
        FFctSet := FFctSet - [FFctPrv];
        Connect;
        Exit;
    end;

    if pop3FctUser in FFctSet then begin
        FFctPrv := pop3FctUser;
        FFctSet := FFctSet - [FFctPrv];
        User;
        Exit;
    end;

    if pop3FctPass in FFctSet then begin
        FFctPrv := pop3FctPass;
        FFctSet := FFctSet - [FFctPrv];
        Pass;
        Exit;
    end;

    if pop3FctRPop in FFctSet then begin
        FFctPrv := pop3FctRPop;
        FFctSet := FFctSet - [FFctPrv];
        RPop;
        Exit;
    end;

    if pop3FctDele in FFctSet then begin
        FFctPrv := pop3FctDele;
        FFctSet := FFctSet - [FFctPrv];
        Dele;
        Exit;
    end;

    if pop3FctNoop in FFctSet then begin
        FFctPrv := pop3FctNoop;
        FFctSet := FFctSet - [FFctPrv];
        Noop;
        Exit;
    end;

    if pop3FctList in FFctSet then begin
        FFctPrv := pop3FctList;
        FFctSet := FFctSet - [FFctPrv];
        List;
        Exit;
    end;

    if pop3FctRSet in FFctSet then begin
        FFctPrv := pop3FctRSet;
        FFctSet := FFctSet - [FFctPrv];
        RSet;
        Exit;
    end;

    if pop3FctAPop in FFctSet then begin
        FFctPrv := pop3FctAPop;
        FFctSet := FFctSet - [FFctPrv];
        APop;
        Exit;
    end;

    if pop3FctRetr in FFctSet then begin
        FFctPrv := pop3FctRetr;
        FFctSet := FFctSet - [FFctPrv];
        Retr;
        Exit;
    end;

    if pop3FctTop in FFctSet then begin
        FFctPrv := pop3FctTop;
        FFctSet := FFctSet - [FFctPrv];
        Top;
        Exit;
    end;

    if pop3FctStat in FFctSet then begin
        FFctPrv := pop3FctStat;
        FFctSet := FFctSet - [FFctPrv];
        Stat;
        Exit;
    end;

    if pop3FctUidl in FFctSet then begin
        FFctPrv := pop3FctUidl;
        FFctSet := FFctSet - [FFctPrv];
        Uidl;
        Exit;
    end;

    if pop3FctLast in FFctSet then begin
        FFctPrv := pop3FctLast;
        FFctSet := FFctSet - [FFctPrv];
        Last;
        Exit;
    end;

    if pop3FctQuit in FFctSet then begin
        FFctPrv := pop3FctQuit;
        FFctSet := FFctSet - [FFctPrv];
        Quit;
        Exit;
    end;

    {$IFDEF TRACE} TriggerDisplay('! HighLevelAsync done'); {$ENDIF}
    FFctSet          := [];
    FNextRequest     := nil;
    FRequestDoneFlag := FALSE;
    TriggerRequestDone(FHighLevelResult);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.HighLevelAsync(
    RqType : Tpop3Request; Fcts : Tpop3FctSet);
begin
    if FConnected and (pop3FctConnect in Fcts) then
        raise pop3Exception.Create('pop3 component already connected');
    CheckReady;
    FLastResponseSave := FLastResponse;
    FStatusCodeSave   := -1;
    FRequestType      := RqType;
    FRequestResult    := 0;
    FFctSet           := Fcts;
    FFctPrv           := pop3FctNone;
    FHighLevelResult  := 0;
    FHighLevelFlag    := TRUE;
    FLastResponse     := '';
    FRestartFlag      := FALSE;
    ClearErrorMessage;
    DoHighLevelAsync;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.ProcessUidl(Sender : TObject);
begin
    ExtractUidl(FMsgNum, FMsgUidl);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.ProcessList(Sender : TObject);
begin
    ExtractNumbers(FMsgNum, FMsgSize);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomPop3Cli.ExtractUidl(var N1 : Integer; var N2 : String) : Boolean;
var
    p : PChar;
begin
    Result := FALSE;
    N1     := 0;
    N2     := '';

{$IFDEF VER80}
    { Delphi 1 do not automatically nul terminate strings }
    FLastResponse := FLastResponse + #0;
{$ENDIF}

    { Search for first digit in response }
    p := @FLastResponse[1];
    while (p^ <> #0) and (not (p^ in ['0'..'9'])) do
        Inc(p);
    if p^ = #0 then { Invalid response, need a number }
        Exit;

    { Convert first number }
    N1 := atoi(p);

    { Search end of number }
    while (p^ <> #0) and (p^ in ['0'..'9']) do
        Inc(p);

    { Search Uidl }
    while (p^ = ' ') do
        Inc(p);

    { Copy UIDL }
    while (p^ <> #0) and (p^ in [#33..#126]) do begin
        N2 := N2 + p^;
        Inc(p);
    end;

    Result := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomPop3Cli.ExtractNumbers(var N1 : Integer; var N2 : Integer) : Boolean;
var
    p : PChar;
begin
    Result := FALSE;

{$IFDEF VER80}
    { Delphi 1 do not automatically nul terminate strings }
    FLastResponse := FLastResponse + #0;
{$ENDIF}

    { Search for first digit in response }
    p := @FLastResponse[1];
    while (p^ <> #0) and (not (p^ in ['0'..'9'])) do
        Inc(p);
    if p^ = #0 then begin
        { Invalid response, need a number }
        N1 := 0;
        N2 := 0;
        Exit;
    end;

    { Convert first number }
    N1 := atoi(p);

    { Search end of number }
    while (p^ <> #0) and (p^ in ['0'..'9']) do
        Inc(p);

    { Search next number }
    p := stpblk(p);

    if p^ = #0 then begin
        { Invalid response, need a number }
        N1 := 0;
        N2 := 0;
        Exit;
    end;

    N2     := atoi(p);
    Result := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.SendCommand(Cmd : String);
begin
    Display('> ' + Cmd);
    Application.ProcessMessages;
    FWSocket.SendStr(Cmd + #13 + #10);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomPop3Cli.OkResponse : Boolean;
begin
    Result := ((Length(FLastResponse) > 0) and (FLastResponse[1] = '+'));
    if Result then
        FStatusCode := 0
    else
        FStatusCode := 500;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.Display(Msg : String);
begin
    if Assigned(FOnDisplay) then
        FOnDisplay(Self, Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.ClearErrorMessage;
begin
    FErrorMessage := '';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.SetErrorMessage;
begin
    if FErrorMessage = '' then
        FErrorMessage := FLastResponse;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.CheckReady;
begin
    if not (FState in [pop3Ready, pop3InternalReady]) then
        raise pop3Exception.Create('POP3 component not ready');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.StateChange(NewState : TPop3State);
begin
    if FState <> NewState then begin
        FState := NewState;
        TriggerStateChange;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.DisplayLastResponse;
begin
     TriggerDisplay('< ' + FLastResponse);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.ExecAsync(
    RqType      : TPop3Request;
    Cmd         : String;             { Command to execute                     }
    NextState   : TPop3ProtocolState; { Next protocol state in case of success }
    DoneAsync   : TPop3NextProc);     { What to do when done                   }
begin
    CheckReady;

    if not FConnected then
        raise Pop3Exception.Create('POP3 component not connected');

    if not FHighLevelFlag then
        FRequestType := RqType;

    FRequestDoneFlag   := FALSE;
    FNext              := NextExecAsync;
    FNextProtocolState := NextState;
    FDoneAsync         := DoneAsync;
    StateChange(pop3WaitingResponse);
    SendCommand(Cmd);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.NextExecAsync;
begin
    DisplayLastResponse;

    if not OkResponse then begin
        FRequestResult := FStatusCode;
        SetErrorMessage;
        TriggerRequestDone(FRequestResult);
        Exit;
    end;

    FRequestResult := 0;
    FProtocolState := FNextProtocolState;

    if Assigned(FDoneAsync) then
        FDoneAsync
    else
        TriggerRequestDone(FRequestResult);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.User;
begin
    if FProtocolState > pop3WaitingUser then begin
        FErrorMessage := '-ERR USER command invalid now';
        Display(FErrorMessage);
        raise Pop3Exception.Create(FErrorMessage);
    end;

    FFctPrv := pop3FctUser;
    ExecAsync(pop3User, 'USER ' + Trim(FUserName), pop3WaitingPass, nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.Connect;
begin
    CheckReady;
    if FConnected then
        raise Pop3Exception.Create('POP3 component already connected');

    if not FHighLevelFlag then
        FRequestType  := pop3Connect;

    FRequestDoneFlag  := FALSE;
    FReceiveLen       := 0;
    FRequestResult    := 0;
    StateChange(pop3DnsLookup);
    ClearErrorMessage;
    FWSocket.OnDataSent      := nil;
    FWSocket.OnDnsLookupDone := WSocketDnsLookupDone;
    FWSocket.DnsLookup(FHost);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.Abort;
begin
    StateChange(pop3Abort);
    FWSocket.CancelDnsLookup;
    FWSocket.Abort;
    StateChange(pop3Ready);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.Pass;
begin
    if FProtocolState > pop3WaitingPass then begin
        FErrorMessage := '-ERR PASS command invalid now';
        Display(FErrorMessage);
        raise Pop3Exception.Create(FErrorMessage);
    end;

    FFctPrv := pop3FctPass;
    ExecAsync(pop3Pass, 'PASS ' + Trim(FPassWord), pop3Transaction, nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.RPop;
begin
    if FProtocolState > pop3WaitingPass then begin
        FErrorMessage := '-ERR RPOP command invalid now';
        Display(FErrorMessage);
        raise Pop3Exception.Create(FErrorMessage);
    end;

    FFctPrv := pop3FctRPop;
    ExecAsync(pop3RPop, 'RPOP ' + Trim(FPassWord), pop3Transaction, nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.APop;
begin
    if FProtocolState <> pop3WaitingUser then begin
        FErrorMessage := '-ERR APOP command invalid now';
        Display(FErrorMessage);
        raise Pop3Exception.Create(FErrorMessage);
    end;

    if FTimeStamp = '' then begin
        FErrorMessage := '-ERR Server do not support APOP (no timestamp)';
        Display(FErrorMessage);
        raise Pop3Exception.Create(FErrorMessage);
    end;

    FFctPrv := pop3FctAPop;
    ExecAsync(pop3APop, 'APOP ' + Trim(FUserName) + ' ' +
                        LowerCase(StrMD5(FTimeStamp + FPassWord)),
                        pop3Transaction, nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.Quit;
begin
    CheckReady;
    FFctPrv := pop3FctQuit;
    if not FConnected then begin
        { We are not connected, it's ok... }
        FRequestType     := pop3Quit;
        FRequestDoneFlag := FALSE;
        TriggerRequestDone(0);
        Exit;
    end;
    ExecAsync(pop3Quit, 'QUIT', pop3Disconnected, nil); { Should I force a FWSocket.Close }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.Stat;
begin
    FFctPrv := pop3FctStat;
    StartTransaction('STAT', '', pop3Stat, pop3Transaction, StatDone);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.StatDone;
begin
    ExtractNumbers(FMsgCount, FMsgSize);
    TriggerRequestDone(FRequestResult);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.List;
begin
    FFctPrv := pop3FctList;
    if FMsgNum <= 0 then
        { Scan LIST command (all messages) }
        StartTransaction('LIST', '', pop3List, pop3Transaction, ListAllDone)
    else
        { Single message LIST command }
        StartTransaction('LIST', IntToStr(FMsgNum), pop3List,
                         pop3Transaction, ListSingleDone);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.Uidl;
begin
    FFctPrv := pop3FctUidl;
    if FMsgNum <= 0 then
        { UIDL command (all messages) }
        StartTransaction('UIDL', '', pop3Uidl, pop3Transaction, UidlAllDone)
    else
        { Single message UIDL command }
        StartTransaction('UIDL', IntToStr(FMsgNum), pop3Uidl,
                         pop3Transaction, UidlSingleDone);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.UidlAllDone;
begin
    StartMultiLine(FOnUidlBegin, FOnUidlLine, FOnUidlEnd, ProcessUidl);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.UidlSingleDone;
begin
    ExtractUidl(FMsgNum, FMsgUidl);
    TriggerRequestDone(FRequestResult);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.ListSingleDone;
begin
    ExtractNumbers(FMsgNum, FMsgSize);
    TriggerRequestDone(FRequestResult);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.ListAllDone;
begin
    StartMultiLine(FOnListBegin, FOnListLine, FOnListEnd, ProcessList);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.Retr;
begin
    FFctPrv := pop3FctRetr;
    StartTransaction('RETR',   IntToStr(FMsgNum),
                     pop3Retr, pop3Transaction, RetrDone);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.Top;
begin
    if FMsgLines < 0 then
        raise Pop3Exception.Create('Invalid MsgLines for TOP command');
    FFctPrv := pop3FctTop;
    StartTransaction('TOP', IntToStr(FMsgNum) + ' ' + IntToStr(FMsgLines),
                     pop3Top, pop3Transaction, RetrDone);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.RetrDone;
begin
    StartMultiLine(FOnMessageBegin, FOnMessageLine, FOnMessageEnd, nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.Dele;
begin
    FFctPrv := pop3FctDele;
    StartTransaction('DELE', IntToStr(FMsgNum),
                     pop3Dele, pop3Transaction, nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.Noop;
begin
    FFctPrv := pop3FctNoop;
    StartTransaction('NOOP', '', pop3Noop, pop3Transaction, nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.RSet;
begin
    FFctPrv := pop3FctRSet;
    StartTransaction('RSET', '', pop3RSet, pop3Transaction, nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.Last;
begin
    FFctPrv := pop3FctLast;
    StartTransaction('LAST', '', pop3Last, pop3Transaction, LastDone);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.LastDone;
begin
    ExtractNumbers(FMsgNum, FMsgSize);
    TriggerRequestDone(FRequestResult);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.Open;
begin
    HighLevelAsync(pop3Open, [pop3FctConnect, pop3FctUser, pop3FctPass]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.StartTransaction(
    OpCode      : String;
    Params      : String;
    RqType      : TPop3Request;
    NextState   : TPop3ProtocolState;  { Next protocol state in case of success}
    DoneTrans   : TPop3NextProc);      { What to do when done                  }
var
    Cmd : String;
begin
    if FProtocolState <> pop3Transaction then begin
        FErrorMessage := '-ERR ' + OpCode + ' command invalid now';
        Display(FErrorMessage);
        raise Pop3Exception.Create(FErrorMessage);
    end;

    Cmd := OpCode;
    if Params <> '' then
        Cmd := Cmd + ' ' + Params;
    ExecAsync(RqType, Cmd, NextState, DoneTrans);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.StartMultiLine(
    aOnBegin : TNotifyEvent;
    aOnLine  : TNotifyEvent;
    aOnEnd   : TNotifyEvent;
    aProcess : TNotifyEvent);
begin
    FMultiLineLine    := aOnLine;
    FMultiLineEnd     := aOnEnd;
    FMultiLineProcess := aProcess;

    { Let the application know that the message is beginning }
    if Assigned(aOnBegin) then
        aOnBegin(Self);

    FNext := GetALine;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.GetALine;
begin
    { Check if we are still connected }
    if not FConnected then begin
        FErrorMessage  := '-ERR Disconneced unexpectedly';
        FRequestResult := 500;
        Display(FErrorMessage);
        TriggerRequestDone(FRequestResult);
        Exit;
    end;

    { Check if end of message }
    if FLastResponse = '.' then begin
        { Let the application know that the message is finished }
        if Assigned(FMultiLineEnd) then
            FMultiLineEnd(Self);
        FLastResponse := '';
        FNext         := nil;
        TriggerRequestDone(0);
        Exit;
    end;

    { Check if message contains end-of-message mark }
    if (Length(FLastResponse) >= 2) and
       (FLastResponse[1] = '.') and (FLastResponse[2] = '.') then
        { Remove byte-stuff }
        FLastResponse := Copy(FLastResponse, 2, Length(FLastResponse));

    { Additional process }
    if Assigned(FMultiLineProcess) then
        FMultiLineProcess(Self);

    { Let the application process the message line }
    if Assigned(FMultiLineLine) then
        FMultiLineLine(Self);

    { To process next line }
    FNext := GetALine;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TSyncPop3Cli.Create(AOwner : TComponent);
begin
    inherited Create(AOwner);
    FTimeout := 15;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncPop3Cli.WaitUntilReady : Boolean;
begin
    Result := TRUE;           { Suppose success }
    FTimeStop := Integer(GetTickCount) + FTimeout * 1000;
    while TRUE do begin
        if FState = pop3Ready then begin
            { Back to ready state, the command is finiched }
            Result := (FRequestResult = 0);
            break;
        end;

        if  Application.Terminated or
            ((FTimeout > 0) and (Integer(GetTickCount) > FTimeStop)) then begin
            { Application is terminated or timeout occured }
            inherited Abort;
            FErrorMessage := '426 Timeout';
            FStatusCode   := 426;
            Result        := FALSE; { Command failed }
            break;
        end;
{$IFNDEF VER80}
        if FMultiThreaded then
            FWSocket.ProcessMessages
        else
{$ENDIF}
            Application.ProcessMessages;
{$IFNDEF VER80}
        { Do not use 100% CPU, but slow down transfert on high speed LAN }
        Sleep(0);
{$ENDIF}
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncPop3Cli.Synchronize(Proc : TPop3NextProc) : Boolean;
begin
    try
        Proc;
        Result := WaitUntilReady;
    except
        Result := FALSE;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncPop3Cli.ConnectSync : Boolean;
begin
    Result := Synchronize(Connect);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncPop3Cli.OpenSync : Boolean;
begin
    Result := Synchronize(Open);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncPop3Cli.UserSync : Boolean;
begin
    Result := Synchronize(User);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncPop3Cli.PassSync : Boolean;
begin
    Result := Synchronize(Pass);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncPop3Cli.RetrSync : Boolean;
begin
    Result := Synchronize(Retr);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncPop3Cli.DeleSync : Boolean;
begin
    Result := Synchronize(Dele);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncPop3Cli.UidlSync : Boolean;
begin
    Result := Synchronize(Uidl);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncPop3Cli.LastSync : Boolean;
begin
    Result := Synchronize(Last);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncPop3Cli.RPopSync : Boolean;
begin
    Result := Synchronize(RPop);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncPop3Cli.TopSync : Boolean;
begin
    Result := Synchronize(Top);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncPop3Cli.ListSync : Boolean;
begin
    Result := Synchronize(List);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncPop3Cli.StatSync : Boolean;
begin
    Result := Synchronize(Stat);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncPop3Cli.QuitSync : Boolean;
begin
    Result := Synchronize(Quit);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncPop3Cli.APopSync : Boolean;
begin
    Result := Synchronize(APop);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncPop3Cli.AbortSync : Boolean;
begin
    Result := Synchronize(Abort);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncPop3Cli.RSetSync : Boolean;
begin
    Result := Synchronize(RSet);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncPop3Cli.NoopSync : Boolean;
begin
    Result := Synchronize(Noop);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure Register;
begin
    RegisterComponents('FPiette', [TPop3Cli, TSyncPop3Cli]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

