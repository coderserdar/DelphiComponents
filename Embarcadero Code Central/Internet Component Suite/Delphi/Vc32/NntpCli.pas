{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  TNntpCli is a client for the NNTP protocol (RFC-977)
Creation:     December 19, 1997
Version:      1.07
EMail:        http://users.swing.be/francois.piette  francois.piette@swing.be
              http://www.rtfm.be/fpiette             francois.piette@rtfm.be
              francois.piette@pophost.eunet.be
Support:      Use the mailing list twsocket@rtfm.be See website for details.
Legal issues: Copyright (C) 1997-2000 by François PIETTE
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

Updates:
Dec 30, 1997  V0.91 Bug: StatusCode was not updated for Connect
              Added PostingPermited property and ParseListLine procedure as
              suggested by J. Peter Mugaas <oma00215@mail.wvnet.edu>
Dec 31, 1997  V0.92 Added XOVER, LIST OVERVIEW.FMT and DATE commands
Jan 10, 1998  V0.93 Added OnStateChange event as suggested by J. Peter Mugaas
              <oma00215@mail.wvnet.edu>
Jan 13, 1998  V0.94 Added readonly property State
Feb 02, 1998  V0.95 Corrected a message in the Quit method.
              Added the NntpCliVersion constant.
Feb 03, 1998  V0.96 Added Authenticate method, UserName and PassWord properties.
Apr 13, 1998  V1.00 Added an intermediate message for OnRequestDone event
              Created the Handle property and related WndProc stuff
Apr 21, 1998  V1.01 Corrected buffer overflow in the OnDataAvailable event.
              Thanks to Tim Skinner tim@palacecs.demon.co.uk who found that bug.
Sep 29, 1998  V1.02 Checked length of FLastResponse before writing it to stream.
              Thanks to Michael Bartos <MBartos@ExpoMedia.de> for the hint.
Feb 01, 1999  V1.03 Added nntpConnect to solve connection problem after an
              abort. Thanks to Eric Fortier <efortier@videotron.ca>.
Feb 27, 1999  V1.04 Made Connect, Abort and Quit method virtual so that they
              can be overriden in descending components.
              Checked line length in ParseListLine.
Mar 31, 1999  V1.05 Made all methods virtual.
Aug 14, 1999  V1.06 Implemented MODE READER and XHDR
Aug 20, 1999  V1.07 Revised conditional compilation, adapted for BCB4, set
              compile options same as TWSocket.

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit NntpCli;

{.DEFINE DUMP}

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
    WinTypes, WinProcs, SysUtils, Messages, Classes, Forms, WinSock, WSocket;

const
    NntpCliVersion     = 107;
    CopyRight : String = ' TNntpCli (c) 1997-2000 F. Piette V1.07 ';
{$IFDEF VER80}
    { Delphi 1 has a 255 characters string limitation }
    NNTP_RCV_BUF_SIZE = 255;
{$ELSE}
    NNTP_RCV_BUF_SIZE = 4096;
{$ENDIF}
  WM_NNTP_REQUEST_DONE = WM_USER + 1;

type
    TNntpDisplay = procedure(Sender: TObject; Msg : PChar; Len : Integer) of object;
    TNntpState = (nntpNotConnected, nntpDnsLookup, nntpWaitingBanner,
                  nntpReady, nntpWaitingResponse);
    TNntpRequest = (nntpGroup,           nntpList,         nntpConnect,
                    nntpPost,            nntpHelp,
                    nntpNewGroups,       nntpNewNews,
                    nntpArticleByNumber, nntpArticleByID,
                    nntpBodyByID,        nntpBodyByNumber,
                    nntpHeadByID,        nntpHeadByNumber,
                    nntpStatByID,        nntpStatByNumber,
                    nntpNext,            nntpLast,
                    nntpQuit,            nntpAbort,
                    nntpXOver,           nntpListOverViewFmt,
                    nntpDate,            nntpAuthenticate,
                    nntpModeReader,      nntpXHdr);
    TRequestDone = procedure(Sender: TObject; RqType: TNntpRequest; Error: Word) of object;

    NntpException = class(Exception);

    TNntpCli = class(TComponent)
        constructor Create(AOwner: TComponent); override;
        destructor  Destroy; override;
        procedure   Connect; virtual;
        procedure   Abort; virtual;
        procedure   Quit; virtual;
        procedure   Group(NewsGroupName : String); virtual;
        procedure   ArticleByNumber(Number : Integer; DestStream : TStream); virtual;
        procedure   ArticleByID(ID : String; DestStream : TStream); virtual;
        procedure   HeadByNumber(Number : Integer; DestStream : TStream); virtual;
        procedure   HeadByID(ID : String; DestStream : TStream); virtual;
        procedure   BodyByNumber(Number : Integer; DestStream : TStream); virtual;
        procedure   BodyByID(ID : String; DestStream : TStream); virtual;
        procedure   StatByNumber(Number : Integer); virtual;
        procedure   StatByID(ID : String); virtual;
        procedure   Next; virtual;
        procedure   Last; virtual;     { It is really Prior, but RFC-977 call it Last !}
        procedure   List(DestStream : TStream); virtual;
        procedure   Post(FromStream : TStream); virtual;
        procedure   Help(DestStream : TStream); virtual;
        procedure   Authenticate; virtual;
        procedure   XOver(Articles : String; DestStream : TStream); virtual;
        procedure   ListOverViewFmt(DestStream : TStream); virtual;
        procedure   Date; virtual;
        procedure   ModeReader; virtual;
        procedure   XHdr(DestStream : TStream;
                         Header     : String;
                         Range      : String); virtual;
        procedure   NewGroups(When          : TDateTime;
                              GMTFLag       : Boolean;
                              Distributions : String;
                              DestStream    : TStream);  virtual;
        procedure   NewNews(When          : TDateTime;
                            GMTFLag       : Boolean;
                            NewsGroupName : String;
                            Distributions : String;
                            DestStream    : TStream); virtual;
    protected
        FWindowHandle       : HWND;
{$IFDEF DUMP}
        FDumpStream         : TFileStream;
        FDumpBuf            : String;
{$ENDIF}
        FHost               : String;
        FState              : TNntpState;
        FWSocket            : TWSocket;
        FRequest            : String;
        FRequestType        : TNntpRequest;
        FRequestDoneFlag    : Boolean;
        FSentFlag           : Boolean;
        FStatusCode         : Integer;
        FSendBuffer         : array [0..NNTP_RCV_BUF_SIZE - 1] of char;
        FReceiveBuffer      : array [0..NNTP_RCV_BUF_SIZE - 1] of char;
        FReceiveLen         : Integer;
        FLastResponse       : String;
        FLastCmdResponse    : String;
        FErrorMessage       : String;
        FArticleEstimated   : Integer;
        FArticleFirst       : Integer;
        FArticleLast        : Integer;
        FArticleNumber      : Integer;
        FArticleID          : String;
        FServerDate         : TDateTime;
        FDataStream         : TStream;
        FUserName           : String;
        FPassWord           : String;
        FNext               : procedure of object;
        FPostingPermited    : Boolean;
        FOnSessionConnected : TSessionConnected;
        FOnSessionClosed    : TSessionClosed;
        FOnDataAvailable    : TDataAvailable;
        FOnRequestDone      : TRequestDone;
        FOnDisplay          : TNntpDisplay;
        FOnMessageBegin     : TNotifyEvent;
        FOnMessageEnd       : TNotifyEvent;
        FOnMessageLine      : TNotifyEvent;
        FOnXHdrBegin        : TNotifyEvent;
        FOnXHdrEnd          : TNotifyEvent;
        FOnXHdrLine         : TNotifyEvent;
        FOnStateChange      : TNotifyEvent;
        procedure WndProc(var MsgRec: TMessage);
        procedure WMNntpRequestDone(var msg: TMessage); message WM_NNTP_REQUEST_DONE;
        procedure WSocketDnsLookupDone(Sender: TObject; Error: Word);
        procedure WSocketSessionConnected(Sender: TObject; Error: Word);
        procedure WSocketDataAvailable(Sender: TObject; Error: Word);
        procedure WSocketSessionClosed(Sender: TObject; Error: Word);
        procedure WSocketDataSent(Sender: TObject; Error: Word);
        procedure TriggerRequestDone(Error: Word); virtual;
        procedure TriggerStateChange; virtual;
        procedure StateChange(NewState : TNntpState); virtual;
        procedure SendRequest; virtual;
        procedure GroupNext; virtual;
        procedure QuitNext; virtual;
        procedure XHdrLineNext; virtual;
        procedure GetArticleNext; virtual;
        procedure GetArticleLineNext; virtual;
        procedure GetArticleByNumber(RqType: TNntpRequest; Number : Integer; DestStream : TStream); virtual;
        procedure GetArticleByID(RqType: TNntpRequest; ID : String; DestStream : TStream); virtual;
        procedure GetArticle(RqType: TNntpRequest; ID : String; DestStream : TStream); virtual;
        procedure PostNext; virtual;
        procedure PostDone; virtual;
        procedure PostBlock; virtual;
        procedure PostSendNext; virtual;
        procedure DateNext; virtual;
        procedure ModeReaderNext; virtual;
        procedure XHdrNext; virtual;
        procedure AuthenticateNext1; virtual;
        procedure AuthenticateNext2; virtual;
        property  Handle        : HWND                  read FWindowHandle;
    published
        property WSocket : TWSocket                     read  FWSocket;
        property State   : TNntpState                   read  FState;
        property Host : String                          read  FHost
                                                        write FHost;
        property ErrorMessage : String                  read  FErrorMessage;
        property LastResponse : String                  read  FLastResponse;
        property StatusCode : Integer                   read  FStatusCode;
        property PostingPermited    : Boolean           read  FPostingPermited;
        property ArticleEstimated   : Integer           read  FArticleEstimated;
        property ArticleFirst       : Integer           read  FArticleFirst;
        property ArticleLast        : Integer           read  FArticleLast;
        property ArticleNumber      : Integer           read  FArticleNumber;
        property ArticleID          : String            read  FArticleID;
        property ServerDate         : TDateTime         read  FServerDate;
        property UserName           : String            read  FUserName
                                                        write FUserName;
        property PassWord           : String            read  FPassWord
                                                        write FPassWord;
        property OnSessionConnected : TSessionConnected read  FOnSessionConnected
                                                        write FOnSessionConnected;
        property OnSessionClosed : TSessionClosed       read  FOnSessionClosed
                                                        write FOnSessionClosed;
        property OnDataAvailable : TDataAvailable       read  FOnDataAvailable
                                                        write FOnDataAvailable;
        property OnRequestDone : TRequestDone           read  FOnRequestDone
                                                        write FOnRequestDone;
        property OnDisplay : TNntpDisplay               read  FOnDisplay
                                                        write FOnDisplay;
        property OnMessageBegin : TNotifyEvent          read  FOnMessageBegin
                                                        write FOnMessageBegin;
        property OnMessageEnd : TNotifyEvent            read  FOnMessageEnd
                                                        write FOnMessageEnd;
        property OnMessageLine : TNotifyEvent           read  FOnMessageLine
                                                        write FOnMessageLine;
        property OnXHdrBegin : TNotifyEvent             read  FOnXHdrBegin
                                                        write FOnXHdrBegin;
        property OnXHdrEnd : TNotifyEvent               read  FOnXHdrEnd
                                                        write FOnXHdrEnd;
        property OnXHdrLine : TNotifyEvent              read  FOnXHdrLine
                                                        write FOnXHdrLine;
        property OnStateChange : TNotifyEvent           read  FOnStateChange
                                                        write FOnStateChange;
    end;

procedure ParseListLine(const Line          : String;
                        var NewsGroupName   : String;
                        var LastArticle     : Integer;
                        var FirstArticle    : Integer;
                        var PostingFlag     : Char);
procedure Register;

implementation


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF VER80}
function TrimRight(Str : String) : String;
var
    i : Integer;
begin
    i := Length(Str);
    while (i > 0) and (Str[i] = ' ') do
        i := i - 1;
    Result := Copy(Str, 1, i);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TrimLeft(Str : String) : String;
var
    i : Integer;
begin
    if Str[1] <> ' ' then
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
    Result := TrimLeft(TrimRight(Str));
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Step over blank spaces                                                    }
function StpBlk(Data : PChar) : PChar;
begin
    Result := Data;
    if Result <> nil then begin
        while (Result^ <> #0) and (Result^ in [' ', #9, #13, #10]) do
            Inc(Result);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  GetInteger(Data : PChar; var Number : Integer) : PChar;
var
    bSign : Boolean;
begin
    Number := 0;
    Result := StpBlk(Data);

    if (Result = nil) then
        Exit;

    { Remember the sign }
    if Result^ in ['-', '+'] then begin
        bSign := (Result^ = '-');
        Inc(Result);
    end
    else
        bSign  := FALSE;

    { Convert any number }
    while (Result^ <> #0) and (Result^ in ['0'..'9']) do begin
        Number := Number * 10 + ord(Result^) - ord('0');
        Inc(Result);
    end;

    { Correct for sign }
    if bSign then
        Number := -Number;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetMessageID(Data : PChar; var ID : String) : PChar;
begin
    ID     := '';
    Result := StpBlk(Data);
    if Data = nil then
        Exit;

    while not (Result^ in [#0, '<']) do
        Inc(Result);
    if Result^ = '<' then begin
        while Result^ <> #0 do begin
            Inc(Result);
            if Result^ = '>' then
                break;
            ID := ID + Result^;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetNewsGroupName(Data : PChar; var GroupName : String) : PChar;
begin
    GroupName := '';
    Result    := StpBlk(Data);
    if Data = nil then
        Exit;

    { Copy until first white space }
    while (Result^ <> #0) and (not (Result^ in [' ', #9])) do begin
        GroupName := GroupName + Result^;
        Inc(Result);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetChar(Data : PChar; var Ch : Char) : PChar;
begin
    Ch     := #0;
    Result := StpBlk(Data);
    if Data = nil then
        Exit;

    Ch := Result^;
    if Ch <> #0 then
        Inc(Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function atoi(Data : String) : Integer;
begin
{$IFDEF VER80}
    { Nul terminate string for Delphi 1 }
    Data[Length(Data) + 1] := #0;
{$ENDIF}
    GetInteger(@Data[1], Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure Register;
begin
    RegisterComponents('FPiette', [TNntpCli]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TNntpCli.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
{$IFDEF DUMP}
    FDumpStream := TFileStream.Create('c:\temp\nntpcli.log', fmCreate);
    FDumpBuf    := '---- START -----' + #13 + #10;
    FDumpStream.WriteBuffer(FDumpBuf[1], Length(FDumpBuf));
{$ENDIF}
    FWindowHandle               := AllocateHWnd(WndProc);
    FState                      := nntpNotConnected;
    FArticleNumber              := -1;
    FArticleID                  := '';
    FArticleFirst               := -1;
    FArticleLast                := -1;
    FArticleEstimated           := -1;
    FStatusCode                 := 503;  { program fault }
    FWSocket                    := TWSocket.Create(Self);
    FWSocket.OnSessionConnected := WSocketSessionConnected;
    FWSocket.OnDataAvailable    := WSocketDataAvailable;
    FWSocket.OnSessionClosed    := WSocketSessionClosed;
    FWSocket.OnDnsLookupDone    := WSocketDnsLookupDone;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TNntpCli.Destroy;
begin
{$IFDEF DUMP}
    if Assigned(FDumpStream) then begin
        FDumpBuf := '---- STOP -----' + #13 + #10;
        FDumpStream.WriteBuffer(FDumpBuf[1], Length(FDumpBuf));
        FDumpStream.Destroy;
    end;
{$ENDIF}
    if Assigned(FWSocket) then
        FWSocket.Destroy;
    DeallocateHWnd(FWindowHandle);
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.WndProc(var MsgRec: TMessage);
begin
     with MsgRec do begin
         case Msg of
         WM_NNTP_REQUEST_DONE : WMNntpRequestDone(MsgRec);
         else
             Result := DefWindowProc(Handle, Msg, wParam, lParam);
         end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.WMNntpRequestDone(var msg: TMessage);
begin
    if Assigned(FOnRequestDone) then
        FOnRequestDone(Self, TNntpRequest(Msg.WParam), Msg.LParam);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.StateChange(NewState : TNntpState);
begin
    if FState <> NewState then begin
        FState := NewState;
        TriggerStateChange;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.SendRequest;
begin
    FLastCmdResponse := '';
{$IFDEF DUMP}
    FDumpBuf := '<|';
    FDumpStream.WriteBuffer(FDumpBuf[1], Length(FDumpBuf));
    FDumpStream.WriteBuffer(FRequest[1], Length(FRequest));
    FDumpBuf := '|' + #13#10;
    FDumpStream.WriteBuffer(FDumpBuf[1], Length(FDumpBuf));
{$ENDIF}
    FWSocket.SendStr(FRequest + #13 + #10);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.Connect;
begin
    if FState <> nntpNotConnected then
         raise NntpException.Create('Already connected');

    FRequestType      := nntpConnect;
    FRequestDoneFlag  := FALSE;
    FReceiveLen       := 0;
    FRequest          := '';
    FArticleNumber    := -1;
    FArticleID        := '';
    FArticleFirst     := -1;
    FArticleLast      := -1;
    FArticleEstimated := -1;
    StateChange(nntpDnsLookup);
    FWSocket.DnsLookup(FHost);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.Group(NewsGroupName : String);
begin
    if FState <> nntpReady then
        raise NntpException.Create('Not ready for GROUP');

    FRequestDoneFlag := FALSE;
    FRequestType     := nntpGroup;
    FRequest         := 'GROUP ' + Trim(NewsGroupName);
    FNext            := GroupNext;
    StateChange(nntpWaitingResponse);
    SendRequest;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.GroupNext;
var
    Data  : PChar;
    Error : Integer;
begin
    Data := GetInteger(@FLastResponse[1], FStatusCode);
    Data := GetInteger(Data, FArticleEstimated);
    Data := GetInteger(Data, FArticleFirst);
    GetInteger(Data, FArticleLast);
    if FStatusCode = 211 then
        Error := 0
    else
        Error := FStatusCode;
    TriggerRequestDone(Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.ArticleByNumber(Number : Integer; DestStream : TStream);
begin
    GetArticleByNumber(nntpArticleByNumber, Number, DestStream);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.ArticleByID(ID : String; DestStream : TStream);
begin
    GetArticleByID(nntpArticleByID, ID, DestStream);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.BodyByNumber(Number : Integer; DestStream : TStream);
begin
    GetArticleByNumber(nntpBodyByNumber, Number, DestStream);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.BodyByID(ID : String; DestStream : TStream);
begin
    GetArticleByID(nntpBodyByID, ID, DestStream);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.HeadByNumber(Number : Integer; DestStream : TStream);
begin
    GetArticleByNumber(nntpHeadByNumber, Number, DestStream);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.HeadByID(ID : String; DestStream : TStream);
begin
    GetArticleByID(nntpHeadByID, ID, DestStream);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.StatByNumber(Number : Integer);
begin
    GetArticleByNumber(nntpStatByNumber, Number, nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.StatByID(ID : String);
begin
    GetArticleByID(nntpStatByID, ID, nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.GetArticleByID(
    RqType     : TNntpRequest;
    ID         : String;
    DestStream : TStream);
begin
    GetArticle(RqType, ' <' + ID + '>', DestStream);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.GetArticleByNumber(
    RqType     : TNntpRequest;
    Number     : Integer;
    DestStream : TStream);
begin
    if Number > 0 then
        GetArticle(RqType, ' ' + IntToStr(Number), DestStream)
    else
        GetArticle(RqType, '', DestStream);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.GetArticle(
    RqType     : TNntpRequest;
    ID         : String;
    DestStream : TStream);
var
    Cmd : String;
begin
    case RqType of
    nntpArticleByID, nntpArticleByNumber:
        Cmd := 'ARTICLE';
    nntpBodyByID, nntpBodyByNumber:
        Cmd := 'BODY';
    nntpHeadByID, nntpHeadByNumber:
        Cmd := 'HEAD';
    nntpStatByID, nntpStatByNumber:
        Cmd := 'STAT';
    else
        raise NntpException.Create('Internal error: Invalid Request Type');
    end;

    if FState <> nntpReady then
        raise NntpException.Create('Not ready for ' + Cmd);
    FDataStream      := DestStream;
    FRequestType     := RqType;
    FRequestDoneFlag := FALSE;
    FArticleNumber   := -1;
    FArticleID       := '';
    FRequest         := Cmd + ID;
    FNext            := GetArticleNext;
    StateChange(nntpWaitingResponse);
    SendRequest;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.GetArticleNext;
var
    Data  : PChar;
begin
    Data := GetInteger(@FLastResponse[1], FStatusCode);
    if not (FStatusCode in [100, 215, 220, 221,
                            222, 223, 224, 231]) then begin
        TriggerRequestDone(FStatusCode);
        Exit;
    end;

    Data := GetInteger(Data, FArticleNumber);
    GetMessageID(Data, FArticleID);

    if FStatusCode in [223] then
        TriggerRequestDone(0)
    else begin
        FNext            := GetArticleLineNext;
        FLastCmdResponse := FLastResponse;;
        StateChange(nntpWaitingResponse);

        if Assigned(FOnMessageBegin) then
            FOnMessageBegin(Self);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.GetArticleLineNext;
const
    CrLf : String[2] = #13#10;
begin
    if FLastResponse = '.' then begin
        if FLastCmdResponse <> '' then begin
            FLastResponse    := FLastCmdResponse;
            FLastCmdResponse := '';
        end;
        if Assigned(FOnMessageEnd) then
            FOnMessageEnd(Self);
        TriggerRequestDone(0);
    end
    else begin
        if FLastResponse = '..' then
            FLastResponse := '.';
        if Assigned(FDataStream) then begin
            if Length(FLastResponse) > 0 then
                FDataStream.Write(FLastResponse[1], Length(FLastResponse));
            FDataStream.Write(CrLf[1], Length(CrLf));
        end;
        if Assigned(FOnMessageLine) then
            FOnMessageLine(Self);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.Next;
begin
    if FState <> nntpReady then
        raise NntpException.Create('Not ready for NEXT');
    FRequestDoneFlag := FALSE;
    FRequestType     := nntpNext;
    FArticleNumber   := -1;
    FArticleID       := '';
    FRequest         := 'NEXT';
    FNext            := GetArticleNext;
    StateChange(nntpWaitingResponse);
    SendRequest;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.Last;
begin
    if FState <> nntpReady then
        raise NntpException.Create('Not ready for LAST');
    FRequestDoneFlag := FALSE;
    FRequestType     := nntpLast;
    FArticleNumber   := -1;
    FArticleID       := '';
    FRequest         := 'LAST';
    FNext            := GetArticleNext;
    StateChange(nntpWaitingResponse);
    SendRequest;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.List(DestStream : TStream);
begin
    if FState <> nntpReady then
        raise NntpException.Create('Not ready for LIST');
    FDataStream      := DestStream;
    FRequestDoneFlag := FALSE;
    FRequestType     := nntpList;
    FRequest         := 'LIST';
    FNext            := GetArticleNext;
    StateChange(nntpWaitingResponse);
    SendRequest;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.Help(DestStream : TStream);
begin
    if FState <> nntpReady then
        raise NntpException.Create('Not ready for HELP');
    FDataStream      := DestStream;
    FRequestDoneFlag := FALSE;
    FRequestType     := nntpHelp;
    FRequest         := 'HELP';
    FNext            := GetArticleNext;
    StateChange(nntpWaitingResponse);
    SendRequest;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.Quit;
begin
    if FState <> nntpReady then
        raise NntpException.Create('Not ready for QUIT');
    FRequestDoneFlag := FALSE;
    FRequestType     := nntpQuit;
    FRequest         := 'QUIT';
    FNext            := QuitNext;
    StateChange(nntpWaitingResponse);
    SendRequest;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.QuitNext;
begin
    GetInteger(@FLastResponse[1], FStatusCode);
    TriggerRequestDone(0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.Abort;
begin
    FRequestType     := nntpAbort;
    FWSocket.Close;
    FLastResponse    := '205 Closing connection - goodbye';
    FStatusCode      := 205;
    FRequestDoneFlag := FALSE;
    TriggerRequestDone(0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.Post(FromStream : TStream);
begin
    if FState <> nntpReady then
        raise NntpException.Create('Not ready for POST');
    FDataStream      := FromStream;
    FRequestDoneFlag := FALSE;
    FRequestType     := nntpPost;
    FRequest         := 'POST';
    FSentFlag        := FALSE;
    FNext            := PostNext;
    StateChange(nntpWaitingResponse);
    SendRequest;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.PostNext;
begin
    GetInteger(@FLastResponse[1], FStatusCode);
    if FStatusCode <> 340 then begin
        TriggerRequestDone(FStatusCode);
        Exit;
    end;
    FNext               := PostSendNext;
    FWSocket.OnDataSent := WSocketDataSent;
    PostBlock;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.PostBlock;
var
    Len  : Integer;
begin
    Len := FDataStream.Read(FSendBuffer, SizeOf(FSendBuffer));
    if Len <= 0 then begin
        if FSentFlag then
            Exit;
        FSentFlag := TRUE;
        StrCopy(@FSendBuffer, #13#10 + '.' + #13#10);
        Len := 5;
    end;
    FWSocket.Send(@FSendBuffer, Len);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.PostSendNext;
begin
    FWSocket.OnDataSent := nil;
    GetInteger(@FLastResponse[1], FStatusCode);
    if FStatusCode = 240 then
        TriggerRequestDone(0)
    else
        TriggerRequestDone(FStatusCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.PostDone;
begin
    FLastResponse := '441 posting failed';
    PostSendNext;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.NewGroups(
    When          : TDateTime;
    GMTFLag       : Boolean;
    Distributions : String;
    DestStream    : TStream);
begin
    if FState <> nntpReady then
        raise NntpException.Create('Not ready for NEWGROUPS');
    FDataStream      := DestStream;
    FRequestDoneFlag := FALSE;
    FRequestType     := nntpNewGroups;
    if When = 0 then
        When := Now;
    FRequest         := 'NEWGROUPS ' + FormatDateTime('yymmdd hhnnss', When);
    if GMTFlag then
        FRequest := FRequest + ' GMT';
    if Length(Distributions) > 0 then
        FRequest     := FRequest + ' <' + Distributions + '>';
    FNext            := GetArticleNext;
    StateChange(nntpWaitingResponse);
    SendRequest;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.NewNews(
    When          : TDateTime;
    GMTFLag       : Boolean;
    NewsGroupName : String;
    Distributions : String;
    DestStream    : TStream);
begin
    if FState <> nntpReady then
        raise NntpException.Create('Not ready for NEWNEWS');
    FDataStream      := DestStream;
    FRequestDoneFlag := FALSE;
    FRequestType     := nntpNewNews;
    if When = 0 then
        When := Now;
    if NewsGroupName = '' then
        NewsGroupName := '*';
    FRequest         := 'NEWNEWS ' + NewsGroupName + ' ' +
                        FormatDateTime('yymmdd hhnnss', When);
    if GMTFlag then
        FRequest := FRequest + ' GMT';
    if Length(Distributions) > 0 then
        FRequest     := FRequest + ' <' + Distributions + '>';
    FNext            := GetArticleNext;
    StateChange(nntpWaitingResponse);
    SendRequest;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Articles can be: a) a single (positive) article number                    }
{                  b) an article number followed by a dash                  }
{                  c) two article numbers separated by a dash               }
procedure TNntpCli.XOver(
    Articles   : String;
    DestStream : TStream);
begin
    if FState <> nntpReady then
        raise NntpException.Create('Not ready for XOVER');
    FDataStream      := DestStream;
    FRequestDoneFlag := FALSE;
    FRequestType     := nntpXOver;
    FRequest         := 'XOVER ' + Articles;
    FNext            := GetArticleNext;
    StateChange(nntpWaitingResponse);
    SendRequest;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.ListOverViewFmt(DestStream : TStream);
begin
    if FState <> nntpReady then
        raise NntpException.Create('Not ready for LIST OVERVIEW.FMT');
    FDataStream      := DestStream;
    FRequestDoneFlag := FALSE;
    FRequestType     := nntpListOverViewFmt;
    FRequest         := 'LIST OVERVIEW.FMT';
    FNext            := GetArticleNext;
    StateChange(nntpWaitingResponse);
    SendRequest;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.DateNext;
var
    Data  : PChar;
    Buf   : String;
    Year, Month, Day, Hour, Min, Sec : Word;
begin
    Data := StpBlk(GetInteger(@FLastResponse[1], FStatusCode));
    if FStatusCode <> 111 then begin
        TriggerRequestDone(FStatusCode);
        Exit;
    end;
    Buf := Trim(StrPas(Data));
    if Length(Buf) = 14 then begin
        Year  := atoi(Copy(Buf, 1, 4));
        Month := atoi(Copy(Buf, 5, 2));
        Day   := atoi(Copy(Buf, 7, 2));
        Hour  := atoi(Copy(Buf, 9, 2));
        Min   := atoi(Copy(Buf, 11, 2));
        Sec   := atoi(Copy(Buf, 13, 2));
        FServerDate := EncodeDate(Year, Month, Day) +
                       EncodeTime(Hour, Min, Sec, 0);
    end;
    TriggerRequestDone(0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.Date;
begin
    if FState <> nntpReady then
        raise NntpException.Create('Not ready for DATE');
    FServerDate      := 0;
    FDataStream      := nil;
    FRequestDoneFlag := FALSE;
    FRequestType     := nntpDate;
    FRequest         := 'DATE';
    FNext            := DateNext;
    StateChange(nntpWaitingResponse);
    SendRequest;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.ModeReaderNext;
begin
    GetInteger(@FLastResponse[1], FStatusCode);
    if FStatusCode in [200, 201] then
        TriggerRequestDone(0)
    else
        TriggerRequestDone(FStatusCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.ModeReader;
begin
    if FState <> nntpReady then
        raise NntpException.Create('Not ready for ModeReader');
    FServerDate      := 0;
    FDataStream      := nil;
    FRequestDoneFlag := FALSE;
    FRequestType     := nntpModeReader;
    FRequest         := 'MODE READER';
    FNext            := ModeReaderNext;
    StateChange(nntpWaitingResponse);
    SendRequest;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.XHdrLineNext;
const
    CrLf : String[2] = #13#10;
begin
    if FLastResponse = '.' then begin
        if FLastCmdResponse <> '' then begin
            FLastResponse    := FLastCmdResponse;
            FLastCmdResponse := '';
        end;
        if Assigned(FOnXHdrEnd) then
            FOnXHdrEnd(Self);
        TriggerRequestDone(0);
    end
    else begin
        if Assigned(FDataStream) then begin
            if Length(FLastResponse) > 0 then
                FDataStream.Write(FLastResponse[1], Length(FLastResponse));
            FDataStream.Write(CrLf[1], Length(CrLf));
        end;
        if Assigned(FOnXHdrLine) then
            FOnXHdrLine(Self);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.XHdrNext;
begin
    GetInteger(@FLastResponse[1], FStatusCode);
    if FStatusCode <> 221 then begin
        TriggerRequestDone(FStatusCode);
        Exit;
    end;

    FNext            := XHdrLineNext;
    FLastCmdResponse := FLastResponse;;
    StateChange(nntpWaitingResponse);

    if Assigned(FOnXHdrBegin) then
        FOnXHdrBegin(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Header is a header line such as "subject".                                }
{ Range is either:                                                          }
{   an article number                                                       }
{   an article number followed by a dash to indicate all following          }
{   an article number followed by a dash followed by another article number }
{ Range can be replaced by a message id.                                    }
{ If range is empty current article is used.                                }
procedure TNntpCli.XHdr(DestStream : TStream; Header : String; Range : String);
begin
    if FState <> nntpReady then
        raise NntpException.Create('Not ready for XHDR');
    FDataStream      := DestStream;
    FRequestDoneFlag := FALSE;
    FRequestType     := nntpXHdr;
    FRequest         := 'XHDR ' + Header;
    if Length(Range) > 0 then
        Frequest     := FRequest + ' ' + Range;
    FNext            := XHdrNext;
    StateChange(nntpWaitingResponse);
    SendRequest;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.AuthenticateNext1;
begin
    StpBlk(GetInteger(@FLastResponse[1], FStatusCode));
    if FStatusCode <> 381 then begin
        TriggerRequestDone(FStatusCode);
        Exit;
    end;
    FRequestDoneFlag := FALSE;
    FRequest         := 'AUTHINFO PASS ' + FPassWord;
    FNext            := AuthenticateNext2;
    StateChange(nntpWaitingResponse);
    SendRequest;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.AuthenticateNext2;
begin
    StpBlk(GetInteger(@FLastResponse[1], FStatusCode));
    if FStatusCode <> 281 then begin
        TriggerRequestDone(FStatusCode);
        Exit;
    end;
    TriggerRequestDone(0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.Authenticate;
begin
    if FState <> nntpReady then
        raise NntpException.Create('Not ready for DATE');
    FRequestDoneFlag := FALSE;
    FRequestType     := nntpAuthenticate;
    FRequest         := 'AUTHINFO USER ' + FUserName;
    FNext            := AuthenticateNext1;
    StateChange(nntpWaitingResponse);
    SendRequest;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure ParseListLine(
    const Line          : String;
    var NewsGroupName   : String;
    var LastArticle     : Integer;
    var FirstArticle    : Integer;
    var PostingFlag     : Char);
var
    Data : PChar;
begin
    if Length(Line) = 0 then
        Exit;
    Data := GetNewsGroupName(@Line[1], NewsGroupName);
    Data := GetInteger(Data, LastArticle);
    Data := GetInteger(Data, FirstArticle);
    GetChar(Data, PostingFlag);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.WSocketDataSent(Sender: TObject; Error: Word);
begin
    if Error <> 0 then begin
        PostDone;
        Exit;
    end;
    PostBlock;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.WSocketDnsLookupDone(Sender: TObject; Error: Word);
begin
    if Error <> 0 then
        TriggerRequestDone(Error)
    else begin
        FWSocket.Addr  := FWSocket.DnsResult;
        FWSocket.Proto := 'tcp';
        FWSocket.Port  := 'nntp';
        StateChange(nntpWaitingBanner);
        FWSocket.Connect;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.WSocketSessionConnected(Sender: TObject; Error: Word);
begin
    { Do not trigger the client SessionConnected from here. We must wait }
    { to have received the server banner.                                }
    if Error <> 0 then begin
        TriggerRequestDone(Error);
        FWSocket.Close
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.WSocketDataAvailable(Sender: TObject; Error: Word);
var
    Len : Integer;
    I   : Integer;
begin
    Len := FWSocket.Receive(@FReceiveBuffer[FReceiveLen],
                            sizeof(FReceiveBuffer) - FReceiveLen - 1);

    if FRequestType = nntpAbort then
        Exit;

    if Len = 0 then begin
        FWSocket.Close;
        Exit;
    end;
    if Len < 0 then
        Exit;

    FReceiveBuffer[FReceiveLen + Len] := #0;
    if Assigned(FOnDisplay) then
        FOnDisplay(Self, @FReceiveBuffer[FReceiveLen], Len);
    FReceiveLen := FReceiveLen + Len;

    while FReceiveLen > 0 do begin
        I := Pos(#13#10, FReceiveBuffer);
        if I <= 0 then
            break;
        if I > FReceiveLen then
            break;

        FLastResponse := Copy(FReceiveBuffer, 1, I - 1);

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

        if FState = nntpWaitingBanner then begin
            StateChange(nntpReady);
            GetInteger(@FLastResponse[1], FStatusCode);
            FPostingPermited := (FStatusCode = 200);
            if Assigned(FOnSessionConnected) then
                FOnSessionConnected(Self, Error);
        end
        else if FState = nntpWaitingResponse then begin
            if Assigned(FNext) then
                FNext
            else
                StateChange(nntpReady);
        end
        else begin
            if Assigned(FOnDataAvailable) then
                FOnDataAvailable(Self, Error);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.WSocketSessionClosed(Sender: TObject; Error: Word);
begin
    if not (FRequestType in [nntpAbort]) then
        TriggerRequestDone(Error);
    if Assigned(FOnSessionClosed) then
        OnSessionClosed(Self, Error);
    StateChange(nntpNotConnected);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.TriggerStateChange;
begin
    if Assigned(FOnStateChange) then
        FOnStateChange(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.TriggerRequestDone(Error: Word);
begin
    if FRequestDoneFlag = FALSE then
        PostMessage(Handle, WM_NNTP_REQUEST_DONE, WORD(FRequestType), Error);
    FRequestDoneFlag := TRUE;
    FNext            := nil;
    if FWSocket.State = wsConnected then
        StateChange(nntpReady)
    else
        StateChange(nntpNotConnected);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

