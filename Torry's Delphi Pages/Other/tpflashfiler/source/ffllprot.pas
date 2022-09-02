{*********************************************************}
{* FlashFiler: Communications protocol class             *}
{*********************************************************}

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower FlashFiler
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{$I ffdefine.inc}

{ Enable the following line to activate Keep Alive logging. }
{.$DEFINE KALog}

unit ffllprot;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  ExtCtrls,
  Forms,
  ffconst,
  ffllbase,
  ffllexcp,
  fflllog,
  ffllwsct,
  ffnetmsg,
  ffsrmgr,
  ffllwsck;

type
  TffProtocolType = (                {Protocol types..}
                     ptSingleUser,   {..single user}
                     ptTCPIP,        {..TCP/IP}
                     ptIPXSPX,       {..IPX/SPX}
                     ptRegistry);    {..value from registry}



{===Constants relating to sending messages and datagrams}
const
  ffc_ConnectRetryTimeout : DWORD = 1000;                              {!!.05}
    { Number of milliseconds before retry of connection request. }     {!!.05}
  ffc_UnblockWait : DWORD = 25;                                        {!!.06}
    { Number of milliseconds to wait before exiting unblock wait loop. } {!!.06}
  ffc_MaxWinsockMsgSize = 24 * 1024;
  ffc_MaxSingleUserMsgSize = 64 * 1024;
  ffc_MaxDatagramSize = 128;
  ffc_CodeLength = 256;
  ffc_LastMsgInterval : longint = 30000;
  ffc_KeepAliveInterval : longint = 5000;
  ffc_KeepAliveRetries : longint = 5;

  ffc_TCPInterface : Integer = 0;  // NIC to use for TCPIP
  ffc_TCPRcvBuf : longint = $8000; // request 32K Rcv Buffer
  ffc_TCPSndBuf : longint = $8000; // request 32K Snd Buffer

  ffc_SingleUserServerName = 'Local server';
  ffc_SendMessageTimeout = 1 * 1000; {1 second}                        {!!.01}{!!.05}

  ffc_SUPErrorTimeout : Integer = 25;                                  {!!.06}
    { # milliseconds to wait if error occurs during SUP send. }        {!!.06}

{===Single user messages constants (for dwData)}
const
  ffsumCallServer = $4631;
  ffsumDataMsg    = $4632;
  ffsumHangUp     = $4633;
  ffsumKeepAlive  = $4634;
  ffm_ServerReply = WM_USER + $0FF9;

{===Datagram types===}
type
  PffDatagram = ^TffDatagram;
  TffDatagram = array [0..pred(ffc_MaxDatagramSize)] of byte;
  PffDatagramArray = ^TffDatagramArray;
  TffDatagramArray = array [0..255] of TffDatagram;

{===Code types===}
type
  PffNetMsgCode = ^TffNetMsgCode;
  TffNetMsgCode = array [0..pred(ffc_CodeLength)] of byte;

{===Event types===}
type
  TffReceiveMsgEvent = function (aSender   : TObject;
                                 clientID  : TffClientID;
                                 replyData : PffByteArray;
                                 replyLen : longInt) : boolean of object;
  TffHeardCallEvent = procedure (aSender     : TObject;
                                 aConnHandle : longint) of object;
  TffReceiveDatagramEvent = procedure (aSender  : TObject;
                                 const aName    : TffNetName;
                                       aData    : PffByteArray;
                                       aDataLen : longint) of object;
  TffHangUpEvent = procedure (aSender   : TObject;
                              aClientID : TffClientID) of object;


{===Base Classes===}
type
  TffBaseCommsProtocol = class;

  TffClientServerType = (           {type defining client or server}
                         csClient,  {..client}
                         csServer); {..server}

  TffConnection = class(TffSelfListItem)
    protected {private}
      FClientID : TffClientID;
      FCode     : PffNetMsgCode;
        { The code used for encrypting messages. }
      FCodeStart    : DWord;                                           {!!.10}
      FHangingUp    : boolean;
      FHangupDone   : boolean;                                         {!!.01}
      FHangupLock   : TffPadlock;                                      {!!.01}
      FOwner        : TffBaseCommsProtocol;
      FRemoteName   : PffShStr;
      FAliveRetries : integer;
      FLastMsgTimer : TffTimer;
      FSendConfirm  : boolean;
    protected
      function GetRemoteName : string;                                 {!!.10}
      procedure AddToList(List : TFFList); virtual;
      procedure RemoveFromList(List : TFFList); virtual;
    public
      constructor Create(aOwner      : TffBaseCommsProtocol;
                         aRemoteName : TffNetAddress);
      destructor Destroy; override;

      procedure ConfirmAlive(SendConfirm : boolean);
      procedure DepleteLife;

      procedure HangupLock;                                            {!!.01}
      procedure HangupUnlock;                                          {!!.01}

      procedure InitCode(const aStart : longint);
        { Initializes the encryption code used for communicating with the
          server. }
      function IsAlive : boolean;
      function IsVeryAlive : boolean;
      function NeedsConfirmSent : boolean;

      property ClientID : TffClientID read FClientID write FClientID;
      property Code : PffNetMsgCode read FCode;
      property CodeStart : DWord read FCodeStart;                      {!!.10}
      property Owner : TffBaseCommsProtocol
         read FOwner;
      property Handle : longint
         read KeyAsInt;
      property HangingUp : boolean
         read FHangingUp write FHangingUp;
        { Set to True when we are deliberately hanging up the connection.
          This variable tells us whether we need to invoke the OnHangUp or
          OnConnectionLost event in the parent protocol. }
      property HangupDone : boolean                                    {!!.01}
         read FHangupDone write FHangupDone;                           {!!.01}
      property RemoteName : string                                     {!!.10}
         read GetRemoteName;
  end;

  { Defines the common interface for all legacy protocols.  This class is
    written with the assumption that only one thread will ever be using an
    instance of this class at any given time.  Therefore no locking/critical
    sections are used. }
  TffBaseCommsProtocol = class
    protected {private}
      FConnLock          : TffPadlock;                               
      FCSType            : TffClientServerType;
      FEventLog          : TffBaseLog;
      FHeardCall         : TffHeardCallEvent;
      FKeepAliveInterval : longInt;
      FKeepAliveRetries  : longInt;
      FLastMsgInterval   : longInt;
      FLocalName         : PffShStr;
      FLogEnabled        : boolean;
      FMaxNetMsgSize     : longint;
      FNetName           : PffShStr;
      FNotifyWindow      : HWND;
      FOnConnectionLost  : TffHangupEvent;
      FOnHangup          : TffHangUpEvent;
      FReceiveDatagram   : TffReceiveDatagramEvent;
      FReceiveMsg        : TffReceiveMsgEvent;
      FSearchTimeOut     : integer;
      FStarted           : boolean;
        {-If True then the protocol is active. }
      FStartedEvent      : TffEvent;

      cpConnList         : TffList;
      cpIndexByOSConnector : TffList;  { This is an index by socket (TCP/IP or
                                         IPX/SPX) or by window handle (SUP). }
      cpIndexByClient      : TffList;  { This is an index by clientID. }
    protected

      function GetLocalName : string;                                  {!!.10}
      function GetNetName : string;                                    {!!.10}

      procedure cpAddConnection(aConnection : TffConnection);
      function cpExistsConnection(aConnHandle : longint) : boolean;
      function cpFindConnection(const aClientID : TffClientID) : Longint;
      function cpGetConnection(const aClientID : TffClientID) : TffConnection;
      function cpGetConnectionIDs(const anIndex : longInt) : TffClientID;
      procedure cpRemoveConnection(aClientID : TffClientID);

      function cpCreateNotifyWindow : boolean; dynamic;
      procedure cpDestroyNotifyWindow;
      procedure cpDoHangUp(aConn : TffConnection); dynamic;
      procedure cpDoHeardCall(aConnHandle : longint); dynamic;
      procedure cpDoReceiveDatagram(const aName    : TffNetName;
                                          aData    : PffByteArray;
                                          aDataLen : longint); dynamic;
      function cpDoReceiveMsg(aConn : TffConnection;
                              msgData : PffByteArray;
                              msgDataLen : longInt) : boolean; dynamic;

      procedure cpPerformShutdown; virtual;
      procedure cpPerformStartUp; virtual; abstract;

      procedure cpSetNetName(aName : string);

      procedure cpCodeMessage(aConn : TffConnection; aData : PffByteArray;
                              aDataLen : longint); virtual;
      procedure cpGotCheckConnection(aConn : TffConnection);
      procedure cpTimerTick;
    public
      constructor Create(const aName : TffNetAddress; aCSType : TffClientServerType); virtual;
      destructor Destroy; override;

      function Call(const aServerName : TffNetName;
                      var aClientID : TffClientID;
                    const timeout : longInt) : TffResult; virtual; abstract;
      function ClientIDExists(const aClientID : TffClientID) : boolean;
        { Used by the legacy transport to determine if it has generated a
          temporary clientID that conflicts with a real clientID. }

      function ConnectionCount : longInt;
        { Returns the number of established connections. }

      procedure ConnLock;
      procedure ConnUnlock;
        { Use these procedures to prevent a newly-attached client from sending
          the protocol a message before the protocol has updated the new
          connection's clientID. }

      procedure GetServerNames(aList : TStrings; const timeout : longInt); virtual; abstract;
        { Protocol-specific method for retrieving servers accessible via the
          protocol. }

      function GetCodeStart(const aClientID : TffClientID) : integer;
        { Get the starting encryption code for the specified client. }

      class function GetProtocolName : string; virtual;
        { Returns the name of the protocol (e.g., 'TCP/IP'). }

      procedure HangUp(aConn : TffConnection); virtual; abstract;
      procedure HangUpByClientID(aClientID : TffClientID); virtual;
      procedure HangupDone(aClientID : TffClientID);                   {!!.01}
      function HangupIsDone(aClientID : TffClientID) : Boolean;        {!!.01}
      procedure HangupLock(aClientID : TffClientID);                   {!!.01}
      procedure HangupUnlock(aClientID : TffClientID);                 {!!.01}
      procedure Listen; virtual; abstract;
      function SendMsg(aClientID : TffClientID;
                       aData     : PffByteArray;
                       aDataLen  : longint;
                       aConnLock : Boolean) : TffResult; virtual; abstract; {!!.06}

      procedure ReceiveDatagram; virtual; abstract;
      procedure SendDatagram(const aName    : TffNetName;
                                   aData    : PffByteArray;
                                   aDataLen : longint); virtual; abstract;

      procedure Shutdown; virtual;

      procedure StartUp; virtual;

      procedure StopReceiveDatagram; virtual; abstract;

      class function Supported : boolean; virtual;
        { Returns True if the protocol is supported on this workstation.
          Default implementation always returns True. }

      procedure Breathe; virtual;
      procedure InitCode(const aClientID : TffClientID;
                         const aStart : longint);
      procedure ResetKeepAliveTimer;

      procedure UpdateClientID(const oldClientID, newClientID : TffClientID);
        { After a client has successfully obtained access to the server, the
          transport uses this method to replace the client's temporary ID
          with the ID returned from the server. }

      procedure LogStr(const aMsg : string);
        { Use this method to write an event string to the protocol's event
          log. }

      procedure LogStrFmt(const aMsg : string; args : array of const);
        { Use this method to write a formatted event string to the protocol's
          event log. }

      property ConnectionIDs[const anIndex : longInt] : TffClientID
         read cpGetConnectionIDs;
        { Use this method to retrieve the connection IDs for the protocol's
          connections. }

      property CSType : TffClientServerType
         read FCSType;
      property EventLog : TffBaseLog
         read FEventLog write FEventLog;
      property IsStarted : boolean
         read FStarted;
      property KeepAliveInterval : longInt
         read FKeepAliveInterval
         write FKeepAliveInterval;
      property KeepAliveRetries : longInt
         read FKeepAliveRetries
         write FKeepAliveRetries;
      property LastMsgInterval : longInt
         read FLastMsgInterval
         write FLastMsgInterval;
      property LocalName : string                                      {!!.10}
         read GetLocalName;
      property LogEnabled : boolean
         read FLogEnabled
         write FLogEnabled;
      property MaxNetMsgSize : longint
         read FMaxNetMsgSize;
      property NetName : string                                        {!!.10}
         read GetNetName;
      property NotifyWindow : HWND
         read FNotifyWindow;
      property OnConnectionLost : TffHangUpEvent
         read FOnConnectionLost write FOnConnectionLost;
        { This event is called when the other end of the connection unexpectedly
          hangs up on this end. }
      property OnHangUp : TffHangUpEvent
         read FOnHangUp write FOnHangUp;
        { This event is called when the protocol deliberately hangs up the
          connection. }
      property OnHeardCall : TffHeardCallEvent
         read FHeardCall write FHeardCall;
      property OnReceiveDatagram: TffReceiveDatagramEvent
         read FReceiveDatagram write FReceiveDatagram;
      property OnReceiveMsg : TffReceiveMsgEvent
         read FReceiveMsg write FReceiveMsg;
      property SearchTimeOut : integer
         read FSearchTimeOut;
      property StartedEvent : TffEvent
         read FStartedEvent;
  end;

  TffCommsProtocolClass = class of TffBaseCommsProtocol;

{===Winsock Classes===}
type
  PffwscPacket = ^TffwscPacket;
  TffwscPacket = packed record
      dwLength     : longint;
      dwStart      : longint;
      lpData       : PffByteArray;
      lpNext       : PffwscPacket;
  end;

type
  TffWinsockConnection = class(TffConnection)
    protected {private}
      FSocket       : TffwsSocket;
      FFamily       : TffWinsockFamily;
      wscNotifyWnd  : HWND;
//      wscPortal     : TffReadWritePortal;                            {Deleted !!.05}
        {!!.05 - Replaced by TffConnection.HangupLock }
        { Controls access to a connection in order that:
          1. The connection is not freed while a reply is outgoing.
          2. No more than one reply is being sent to the connection at
             any one time.
        }
      wscRcvBuffer  : PffByteArray;
      wscRcvBufOfs  : integer;
//      wscSendBuffer : PffByteArray;
    protected
      wscRcvBuf     : longint;
      wscSndBuf     : longint;
      wscPacketHead : PffwscPacket;
      wscPacketTail : PffwscPacket;
      wscIsSending  : Boolean;
      procedure AddToList(List : TFFList); override;
      procedure RemoveFromList(List : TFFList); override;
    public
      constructor Create(aOwner      : TffBaseCommsProtocol;
                         aRemoteName : TffNetAddress;
                         aSocket     : TffwsSocket;
                         aFamily     : TffWinsockFamily;
                         aNotifyWnd  : HWND);
      destructor Destroy; override;

      function Send(aData : PffByteArray;
                    aDataLen   : longint;
                    aDataStart : longint;
                var aBytesSent : longint;
                    aConnLock  : Boolean) : integer;                  {!!.06}
      procedure StartReceive;

      property IsSending : Boolean                                    {!!.06}
         read wscIsSending write wscIsSending;                        {!!.06}
         
      property RcvBuffer : PffByteArray
         read wscRcvBuffer;

      property RcvBufferOffset : integer
         read wscRcvBufOfs write wscRcvBufOfs;

      property Socket : TffwsSocket
         read FSocket;
  end;

type
  TffWinsockProtocol = class(TffBaseCommsProtocol)
    protected {private}
      FCollectingServerNames : boolean;
      FDatagramPadlock     : TffPadlock;
      FFamily              : TffWinsockFamily;
      FServerNames         : TStringList;
      wspLocalInAddr       : TffwsInAddr;
      wspLocalIPXNetNum    : TffwsIPXNetNum;
      wspLocalIPXAddr      : TffwsIPXAddr;
      wspListening         : boolean;
      wspListenSocket      : TffwsSocket;
      wspRcvDatagramSocket : TffwsSocket;
      wspRcvDGBuffer       : PffByteArray;
      wspReceivingDatagram : boolean;
      wspWaitingForConnect : boolean;
      wspWaitingForSendToUnblock : boolean;
    protected
      procedure SetFamily(F : TffWinsockFamily);
      function cpCreateNotifyWindow : boolean; override;
      procedure cpDoReceiveDatagram(const aName    : TffNetName;
                                          aData    : PffByteArray;
                                          aDataLen : longint); override;
      procedure cpPerformStartUp; override;

      procedure wspConnectCompleted(aSocket : TffwsSocket);
      function wspGetConnForSocket(aSocket : TffwsSocket) : TffWinsockConnection;
      procedure wspHangupDetected(aSocket : TffwsSocket);
      procedure wspListenCompleted(aSocket : TffwsSocket);
      procedure wspProcessCompletedWSACall(WParam, LParam : longint);
      procedure wspSendMsgCompleted(aSocket : TffwsSocket);
      procedure wspReceiveCompleted(aSocket : TffwsSocket);
      procedure wspReceiveDatagramCompleted(aSocket : TffwsSocket);
      procedure wspReceiveMsgCompleted(aSocket : TffwsSocket);
      procedure wspWaitForConnect(aTimeOut : integer);
      function wspWaitForSendToUnblock : Boolean;                      {!!.06}
      procedure wspWSAEventCompleted(var WSMsg : TMessage);
    public
      constructor Create(const aName : TffNetAddress;
                               aCSType : TffClientServerType); override;
      destructor Destroy; override;

      function Call(const aServerName : TffNetName;
                      var aClientID : TffClientID;
                    const timeOut : longInt) : TffResult; override;
      procedure GetServerNames(aList : TStrings; const timeout : longInt); override;
      procedure HangUp(aConn : TffConnection); override;
      procedure Listen; override;
      function SendMsg(aClientID : TffClientID;
                       aData     : PffByteArray;
                       aDataLen  : longint;
                       aConnLock : Boolean) : TffResult; override;    {!!.06}

      procedure ReceiveDatagram; override;
      procedure SendDatagram(const aName    : TffNetName;
                                   aData    : PffByteArray;
                                   aDataLen : longint); override;
      procedure StopReceiveDatagram; override;

      property Family : TffWinsockFamily
         read FFamily write SetFamily;
  end;

  TffTCPIPProtocol = class(TffWinsockProtocol)
    protected
    public
      constructor Create(const aName : TffNetAddress;
                               aCSType : TffClientServerType); override;
      class function GetProtocolName : string; override;
        { Returns the name of the protocol (e.g., 'TCP/IP'). }

      class function Supported : boolean; override;

  end;

  TffIPXSPXProtocol = class(TffWinsockProtocol)
    protected
    public
      constructor Create(const aName : TffNetAddress;
                               aCSType : TffClientServerType); override;
      class function GetProtocolName : string; override;
        { Returns the name of the protocol (e.g., 'TCP/IP'). }

      class function Supported : boolean; override;

  end;

  TffSingleUserConnection = class(TffConnection)
    protected {private}
      FPartner : HWND;
      FUs      : HWND;
      sucSendBuffer   : PffByteArray;
    protected
      procedure AddToList(List : TFFList); override;
      procedure RemoveFromList(List : TFFList); override;
    public
      constructor Create(aOwner      : TffBaseCommsProtocol;
                         aRemoteName : TffNetAddress;
                         aUs         : HWND;
                         aPartner    : HWND);
      destructor Destroy; override;
      procedure Send(aData    : PffByteArray;
                     aDataLen : longint;
                     aConnLock : Boolean);                            {!!.06}
      property Partner : HWND read FPartner write FPartner;
  end;

  TffSingleUserProtocol = class(TffBaseCommsProtocol)
    protected {private}
      supMsgID  : TffWord32;
      supPostMsgID : TffWord32;
      supPartner : HWND;
      supReceivingDatagram : boolean;
    protected
      function cpCreateNotifyWindow : boolean; override;
      procedure cpPerformStartUp; override;

      procedure supDataMsgReceived(const aClientID : TffClientID;
                                   const aCDS : TCopyDataStruct);
      function supGetConnForPartner(aPartner : HWND) : TffSingleUserConnection;
      procedure supHangupDetected(const aClientID : TffClientID);
      procedure supListenCompleted(aClientID : TffClientID; Wnd : HWND);
      procedure supMsgReceived(var SUMsg : TMessage);
      function supFindPartner(const aClientID : TffClientID;
                              const timeout : longInt): HWND;
    public
      constructor Create(const aName : TffNetAddress; aCSType : TffClientServerType); override;
      function Call(const aServerName : TffNetName;
                      var aClientID : TffClientID;
                    const timeout : longInt) : TffResult; override;
      class function GetProtocolName : string; override;
        { Returns the name of the protocol (e.g., 'TCP/IP'). }

      procedure GetServerNames(aList : TStrings; const timeout : longInt); override;
      procedure HangUp(aConn : TffConnection); override;
      procedure Listen; override;
      function SendMsg(aClientID : TffClientID;
                       aData     : PffByteArray;
                       aDataLen  : longint;
                       aConnLock : Boolean) : TffResult; override;    {!!.06}

      procedure ReceiveDatagram; override;
      procedure SendDatagram(const aName    : TffNetName;
                                   aData    : PffByteArray;
                                   aDataLen : longint); override;
      procedure StopReceiveDatagram; override;

  end;

{===Helper routines===}
procedure FFSplitNetAddress(const aAddress   : TffNetAddress;
                              var aLocalName : TffNetName;
                              var aNetName    : TffNetName);
procedure FFMakeNetAddress(var aAddress   : TffNetAddress;
                         const aLocalName : TffNetName;
                         const aNetName    : TffNetName);

{ TCP & UDP - FFSetxxx routines expect port number to be in
  host byte order. }
procedure FFSetTCPPort(const aPort : integer);
procedure FFSetUDPPortServer (const aPort : integer);
procedure FFSetUDPPortClient (const aPort : integer);

function FFGetTCPPort : integer;
function FFGetUDPPortServer : integer;
function FFGetUDPPortClient : integer;

{ IPX/SPX - FFSetxxx routines expect port number to be in
  host byte order. }
procedure FFSetIPXSocketServer (const aSocket : integer);
procedure FFSetIPXSocketClient (const aSocket : integer);
procedure FFSetSPXSocket (const aSocket : integer);

function FFGetIPXSocketServer : integer;
function FFGetIPXSocketClient : integer;
function FFGetSPXSocket : integer;

{$IFDEF KALog}
var
  KALog : TffEventLog;
{$ENDIF}

implementation

uses
  ffsrbde;

const
  DeallocTimeOut = 500;

  { Port constants - define in network-byte order. }
  ffc_TCPPort : integer = $6563;
  ffc_UDPPortServer : integer = $6563;
  ffc_UDPPortClient : integer = $6564;
  ffc_IPXSocketServer : integer = $6563;
  ffc_IPXSocketClient : integer = $6564;
  ffc_SPXSocket : integer = $6565;

{===Helper routines==================================================}
procedure CodeBuffer(var aCode : TffNetMsgCode; var aBuf; aBufLen : integer);
register;
asm
  push ebx
  push esi
  push edi
  mov edi, eax
@@ResetCode:
  mov ebx, ffc_CodeLength
  mov esi, edi
@@NextByte:
  mov al, [edx]
  xor al, [esi]
  mov [edx], al
  dec ecx
  jz @@Exit
  inc edx
  dec ebx
  jz @@ResetCode
  inc esi
  jmp @@NextByte
@@Exit:
  pop edi
  pop esi
  pop ebx
end;
{--------}
procedure GenerateCode(aStart : longint; var aCode : TffNetMsgCode);
const
  im = 259200;
  ia = 7141;
  ic = 54773;
var
  i : integer;
begin
  {Note: routine and constants are from Numerical Recipes in Pascal, page 218}
  aStart := aStart mod im;
  for i := 0 to pred(ffc_CodeLength) do begin
    aStart := ((aStart * ia) + ic) mod im;
    aCode[i] := (aStart * 256) div im;
  end;
end;
{--------}
procedure CheckWinsockError(const ErrorCode : Integer; const Connecting : Boolean);
{ Rewritten !!.05}
{ When doing mass numbers of connects/disconnects and retrying connections
  (see TffWinsockProtocol.Call), certain errors may occur that appear to be
  timing-related (i.e., the code doesn't see that the socket is connected
  because the event from the Winsock layer has yet to be processed).
  WsaEALREADY & WsaEISCONN showed up consistently on Windows 2000.
  WsaEINVAL showed up consistently on W95. }
var
  TmpCode : Integer;
begin
  if (ErrorCode = SOCKET_ERROR) then begin
    TmpCode := WinsockRoutines.WSAGetLastError;
    if (TmpCode <> 0) and (TmpCode <> WSAEWOULDBLOCK) then
      if not (Connecting and
              ((TmpCode = WsaEALREADY) or
               (TmpCode = WsaEISCONN) or
               (TmpCode = WsaEINVAL)
              )
             ) then
      raise EffWinsockException.CreateTranslate(TmpCode, nil);
  end;  { if }
end;
{--------}
procedure FFSplitNetAddress(const aAddress   : TffNetAddress;
                              var aLocalName : TffNetName;
                              var aNetName    : TffNetName);
var
  PosAt : integer;
begin
  PosAt := Pos('@', aAddress);
  if (PosAt > 0) then begin
    aLocalName := Copy(aAddress, 1, FFMinI(Pred(PosAt), ffcl_NetNameSize)); {!!.06}
    aNetName := Copy(aAddress, succ(PosAt), FFMinI(Length(aAddress) - PosAt, ffcl_NetNameSize)); {!!.06}
  end
  else begin
    aLocalName := aAddress;
    aNetName := aAddress;
  end;
end;
{--------}
procedure FFMakeNetAddress(var aAddress   : TffNetAddress;
                         const aLocalName : TffNetName;
                         const aNetName    : TffNetName);
begin
  aAddress := aLocalName;
{Begin !!.03}
{$IFDEF IsDelphi}
  if (FFCmpShStr(aLocalName, aNetName, 255) <> 0) then begin
    FFShStrAddChar(aAddress, '@');
    FFShStrConcat(aAddress, aNetName);
  end;
{$ELSE}
  if aLocalName <> aNetName then
    aAddress := aAddress + '@' + aNetName;
{$ENDIF}
{End !!.03}
end;
{--------}
procedure FFSetTCPPort(const aPort : integer);
begin
  if not FFWSInstalled then
    raise EffWinsockException.CreateNoData(ffStrResGeneral, fferrWSNoWinsock);
  ffc_TCPPort := WinsockRoutines.htons(aPort);
end;
{--------}
procedure FFSetUDPPortServer (const aPort : integer);
begin
  if not FFWSInstalled then
    raise EffWinsockException.CreateNoData(ffStrResGeneral, fferrWSNoWinsock);
  ffc_UDPPortServer := WinsockRoutines.htons(aPort);
end;
{--------}
procedure FFSetUDPPortClient (const aPort : integer);
begin
  if not FFWSInstalled then
    raise EffWinsockException.CreateNoData(ffStrResGeneral, fferrWSNoWinsock);
  ffc_UDPPortClient := WinsockRoutines.htons(aPort);
end;
{--------}
function FFGetTCPPort : integer;
begin
  if FFWSInstalled then
    Result := WinsockRoutines.ntohs(ffc_TCPPort)
  else
    Result := 0;
end;
{--------}
function FFGetUDPPortServer : integer;
begin
  if FFWSInstalled then
    Result := WinsockRoutines.ntohs(ffc_UDPPortServer)
  else
    Result := 0;
end;
{--------}
function FFGetUDPPortClient : integer;
begin
  if FFWSInstalled then
    Result := WinsockRoutines.ntohs(ffc_UDPPortClient)
  else
    Result := 0;
end;
{--------}
procedure FFSetIPXSocketServer (const aSocket : integer);
begin
  if not FFWSInstalled then
    raise EffWinsockException.CreateNoData(ffStrResGeneral, fferrWSNoWinsock);
  ffc_IPXSocketServer := WinsockRoutines.htons(aSocket);
end;
{--------}
procedure FFSetIPXSocketClient (const aSocket : integer);
begin
  if not FFWSInstalled then
    raise EffWinsockException.CreateNoData(ffStrResGeneral, fferrWSNoWinsock);
  ffc_IPXSocketClient := WinsockRoutines.htons(aSocket);
end;
{--------}
procedure FFSetSPXSocket (const aSocket : integer);
begin
  if not FFWSInstalled then
    raise EffWinsockException.CreateNoData(ffStrResGeneral, fferrWSNoWinsock);
  ffc_SPXSocket := WinsockRoutines.htons(aSocket);
end;
{--------}
function FFGetIPXSocketServer : integer;
begin
  if FFWSInstalled then
    Result := WinsockRoutines.ntohs(ffc_IPXSocketServer)
  else
    Result := 0;
end;
{--------}
function FFGetIPXSocketClient : integer;
begin
  if FFWSInstalled then
    Result := WinsockRoutines.ntohs(ffc_IPXSocketClient)
  else
    Result := 0;
end;
{--------}
function FFGetSPXSocket : integer;
begin
  if FFWSInstalled then
    Result := WinsockRoutines.ntohs(ffc_SPXSocket)
  else
    Result := 0;
end;
{====================================================================}


{===TffConnection====================================================}
constructor TffConnection.Create(aOwner      : TffBaseCommsProtocol;
                                 aRemoteName : TffNetAddress);
begin
  inherited Create;
  FFGetMem(FCode, SizeOf(TffNetMsgCode));
  FClientID := 0;
  FHangingUp := True;
  FHangupDone := False;                                                {!!.01}
  FHangupLock := TffPadlock.Create;                                    {!!.01}
  FOwner := aOwner;
  FRemoteName := FFShStrAlloc(aRemoteName);
  MaintainLinks := False;                                              {!!.05}
end;
{--------}
destructor TffConnection.Destroy;
begin
  FHangupLock.Free;
  FFFreeMem(FCode, SizeOf(TffNetMsgCode));
  FFShStrFree(FRemoteName);
  inherited Destroy;
end;
{--------}
Procedure TffConnection.AddToList(List : TFFList);
begin {do nothing, descendant must do the work}
end;
{--------}
function TffConnection.GetRemoteName : string;                         {!!.10}
begin
  Result := FRemoteName^;
end;
{--------}
procedure TffConnection.ConfirmAlive(SendConfirm : boolean);
begin
  FAliveRetries := 0;
  FFLLBASE.SetTimer(FLastMsgTimer, FOwner.LastMsgInterval);
  FSendConfirm := SendConfirm;
end;
{--------}
procedure TffConnection.DepleteLife;
begin
{$IFDEF KALog}
  KALog.WriteStringFmt('DepleteLife, client %d', [ClientID]);
{$ENDIF}
  inc(FAliveRetries);
end;
{Begin !!.01}
{--------}
procedure TffConnection.HangupLock;
begin
  FHangupLock.Lock;
end;
{--------}
procedure TffConnection.HangupUnlock;
begin
  FHangupLock.Unlock;
end;
{End !!.01}
{--------}
procedure TffConnection.InitCode(const aStart : longint);
begin
  { Find the connection associated with this client. }

  if (aStart = 0) then begin
    FCodeStart := GetTickCount;
    if (FCodeStart = 0) then
      FCodeStart := $12345678;
  end
  else
    FCodeStart := aStart;

  GenerateCode(FCodeStart, FCode^);
end;
{--------}
function TffConnection.IsAlive : boolean;
begin
  Result := FAliveRetries < FOwner.KeepAliveRetries;
end;
{--------}
function TffConnection.IsVeryAlive : boolean;
begin
  Result := not HasTimerExpired(FLastMsgTimer);
end;
{--------}
function TffConnection.NeedsConfirmSent : boolean;
begin
  Result := FSendConfirm;
  FSendConfirm := false;
end;
{--------}
procedure TffConnection.RemoveFromList(List : TFFList);
begin {do nothing, descendant must do the work}
end;
{====================================================================}



{===TffBaseCommsProtocol=================================================}
constructor TffBaseCommsProtocol.Create(const aName   : TffNetAddress;
                                              aCSType : TffClientServerType);
var
  LocalNm : TffNetName;
  NetNm : TffNetName;
begin
  inherited Create;
  FConnLock := TffPadlock.Create;                                   
  FCSType := aCSType;
  FEventLog := nil;
  FKeepAliveInterval := ffc_KeepAliveInterval;
  FKeepAliveRetries := ffc_KeepAliveRetries;
  FLastMsgInterval := ffc_LastMsgInterval;
  FFSplitNetAddress(aName, LocalNm, NetNm);
  FLocalName := FFShStrAlloc(LocalNm);
  FLogEnabled := false;
  cpSetNetName('Local');
  FSearchTimeOut := 500;
  FStarted := false;
  FStartedEvent := TffEvent.Create;
  {the net name is set by our descendants}
  cpConnList := TffList.Create;
  cpIndexByClient := TffList.Create;
  cpIndexByClient.Sorted := True;
  cpIndexByOSConnector := nil;
  { If this protocol is for a server then create a connection lookup list.
    The lookup list serves as an index, allowing us to quickly find a
    connection object.  This is much faster than doing a sequential search
    through the cpConnList. }
  if aCSType = csServer then begin
    cpIndexByOSConnector := TFFList.Create;
    cpIndexByOSConnector.Sorted := True;
  end;
end;
{--------}
destructor TffBaseCommsProtocol.Destroy;
begin
  FStarted := false;
  FConnLock.Free;                                                   
  if assigned(FStartedEvent) then
    FStartedEvent.Free;
  FFShStrFree(FLocalName);
  FFShStrFree(FNetName);
  cpConnList.Free;
  cpIndexByClient.Free;
  if assigned(cpIndexByOSConnector) then
    cpIndexByOSConnector.Free;
  inherited Destroy;
end;
{--------}
procedure TffBaseCommsProtocol.Breathe;
var
   dummy  : pointer;
   Msg : TMsg;
begin
  if PeekMessage(Msg, FNotifyWindow, 0, 0, PM_NOREMOVE) then begin
    while PeekMessage(Msg, FNotifyWindow, 0, 0, PM_REMOVE) do
      DispatchMessage(Msg);
  end
  else begin
    dummy := nil;
    MsgWaitForMultipleObjects(0, dummy, false, 1, QS_ALLINPUT);
  end;
end;
{--------}
function TffBaseCommsProtocol.ClientIDExists(const aClientID : TffClientID) : boolean;
{Rewritten !!.05}
begin
  ConnLock;
  try
    Result := (cpIndexByClient.Index(aClientID) <> -1);
  finally
    ConnUnlock;
  end;
end;
{--------}
function TffBaseCommsProtocol.ConnectionCount : longInt;
begin
  Result := 0;
  if assigned(cpConnList) then
    Result := cpConnList.Count;
end;
{--------}
procedure TffBaseCommsProtocol.ConnLock;
begin
  FConnLock.Lock;
end;
{--------}
procedure TffBaseCommsProtocol.ConnUnlock;
begin
  FConnLock.Unlock;
end;
{--------}
procedure TffBaseCommsProtocol.cpAddConnection(aConnection : TffConnection);
{Rewritten !!.05}
var
  anItem : TffIntListItem;
begin
  ConnLock;
  try
    aConnection.InitCode(0);
    cpConnList.Insert(aConnection);
    { Add an entry to the index by client. }
    anItem := TffIntListItem.Create(aConnection.ClientID);
    anItem.ExtraData := aConnection;
    cpIndexByClient.Insert(anItem);
    if Assigned(cpIndexByOSConnector) then
      aConnection.AddToList(cpIndexByOSConnector);
  finally
    ConnUnlock;
  end;
end;
{--------}
procedure TffBaseCommsProtocol.cpCodeMessage(aConn : TffConnection;
                                             aData : PffByteArray;
                                             aDataLen : longint);
const
  LeaveRawLen = 2 * sizeof(longint);
var
  aCode : TffNetMsgCode;
begin
  if (aDataLen >= LeaveRawLen) then begin
    if (PffLongint(aData)^ <> ffnmAttachServer) then begin
      aCode := aConn.Code^;
      CodeBuffer(aCode, aData^[LeaveRawLen], aDataLen - LeaveRawLen);
    end;
  end
end;
{--------}
function TffBaseCommsProtocol.cpCreateNotifyWindow : boolean;
begin
  FNotifyWindow := 0;
  Result := false;
end;
{--------}
procedure TffBaseCommsProtocol.cpDestroyNotifyWindow;
begin
  if (FNotifyWindow <> 0) then begin
    KillTimer(FNotifyWindow, 1);
    {$IFDEF DCC6OrLater}                                               {!!.11}
      {$WARN SYMBOL_DEPRECATED OFF}
    {$ENDIF}
    DeallocateHWnd(FNotifyWindow);
  {$IFDEF DCC6OrLater}                                                 {!!.11}
    {$WARN SYMBOL_DEPRECATED ON}
  {$ENDIF}
  end;
end;
{--------}
procedure TffBaseCommsProtocol.cpDoHangUp(aConn : TffConnection);
begin
{Begin !!.01}
  aConn.HangupLock;
  try
    if aConn.HangupDone then
      Exit;
    { Are we hanging up on purpose? }
    if aConn.HangingUp then begin
      { Yes. Call the OnHangUp event if it is declared. }
      if Assigned(FOnHangUp) then
        FOnHangUp(Self, aConn.ClientID);
    end
    { No. This is an unexpected hangup.  Invoke OnConnectionLost if it is
      declared. }
    else if Assigned(FOnConnectionLost) then
      FOnConnectionLost(Self, aConn.ClientID);
    aConn.HangupDone := True;
  finally
    aConn.HangupUnlock;
  end;
{End !!.01}
end;
{--------}
procedure TffBaseCommsProtocol.cpDoHeardCall(aConnHandle : longint);
begin
  if Assigned(FHeardCall) then
    FHeardCall(Self, aConnHandle);
end;
{--------}
procedure TffBaseCommsProtocol.cpPerformShutdown;
begin
  cpDestroyNotifyWindow;
end;
{--------}
procedure TffBaseCommsProtocol.cpSetNetName(aName : string);
begin
  if assigned(FNetName) then
    FFShStrFree(FNetName);

  FNetName := FFShStrAlloc(aName);
end;
{--------}
procedure TffBaseCommsProtocol.cpDoReceiveDatagram(const aName    : TffNetName;
                                                         aData    : PffByteArray;
                                                         aDataLen : longint);
begin
  if Assigned(FReceiveDatagram) then
    FReceiveDatagram(Self, aName, aData, aDataLen);
end;
{--------}
function TffBaseCommsProtocol.cpDoReceiveMsg(aConn : TffConnection;
                                             msgData : PffByteArray;
                                             msgDataLen : longInt) : boolean;
begin
  {Look out for keep alives}
  if (PffLongint(msgData)^ = ffnmCheckConnection) then begin
    cpGotCheckConnection(aConn);
    Result := true;
    Exit;
  end;

  {process normal FF message}
{$IFDEF KALog}
  KALog.WriteStringFmt('RcvMsg, client %d', [aConn.ClientID]);
{$ENDIF}
  aConn.ConfirmAlive(false);
  { If this message is too big for us then reject it. }

  if msgDataLen > FMaxNetMsgSize then begin
    LogStrFmt('Message size %d too large.', [msgDataLen]);
    Result := False;
  end
  { Otherwise if we have a handler for the message then send the message
    to the handler. }
  else if Assigned(FReceiveMsg) then begin
    cpCodeMessage(aConn, msgData, msgDataLen);
    Result := FReceiveMsg(Self, aConn.ClientID, msgData, msgDataLen);
  end else
    { Otherwise no handler so just smile. }
    Result := true;
end;
{--------}
function TffBaseCommsProtocol.cpExistsConnection(aConnHandle : longint) : boolean;
begin
  Result := cpConnList.Exists(aConnHandle);
end;
{--------}
function TffBaseCommsProtocol.cpFindConnection(const aClientID : TffClientID) : Longint;
var
  Inx : Longint;
begin
  Result := -1;
  for Inx := 0 to pred(cpConnList.Count) do
    if TffConnection(cpConnList[Inx]).ClientID = aClientID then begin
      Result := Inx;
      break;
    end;
end;
{--------}
function TffBaseCommsProtocol.cpGetConnection(const aClientID : TffClientID) : TffConnection;
{ Modified !!.05}
var
  Inx : integer;
begin
  { Note: It is possible for a newly-attached client to send another request to
          the server before the server has had a chance to update the new
          client's server-side clientID.  So we use a lock to prevent this
          from happening. }
  ConnLock;
  try
    Inx := cpIndexByClient.Index(aClientID);
    if (Inx = -1) then
      Result := nil
    else
      Result := TffConnection(TffIntListItem(cpIndexByClient[Inx]).ExtraData);
  finally
    ConnUnlock;
  end;
end;
{--------}
function TffBaseCommsProtocol.cpGetConnectionIDs(const anIndex : longInt) : TffClientID;
{Begin !!.01}
var
  aConn : TffConnection;
begin
  aConn := TffConnection(cpConnList[anIndex]);
  if aConn = nil then
    Result := 0
  else
    Result := TffConnection(cpConnList[anIndex]).ClientID;
{End !!.01}
end;
{--------}
procedure TffBaseCommsProtocol.cpGotCheckConnection(aConn : TffConnection);
begin
  {Reset keepalives}
  if assigned(aConn) then begin
{$IFDEF KALog}
    KALog.WriteStringFmt('RcvKA, client %d', [aConn.ClientID]);
{$ENDIF}
    aConn.ConfirmAlive(true);
  end;
end;
{--------}
procedure TffBaseCommsProtocol.cpRemoveConnection(aClientID : TffClientID);
var
  Inx : integer;
  aConn : TffConnection;
begin
{Begin !!.05}
  ConnLock;
  try
    Inx := cpIndexByClient.Index(aClientID);
    { Did we find the connection in the index? }
    if (Inx >= 0) then begin
      { Yes. Remove the connection from the index and from the connection
        list. }
      aConn := TffConnection(cpIndexByClient[Inx]).ExtraData;
      cpIndexByClient.DeleteAt(Inx);
      cpConnList.Remove(aConn);
      if assigned(cpIndexByOSConnector) then
        aConn.RemoveFromList(cpIndexByOSConnector);
      aConn.Free;
    end
    else begin
      { No. It may be that we have encountered a client that could not
        successfully connect. We have an entry in the connection list but not
        in the index. Do a sequential search for the client. }
      Inx := cpFindConnection(aClientID);
      if Inx >= 0 then begin
        aConn := TffConnection(cpConnList[Inx]);
        cpConnList.RemoveAt(Inx);
        aConn.Free;
      end;
    end;
  finally
    ConnUnlock;
  end;
{End !!.05}
end;
{--------}
procedure TffBaseCommsProtocol.cpTimerTick;
var
  Inx   : integer;
  Conn  : TffConnection;
  KAMsg : longint;
begin
{Begin !!.05}
  ConnLock;
  try
    KAMsg := ffnmCheckConnection;
    for Inx := pred(cpConnList.Count) downto 0 do begin
      Conn := TffConnection(cpConnList[Inx]);
      with Conn do begin
        if (not Conn.FHangupLock.Locked) and (not IsAlive) then begin  {!!.11}
{$IFDEF KALog}
          KALog.WriteStringFmt('Hangup, client %d', [Conn.ClientID]);
{$ENDIF}
          Conn.HangingUp := False;                                     {!!.06}
          HangUp(Conn);
        end
        else if NeedsConfirmSent or (not IsVeryAlive) then begin
{$IFDEF KALog}
          KALog.WriteStringFmt('Send KA, client %d', [Conn.ClientID]);
{$ENDIF}
          SendMsg(ClientID, @KAMsg, sizeof(KAMsg), False);            {!!.06}
          DepleteLife;
        end;
      end;
    end;
  finally
    ConnUnlock;
  end;
{End !!.05}
end;
{--------}
function TffBaseCommsProtocol.GetLocalName : string;                   {!!.10}
begin
  if Assigned(FLocalName) then
    Result := FLocalName^
  else
    Result := '';
end;
{--------}
function TffBaseCommsProtocol.GetNetName : string;                     {!!.10}
begin
  if Assigned(FNetName) then
    Result := FNetName^
  else
    Result := '';
end;
{--------}
function TffBaseCommsProtocol.GetCodeStart(const aClientID : TffClientID) : integer;
var
  aConn : TffConnection;
  anItem : TffIntListItem;
begin
  { Assumption: Connection lists locked via ConnLock at a higher level. }
  Result := 0;
  { Find the connection associated with this client. }
  anItem := TffIntListItem(cpIndexByClient[cpIndexByClient.Index(aClientID)]);
  if assigned(anItem) then begin
    aConn := TffConnection(anItem.ExtraData);
    Result := aConn.CodeStart;
  end;
end;
{--------}
class function TffBaseCommsProtocol.GetProtocolName : string;
begin
  { return nothing at this level }
  Result := '';
end;
{--------}
procedure TffBaseCommsProtocol.HangUpByClientID(aClientID : TffClientID);
var
  aConn : TffConnection;
begin
  aConn := cpGetConnection(aClientID);
  if assigned(aConn) then begin
    aConn.HangingUp := True;
    HangUp(aConn);
  end;
end;
{Begin !!.01}
{--------}
procedure TffBaseCommsProtocol.HangupDone(aClientID : TffClientID);
var
  aConn : TffConnection;
begin
  aConn := cpGetConnection(aClientID);
  if assigned(aConn) then
    aConn.HangupDone := True;
end;
{--------}
function TffBaseCommsProtocol.HangupIsDone(aClientID : TffClientID) : Boolean;
var
  aConn : TffConnection;
begin
  Result := False;
  aConn := cpGetConnection(aClientID);
  if assigned(aConn) then
    Result := aConn.HangupDone;
end;
{--------}
procedure TffBaseCommsProtocol.HangupLock(aClientID : TffClientID);
var
  aConn : TffConnection;
begin
  aConn := cpGetConnection(aClientID);
  if assigned(aConn) then
    aConn.HangupLock;
end;
{--------}
procedure TffBaseCommsProtocol.HangupUnlock(aClientID : TffClientID);
var
  aConn : TffConnection;
begin
  aConn := cpGetConnection(aClientID);
  if assigned(aConn) then
    aConn.HangupUnlock;
end;
{End !!.01}
{--------}
procedure TffBaseCommsProtocol.InitCode(const aClientID : TffClientID;
                                        const aStart : longint);
var
  aConn : TffConnection;
  anItem : TffIntListItem;
begin
  { Find the connection associated with this client. }
  anItem := TffIntListItem(cpIndexByClient[cpIndexByClient.Index(aClientID)]);
  if assigned(anItem) then begin
    aConn := TffConnection(anItem.ExtraData);
    aConn.InitCode(aStart);
  end;
end;
{--------}
procedure TffBaseCommsProtocol.ResetKeepAliveTimer;
begin
  if (FNotifyWindow <> 0) then begin
{$IFDEF KALog}
    KALog.WriteStringFmt('ResetKeepAliveTimer: protocol %d', [Longint(Self)]);
{$ENDIF}
    KillTimer(FNotifyWindow, 1);
    Windows.SetTimer(FNotifyWindow, 1, FKeepAliveInterval, nil);       {!!.05}
  end;
end;
{--------}
procedure TffBaseCommsProtocol.Shutdown;
begin
  if IsStarted then begin
    cpPerformShutdown;
    FStarted := false;
  end;
end;
{--------}
procedure TffBaseCommsProtocol.StartUp;
begin
  if not IsStarted then begin
    cpPerformStartUp;
    FStarted := true;
    FStartedEvent.SignalEvent;
  end;
end;
{--------}
class function TffBaseCommsProtocol.Supported : boolean;
begin
  Result := True;
end;
{--------}
procedure TffBaseCommsProtocol.UpdateClientID(const oldClientID,
                                                    newClientID : TffClientID);
var
  aConn : TffConnection;
  anItem : TffIntListItem;
begin
{Begin !!.05}
  ConnLock;
  try
    anItem := TffIntListItem(cpIndexByClient[cpIndexByClient.Index(oldClientID)]);
    if assigned(anItem) then begin
      aConn := anItem.ExtraData;
      aConn.ClientID := newClientID;

      { Get rid of the old index entry; as a side effect, anItem should be
        freed. }
      cpIndexByClient.Delete(oldClientID);

      { Create a new entry for the index. }
      anItem := TffIntListItem.Create(newClientID);
      anItem.ExtraData := aConn;
      cpIndexByClient.Insert(anItem);
    end;
  finally
    ConnUnlock;
  end;
{End !!.05}
end;
{--------}
procedure TffBaseCommsProtocol.LogStr(const aMsg : string);
begin
  if FLogEnabled and assigned(FEventLog) then
    FEventLog.WriteSTring(format('%s: %s',
                                 [Self.GetProtocolName, aMsg]));
end;
{--------}
procedure TffBaseCommsProtocol.LogStrFmt(const aMsg : string;
                                         args : array of const);
begin
  if FLogEnabled and assigned(FEventLog) then
    LogStr(format(aMsg, args));
end;
{====================================================================}

{===TffWinsockConnection=============================================}
constructor TffWinsockConnection.Create(aOwner      : TffBaseCommsProtocol;
                                        aRemoteName : TffNetAddress;
                                        aSocket     : TffwsSocket;
                                        aFamily     : TffWinsockFamily;
                                        aNotifyWnd  : HWND);
var
  NagelOn   : Bool;
begin
  inherited Create(aOwner, aRemoteName);
  FHangingUp := False;
    { Note that we are overriding the initial value of FHangingUp on purpose. }
  FSocket := aSocket;
  FFamily := aFamily;
  if (aFamily = wfTCP) then begin
    FFWSGetSocketOption(aSocket, IPPROTO_TCP, TCP_NODELAY, NagelOn, sizeof(NagelOn));
    if NagelOn then begin
      NagelOn := false;
      FFWSSetSocketOption(aSocket, IPPROTO_TCP, TCP_NODELAY, NagelOn, sizeof(NagelOn));
    end;
  end;
  FFWSSetSocketOption(aSocket, SOL_SOCKET, So_RCVBUF, ffc_TCPRcvBuf,
                      sizeof(ffc_TCPRcvBuf));
  FFWSSetSocketOption(aSocket, SOL_SOCKET, So_SNDBUF, ffc_TCPSndBuf,
                      sizeof(ffc_TCPSndBuf));
  FFWSGetSocketOption(aSocket, SOL_SOCKET, So_RCVBUF, wscRcvBuf,
                      sizeof(wscRcvBuf));
  FFWSGetSocketOption(aSocket, SOL_SOCKET, So_SNDBUF, wscSndBuf,
                      sizeof(wscSndBuf));
  wscNotifyWnd := aNotifyWnd;
//  wscPortal := TffReadWritePortal.Create;                            {Deleted !!.05}
  GetMem(wscRcvBuffer, ffc_MaxWinsockMsgSize);
  wscPacketHead := Nil;
  wscPacketTail := Nil;
  wscIsSending := False;
end;
{--------}
destructor TffWinsockConnection.Destroy;
var
  aPacket : PffwscPacket;
begin
  HangupLock;                                                          {!!.05}
//  wscPortal.BeginWrite;                                              {Deleted !!.05}
  try
    try
       FFWSDestroySocket(Socket);
    except
    end;
    while wscPacketHead <> Nil do begin
      aPacket := wscPacketHead^.lpNext;
      ffFreeMem(wscPacketHead^.lpData, wscPacketHead^.dwLength);
      ffFreeMem(wscPacketHead, sizeof(TffwscPacket));
      wscPacketHead := aPacket;
    end;
    FreeMem(wscRcvBuffer, ffc_MaxWinsockMsgSize);
  finally
    HangupUnlock;                                                      {!!.05}
//    wscPortal.EndWrite;                                              {Deleted !!.05}
//    wscPortal.Free;                                                  {Deleted !!.05}
  end;
  inherited Destroy;
end;
{--------}
Procedure TffWinsockConnection.AddToList(List : TFFList);
var
  T : TffIntListItem;
begin {add a list entry to allow socket lookups}
  T := TffIntListItem.Create(Socket);
  T.ExtraData := Self;
  List.Insert(T);
end;
{--------}
Procedure TffWinsockConnection.RemoveFromList(List : TFFList);
begin
   List.Delete(FSocket);
end;
{--------}
function TffWinsockConnection.Send(aData      : PffByteArray;
                                   aDataLen   : longint;
                                   aDataStart : longint;
                               var aBytesSent : longint;
                                   aConnLock  : Boolean) : integer;   {!!.06}
var
  BytesSent    : longint;
  PacketBuffer : PffwscPacket;
begin
  if aConnLock then                                                   {!!.06}
    HangupLock;                                                       {!!.05}
//  wscPortal.BeginWrite;                                             {Deleted !!.05}
  try
    Result := 0;
    if (aDataLen-aDataStart) > 0 then begin
      {Add the data packet to the wscPacketList }
      ffGetMem(PacketBuffer,sizeof(TffwscPacket));
      ffGetMem(PacketBuffer^.lpData, aDataLen);
      PacketBuffer^.dwLength := aDataLen;
      PacketBuffer^.dwStart := aDataStart;
      Move(aData^[0], PacketBuffer^.lpData^, PacketBuffer^.dwLength);
      Owner.cpCodeMessage(Self, PacketBuffer^.lpData, PacketBuffer^.dwLength);
      PacketBuffer^.lpNext := Nil;
      {Add the packet to the end of the list }
      if not assigned(wscPacketHead) then
        wscPacketHead := PacketBuffer
      else if assigned(wscPacketTail) then
        wscPacketTail^.lpNext := PacketBuffer;
      wscPacketTail := PacketBuffer;
      aBytesSent := 0;                                                 {!!.06}
//      aBytesSent := aDataLen-aDataStart; {Always report all bytes sent} {Deleted !!.06}
    end;
    if (not wscIsSending) and Assigned(wscPacketHead) then begin
      {now try to send some data}
      try
        {send the first waiting data packet}
        BytesSent := WinsockRoutines.send(Socket,
                                          wscPacketHead^.lpData^[wscPacketHead^.dwStart],
                                          wscPacketHead^.dwLength-wscPacketHead^.dwStart,
                                          0);
      except
        BytesSent := SOCKET_ERROR;
      end;
      if (BytesSent = SOCKET_ERROR) then begin
        {There was an error sending }
        Result := WinsockRoutines.WSAGetLastError;
        if (Result = WSAEWOULDBLOCK) then begin
           { Mark this connection as blocked and leave the Packet on the list. }
           wscIsSending := True;
//           Result := 0;                                              {Deleted !!.06}
        end
{Begin !!.06}
        else if Result = 0 then
          { If no error code returned then reset the Result to -1 so that we
            break out of the send loop, avoiding a re-add of the current
            packet to the packet list. }
          Result := -1;
{End !!.06}
      end else if BytesSent < (wscPacketHead^.dwLength - wscPacketHead^.dwStart) then begin
        { we didn't send the whole thing, so re-size the data packet}
        inc(wscPacketHead^.dwStart, BytesSent);
        inc(aBytesSent, BytesSent);                                    {!!.06}
        { now try sending the remaining data again }
        Result := Send(nil, 0, 0, aBytesSent, aConnLock);              {!!.06}
      end else begin
        {we sent the packet, so remove it and continue }
        ffFreeMem(wscPacketHead^.lpData, wscPacketHead^.dwLength);
        PacketBuffer := wscPacketHead;
        wscPacketHead := wscPacketHead^.lpNext;
        if not Assigned(wscPacketHead) then
          wscPacketTail := Nil;
        ffFreeMem(PacketBuffer, sizeof(TffwscPacket));
        inc(aBytesSent, BytesSent);                                    {!!.11}
        Result := 0;
      end;
    end;
  finally
    if aConnLock then                                                  {!!.06}
      HangupUnlock;                                                    {!!.05}
//    wscPortal.EndWrite;                                              {Deleted !!.05}
  end;
end;
{--------}
procedure TffWinsockConnection.StartReceive;
begin
  FFWSAsyncSelect(Socket, wscNotifyWnd,
                  FD_READ or FD_WRITE or FD_CLOSE);
end;
{====================================================================}


{===TffWinsockProtocol===============================================}
constructor TffWinsockProtocol.Create(const aName   : TffNetAddress;
                                            aCSType : TffClientServerType);
begin
  {make sure Winsock is installed}
  if not FFWSInstalled then
    raise EffWinsockException.CreateNoData(ffStrResGeneral, fferrWSNoWinsock);
  {let our ancestor create itself}
  inherited Create(aName, aCSType);
  FCollectingServerNames := false;
  FDatagramPadlock := TffPadlock.Create;
  FMaxNetMsgSize := ffc_MaxWinsockMsgSize;
  FServerNames := TStringList.Create;
  FServerNames.Sorted := True;
  FServerNames.Duplicates := dupIgnore;
  {set the sockets we use to default values}
  wspListenSocket := INVALID_SOCKET;
  wspRcvDatagramSocket := INVALID_SOCKET;
  {allocate a receive datagram buffer}
  GetMem(wspRcvDGBuffer, ffc_MaxDatagramSize);
end;
{--------}
destructor TffWinsockProtocol.Destroy;
begin
  if assigned(FServerNames) then
    FServerNames.Free;
  if assigned(FDatagramPadlock) then
    FDatagramPadlock.Free;
  FFWSDestroySocket(wspListenSocket);
  FFWSDestroySocket(wspRcvDatagramSocket);
  inherited Destroy;
  FFShStrFree(FNetName);
  FreeMem(wspRcvDGBuffer, ffc_MaxDatagramSize);
end;
{--------}
function TffWinsockProtocol.Call(const aServerName : TffNetName;
                                   var aClientID : TffClientID;
                                 const timeout : longInt) : TffResult;
var
  NewSocket   : TffwsSocket;
  Conn        : TffWinsockConnection;
  SASize      : integer;
  AddrFamily  : integer;
  Protocol    : integer;
  RemSockAddr : TffwsSockAddr;
  aNetName    : TffNetName;
  T           : TffTimer;                                              {!!.05}
  StartTime   : DWORD;                                                 {!!.05}
begin

  Result := DBIERR_NONE;

  {servers don't call}
  if (CSType = csServer) then
    raise EffCommsException.CreateNoData(ffStrResGeneral, fferrCommsCannotCall);

  { If no servername then we cannot connect. }
  if (aServerName = '') then begin
    Result := DBIERR_SERVERNOTFOUND;
    Exit;
  end;

  {either create a socket address record for TCP...}
  if (Family = wfTCP) then begin
    AddrFamily := AF_INET;
    Protocol := 0;
    SASize := sizeof(TffwsSockAddrIn);
    FillChar(RemSockAddr, SASize, 0);
    with RemSockAddr.TCP do begin
      sin_family := PF_INET;
      sin_port := ffc_TCPPort;
      if FFWSCvtStrToAddr(aServerName, sin_addr) then
//      aNetName := FFWSGetRemoteNameFromAddr(sin_addr)
      else begin
        if not FFWSGetRemoteHost(aServerName, aNetName, sin_addr) then begin
          Result := DBIERR_SERVERNOTFOUND;                                      {!!.06}
          Exit;
        end;
      end;
    end;
  end
  {or for IPX...}
  else {if (Family = wfIPX) then} begin
    AddrFamily := AF_IPX;
    Protocol := NSPROTO_SPX;
    SASize := sizeof(TffwsSockAddrIPX);
    FillChar(RemSockAddr, SASize, 0);
    with RemSockAddr.IPX do begin
      sipx_family := PF_IPX;
      if not FFWSCvtStrToIPXAddr(aServerName,
                                 sipx_netnum,
                                 sipx_nodenum) then
        Exit;
      sipx_socket := ffc_SPXSocket;
    end;
  end;
  {open a call socket}
  NewSocket := FFWSCreateSocket(AddrFamily, SOCK_STREAM, Protocol);
  try
    {set the socket to non-blocking mode}
    FFWSAsyncSelect(NewSocket, FNotifyWindow, FD_CONNECT);
    {try and connect}
    wspWaitingForConnect := true;
    CheckWinsockError(
       WinsockRoutines.connect(NewSocket, RemSockAddr, SASize), False);
{Begin !!.05}
//    wspWaitForConnect(timeout, NewSocket);
    StartTime := GetTickCount;
    SetTimer(T, timeout);
    while wspWaitingForConnect and (not HasTimerExpired(T)) do begin
      if (GetTickCount - StartTime) > ffc_ConnectRetryTimeout then begin
        CheckWinsockError(WinsockRoutines.connect(NewSocket, RemSockAddr,
                                                  SASize), True);
        Starttime := GetTickCount;
      end;
      Breathe;
    end;
{End !!.05}
    {if we connected...}
    if not wspWaitingForConnect then begin
      {create a new connection}
      Conn := TffWinsockConnection.Create(Self, aNetName, NewSocket, Family,
                                          FNotifyWindow);
      Conn.ClientID := Conn.Handle;
      aClientID := Conn.Handle;
      cpAddConnection(Conn);
      Conn.StartReceive;
    end
    else begin {we didn't connect}
      FFWSDestroySocket(NewSocket);
      Result := DBIERR_SERVERNOTFOUND;
    end;
  except
    FFWSDestroySocket(NewSocket);
    raise;
  end;{try..except}
end;
{--------}
procedure TffWinsockProtocol.cpDoReceiveDatagram(const aName    : TffNetName;
                                                       aData    : PffByteArray;
                                                       aDataLen : longint);
var
  Addr : TffNetAddress;                           { sender }
  Datagram : PffnmServerNameReply absolute aData; { sender }
  Msg : PffnmRequestServerName absolute aData;    { listener }
  Reply : TffnmServerNameReply;                   { listener }
begin
  inherited cpDoReceiveDatagram(aName, aData, aDataLen);
  FDatagramPadlock.Lock;
  try
    { If we are on the sending side, waiting for server names to roll in
      then get the server's reply and add it to our list of server names. }
    if FCollectingServerNames then begin
      if assigned(aData) and (Datagram^.MsgID = ffnmServerNameReply) then begin
        FFMakeNetAddress(Addr, Datagram^.ServerLocalName, aName);
        FServerNames.Add(Addr);
      end;
    end else
      { Otherwise, we are on the listening side and a client is asking us to
        identify ourself. }
      if (aDataLen = sizeof(TffnmRequestServerName)) and
         (Msg^.MsgID = ffnmRequestServerName) then begin
        {send a message back to the caller with our name}
        Reply.MsgID := ffnmServerNameReply;
        Reply.ServerLocalName := LocalName;
        Reply.ServerNetName := NetName;
        SendDatagram(aName, @Reply, sizeof(Reply));
      end;
  finally
    FDatagramPadlock.Unlock;
  end;
end;
{--------}
procedure TffWinsockProtocol.cpPerformStartUp;
var
  AddrFamily : integer;
  Protocol   : integer;
  SASize     : integer;
  SockAddr   : TffwsSockAddr;
begin
  {create our notify window}
  if not cpCreateNotifyWindow then begin
    LogStr('Could not create notification window.');
    raise EffCommsException.CreateNoData(ffStrResGeneral, fferrCommsNoWinRes);
  end;

  {create and bind the listen socket if we're a server; for a client,
   we never would listen}
  if (CSType = csServer) then begin
    {==the listen socket==}
    {create a socket address record}
    if (Family = wfTCP) then begin
      AddrFamily := AF_INET;
      Protocol := 0;
      SASize := sizeof(TffwsSockAddrIn);
      FillChar(SockAddr, SASize, 0);
      with SockAddr.TCP do begin
        sin_family := PF_INET;
        sin_port := ffc_TCPPort;
        sin_addr := wspLocalInAddr;
      end;
    end
    else {if (Family = wfIPX) then} begin
      AddrFamily := AF_IPX;
      Protocol := NSPROTO_SPX;
      SASize := sizeof(TffwsSockAddrIPX);
      FillChar(SockAddr, SASize, 0);
      with SockAddr.IPX do begin
        sipx_family := PF_IPX;
        sipx_netnum := wspLocalIPXNetNum;
        sipx_nodenum := wspLocalIPXAddr;
        sipx_socket := ffc_SPXSocket;
      end;
    end;
    {open a listen socket}
    wspListenSocket := FFWSCreateSocket(AddrFamily, SOCK_STREAM, Protocol);
    {bind the socket to the address}
    CheckWinsockError(
       WinsockRoutines.bind(wspListenSocket, SockAddr, SASize), False);
  end;
end;
{--------}
procedure TffWinsockProtocol.GetServerNames(aList : TStrings; const timeout : longInt);
var
  TotalTimer : TffTimer;
  NameReq    : TffnmRequestServerName;
begin
  if not assigned(aList) then
    exit;

  { Open and prepare a UDP socket. }
  ReceiveDatagram;
  FCollectingServerNames := true;
  try
    aList.Clear;
    FServerNames.Clear;
    NameReq.MsgID := ffnmRequestServerName;
    SetTimer(TotalTimer, timeout);                                     {!!.13}
    SendDatagram('', @NameReq, sizeOf(NameReq));
    repeat
      Breathe;
    until HasTimerExpired(TotalTimer);
    aList.Assign(FServerNames);
  finally
    FCollectingServerNames := false;
    StopReceiveDatagram;
  end;

end;
{--------}
procedure TffWinsockProtocol.HangUp(aConn : TffConnection);
begin
  cpDoHangUp(aConn);
  cpRemoveConnection(aConn.ClientID);
end;
{--------}
procedure TffWinsockProtocol.Listen;
begin
  {clients don't listen}
  if (CSType = csClient) then
    raise EffCommsException.CreateNoData(ffStrResGeneral, fferrCommsCantListen);
  {start listening, if not doing so already}
  if not wspListening then begin
    FFWSAsyncSelect(wspListenSocket, FNotifyWindow, FD_ACCEPT);
    CheckWinsockError(WinsockRoutines.listen(wspListenSocket, SOMAXCONN), False);
    wspListening := true;
  end;
end;
{--------}
procedure TffWinsockProtocol.ReceiveDatagram;
var
  AddrFamily : integer;
  Protocol   : integer;
  SASize     : integer;
  BCastOn    : Bool;
  SockAddr   : TffwsSockAddr;
begin
  if not wspReceivingDatagram then begin
    {create and bind the receive datagram socket}
    {create a socket address record}
    if (Family = wfTCP) then begin
      AddrFamily := AF_INET;
      Protocol := 0;
      SASize := sizeof(TffwsSockAddrIn);
      FillChar(SockAddr, SASize, 0);
      with SockAddr.TCP do begin
        sin_family := PF_INET;
        if (CSType = csClient) then
          sin_port := ffc_UDPPortClient
        else
          sin_port := ffc_UDPPortServer;
        sin_addr := wspLocalInAddr;
      end;
    end
    else {if (Family = wfIPX) then} begin
      AddrFamily := AF_IPX;
      Protocol := NSPROTO_IPX;
      SASize := sizeof(TffwsSockAddrIPX);
      FillChar(SockAddr, SASize, 0);
      with SockAddr.IPX do begin
        sipx_family := PF_IPX;
        sipx_netnum := wspLocalIPXNetNum;
        sipx_nodenum := wspLocalIPXAddr;
        if (CSType = csClient) then
          sipx_socket := ffc_IPXSocketClient
        else
          sipx_socket := ffc_IPXSocketServer;
      end;
    end;
    {open a receivedatagram socket}
    wspRcvDatagramSocket := FFWSCreateSocket(AddrFamily,
                                             SOCK_DGRAM,
                                             Protocol);
    {make sure the socket can do broadcasts (req for IPX)}
    if (Family = wfIPX) then begin
      BCastOn := true;
      FFWSSetSocketOption(wspRcvDatagramSocket, SOL_SOCKET, SO_BROADCAST,
                          BCastOn, sizeof(BCastOn));
    end;
    {bind the socket to the address}
    CheckWinsockError(
       WinsockRoutines.bind(wspRcvDatagramSocket, SockAddr, SASize), False);
    FFWSAsyncSelect(wspRcvDatagramSocket, FNotifyWindow, FD_READ or FD_WRITE);
    wspReceivingDatagram := true;
  end;
end;
{--------}
procedure TffWinsockProtocol.SendDatagram(const aName    : TffNetName;
                                                aData    : PffByteArray;
                                                aDataLen : longint);
var
  SockAddr : TffwsSockAddr;
  Socket   : TffwsSocket;
  SASize   : integer;
  BCastOn  : Bool;
  NetName  : TffNetName;
begin
  {create a send datagram socket}
  if (Family = wfTCP) then begin
    Socket := FFWSCreateSocket(AF_INET, SOCK_DGRAM, 0);
  end
  else {Family <> wfTCP} begin
    Socket := FFWSCreateSocket(AF_IPX, SOCK_DGRAM, NSPROTO_IPX);
  end;
  try
    {create the socket address to bind to}
    if (aName = '') then begin {a broadcast message}
      {create a socket address record}
      if (Family = wfTCP) then begin
        SASize := sizeof(TffwsSockAddrIn);
        FillChar(SockAddr, SASize, 0);
        with SockAddr.TCP do begin
          sin_family := PF_INET;
          if (CSType = csClient) then
            sin_port := ffc_UDPPortServer
          else
            sin_port := ffc_UDPPortClient;
          sin_addr := INADDR_BROADCAST;
        end;
      end
      else {Family <> wfTCP} begin
        SASize := sizeof(TffwsSockAddrIPX);
        FillChar(SockAddr, SASize, 0);
        with SockAddr.IPX do begin
          sipx_family := PF_IPX;
          FillChar(sipx_nodenum, sizeof(sipx_nodenum), $FF);
          if (CSType = csClient) then
            sipx_socket := ffc_IPXSocketServer
          else
            sipx_socket := ffc_IPXSocketClient;
        end;
      end;
      {make sure the socket can do broadcasts}
      BCastOn := true;
      FFWSSetSocketOption(Socket, SOL_SOCKET, SO_BROADCAST, BCastOn, sizeof(BCastOn));
    end
    else begin {a specific target}
      {create a socket address record}
      if (Family = wfTCP) then begin
        SASize := sizeof(TffwsSockAddrIn);
        FillChar(SockAddr, SASize, 0);
        with SockAddr.TCP do begin
          sin_family := PF_INET;
          if (CSType = csClient) then
            sin_port := ffc_UDPPortServer
          else
            sin_port := ffc_UDPPortClient;
          if not FFWSCvtStrToAddr(aName, sin_addr) then
            if not FFWSGetRemoteHost(aName, NetName, sin_addr) then
              Exit;
        end;
      end
      else {Family <> wfTCP} begin
        SASize := sizeof(TffwsSockAddrIPX);
        FillChar(SockAddr, SASize, 0);
        with SockAddr.IPX do begin
          sipx_family := PF_IPX;
          if not FFWSCvtStrToIPXAddr(aName, sipx_netnum, sipx_nodenum) then
            Exit;
          if (CSType = csClient) then
            sipx_socket := ffc_IPXSocketServer
          else
            sipx_socket := ffc_IPXSocketClient;
        end;
      end;
    end;
    CheckWinsockError(
       WinsockRoutines.sendto(Socket, aData^, aDataLen, 0, SockAddr, SASize),
       False);
  finally
    FFWSDestroySocket(Socket);
  end;{try.finally}
end;
{--------}
function TffWinsockProtocol.SendMsg(aClientID : TffClientID;
                                    aData     : PffByteArray;
                                    aDataLen  : longint;
                                    aConnLock : Boolean) : TffResult; {!!.06}
var
  Conn : TffWinsockConnection;
  SendResult : integer;
  BytesSent  : longint;
  SentSoFar  : longint;
  DataPtr : PffByteArray;                                              {!!.06}
  DataLen : Longint;                                                   {!!.06}
  TimerExpired : Boolean;                                              {!!.06}
begin
  Result := DBIERR_NONE;
  Conn := TffWinsockConnection(cpGetConnection(aClientID));
  if Assigned(Conn) then begin
    DataPtr := aData;                                                  {!!.06}
    DataLen := aDataLen;                                               {!!.06}
    SentSoFar := 0;
    while (SentSoFar < DataLen) do begin
      SendResult := Conn.Send(DataPtr, DataLen, SentSoFar, BytesSent,  {!!.06}
                              aConnLock);                              {!!.06}
      if (SendResult = WSAEWOULDBLOCK) then begin
{Begin !!.06}
        TimerExpired := wspWaitForSendToUnblock;
        { The connection has the packet already on its list, waiting to be
          resent. Reset the data pointer & length so that the connection
          does not add a duplicate packet to its list. }
        DataPtr := nil;
        DataLen := 0;
        { The connection may have been killed (hung up), so recheck. }
        Conn := TffWinsockConnection(cpGetConnection(aClientID));
        if Conn = nil then
          Exit
        else if TimerExpired then begin
          wspWaitingForSendToUnblock := False;
          Conn.IsSending := False;
        end;
{End !!.06}
      end
      else if (SendResult <> 0) then begin
        LogStrFmt('Unhandled Winsock Exception   %d', [SendResult]);
        Result := SendResult;
//        Conn.HangingUp := True;                                      {Deleted !!.06}
//        HangUp(Conn);                                                {Deleted !!.06}
        exit;
      end
      else begin
        inc(SentSoFar, BytesSent);
      end;
    end;  { while }
  end else
    Result := fferrConnectionLost;
end;
{--------}
procedure TffWinsockProtocol.SetFamily(F : TffWinsockFamily);
var
  LocalHost : TffNetName;
begin
  if (FNetName <> nil) then
    FFShStrFree(FNetName);
  FFamily := F;
  if (F = wfTCP) then begin
    {get our name and address}
    if not FFWSGetLocalHostByNum(ffc_TCPInterface, LocalHost,
                                 wspLocalInAddr) then
      raise EffWinsockException.CreateNoData(ffStrResGeneral, fferrWSNoLocalAddr);
    cpSetNetName(FFWSCvtAddrToStr(wspLocalInAddr));
  end
  else if (F = wfIPX) then begin
    {get our IPX address}
    if not FFWSGetLocalIPXAddr(wspLocalIPXNetNum, wspLocalIPXAddr) then
      raise EffWinsockException.CreateNoData(ffStrResGeneral, fferrWSNoLocalAddr);
    cpSetNetName(FFWSCvtIPXAddrToStr(wspLocalIPXNetNum, wspLocalIPXAddr));
  end;
end;
{--------}
procedure TffWinsockProtocol.StopReceiveDatagram;
begin
  if wspReceivingDatagram then begin
    FFWSDestroySocket(wspRcvDatagramSocket);
    wspRcvDatagramSocket := INVALID_SOCKET;
    wspReceivingDatagram := false;
  end;
end;
{--------}
procedure TffWinsockProtocol.wspConnectCompleted(aSocket : TffwsSocket);
begin
  wspWaitingForConnect := false;
end;
{--------}
function TffWinsockProtocol.cpCreateNotifyWindow : boolean;
begin
  {$IFDEF DCC6OrLater}                                                 {!!.11}
    {$WARN SYMBOL_DEPRECATED OFF}
  {$ENDIF}
  FNotifyWindow := AllocateHWnd(wspWSAEventCompleted);
  {$IFDEF DCC6OrLater}                                                 {!!.11}
    {$WARN SYMBOL_DEPRECATED ON}
  {$ENDIF}
  Result := FNotifyWindow <> 0;
  if Result then begin
{$IFDEF KALog}
    KALog.WriteStringFmt('Winsock.cpCreateNotifyWindow: protocol %d',
                         [Longint(Self)]);
{$ENDIF}
    Windows.SetTimer(FNotifyWindow, 1, FKeepAliveInterval, nil);       {!!.05}
  end;
end;
{--------}
function TffWinsockProtocol.wspGetConnForSocket(aSocket : TffwsSocket) : TffWinsockConnection;
var
  Inx : integer;
  T   : TffIntListItem;
begin

  ConnLock;
  try
    { If indexing connections then use the index to find the connection. }
    if Assigned(cpIndexByOSConnector) then begin
      T := TffIntListItem(cpIndexByOSConnector.Items[cpIndexByOSConnector.Index(aSocket)]);
      if T = Nil then
        Result := Nil
      else
        Result := T.ExtraData;
      exit;
    end;
    for Inx := 0 to pred(cpConnList.Count) do begin
      Result := TffWinsockConnection(cpConnList[Inx]);
      if (Result.Socket = aSocket) then
        Exit;
    end;
  finally
    ConnUnlock;
  end;
  Result := nil;
end;
{--------}
procedure TffWinsockProtocol.wspHangupDetected(aSocket : TffwsSocket);
{Rewritten !!.06}
var
  Conn : TffWinsockConnection;
begin
  Conn := wspGetConnForSocket(aSocket);
  if (Conn <> nil) then begin
    Conn.HangingUp := False;
    HangUp(Conn);
  end;
end;
{--------}
procedure TffWinsockProtocol.wspListenCompleted(aSocket : TffwsSocket);
var
  NewSocket  : TffwsSocket;
  SocketAddr : TffwsSockAddr;
  AddrLen    : integer;
  Conn       : TffWinsockConnection;
  RemoteName : TffNetName;
  WasAdded   : boolean;
begin
  AddrLen := sizeof(SocketAddr);
  NewSocket := WinsockRoutines.accept(aSocket, SocketAddr, AddrLen);
  if (NewSocket <> INVALID_SOCKET) then begin
    {a listen event has been accepted, create a connection}
    WasAdded := false;
    Conn := nil;
    try
      RemoteName := ''; {!!!!}
      { When we first create this connection, we don't have a clientID so
        we temporarily use the connection's handle.  There is also a temporary
        clientID on the client-side of things.
        When the client is given a real clientID, the temporary clientIDs on
        both client and server are replaced with the true clientID. }
      Conn := TffWinsockConnection.Create(Self, RemoteName, NewSocket, Family,
                                          FNotifyWindow);
      Conn.ClientID := Conn.Handle;
//      Conn.InitCode(0);                                              {Deleted !!.05}
      cpAddConnection(Conn);
      WasAdded := True;                                                {!!.03}
      Conn.StartReceive;
      cpDoHeardCall(Conn.ClientID);
    except
      if WasAdded then
        cpRemoveConnection(Conn.ClientID);
      raise;
    end;
  end;
end;
{--------}
procedure TffWinsockProtocol.wspProcessCompletedWSACall(WParam, LParam : longint);
begin
  {check the error code}
  if (WSAGetSelectError(LParam) <> 0) then
  begin
    wspHangupDetected(TffwsSocket(WParam));
    wspWaitingForSendToUnblock := false;
    Exit;
  end;
  {check for event completion (note case is in numeric sequence)}
  case WSAGetSelectEvent(LParam) of
    FD_READ :
      wspReceiveCompleted(TffwsSocket(WParam));
    FD_WRITE :
      wspSendMsgCompleted(TffwsSocket(WParam));
    FD_OOB :
      {do nothing};
    FD_ACCEPT :
      wspListenCompleted(TffwsSocket(WParam));
    FD_CONNECT :
      wspConnectCompleted(TffwsSocket(WParam));
    FD_CLOSE :
      wspHangupDetected(TffwsSocket(WParam));
  end;{case}
end;
{--------}
procedure TffWinsockProtocol.wspSendMsgCompleted(aSocket : TffwsSocket);
var
  SocketType : integer;
  Conn       : TffWinsockConnection;
  dummy      : longint;
begin
  wspWaitingForSendToUnblock := false;
  SocketType := 0;
  FFWSGetSocketOption(aSocket, SOL_SOCKET, SO_TYPE, SocketType,
                      sizeof(SocketType));
  if (SocketType = SOCK_STREAM) then begin
    Conn := wspGetConnForSocket(aSocket);
    if Assigned(Conn) then begin
      Conn.wscIsSending := False;
      while (Not Conn.wscIsSending) and Assigned(Conn.wscPacketHead) do
        {try to send all outstanding packets}
         Conn.Send(nil, 0, 0, dummy, True);                           {!!.06}
    end;
  end;
end;
{--------}
procedure TffWinsockProtocol.wspReceiveCompleted(aSocket : TffwsSocket);
var
  SocketType     : integer;
begin
  SocketType := 0;
  FFWSGetSocketOption(aSocket, SOL_SOCKET, SO_TYPE, SocketType, sizeof(SocketType));
  if (SocketType = SOCK_STREAM) then
    wspReceiveMsgCompleted(aSocket)
  else if (SocketType = SOCK_DGRAM) then
    wspReceiveDatagramCompleted(aSocket);
end;
{--------}
procedure TffWinsockProtocol.wspReceiveDatagramCompleted(aSocket : TffwsSocket);
var
  RemNetName : TffNetName;
  BytesAvail : longint;
  BytesRead  : integer;
  Error      : integer;
  SockAddrLen: integer;
  SockAddr   : TffwsSockAddr;
begin
  Error := WinsockRoutines.ioctlsocket(aSocket, FIONREAD, BytesAvail);
  if (Error <> SOCKET_ERROR) and (BytesAvail > 0) then begin
    FillChar(SockAddr, sizeof(SockAddr), 0);
    if (Family = wfTCP) then begin
      SockAddrLen := sizeof(TffwsSockAddrIn);
    end
    else {Family <> wfTCP} begin
      SockAddrLen := sizeof(TffwsSockAddrIPX);
    end;
    BytesRead := WinsockRoutines.recvfrom(aSocket,
                                          wspRcvDGBuffer^,
                                          ffc_MaxDatagramSize,
                                          0,
                                          SockAddr,
                                          SockAddrLen);
    if (BytesRead <> SOCKET_ERROR) then begin
      {get our user to process the data}
      if (Family = wfTCP) then begin
        RemNetName := FFWSCvtAddrToStr(SockAddr.TCP.sin_addr);
      end
      else {Family <> wfTCP} begin
        with SockAddr.IPX do
          RemNetName :=
             FFWSCvtIPXAddrToStr(sipx_netnum, sipx_nodenum);
      end;
      cpDoReceiveDatagram(RemNetName, wspRcvDGBuffer, BytesRead);
    end;
  end;
end;
{--------}
procedure TffWinsockProtocol.wspReceiveMsgCompleted(aSocket : TffwsSocket);
var
  BytesAvail : longint;
  BytesRead  : integer;
  Conn       : TffWinsockConnection;
  Error      : integer;
  MsgLen     : integer;
  Parsing    : boolean;
begin
  Error := WinsockRoutines.ioctlsocket(aSocket, FIONREAD, BytesAvail);
  if (Error <> SOCKET_ERROR) and (BytesAvail > 0) then begin
    Conn := wspGetConnForSocket(aSocket);
    if assigned(Conn) then
      with Conn do begin
        {read everything we can}
        BytesRead := WinsockRoutines.recv(aSocket,
                                          RcvBuffer^[RcvBufferOffset],
                                          ffc_MaxWinsockMsgSize - RcvBufferOffset,
                                          0);
        if (BytesRead <> SOCKET_ERROR) then begin
          {calculate the number of valid bytes in our receive buffer}
          RcvBufferOffset := RcvBufferOffset + BytesRead;
          Parsing := true;
          while Parsing do begin
            Parsing := false;
            {discard check connection (keepalive) messages now, we may
             have real messages piggybacking one}
            while (RcvBufferOffset >= sizeof(longint)) and
                  (PLongint(RcvBuffer)^ = ffnmCheckConnection) do begin
              {move the remainder of the received data up by 4 bytes}
              RcvBufferOffset := RcvBufferOffset - sizeof(longint);
              if (RcvBufferOffset > 0) then
                Move(RcvBuffer^[sizeof(longint)], RcvBuffer^[0], RcvBufferOffset);
              cpGotCheckConnection(Conn);
              Parsing := true;
            end; { while }
            {if we have something left..., and enough of it...}
            if (RcvBufferOffset >= ffc_NetMsgHeaderSize) then begin
              MsgLen := PffnmHeader(RcvBuffer)^.nmhMsgLen;
              if (RcvBufferOffset >= MsgLen) then begin
                {get our ancestor to process the data}
                if cpDoReceiveMsg(Conn, RcvBuffer, MsgLen) then begin
                  {remove the message}
                  RcvBufferOffset := RcvBufferOffset - MsgLen;
                  if (RcvBufferOffset > 0) then
                    Move(RcvBuffer^[MsgLen], RcvBuffer^[0], RcvBufferOffset);
                  Parsing := true;
                end;
              end;
            end;  { if }
          end;  { while }
        end;  { if }
      end  { with }
    else
      LogStrFmt('Could not find connection for socket %d', [aSocket]);
  end;  { if }
end;
{--------}
procedure TffWinsockProtocol.wspWaitForConnect(aTimeOut : integer);
var
  T : TffTimer;
begin
  SetTimer(T, aTimeOut);
  while wspWaitingForConnect and (not HasTimerExpired(T)) do begin
    Breathe;
  end;
end;
{--------}
function TffWinsockProtocol.wspWaitForSendToUnblock : Boolean;
{ Rewritten !!.06}
var
  UnblockTimer : TffTimer;
begin
  wspWaitingForSendToUnblock := true;
  SetTimer(UnblockTimer, ffc_UnblockWait);
  repeat
    Breathe;
    Result := HasTimerExpired(UnblockTimer);
  until (not wspWaitingForSendToUnblock) or Result;
end;
{--------}
procedure TffWinsockProtocol.wspWSAEventCompleted(var WSMsg : TMessage);
begin
  with WSMsg do begin
    if (Msg = ffwscEventComplete) then begin
      wspProcessCompletedWSACall(WParam, LParam);
      Result := 0;
    end
    else if (Msg = WM_TIMER) then begin
      cpTimerTick;
    end
    else
      Result := DefWindowProc(FNotifyWindow, Msg, WParam, LParam);
  end;
end;
{====================================================================}


{===TffTCPIPProtocol=================================================}
constructor TffTCPIPProtocol.Create(const aName : TffNetAddress;
                                          aCSType : TffClientServerType);
begin
  inherited Create(aName, aCSType);
  Family := wfTCP;
end;
{--------}
class function TffTCPIPProtocol.GetProtocolName : string;
begin
  Result := 'TCP/IP (FF)';
end;
{--------}
class function TffTCPIPProtocol.Supported : boolean;
begin
  if FFWSInstalled then
    Result := wfTCP in ffwsFamiliesInstalled
  else
    Result := False;
end;
{====================================================================}


{===TffIPXSPXProtocol================================================}
constructor TffIPXSPXProtocol.Create(const aName : TffNetAddress;
                                           aCSType : TffClientServerType);
begin
  inherited Create(aName, aCSType);
  Family := wfIPX;
end;
{--------}
class function TffIPXSPXProtocol.GetProtocolName : string;
begin
  Result := 'IPX/SPX (FF)';
end;
{--------}
class function TffIPXSPXProtocol.Supported : boolean;
begin
  if FFWSInstalled then
    Result := wfIPX in ffwsFamiliesInstalled
  else
    Result := False;
end;
{====================================================================}


{===Helper routines for single user==================================}
type
  PffSUEnumData = ^TffSUEnumData;
  TffSUEnumData = packed record
    MsgID  : integer;
    OurWnd : HWND;
    SrvWnd : HWND;
  end;
{====================================================================}


{===TffSingleUserConnection==========================================}
constructor TffSingleUserConnection.Create(aOwner      : TffBaseCommsProtocol;
                                           aRemoteName : TffNetAddress;
                                           aUs         : HWND;
                                           aPartner    : HWND);
begin
  inherited Create(aOwner, aRemoteName);
  FUs := aUs;
  FPartner := aPartner;
  GetMem(sucSendBuffer, ffc_MaxSingleUserMsgSize);
end;
{--------}
destructor TffSingleUserConnection.Destroy;
var
  CDS : TCopyDataStruct;
  MsgResult : DWORD;
  WinError : TffWord32;                                               {!!.12}
begin
  { If we are deliberately hanging up then send a message to our partner. }
  if FHangingUp then begin
    if IsWindow(Partner) then begin
      CDS.dwData := ffsumHangUp;
      CDS.cbData := 0;
      CDS.lpData := nil;
{Begin !!.12}
      if not LongBool(SendMessageTimeout(FPartner, WM_COPYDATA, FClientID,
                                         longint(@CDS),
{$IFDEF RunningUnitTests}
                                         SMTO_ABORTIFHUNG,
{$ELSE}
                                         SMTO_ABORTIFHUNG or SMTO_BLOCK,
{$ENDIF}
                                         ffc_SendMessageTimeout, MsgResult)) or
        (MsgResult <> 0) then begin
        Sleep(ffc_SUPErrorTimeout);
          { Experimentation shows the following:
            1. The first SendMessageTimeout will return False but
               GetLastError returns zero.
            2. Leaving out the Sleep() leads to a failure in the following
               call to SendMessageTimeout. Note that error code is still
               set to zero in that case.
            3. Inserting a Sleep(1) resolves one timeout scenario (loading
               JPEGs from table). However, it does not resolve the issue
               where Keep Alive Interval >= 20000 and scrolling through
               large table in FFE.
            4. Inserting a Sleep(25) resolves the latter case mentioned in
               Item 3. }
        if not LongBool(SendMessageTimeout(FPartner, WM_COPYDATA, FClientID,
                                           longint(@CDS),
{$IFDEF RunningUnitTests}
                                           SMTO_ABORTIFHUNG,
{$ELSE}
                                           SMTO_ABORTIFHUNG or SMTO_BLOCK,
{$ENDIF}
                                           ffc_SendMessageTimeout, MsgResult)) then begin
          WinError := GetLastError;
          FOwner.LogStrFmt('Error %d sending message via SUP connection: %s',
                           [WinError, SysErrorMessage(WinError)]);
        end;
      end;
{End !!.12}
    end;
  end;
  FreeMem(sucSendBuffer, ffc_MaxSingleUserMsgSize);
  inherited Destroy;
end;
{--------}
Procedure TffSingleUserConnection.AddToList(List : TFFList);
var
  T : TffIntListItem;
  {$IFNDEF WIN32}
  tmpLongInt : longInt;
  {$ENDIF}
begin {add a list entry to allow partner hwnd lookups}
  {$IFDEF WIN32}
  T := TffIntListItem.Create(FPartner);
  {$ELSE}
  { The 16-bit HWND is a Word.  Cast it to a longInt so that
    our TffIntList comparison will work. }
  tmpLongInt := FPartner;
  T := TffIntListItem.Create(tmpLongInt);
  {$ENDIF}
  T.ExtraData := Self;
  List.Insert(T);
end;
{--------}
class function TffSingleUserProtocol.GetProtocolName : string;
begin
  Result := 'Single User (FF)';
end;
{--------}
Procedure TffSingleUserConnection.RemoveFromList(List : TFFList);
begin
  List.Delete(FPartner);
end;
{--------}
procedure TffSingleUserConnection.Send(aData     : PffByteArray;
                                       aDataLen  : longint;
                                       aConnLock : Boolean);          {!!.06}
var
  CDS : TCopyDataStruct;
  MsgResult : DWORD;
  WinError : TffWord32;                                               {!!.05}
begin
  if IsWindow(Partner) then begin
    if aConnLock then                                                 {!!.06}
      HangupLock;                                                     {!!.05}
    try                                                               {!!.05}
      if (aDataLen <> 0) then begin
        Move(aData^, sucSendBuffer^, aDataLen);
        Owner.cpCodeMessage(Self, sucSendBuffer, aDataLen);
        CDS.lpData := sucSendBuffer;
        CDS.cbData := aDataLen;
      end
      else begin
        CDS.lpData := nil;
        CDS.cbData := 0;
      end;
      CDS.dwData := ffsumDataMsg;
{Begin !!.05}
      if not LongBool(SendMessageTimeout(FPartner, WM_COPYDATA, FClientID,
                                         longint(@CDS),
{$IFDEF RunningUnitTests}
                                         SMTO_ABORTIFHUNG,
{$ELSE}
                                         SMTO_ABORTIFHUNG or SMTO_BLOCK,
{$ENDIF}
                                         ffc_SendMessageTimeout, MsgResult)) or
        (MsgResult <> 0) then begin
{Begin !!.06}
        Sleep(ffc_SUPErrorTimeout);
          { Experimentation shows the following:
            1. The first SendMessageTimeout will return False but
               GetLastError returns zero.
            2. Leaving out the Sleep() leads to a failure in the following
               call to SendMessageTimeout. Note that error code is still
               set to zero in that case.
            3. Inserting a Sleep(1) resolves one timeout scenario (loading
               JPEGs from table). However, it does not resolve the issue
               where Keep Alive Interval >= 20000 and scrolling through
               large table in FFE.
            4. Inserting a Sleep(25) resolves the latter case mentioned in
               Item 3. }
{End !!.06}
        if not LongBool(SendMessageTimeout(FPartner, WM_COPYDATA, FClientID,
                                           longint(@CDS),
{$IFDEF RunningUnitTests}
                                           SMTO_ABORTIFHUNG,
{$ELSE}
                                           SMTO_ABORTIFHUNG or SMTO_BLOCK,
{$ENDIF}
                                           ffc_SendMessageTimeout, MsgResult)) then begin
          WinError := GetLastError;
          FOwner.LogStrFmt('Error %d sending message via SUP connection: %s',
                           [WinError, SysErrorMessage(WinError)]);
        end;
{End !!.05}
      end;
    finally                                                           {!!.05}
      if aConnLock then                                               {!!.06}
        HangupUnlock;                                                 {!!.05}
    end;                                                              {!!.05}
  end;
end;
{====================================================================}


{===TffSingleUserProtocol============================================}
constructor TffSingleUserProtocol.Create(const aName : TffNetAddress;
                                               aCSType : TffClientServerType);
begin
  inherited Create(aName, aCSType);
  FMaxNetMsgSize := ffc_MaxSingleUserMsgSize;
  { Create a new Windows message. }
  supMsgID := RegisterWindowMessage('FlashFiler2SingleUser');
  supPostMsgID := RegisterWindowMessage('FlashFiler2SingleUserPostMessage');
end;
{--------}
function TffSingleUserProtocol.Call(const aServerName : TffNetName;
                                      var aClientID : TffClientID;
                                    const timeout : longInt) : TffResult;
var
  Conn : TffSingleUserConnection;
  SUED : TffSUEnumData;
begin

  Result := DBIERR_NONE;

  {servers don't call}
  if (CSType = csServer) then
    raise EffCommsException.CreateNoData(ffStrResGeneral, fferrCommsCannotCall);
  {assume failure}

  {enumerate the top-level windows, looking for a server}
  SUED.MsgID := supMsgID;
  SUED.OurWnd := FNotifyWindow;
  SUED.SrvWnd := 0;

  { Create a connection object with the assumption we find a server. }
  Conn := TffSingleUserConnection.Create(Self, '', FNotifyWindow, SUED.SrvWnd);
  Conn.ClientID := Conn.Handle;

  SUED.SrvWnd := supFindPartner(Conn.ClientID, timeout);

  {did we find one?}
  if (SUED.SrvWnd <> 0) then begin
    Conn.Partner := SUED.SrvWnd;
    cpAddConnection(Conn);
    aClientID := Conn.ClientID;
  end else begin
    Conn.Free;
    Result := DBIERR_SERVERNOTFOUND;
  end;
end;
{--------}
procedure TffSingleUserProtocol.cpPerformStartUp;
begin
  {create our Window}
  if not cpCreateNotifyWindow then begin
    LogStr('Could not create notification window.');
    raise EffCommsException.CreateNoData(ffStrResGeneral, fferrCommsNoWinRes);
  end;
end;
{--------}
procedure TffSingleUserProtocol.GetServerNames(aList : TStrings; const timeout : longInt);
begin
  if not assigned(aList) then
    exit;

  aList.Clear;
  aList.Add(ffc_SingleUserServerName);
end;
{--------}
procedure TffSingleUserProtocol.HangUp(aConn : TffConnection);
begin
  cpDoHangUp(aConn);
  cpRemoveConnection(aConn.ClientID);
end;
{--------}
procedure TffSingleUserProtocol.Listen;
begin
end;
{--------}
procedure TffSingleUserProtocol.ReceiveDatagram;
begin
  if not supReceivingDatagram then
    supReceivingDatagram := true;
end;
{--------}
procedure TffSingleUserProtocol.SendDatagram(const aName    : TffNetName;
                                                   aData    : PffByteArray;
                                                   aDataLen : longint);
begin
end;
{--------}
function TffSingleUserProtocol.SendMsg(aClientID : TffClientID;
                                       aData     : PffByteArray;
                                       aDataLen  : longint;
                                       aConnLock : Boolean) : TffResult; {!!.06}
var
  Conn : TffSingleUserConnection;
begin
  Result := DBIERR_NONE;
  Conn := TffSingleUserConnection(cpGetConnection(aClientID));
  if Assigned(Conn) then
    Conn.Send(aData, aDataLen, aConnLock)                             {!!.06}
  else
    Result := fferrConnectionLost;
end;
{--------}
procedure TffSingleUserProtocol.StopReceiveDatagram;
begin
  if supReceivingDatagram then
    supReceivingDatagram := false;
end;
{--------}
function TffSingleUserProtocol.cpCreateNotifyWindow : boolean;
begin
  {$IFDEF DCC6OrLater}                                                 {!!.11}
    {$WARN SYMBOL_DEPRECATED OFF}
  {$ENDIF}
  FNotifyWindow := AllocateHWnd(supMsgReceived);
  {$IFDEF DCC6OrLater}                                                 {!!.11}
    {$WARN SYMBOL_DEPRECATED ON}
  {$ENDIF}
  Result := FNotifyWindow <> 0;
  if Result then begin
{$IFDEF KALog}
    KALog.WriteStringFmt('SingleUser.cpCreateNotifyWindow: protocol %d',
                         [Longint(Self)]);
{$ENDIF}
    Windows.SetTimer(FNotifyWindow, 1, FKeepAliveInterval, nil);       {!!.05}
  end;
end;
{--------}
procedure TffSingleUserProtocol.supDataMsgReceived(const aClientID : TffClientID;
                                                   const aCDS : TCopyDataStruct);
var
  Conn : TffSingleUserConnection;
begin

  Conn := TffSingleUserConnection(cpGetConnection(aClientID));
  {get our user to process the data}
  if assigned(Conn) then
    cpDoReceiveMsg(Conn, aCDS.lpData, aCDS.cbData)
  else
    LogStrFmt('Could not find connection for client %d', [aClientID]);
end;
{--------}
function TffSingleUserProtocol.supGetConnForPartner(aPartner : HWND) : TffSingleUserConnection;
var
  Inx : integer;
  T   : TffIntListItem;
begin
  { If we are indexing connections then use the index to locate
    the connection. }
  if Assigned(cpIndexByOSConnector) then begin
    T := TffIntListItem(cpIndexByOSConnector.Items[cpIndexByOSConnector.Index(aPartner)]);
    if T = Nil then
      Result := Nil
    else
      Result := T.ExtraData;
    exit;
  end;
  for Inx := 0 to pred(cpConnList.Count) do begin
    Result := TffSingleUserConnection(cpConnList[Inx]);
    if (Result.Partner = aPartner) then
      Exit;
  end;
  Result := nil;
end;
{--------}
procedure TffSingleUserProtocol.supHangupDetected(const aClientID : TffClientID);
{Rewritten !!.06}
var
  Conn : TffSingleUserConnection;
begin
  Conn := TffsingleUserConnection(cpGetConnection(aClientID));
  if Conn <> nil then begin
    Conn.HangingUp := False;
    HangUp(Conn);
  end;
end;
{--------}
procedure TffSingleUserProtocol.supListenCompleted(aClientID : TffClientID;
                                                   Wnd : HWND);
var
  Conn     : TffSingleUserConnection;
  WasAdded : boolean;
begin
  {a listen event has been accepted, create a connection}
  WasAdded := false;
  Conn := nil;
  try
    { When we first create this connection, we don't have a clientID so
      we temporarily use the connection's handle.  There is also a temporary
      clientID on the client-side of things.
      When the client is given a real clientID, the temporary clientIDs on
      both client and server are replaced with the true clientID. }
    Conn := TffSingleUserConnection.Create(Self, '', FNotifyWindow, Wnd);
    Conn.ClientID := aClientID;
//    Conn.InitCode(0);                                                {Deleted !!.05}
    cpAddConnection(Conn);
    WasAdded := True;
    cpDoHeardCall(Conn.ClientID);
  except
    if WasAdded then
      cpRemoveConnection(Conn.ClientID);
    raise;
  end;{try..except}
end;
{--------}
procedure TffSingleUserProtocol.supMsgReceived(var SUMsg : TMessage);
begin
  with SUMsg do begin
    if (Msg = supMsgID) then begin
      if (CSType = csServer) then begin
        Result := ffsumCallServer {'FF'};
        supListenCompleted(WParam, LParam);
      end
      else
        Result := 0;
    end
    else if Msg = supPostMsgID then begin
      if CSType = csServer then begin
        { Client is trying to initiate conversation with us.  Send back
          a reply. }
        if LParam = ffsumCallServer {'FF'} then begin
          if IsWindow(WParam) then
            PostMessage(WParam, ffm_ServerReply, FNotifyWindow, ffsumCallServer);
        end;
      end;
    end
    else if Msg = ffm_ServerReply then begin
      if supPartner = 0 then begin
        if CSType = csClient then begin
          if LParam = ffsumCallServer {'FF'} then begin
            if IsWindow(WParam) then
              supPartner := WParam;
          end;
        end;
      end;
    end
    else if (Msg = WM_COPYDATA) then begin
      case PCopyDataStruct(LParam)^.dwData of
        ffsumDataMsg   : supDataMsgReceived(WParam, PCopyDataStruct(LParam)^);
        ffsumHangUp    : supHangUpDetected(WParam);
      end;
    end
    else if (Msg = WM_TIMER) then
      cpTimerTick
    else
      Result := DefWindowProc(FNotifyWindow, Msg, WParam, LParam);
  end;
end;
{--------}
function TffSingleUserProtocol.supFindPartner(const aClientID : TffClientID;
                                              const timeout : longInt): HWND;

var
  WaitUntil : Tffword32;
  MsgResult : DWORD;
  Msg : TMsg;
  StartTime : DWORD;                                                   {!!.05}
  WinError : TffWord32;                                                {!!.05}
begin
  supPartner:=0;
  PostMessage(HWND_BROADCAST, supPostMsgID, FNotifyWindow, ffsumCallServer);
  WaitUntil := GetTickCount + DWORD(timeout);
  StartTime := GetTickCount;                                           {!!.05}
  while (GetTickCount < WaitUntil) and (supPartner=0) do begin
    if PeekMessage(Msg, FNotifyWindow, ffm_ServerReply,
                   ffm_ServerReply, PM_REMOVE) then begin
      TranslateMessage(Msg);
      DispatchMessage(Msg);
{Begin !!.05}
    end
    else if GetTickCount - StartTime > ffc_ConnectRetryTimeout then begin
      PostMessage(HWND_BROADCAST, supPostMsgID, FNotifyWindow, ffsumCallServer);
      StartTime := GetTickCount;
    end;
{End !!.05}
    if supPartner = 0 then
      Breathe;
  end;
  Result := supPartner;
  if Result <> 0 then begin
    if LongBool(SendMessageTimeout(Result, supMsgID, aClientID, FNotifyWindow,
                                   SMTO_ABORTIFHUNG or SMTO_BLOCK,
                                   timeout, MsgResult)) then begin
        if MsgResult <> ffsumCallServer{FF} then begin
{Begin !!.05}
          if LongBool(SendMessageTimeout(Result, supMsgID, aClientID, FNotifyWindow,
                                         SMTO_ABORTIFHUNG or SMTO_BLOCK,
                                         timeout, MsgResult)) then
            if MsgResult <> ffsumCallServer{FF} then begin
              WinError := GetLastError;
              LogStrFmt('Error %d when finding SUP partner: %s',
                        [WinError, SysErrorMessage(WinError)]);
              Result :=0;
            end;  { if }
        end;  { if }
{End !!.05}
    end
    else
      Result := 0;
  end;
end;
{====================================================================}

{$IFDEF KALog}
initialization
  KALog := TffEventLog.Create(nil);
  KALog.FileName := ChangeFileExt(ParamStr(0), '') + 'KA.log';
  KALog.Enabled := True;

finalization
  KALog.Free;
{$ENDIF}

end.

