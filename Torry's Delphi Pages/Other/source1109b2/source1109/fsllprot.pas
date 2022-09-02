{$I fsdefine.inc}

{ Enable the following line to activate Keep Alive logging. }
{.$DEFINE KALog}

Unit fsllprot;

Interface

Uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  ExtCtrls,
  Forms,
  fsconst,
  fsllbase,
  fsllexcp,
  fslllog,
  fsllwsct,
  fsnetmsg,
  fssrmgr,
  fsllwsck;

Type
  TfsProtocolType = ({Protocol types..}
    ptSingleUser, {..single user}
    ptTCPIP, {..TCP/IP}
    ptIPXSPX, {..IPX/SPX}
    ptRegistry); {..value from registry}

  {===Constants relating to sending messages and datagrams}
Const
  fsc_ConnectRetryTimeout: DWORD = 1000; {!!.05}
  { Number of milliseconds before retry of connection request. }{!!.05}
  fsc_UnblockWait: DWORD = 25; {!!.06}
  { Number of milliseconds to wait before exiting unblock wait loop. }{!!.06}
  fsc_MaxWinsockMsgSize = 24 * 1024;
  fsc_MaxSingleUserMsgSize = 64 * 1024;
  fsc_MaxDatagramSize = 128;
  fsc_CodeLength = 256;
  fsc_LastMsgInterval: Longint = 30000;
  fsc_KeepAliveInterval: Longint = 5000;
  fsc_KeepAliveRetries: Longint = 5;

  fsc_TCPInterface: Integer = 0; // NIC to use for TCPIP
  fsc_TCPRcvBuf: Longint = $8000; // request 32K Rcv Buffer
  fsc_TCPSndBuf: Longint = $8000; // request 32K Snd Buffer

  fsc_SingleUserServerName = 'Local';
  fsc_SendMessageTimeout = 1 * 1000; {1 second} {!!.01} {!!.05}

  fsc_SUPErrorTimeout: Integer = 25; {!!.06}
  { # milliseconds to wait if error occurs during SUP send. }{!!.06}

{===Single user messages constants (for dwData)}
// Konwersja serwera
Const
  fssumCallServer = $4632; //1...
  fssumDataMsg = $4633;
  fssumHangUp = $4634;
  fssumKeepAlive = $4635;
  fsm_ServerReply = WM_USER + $0FFA; //$0FF9;

  {===Datagram types===}
Type
  PfsDatagram = ^TfsDatagram;
  TfsDatagram = Array[0..pred(fsc_MaxDatagramSize)] Of Byte;
  PfsDatagramArray = ^TfsDatagramArray;
  TfsDatagramArray = Array[0..255] Of TfsDatagram;

  {===Code types===}
Type
  PfsNetMsgCode = ^TfsNetMsgCode;
  TfsNetMsgCode = Array[0..pred(fsc_CodeLength)] Of Byte;

  {===Event types===}
Type
  TfsReceiveMsgEvent = Function(aSender: TObject;
    clientID: TffClientID;
    replyData: PffByteArray;
    replyLen: Longint): boolean Of Object;
  TfsHeardCallEvent = Procedure(aSender: TObject;
    aConnHandle: Longint) Of Object;
  TfsReceiveDatagramEvent = Procedure(aSender: TObject;
    Const aName: TffNetName;
    aData: PffByteArray;
    aDataLen: Longint) Of Object;
  TfsHangUpEvent = Procedure(aSender: TObject;
    aClientID: TffClientID) Of Object;

  {===Base Classes===}
Type
  TfsBaseCommsProtocol = Class;

  TfsClientServerType = ({type defining client or server}
    csClient, {..client}
    csServer); {..server}

  TfsConnection = Class(TfsSelfListItem)
  Protected {private}
    FClientID: TffClientID;
    FCode: PfsNetMsgCode;
    { The code used for encrypting messages. }
    FCodeStart: Int64; {!!.10}
    FHangingUp: boolean;
    FHangupDone: boolean; {!!.01}
    FHangupLock: TfsPadlock; {!!.01}
    FOwner: TfsBaseCommsProtocol;
    FRemoteName: PffShStr;
    FAliveRetries: Integer;
    FLastMsgTimer: TfsTimer;
    FSendConfirm: boolean;
  Protected
    Function GetRemoteName: String; {!!.10}
    Procedure AddToList(List: TFSNormalList); Virtual;
    Procedure RemoveFromList(List: TFSNormalList); Virtual;
  Public
    Constructor Create(aOwner: TfsBaseCommsProtocol;
      aRemoteName: TffNetAddress);
    Destructor Destroy; Override;

    Procedure ConfirmAlive(SendConfirm: boolean);
    Procedure DepleteLife;

    Procedure HangupLock; {!!.01}
    Procedure HangupUnlock; {!!.01}

    Procedure InitCode(Const aStart: Int64);
    { Initializes the encryption code used for communicating with the
      server. }
    Function IsAlive: boolean;
    Function IsVeryAlive: boolean;
    Function NeedsConfirmSent: boolean;

    Property ClientID: TffClientID Read FClientID Write FClientID;
    Property Code: PfsNetMsgCode Read FCode;
    Property CodeStart: Int64 Read FCodeStart; {!!.10}
    Property Owner: TfsBaseCommsProtocol
      Read FOwner;
    Property Handle: Longint
      Read KeyAsInt;
    Property HangingUp: boolean
      Read FHangingUp Write FHangingUp;
    { Set to True when we are deliberately hanging up the connection.
      This variable tells us whether we need to invoke the OnHangUp or
      OnConnectionLost event in the parent protocol. }
    Property HangupDone: boolean {!!.01}
    Read FHangupDone Write FHangupDone; {!!.01}
    Property RemoteName: String {!!.10}
    Read GetRemoteName;
  End;

  { Defines the common interface for all legacy protocols.  This class is
    written with the assumption that only one thread will ever be using an
    instance of this class at any given time.  Therefore no locking/critical
    sections are used. }
  TfsBaseCommsProtocol = Class
  Protected {private}
    FConnLock: TfsPadlock;
    FCSType: TfsClientServerType;
    FEventLog: TFSBaseLog;
    FHeardCall: TfsHeardCallEvent;
    FKeepAliveInterval: Longint;
    FKeepAliveRetries: Longint;
    FLastMsgInterval: Longint;
    FLocalName: PffShStr;
    FLogEnabled: boolean;
    FMaxNetMsgSize: Longint;
    FNetName: PffShStr;
    FNotifyWindow: HWND;
    FOnConnectionLost: TfsHangUpEvent;
    FOnHangup: TfsHangUpEvent;
    FReceiveDatagram: TfsReceiveDatagramEvent;
    FReceiveMsg: TfsReceiveMsgEvent;
    FSearchTimeOut: Integer;
    FStarted: boolean;
    {-If True then the protocol is active. }
    FStartedEvent: TFSNormalEvent;

    cpConnList: TFSNormalList;
    cpIndexByOSConnector: TFSNormalList; { This is an index by socket (TCP/IP or
    IPX/SPX) or by window handle (SUP). }
    cpIndexByClient: TFSNormalList; { This is an index by clientID. }
  Protected

    Function GetLocalName: String; {!!.10}
    Function GetNetName: String; {!!.10}

    Procedure cpAddConnection(aConnection: TfsConnection);
    Function cpExistsConnection(aConnHandle: Longint): boolean;
    Function cpFindConnection(Const aClientID: TffClientID): Longint;
    Function cpGetConnection(Const aClientID: TffClientID): TfsConnection;
    Function cpGetConnectionIDs(Const anIndex: Longint): TffClientID;
    Procedure cpRemoveConnection(aClientID: TffClientID);

    Function cpCreateNotifyWindow: boolean; Dynamic;
    Procedure cpDestroyNotifyWindow;
    Procedure cpDoHangUp(aConn: TfsConnection); Dynamic;
    Procedure cpDoHeardCall(aConnHandle: Longint); Dynamic;
    Procedure cpDoReceiveDatagram(Const aName: TffNetName;
      aData: PffByteArray;
      aDataLen: Longint); Dynamic;
    Function cpDoReceiveMsg(aConn: TfsConnection;
      msgData: PffByteArray;
      msgDataLen: Longint): boolean; Dynamic;

    Procedure cpPerformShutdown; Virtual;
    Procedure cpPerformStartUp; Virtual; Abstract;

    Procedure cpSetNetName(aName: String);

    Procedure cpCodeMessage(aConn: TfsConnection; aData: PffByteArray;
      aDataLen: Longint); Virtual;
    Procedure cpGotCheckConnection(aConn: TfsConnection);
    Procedure cpTimerTick;
  Public
    Constructor Create(Const aName: TffNetAddress; aCSType: TfsClientServerType); Virtual;
    Destructor Destroy; Override;

    Function Call(Const aServerName: TffNetName;
      Var aClientID: TffClientID;
      Const timeout: Longint): TffResult; Virtual; Abstract;
    Function ClientIDExists(Const aClientID: TffClientID): boolean;
    { Used by the legacy transport to determine if it has generated a
      temporary clientID that conflicts with a real clientID. }

    Function ConnectionCount: Longint;
    { Returns the number of established connections. }

    Procedure ConnLock;
    Procedure ConnUnlock;
    { Use these procedures to prevent a newly-attached client from sending
      the protocol a message before the protocol has updated the new
      connection's clientID. }

    Procedure GetServerNames(aList: TStrings; Const timeout: Longint); Virtual; Abstract;
    { Protocol-specific method for retrieving servers accessible via the
      protocol. }

    Function GetCodeStart(Const aClientID: TffClientID): Integer;
    { Get the starting encryption code for the specified client. }

    Class Function GetProtocolName: String; Virtual;
    { Returns the name of the protocol (e.g., 'TCP/IP'). }

    Procedure HangUp(aConn: TfsConnection); Virtual; Abstract;
    Procedure HangUpByClientID(aClientID: TffClientID); Virtual;
    Procedure HangupDone(aClientID: TffClientID); {!!.01}
    Function HangupIsDone(aClientID: TffClientID): Boolean; {!!.01}
    Procedure HangupLock(aClientID: TffClientID); {!!.01}
    Procedure HangupUnlock(aClientID: TffClientID); {!!.01}
    Procedure Listen; Virtual; Abstract;
    Function SendMsg(aClientID: TffClientID;
      aData: PffByteArray;
      aDataLen: Longint;
      aConnLock: Boolean): TffResult; Virtual; Abstract; {!!.06}

    Procedure ReceiveDatagram; Virtual; Abstract;
    Procedure SendDatagram(Const aName: TffNetName;
      aData: PffByteArray;
      aDataLen: Longint); Virtual; Abstract;

    Procedure Shutdown; Virtual;

    Procedure StartUp; Virtual;

    Procedure StopReceiveDatagram; Virtual; Abstract;

    Class Function Supported: boolean; Virtual;
    { Returns True if the protocol is supported on this workstation.
      Default implementation always returns True. }

    Procedure Breathe; Virtual;
    Procedure InitCode(Const aClientID: TffClientID;
      Const aStart: Int64);
    Procedure ResetKeepAliveTimer;

    Procedure UpdateClientID(Const oldClientID, newClientID: TffClientID);
    { After a client has successfully obtained access to the server, the
      transport uses this method to replace the client's temporary ID
      with the ID returned from the server. }

    Procedure LogStr(Const aMsg: String);
    { Use this method to write an event string to the protocol's event
      log. }

    Procedure LogStrFmt(Const aMsg: String; args: Array Of Const);
    { Use this method to write a formatted event string to the protocol's
      event log. }

    Property ConnectionIDs[Const anIndex: Longint]: TffClientID
    Read cpGetConnectionIDs;
    { Use this method to retrieve the connection IDs for the protocol's
      connections. }

    Property CSType: TfsClientServerType
      Read FCSType;
    Property EventLog: TFSBaseLog
      Read FEventLog Write FEventLog;
    Property IsStarted: boolean
      Read FStarted;
    Property KeepAliveInterval: Longint
      Read FKeepAliveInterval
      Write FKeepAliveInterval;
    Property KeepAliveRetries: Longint
      Read FKeepAliveRetries
      Write FKeepAliveRetries;
    Property LastMsgInterval: Longint
      Read FLastMsgInterval
      Write FLastMsgInterval;
    Property LocalName: String {!!.10}
    Read GetLocalName;
    Property LogEnabled: boolean
      Read FLogEnabled
      Write FLogEnabled;
    Property MaxNetMsgSize: Longint
      Read FMaxNetMsgSize;
    Property NetName: String {!!.10}
    Read GetNetName;
    Property NotifyWindow: HWND
      Read FNotifyWindow;
    Property OnConnectionLost: TfsHangUpEvent
      Read FOnConnectionLost Write FOnConnectionLost;
    { This event is called when the other end of the connection unexpectedly
      hangs up on this end. }
    Property OnHangUp: TfsHangUpEvent
      Read FOnHangUp Write FOnHangUp;
    { This event is called when the protocol deliberately hangs up the
      connection. }
    Property OnHeardCall: TfsHeardCallEvent
      Read FHeardCall Write FHeardCall;
    Property OnReceiveDatagram: TfsReceiveDatagramEvent
      Read FReceiveDatagram Write FReceiveDatagram;
    Property OnReceiveMsg: TfsReceiveMsgEvent
      Read FReceiveMsg Write FReceiveMsg;
    Property SearchTimeOut: Integer
      Read FSearchTimeOut;
    Property StartedEvent: TFSNormalEvent
      Read FStartedEvent;
  End;

  TfsCommsProtocolClass = Class Of TfsBaseCommsProtocol;

  {===Winsock Classes===}
Type
  PfswscPacket = ^TfswscPacket;
  TfswscPacket = Packed Record
    dwLength: Longint;
    dwStart: Longint;
    lpData: PffByteArray;
    lpNext: PfswscPacket;
  End;

Type
  TfsWinsockConnection = Class(TfsConnection)
  Protected {private}
    FSocket: TfswsSocket;
    FFamily: TfsWinsockFamily;
    wscNotifyWnd: HWND;
    //      wscPortal     : TffReadWritePortal;                            {Deleted !!.05}
            {!!.05 - Replaced by TfsConnection.HangupLock }
            { Controls access to a connection in order that:
              1. The connection is not freed while a reply is outgoing.
              2. No more than one reply is being sent to the connection at
                 any one time.
            }
    wscRcvBuffer: PffByteArray;
    wscRcvBufOfs: Integer;
    //      wscSendBuffer : PffByteArray;
  Protected
    wscRcvBuf: Longint;
    wscSndBuf: Longint;
    wscPacketHead: PfswscPacket;
    wscPacketTail: PfswscPacket;
    wscIsSending: Boolean;
    Procedure AddToList(List: TFSNormalList); Override;
    Procedure RemoveFromList(List: TFSNormalList); Override;
  Public
    Constructor Create(aOwner: TfsBaseCommsProtocol;
      aRemoteName: TffNetAddress;
      aSocket: TfswsSocket;
      aFamily: TfsWinsockFamily;
      aNotifyWnd: HWND);
    Destructor Destroy; Override;

    Function Send(aData: PffByteArray;
      aDataLen: Longint;
      aDataStart: Longint;
      Var aBytesSent: Longint;
      aConnLock: Boolean): Integer; {!!.06}
    Procedure StartReceive;

    Property IsSending: Boolean {!!.06}
    Read wscIsSending Write wscIsSending; {!!.06}

    Property RcvBuffer: PffByteArray
      Read wscRcvBuffer;

    Property RcvBufferOffset: Integer
      Read wscRcvBufOfs Write wscRcvBufOfs;

    Property Socket: TfswsSocket
      Read FSocket;
  End;

Type
  TfsWinsockProtocol = Class(TfsBaseCommsProtocol)
  Protected {private}
    FCollectingServerNames: boolean;
    FDatagramPadlock: TfsPadlock;
    FFamily: TfsWinsockFamily;
    FServerNames: TStringList;
    wspLocalInAddr: TfswsInAddr;
    wspLocalIPXNetNum: TfswsIPXNetNum;
    wspLocalIPXAddr: TfswsIPXAddr;
    wspListening: boolean;
    wspListenSocket: TfswsSocket;
    wspRcvDatagramSocket: TfswsSocket;
    wspRcvDGBuffer: PffByteArray;
    wspReceivingDatagram: boolean;
    wspWaitingForConnect: boolean;
    wspWaitingForSendToUnblock: boolean;
  Protected
    Procedure SetFamily(F: TfsWinsockFamily);
    Function cpCreateNotifyWindow: boolean; Override;
    Procedure cpDoReceiveDatagram(Const aName: TffNetName;
      aData: PffByteArray;
      aDataLen: Longint); Override;
    Procedure cpPerformStartUp; Override;

    Procedure wspConnectCompleted(aSocket: TfswsSocket);
    Function wspGetConnForSocket(aSocket: TfswsSocket): TfsWinsockConnection;
    Procedure wspHangupDetected(aSocket: TfswsSocket);
    Procedure wspListenCompleted(aSocket: TfswsSocket);
    Procedure wspProcessCompletedWSACall(WParam, LParam: Longint);
    Procedure wspSendMsgCompleted(aSocket: TfswsSocket);
    Procedure wspReceiveCompleted(aSocket: TfswsSocket);
    Procedure wspReceiveDatagramCompleted(aSocket: TfswsSocket);
    Procedure wspReceiveMsgCompleted(aSocket: TfswsSocket);
    Procedure wspWaitForConnect(aTimeOut: Integer);
    Function wspWaitForSendToUnblock: Boolean; {!!.06}
    Procedure wspWSAEventCompleted(Var WSMsg: TMessage);
  Public
    Constructor Create(Const aName: TffNetAddress;
      aCSType: TfsClientServerType); Override;
    Destructor Destroy; Override;

    Function Call(Const aServerName: TffNetName;
      Var aClientID: TffClientID;
      Const timeOut: Longint): TffResult; Override;
    Procedure GetServerNames(aList: TStrings; Const timeout: Longint); Override;
    Procedure HangUp(aConn: TfsConnection); Override;
    Procedure Listen; Override;
    Function SendMsg(aClientID: TffClientID;
      aData: PffByteArray;
      aDataLen: Longint;
      aConnLock: Boolean): TffResult; Override; {!!.06}

    Procedure ReceiveDatagram; Override;
    Procedure SendDatagram(Const aName: TffNetName;
      aData: PffByteArray;
      aDataLen: Longint); Override;
    Procedure StopReceiveDatagram; Override;

    Property Family: TfsWinsockFamily
      Read FFamily Write SetFamily;
  End;

  TfsTCPIPProtocol = Class(TfsWinsockProtocol)
  Protected
  Public
    Constructor Create(Const aName: TffNetAddress;
      aCSType: TfsClientServerType); Override;
    Class Function GetProtocolName: String; Override;
    { Returns the name of the protocol (e.g., 'TCP/IP'). }

    Class Function Supported: boolean; Override;

  End;

  TfsIPXSPXProtocol = Class(TfsWinsockProtocol)
  Protected
  Public
    Constructor Create(Const aName: TffNetAddress;
      aCSType: TfsClientServerType); Override;
    Class Function GetProtocolName: String; Override;
    { Returns the name of the protocol (e.g., 'TCP/IP'). }

    Class Function Supported: boolean; Override;

  End;

  TfsSingleUserConnection = Class(TfsConnection)
  Protected {private}
    FPartner: HWND;
    FUs: HWND;
    sucSendBuffer: PffByteArray;
  Protected
    Procedure AddToList(List: TFSNormalList); Override;
    Procedure RemoveFromList(List: TFSNormalList); Override;
  Public
    Constructor Create(aOwner: TfsBaseCommsProtocol;
      aRemoteName: TffNetAddress;
      aUs: HWND;
      aPartner: HWND);
    Destructor Destroy; Override;
    Procedure Send(aData: PffByteArray;
      aDataLen: Longint;
      aConnLock: Boolean); {!!.06}
    Property Partner: HWND Read FPartner Write FPartner;
  End;

  TfsSingleUserProtocol = Class(TfsBaseCommsProtocol)
  Protected {private}
    supMsgID: TffWord32;
    supPostMsgID: TffWord32;
    supPartner: HWND;
    supReceivingDatagram: boolean;
  Protected
    Function cpCreateNotifyWindow: boolean; Override;
    Procedure cpPerformStartUp; Override;

    Procedure supDataMsgReceived(Const aClientID: TffClientID;
      Const aCDS: TCopyDataStruct);
    Function supGetConnForPartner(aPartner: HWND): TfsSingleUserConnection;
    Procedure supHangupDetected(Const aClientID: TffClientID);
    Procedure supListenCompleted(aClientID: TffClientID; Wnd: HWND);
    Procedure supMsgReceived(Var SUMsg: TMessage);
    Function supFindPartner(Const aClientID: TffClientID;
      Const timeout: Longint): HWND;
  Public
    Constructor Create(Const aName: TffNetAddress; aCSType: TfsClientServerType); Override;
    Function Call(Const aServerName: TffNetName;
      Var aClientID: TffClientID;
      Const timeout: Longint): TffResult; Override;
    Class Function GetProtocolName: String; Override;
    { Returns the name of the protocol (e.g., 'TCP/IP'). }

    Procedure GetServerNames(aList: TStrings; Const timeout: Longint); Override;
    Procedure HangUp(aConn: TfsConnection); Override;
    Procedure Listen; Override;
    Function SendMsg(aClientID: TffClientID;
      aData: PffByteArray;
      aDataLen: Longint;
      aConnLock: Boolean): TffResult; Override; {!!.06}

    Procedure ReceiveDatagram; Override;
    Procedure SendDatagram(Const aName: TffNetName;
      aData: PffByteArray;
      aDataLen: Longint); Override;
    Procedure StopReceiveDatagram; Override;

  End;

  {===Helper routines===}
Procedure FSSplitNetAddress(Const aAddress: TffNetAddress;
  Var aLocalName: TffNetName;
  Var aNetName: TffNetName);
Procedure FSMakeNetAddress(Var aAddress: TffNetAddress;
  Const aLocalName: TffNetName;
  Const aNetName: TffNetName);

{ TCP & UDP - FFSetxxx routines expect port number to be in
  host byte order. }
Procedure FSSetTCPPort(Const aPort: Integer);
Procedure FSSetUDPPortServer(Const aPort: Integer);
Procedure FSSetUDPPortClient(Const aPort: Integer);

Function FSGetTCPPort: Integer;
Function FSGetUDPPortServer: Integer;
Function FSGetUDPPortClient: Integer;

{ IPX/SPX - FFSetxxx routines expect port number to be in
  host byte order. }
Procedure FSSetIPXSocketServer(Const aSocket: Integer);
Procedure FSSetIPXSocketClient(Const aSocket: Integer);
Procedure FSSetSPXSocket(Const aSocket: Integer);

Function FSGetIPXSocketServer: Integer;
Function FSGetIPXSocketClient: Integer;
Function FSGetSPXSocket: Integer;

{$IFDEF KALog}
Var
  KALog: TFSNormalEventLog;
  {$ENDIF}

Implementation

Uses
  fssrbde;

Const
  DeallocTimeOut = 500;

  { Port vars - define in network-byte order. }
Var
  fsc_TCPPort: Integer = $6365; //$6563;
  fsc_UDPPortServer: Integer = $6365; //$6563;
  fsc_UDPPortClient: Integer = $6465; //$6564;
  fsc_IPXSocketServer: Integer = $6365; //$6563;
  fsc_IPXSocketClient: Integer = $6465; //$6564;
  fsc_SPXSocket: Integer = $6565;

  {===Helper routines==================================================}

Procedure CodeBuffer(Var aCode: TfsNetMsgCode; Var aBuf; aBufLen: Integer);
  Register;
Asm
  push ebx
  push esi
  push edi
  mov edi, eax
@@ResetCode:
  mov ebx, fsc_CodeLength
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
End;
{--------}

Procedure GenerateCode(aStart: Int64; Var aCode: TfsNetMsgCode);
Const
  im = 259200;
  ia = 7141;
  ic = 54773;
Var
  i: Integer;
Begin
  {Note: routine and constants are from Numerical Recipes in Pascal, page 218}
  aStart := aStart Mod im;
  For i := 0 To pred(fsc_CodeLength) Do
    Begin
      aStart := ((aStart * ia) + ic) Mod im;
      aCode[i] := (aStart * 256) Div im;
    End;
End;
{--------}

Procedure CheckWinsockError(Const ErrorCode: Integer; Const Connecting: Boolean);
{ Rewritten !!.05}
{ When doing mass numbers of connects/disconnects and retrying connections
  (see TfsWinsockProtocol.Call), certain errors may occur that appear to be
  timing-related (i.e., the code doesn't see that the socket is connected
  because the event from the Winsock layer has yet to be processed).
  WsaEALREADY & WsaEISCONN showed up consistently on Windows 2000.
  WsaEINVAL showed up consistently on W95. }
Var
  TmpCode: Integer;
Begin
  If (ErrorCode = SOCKET_ERROR) Then
    Begin
      TmpCode := WinsockRoutines.WSAGetLastError;
      If (TmpCode <> 0) And (TmpCode <> WSAEWOULDBLOCK) Then
        If Not (Connecting And
          ((TmpCode = WsaEALREADY) Or
          (TmpCode = WsaEISCONN) Or
          (TmpCode = WsaEINVAL)
          )
          ) Then
          Raise EfsWinsockException.CreateTranslate(TmpCode, Nil);
    End; { if }
End;
{--------}

Procedure FSSplitNetAddress(Const aAddress: TffNetAddress;
  Var aLocalName: TffNetName;
  Var aNetName: TffNetName);
Var
  PosAt: Integer;
Begin
  PosAt := Pos('@', aAddress);
  If (PosAt > 0) Then
    Begin
      aLocalName := Copy(aAddress, 1, FFMinI(Pred(PosAt), fscl_NetNameSize)); {!!.06}
      aNetName := Copy(aAddress, succ(PosAt), FFMinI(Length(aAddress) - PosAt, fscl_NetNameSize)); {!!.06}
    End
  Else
    Begin
      aLocalName := aAddress;
      aNetName := aAddress;
    End;
End;
{--------}

Procedure FSMakeNetAddress(Var aAddress: TffNetAddress;
  Const aLocalName: TffNetName;
  Const aNetName: TffNetName);
Begin
  aAddress := aLocalName;
  {Begin !!.03}
  {$IFDEF IsDelphi}
  If (FFCmpShStr(aLocalName, aNetName, 255) <> 0) Then
    Begin
      FFShStrAddChar(aAddress, '@');
      FFShStrConcat(aAddress, aNetName);
    End;
  {$ELSE}
  If aLocalName <> aNetName Then
    aAddress := aAddress + '@' + aNetName;
  {$ENDIF}
  {End !!.03}
End;
{--------}

Procedure FSSetTCPPort(Const aPort: Integer);
Begin
  If Not FSWSInstalled Then
    Raise EfsWinsockException.CreateNoData(fsStrResGeneral, fserrWSNoWinsock);
  fsc_TCPPort := aPort; //WinsockRoutines.htons(aPort);
End;
{--------}

Procedure FSSetUDPPortServer(Const aPort: Integer);
Begin
  If Not FSWSInstalled Then
    Raise EfsWinsockException.CreateNoData(fsStrResGeneral, fserrWSNoWinsock);
  fsc_UDPPortServer := aPort; //WinsockRoutines.htons(aPort);
End;
{--------}

Procedure FSSetUDPPortClient(Const aPort: Integer);
Begin
  If Not FSWSInstalled Then
    Raise EfsWinsockException.CreateNoData(fsStrResGeneral, fserrWSNoWinsock);
  fsc_UDPPortClient := aPort; //WinsockRoutines.htons(aPort);
End;
{--------}

Function FSGetTCPPort: Integer;
Begin
  If FSWSInstalled Then
    Result := {WinsockRoutines.ntohs(} fsc_TCPPort //)
  Else
    Result := 0;
End;
{--------}

Function FSGetUDPPortServer: Integer;
Begin
  If FSWSInstalled Then
    Result := {WinsockRoutines.ntohs(} fsc_UDPPortServer //)
  Else
    Result := 0;
End;
{--------}

Function FSGetUDPPortClient: Integer;
Begin
  If FSWSInstalled Then
    Result := {WinsockRoutines.ntohs(} fsc_UDPPortClient //)
  Else
    Result := 0;
End;
{--------}

Procedure FSSetIPXSocketServer(Const aSocket: Integer);
Begin
  If Not FSWSInstalled Then
    Raise EfsWinsockException.CreateNoData(fsStrResGeneral, fserrWSNoWinsock);
  fsc_IPXSocketServer := aSocket; //WinsockRoutines.htons(aSocket);
End;
{--------}

Procedure FSSetIPXSocketClient(Const aSocket: Integer);
Begin
  If Not FSWSInstalled Then
    Raise EfsWinsockException.CreateNoData(fsStrResGeneral, fserrWSNoWinsock);
  fsc_IPXSocketClient := aSocket; //WinsockRoutines.htons(aSocket);
End;
{--------}

Procedure FSSetSPXSocket(Const aSocket: Integer);
Begin
  If Not FSWSInstalled Then
    Raise EfsWinsockException.CreateNoData(fsStrResGeneral, fserrWSNoWinsock);
  fsc_SPXSocket := aSocket; //WinsockRoutines.htons(aSocket);
End;
{--------}

Function FSGetIPXSocketServer: Integer;
Begin
  If FSWSInstalled Then
    Result := {WinsockRoutines.ntohs(} fsc_IPXSocketServer //)
  Else
    Result := 0;
End;
{--------}

Function FSGetIPXSocketClient: Integer;
Begin
  If FSWSInstalled Then
    Result := {WinsockRoutines.ntohs(} fsc_IPXSocketClient //)
  Else
    Result := 0;
End;
{--------}

Function FSGetSPXSocket: Integer;
Begin
  If FSWSInstalled Then
    Result := {WinsockRoutines.ntohs(} fsc_SPXSocket //)
  Else
    Result := 0;
End;
{====================================================================}

{===TfsConnection====================================================}

Constructor TfsConnection.Create(aOwner: TfsBaseCommsProtocol;
  aRemoteName: TffNetAddress);
Begin
  Inherited Create;
  FFGetMem(FCode, SizeOf(TfsNetMsgCode));
  FClientID := 0;
  FHangingUp := True;
  FHangupDone := False; {!!.01}
  FHangupLock := TfsPadlock.Create; {!!.01}
  FOwner := aOwner;
  FRemoteName := FFShStrAlloc(aRemoteName);
  MaintainLinks := False; {!!.05}
End;
{--------}

Destructor TfsConnection.Destroy;
Begin
  FHangupLock.Free;
  FFFreeMem(FCode, SizeOf(TfsNetMsgCode));
  FFShStrFree(FRemoteName);
  Inherited Destroy;
End;
{--------}

Procedure TfsConnection.AddToList(List: TFSNormalList);
Begin {do nothing, descendant must do the work}
End;
{--------}

Function TfsConnection.GetRemoteName: String; {!!.10}
Begin
  Result := FRemoteName^;
End;
{--------}

Procedure TfsConnection.ConfirmAlive(SendConfirm: boolean);
Begin
  FAliveRetries := 0;
  FsLLBASE.SetTimer(FLastMsgTimer, FOwner.LastMsgInterval);
  FSendConfirm := SendConfirm;
End;
{--------}

Procedure TfsConnection.DepleteLife;
Begin
  {$IFDEF KALog}
  KALog.WriteStringFmt('DepleteLife, client %d', [ClientID]);
  {$ENDIF}
  inc(FAliveRetries);
End;
{Begin !!.01}
{--------}

Procedure TfsConnection.HangupLock;
Begin
  FHangupLock.Lock;
End;
{--------}

Procedure TfsConnection.HangupUnlock;
Begin
  FHangupLock.Unlock;
End;
{End !!.01}
{--------}

Procedure TfsConnection.InitCode(Const aStart: Int64);
Begin
  { Find the connection associated with this client. }

  If (aStart = 0) Then
    Begin
      FCodeStart := GetTickCount;
      If (FCodeStart = 0) Then
        FCodeStart := $12345678;
    End
  Else
    FCodeStart := aStart;

  GenerateCode(FCodeStart, FCode^);
End;
{--------}

Function TfsConnection.IsAlive: boolean;
Begin
  Result := FAliveRetries < FOwner.KeepAliveRetries;
End;
{--------}

Function TfsConnection.IsVeryAlive: boolean;
Begin
  Result := Not HasTimerExpired(FLastMsgTimer);
End;
{--------}

Function TfsConnection.NeedsConfirmSent: boolean;
Begin
  Result := FSendConfirm;
  FSendConfirm := False;
End;
{--------}

Procedure TfsConnection.RemoveFromList(List: TFSNormalList);
Begin {do nothing, descendant must do the work}
End;
{====================================================================}

{===TfsBaseCommsProtocol=================================================}

Constructor TfsBaseCommsProtocol.Create(Const aName: TffNetAddress;
  aCSType: TfsClientServerType);
Var
  LocalNm: TffNetName;
  NetNm: TffNetName;
Begin
  Inherited Create;
  FConnLock := TfsPadlock.Create;
  FCSType := aCSType;
  FEventLog := Nil;
  FKeepAliveInterval := fsc_KeepAliveInterval;
  FKeepAliveRetries := fsc_KeepAliveRetries;
  FLastMsgInterval := fsc_LastMsgInterval;
  FSSplitNetAddress(aName, LocalNm, NetNm);
  FLocalName := FFShStrAlloc(LocalNm);
  FLogEnabled := False;
  cpSetNetName('Local');
  FSearchTimeOut := 500;
  FStarted := False;
  FStartedEvent := TFSNormalEvent.Create;
  {the net name is set by our descendants}
  cpConnList := TFSNormalList.Create;
  cpIndexByClient := TFSNormalList.Create;
  cpIndexByClient.Sorted := True;
  cpIndexByOSConnector := Nil;
  { If this protocol is for a server then create a connection lookup list.
    The lookup list serves as an index, allowing us to quickly find a
    connection object.  This is much faster than doing a sequential search
    through the cpConnList. }
  If aCSType = csServer Then
    Begin
      cpIndexByOSConnector := TFSNormalList.Create;
      cpIndexByOSConnector.Sorted := True;
    End;
End;
{--------}

Destructor TfsBaseCommsProtocol.Destroy;
Begin
  FStarted := False;
  FConnLock.Free;
  If assigned(FStartedEvent) Then
    FStartedEvent.Free;
  FFShStrFree(FLocalName);
  FFShStrFree(FNetName);
  cpConnList.Free;
  cpIndexByClient.Free;
  If assigned(cpIndexByOSConnector) Then
    cpIndexByOSConnector.Free;
  Inherited Destroy;
End;
{--------}

Procedure TfsBaseCommsProtocol.Breathe;
Var
  dummy: pointer;
  Msg: TMsg;
Begin
  If PeekMessage(Msg, FNotifyWindow, 0, 0, PM_NOREMOVE) Then
    Begin
      While PeekMessage(Msg, FNotifyWindow, 0, 0, PM_REMOVE) Do
        DispatchMessage(Msg);
    End
  Else
    Begin
      dummy := Nil;
      MsgWaitForMultipleObjects(0, dummy, False, 1, QS_ALLINPUT);
    End;
End;
{--------}

Function TfsBaseCommsProtocol.ClientIDExists(Const aClientID: TffClientID): boolean;
{Rewritten !!.05}
Begin
  ConnLock;
  Try
    Result := (cpIndexByClient.Index(aClientID) <> -1);
  Finally
    ConnUnlock;
  End;
End;
{--------}

Function TfsBaseCommsProtocol.ConnectionCount: Longint;
Begin
  Result := 0;
  If assigned(cpConnList) Then
    Result := cpConnList.Count;
End;
{--------}

Procedure TfsBaseCommsProtocol.ConnLock;
Begin
  FConnLock.Lock;
End;
{--------}

Procedure TfsBaseCommsProtocol.ConnUnlock;
Begin
  FConnLock.Unlock;
End;
{--------}

Procedure TfsBaseCommsProtocol.cpAddConnection(aConnection: TfsConnection);
{Rewritten !!.05}
Var
  anItem: TfsIntListItem;
Begin
  ConnLock;
  Try
    aConnection.InitCode(0);
    cpConnList.Insert(aConnection);
    { Add an entry to the index by client. }
    anItem := TfsIntListItem.Create(aConnection.ClientID);
    anItem.ExtraData := aConnection;
    cpIndexByClient.Insert(anItem);
    If Assigned(cpIndexByOSConnector) Then
      aConnection.AddToList(cpIndexByOSConnector);
  Finally
    ConnUnlock;
  End;
End;
{--------}

Procedure TfsBaseCommsProtocol.cpCodeMessage(aConn: TfsConnection;
  aData: PffByteArray;
  aDataLen: Longint);
Const
  LeaveRawLen = 2 * sizeof(Longint);
Var
  aCode: TfsNetMsgCode;
Begin
  If (aDataLen >= LeaveRawLen) Then
    Begin
      If (PffLongint(aData)^ <> fsnmAttachServer) Then
        Begin
          aCode := aConn.Code^;
          CodeBuffer(aCode, aData^[LeaveRawLen], aDataLen - LeaveRawLen);
        End;
    End
End;
{--------}

Function TfsBaseCommsProtocol.cpCreateNotifyWindow: boolean;
Begin
  FNotifyWindow := 0;
  Result := False;
End;
{--------}

Procedure TfsBaseCommsProtocol.cpDestroyNotifyWindow;
Begin
  If (FNotifyWindow <> 0) Then
    Begin
      KillTimer(FNotifyWindow, 1);
      {$IFDEF DCC6OrLater} {!!.11}
      {$WARN SYMBOL_DEPRECATED OFF}
      {$ENDIF}
      DeallocateHWnd(FNotifyWindow);
      {$IFDEF DCC6OrLater} {!!.11}
      {$WARN SYMBOL_DEPRECATED ON}
      {$ENDIF}
    End;
End;
{--------}

Procedure TfsBaseCommsProtocol.cpDoHangUp(aConn: TfsConnection);
Begin
  {Begin !!.01}
  aConn.HangupLock;
  Try
    If aConn.HangupDone Then
      Exit;
    { Are we hanging up on purpose? }
    If aConn.HangingUp Then
      Begin
        { Yes. Call the OnHangUp event if it is declared. }
        If Assigned(FOnHangUp) Then
          FOnHangUp(Self, aConn.ClientID);
      End
        { No. This is an unexpected hangup.  Invoke OnConnectionLost if it is
          declared. }
    Else If Assigned(FOnConnectionLost) Then
      FOnConnectionLost(Self, aConn.ClientID);
    aConn.HangupDone := True;
  Finally
    aConn.HangupUnlock;
  End;
  {End !!.01}
End;
{--------}

Procedure TfsBaseCommsProtocol.cpDoHeardCall(aConnHandle: Longint);
Begin
  If Assigned(FHeardCall) Then
    FHeardCall(Self, aConnHandle);
End;
{--------}

Procedure TfsBaseCommsProtocol.cpPerformShutdown;
Begin
  cpDestroyNotifyWindow;
End;
{--------}

Procedure TfsBaseCommsProtocol.cpSetNetName(aName: String);
Begin
  If assigned(FNetName) Then
    FFShStrFree(FNetName);

  FNetName := FFShStrAlloc(aName);
End;
{--------}

Procedure TfsBaseCommsProtocol.cpDoReceiveDatagram(Const aName: TffNetName;
  aData: PffByteArray;
  aDataLen: Longint);
Begin
  If Assigned(FReceiveDatagram) Then
    FReceiveDatagram(Self, aName, aData, aDataLen);
End;
{--------}

Function TfsBaseCommsProtocol.cpDoReceiveMsg(aConn: TfsConnection;
  msgData: PffByteArray;
  msgDataLen: Longint): boolean;
Begin
  {Look out for keep alives}
  If (PffLongint(msgData)^ = fsnmCheckConnection) Then
    Begin
      cpGotCheckConnection(aConn);
      Result := True;
      Exit;
    End;

  {process normal FF message}
  {$IFDEF KALog}
  KALog.WriteStringFmt('RcvMsg, client %d', [aConn.ClientID]);
  {$ENDIF}
  aConn.ConfirmAlive(False);
  { If this message is too big for us then reject it. }

  If msgDataLen > FMaxNetMsgSize Then
    Begin
      LogStrFmt('Message size %d too large.', [msgDataLen]);
      Result := False;
    End
      { Otherwise if we have a handler for the message then send the message
        to the handler. }
  Else If Assigned(FReceiveMsg) Then
    Begin
      cpCodeMessage(aConn, msgData, msgDataLen);
      Result := FReceiveMsg(Self, aConn.ClientID, msgData, msgDataLen);
    End
  Else
    { Otherwise no handler so just smile. }
    Result := True;
End;
{--------}

Function TfsBaseCommsProtocol.cpExistsConnection(aConnHandle: Longint): boolean;
Begin
  Result := cpConnList.Exists(aConnHandle);
End;
{--------}

Function TfsBaseCommsProtocol.cpFindConnection(Const aClientID: TffClientID): Longint;
Var
  Inx: Longint;
Begin
  Result := -1;
  For Inx := 0 To pred(cpConnList.Count) Do
    If TfsConnection(cpConnList[Inx]).ClientID = aClientID Then
      Begin
        Result := Inx;
        break;
      End;
End;
{--------}

Function TfsBaseCommsProtocol.cpGetConnection(Const aClientID: TffClientID): TfsConnection;
{ Modified !!.05}
Var
  Inx: Integer;
Begin
  { Note: It is possible for a newly-attached client to send another request to
          the server before the server has had a chance to update the new
          client's server-side clientID.  So we use a lock to prevent this
          from happening. }
  ConnLock;
  Try
    Inx := cpIndexByClient.Index(aClientID);
    If (Inx = -1) Then
      Result := Nil
    Else
      Result := TfsConnection(TfsIntListItem(cpIndexByClient[Inx]).ExtraData);
  Finally
    ConnUnlock;
  End;
End;
{--------}

Function TfsBaseCommsProtocol.cpGetConnectionIDs(Const anIndex: Longint): TffClientID;
{Begin !!.01}
Var
  aConn: TfsConnection;
Begin
  aConn := TfsConnection(cpConnList[anIndex]);
  If aConn = Nil Then
    Result := 0
  Else
    Result := TfsConnection(cpConnList[anIndex]).ClientID;
  {End !!.01}
End;
{--------}

Procedure TfsBaseCommsProtocol.cpGotCheckConnection(aConn: TfsConnection);
Begin
  {Reset keepalives}
  If assigned(aConn) Then
    Begin
      {$IFDEF KALog}
      KALog.WriteStringFmt('RcvKA, client %d', [aConn.ClientID]);
      {$ENDIF}
      aConn.ConfirmAlive(True);
    End;
End;
{--------}

Procedure TfsBaseCommsProtocol.cpRemoveConnection(aClientID: TffClientID);
Var
  Inx: Integer;
  aConn: TfsConnection;
Begin
  {Begin !!.05}
  ConnLock;
  Try
    Inx := cpIndexByClient.Index(aClientID);
    { Did we find the connection in the index? }
    If (Inx >= 0) Then
      Begin
        { Yes. Remove the connection from the index and from the connection
          list. }
        aConn := TfsConnection(cpIndexByClient[Inx]).ExtraData;
        cpIndexByClient.DeleteAt(Inx);
        cpConnList.Remove(aConn);
        If assigned(cpIndexByOSConnector) Then
          aConn.RemoveFromList(cpIndexByOSConnector);
        aConn.Free;
      End
    Else
      Begin
        { No. It may be that we have encountered a client that could not
          successfully connect. We have an entry in the connection list but not
          in the index. Do a sequential search for the client. }
        Inx := cpFindConnection(aClientID);
        If Inx >= 0 Then
          Begin
            aConn := TfsConnection(cpConnList[Inx]);
            cpConnList.RemoveAt(Inx);
            aConn.Free;
          End;
      End;
  Finally
    ConnUnlock;
  End;
  {End !!.05}
End;
{--------}

Procedure TfsBaseCommsProtocol.cpTimerTick;
Var
  Inx: Integer;
  Conn: TfsConnection;
  KAMsg: Longint;
Begin
  {Begin !!.05}
  ConnLock;
  Try
    KAMsg := fsnmCheckConnection;
    For Inx := pred(cpConnList.Count) Downto 0 Do
      Begin
        Conn := TfsConnection(cpConnList[Inx]);
        With Conn Do
          Begin
            If (Not Conn.FHangupLock.Locked) And (Not IsAlive) Then
              Begin {!!.11}
                {$IFDEF KALog}
                KALog.WriteStringFmt('Hangup, client %d', [Conn.ClientID]);
                {$ENDIF}
                Conn.HangingUp := False; {!!.06}
                HangUp(Conn);
              End
            Else If NeedsConfirmSent Or (Not IsVeryAlive) Then
              Begin
                {$IFDEF KALog}
                KALog.WriteStringFmt('Send KA, client %d', [Conn.ClientID]);
                {$ENDIF}
                SendMsg(ClientID, @KAMsg, sizeof(KAMsg), False); {!!.06}
                DepleteLife;
              End;
          End;
      End;
  Finally
    ConnUnlock;
  End;
  {End !!.05}
End;
{--------}

Function TfsBaseCommsProtocol.GetLocalName: String; {!!.10}
Begin
  If Assigned(FLocalName) Then
    Result := FLocalName^
  Else
    Result := '';
End;
{--------}

Function TfsBaseCommsProtocol.GetNetName: String; {!!.10}
Begin
  If Assigned(FNetName) Then
    Result := FNetName^
  Else
    Result := '';
End;
{--------}

Function TfsBaseCommsProtocol.GetCodeStart(Const aClientID: TffClientID): Integer;
Var
  aConn: TfsConnection;
  anItem: TfsIntListItem;
Begin
  { Assumption: Connection lists locked via ConnLock at a higher level. }
  Result := 0;
  { Find the connection associated with this client. }
  anItem := TfsIntListItem(cpIndexByClient[cpIndexByClient.Index(aClientID)]);
  If assigned(anItem) Then
    Begin
      aConn := TfsConnection(anItem.ExtraData);
      Result := aConn.CodeStart;
    End;
End;
{--------}

Class Function TfsBaseCommsProtocol.GetProtocolName: String;
Begin
  { return nothing at this level }
  Result := '';
End;
{--------}

Procedure TfsBaseCommsProtocol.HangUpByClientID(aClientID: TffClientID);
Var
  aConn: TfsConnection;
Begin
  aConn := cpGetConnection(aClientID);
  If assigned(aConn) Then
    Begin
      aConn.HangingUp := True;
      HangUp(aConn);
    End;
End;
{Begin !!.01}
{--------}

Procedure TfsBaseCommsProtocol.HangupDone(aClientID: TffClientID);
Var
  aConn: TfsConnection;
Begin
  aConn := cpGetConnection(aClientID);
  If assigned(aConn) Then
    aConn.HangupDone := True;
End;
{--------}

Function TfsBaseCommsProtocol.HangupIsDone(aClientID: TffClientID): Boolean;
Var
  aConn: TfsConnection;
Begin
  Result := False;
  aConn := cpGetConnection(aClientID);
  If assigned(aConn) Then
    Result := aConn.HangupDone;
End;
{--------}

Procedure TfsBaseCommsProtocol.HangupLock(aClientID: TffClientID);
Var
  aConn: TfsConnection;
Begin
  aConn := cpGetConnection(aClientID);
  If assigned(aConn) Then
    aConn.HangupLock;
End;
{--------}

Procedure TfsBaseCommsProtocol.HangupUnlock(aClientID: TffClientID);
Var
  aConn: TfsConnection;
Begin
  aConn := cpGetConnection(aClientID);
  If assigned(aConn) Then
    aConn.HangupUnlock;
End;
{End !!.01}
{--------}

Procedure TfsBaseCommsProtocol.InitCode(Const aClientID: TffClientID;
  Const aStart: Int64);
Var
  aConn: TfsConnection;
  anItem: TfsIntListItem;
Begin
  { Find the connection associated with this client. }
  anItem := TfsIntListItem(cpIndexByClient[cpIndexByClient.Index(aClientID)]);
  If assigned(anItem) Then
    Begin
      aConn := TfsConnection(anItem.ExtraData);
      aConn.InitCode(aStart);
    End;
End;
{--------}

Procedure TfsBaseCommsProtocol.ResetKeepAliveTimer;
Begin
  If (FNotifyWindow <> 0) Then
    Begin
      {$IFDEF KALog}
      KALog.WriteStringFmt('ResetKeepAliveTimer: protocol %d', [Longint(Self)]);
      {$ENDIF}
      KillTimer(FNotifyWindow, 1);
      Windows.SetTimer(FNotifyWindow, 1, FKeepAliveInterval, Nil); {!!.05}
    End;
End;
{--------}

Procedure TfsBaseCommsProtocol.Shutdown;
Begin
  If IsStarted Then
    Begin
      cpPerformShutdown;
      FStarted := False;
    End;
End;
{--------}

Procedure TfsBaseCommsProtocol.StartUp;
Begin
  If Not IsStarted Then
    Begin
      cpPerformStartUp;
      FStarted := True;
      FStartedEvent.SignalEvent;
    End;
End;
{--------}

Class Function TfsBaseCommsProtocol.Supported: boolean;
Begin
  Result := True;
End;
{--------}

Procedure TfsBaseCommsProtocol.UpdateClientID(Const oldClientID,
  newClientID: TffClientID);
Var
  aConn: TfsConnection;
  anItem: TfsIntListItem;
Begin
  {Begin !!.05}
  ConnLock;
  Try
    anItem := TfsIntListItem(cpIndexByClient[cpIndexByClient.Index(oldClientID)]);
    If assigned(anItem) Then
      Begin
        aConn := anItem.ExtraData;
        aConn.ClientID := newClientID;

        { Get rid of the old index entry; as a side effect, anItem should be
          freed. }
        cpIndexByClient.Delete(oldClientID);

        { Create a new entry for the index. }
        anItem := TfsIntListItem.Create(newClientID);
        anItem.ExtraData := aConn;
        cpIndexByClient.Insert(anItem);
      End;
  Finally
    ConnUnlock;
  End;
  {End !!.05}
End;
{--------}

Procedure TfsBaseCommsProtocol.LogStr(Const aMsg: String);
Begin
  If FLogEnabled And assigned(FEventLog) Then
    FEventLog.WriteSTring(format('%s: %s',
      [Self.GetProtocolName, aMsg]));
End;
{--------}

Procedure TfsBaseCommsProtocol.LogStrFmt(Const aMsg: String;
  args: Array Of Const);
Begin
  If FLogEnabled And assigned(FEventLog) Then
    LogStr(format(aMsg, args));
End;
{====================================================================}

{===TfsWinsockConnection=============================================}

Constructor TfsWinsockConnection.Create(aOwner: TfsBaseCommsProtocol;
  aRemoteName: TffNetAddress;
  aSocket: TfswsSocket;
  aFamily: TfsWinsockFamily;
  aNotifyWnd: HWND);
Var
  NagelOn: Bool;
Begin
  Inherited Create(aOwner, aRemoteName);
  FHangingUp := False;
  { Note that we are overriding the initial value of FHangingUp on purpose. }
  FSocket := aSocket;
  FFamily := aFamily;
  If (aFamily = wfTCP) Then
    Begin
      FSWSGetSocketOption(aSocket, IPPROTO_TCP, TCP_NODELAY, NagelOn, sizeof(NagelOn));
      If NagelOn Then
        Begin
          NagelOn := False;
          FSWSSetSocketOption(aSocket, IPPROTO_TCP, TCP_NODELAY, NagelOn, sizeof(NagelOn));
        End;
    End;
  FSWSSetSocketOption(aSocket, SOL_SOCKET, So_RCVBUF, fsc_TCPRcvBuf,
    sizeof(fsc_TCPRcvBuf));
  FSWSSetSocketOption(aSocket, SOL_SOCKET, So_SNDBUF, fsc_TCPSndBuf,
    sizeof(fsc_TCPSndBuf));
  FSWSGetSocketOption(aSocket, SOL_SOCKET, So_RCVBUF, wscRcvBuf,
    sizeof(wscRcvBuf));
  FSWSGetSocketOption(aSocket, SOL_SOCKET, So_SNDBUF, wscSndBuf,
    sizeof(wscSndBuf));
  wscNotifyWnd := aNotifyWnd;
  //  wscPortal := TffReadWritePortal.Create;                            {Deleted !!.05}
  GetMem(wscRcvBuffer, fsc_MaxWinsockMsgSize);
  wscPacketHead := Nil;
  wscPacketTail := Nil;
  wscIsSending := False;
End;
{--------}

Destructor TfsWinsockConnection.Destroy;
Var
  aPacket: PfswscPacket;
Begin
  HangupLock; {!!.05}
  //  wscPortal.BeginWrite;                                              {Deleted !!.05}
  Try
    Try
      FSWSDestroySocket(Socket);
    Except
    End;
    While wscPacketHead <> Nil Do
      Begin
        aPacket := wscPacketHead^.lpNext;
        ffFreeMem(wscPacketHead^.lpData, wscPacketHead^.dwLength);
        ffFreeMem(wscPacketHead, sizeof(TfswscPacket));
        wscPacketHead := aPacket;
      End;
    FreeMem(wscRcvBuffer, fsc_MaxWinsockMsgSize);
  Finally
    HangupUnlock; {!!.05}
  End;
  Inherited Destroy;
End;
{--------}

Procedure TfsWinsockConnection.AddToList(List: TFSNormalList);
Var
  T: TfsIntListItem;
Begin {add a list entry to allow socket lookups}
  T := TfsIntListItem.Create(Socket);
  T.ExtraData := Self;
  List.Insert(T);
End;
{--------}

Procedure TfsWinsockConnection.RemoveFromList(List: TFSNormalList);
Begin
  List.Delete(FSocket);
End;
{--------}

Function TfsWinsockConnection.Send(aData: PffByteArray;
  aDataLen: Longint;
  aDataStart: Longint;
  Var aBytesSent: Longint;
  aConnLock: Boolean): Integer; {!!.06}
Var
  BytesSent: Longint;
  PacketBuffer: PfswscPacket;
Begin
  If aConnLock Then {!!.06}
    HangupLock; {!!.05}
  Try
    Result := 0;
    If (aDataLen - aDataStart) > 0 Then
      Begin
        {Add the data packet to the wscPacketList }
        ffGetMem(PacketBuffer, sizeof(TfswscPacket));
        ffGetMem(PacketBuffer^.lpData, aDataLen);
        PacketBuffer^.dwLength := aDataLen;
        PacketBuffer^.dwStart := aDataStart;
        Move(aData^[0], PacketBuffer^.lpData^, PacketBuffer^.dwLength);
        Owner.cpCodeMessage(Self, PacketBuffer^.lpData, PacketBuffer^.dwLength);
        PacketBuffer^.lpNext := Nil;
        {Add the packet to the end of the list }
        If Not assigned(wscPacketHead) Then
          wscPacketHead := PacketBuffer
        Else If assigned(wscPacketTail) Then
          wscPacketTail^.lpNext := PacketBuffer;
        wscPacketTail := PacketBuffer;
        aBytesSent := 0; {!!.06}
        //      aBytesSent := aDataLen-aDataStart; {Always report all bytes sent} {Deleted !!.06}
      End;
    If (Not wscIsSending) And Assigned(wscPacketHead) Then
      Begin
        {now try to send some data}
        Try
          {send the first waiting data packet}
          BytesSent := WinsockRoutines.send(Socket,
            wscPacketHead^.lpData^[wscPacketHead^.dwStart],
            wscPacketHead^.dwLength - wscPacketHead^.dwStart,
            0);
        Except
          BytesSent := SOCKET_ERROR;
        End;
        If (BytesSent = SOCKET_ERROR) Then
          Begin
            {There was an error sending }
            Result := WinsockRoutines.WSAGetLastError;
            If (Result = WSAEWOULDBLOCK) Then
              Begin
                { Mark this connection as blocked and leave the Packet on the list. }
                wscIsSending := True;
                //           Result := 0;                                              {Deleted !!.06}
              End
                {Begin !!.06}
            Else If Result = 0 Then
              { If no error code returned then reset the Result to -1 so that we
                break out of the send loop, avoiding a re-add of the current
                packet to the packet list. }
              Result := -1;
            {End !!.06}
          End
        Else If BytesSent < (wscPacketHead^.dwLength - wscPacketHead^.dwStart) Then
          Begin
            { we didn't send the whole thing, so re-size the data packet}
            inc(wscPacketHead^.dwStart, BytesSent);
            inc(aBytesSent, BytesSent); {!!.06}
            { now try sending the remaining data again }
            Result := Send(Nil, 0, 0, aBytesSent, aConnLock); {!!.06}
          End
        Else
          Begin
            {we sent the packet, so remove it and continue }
            ffFreeMem(wscPacketHead^.lpData, wscPacketHead^.dwLength);
            PacketBuffer := wscPacketHead;
            wscPacketHead := wscPacketHead^.lpNext;
            If Not Assigned(wscPacketHead) Then
              wscPacketTail := Nil;
            ffFreeMem(PacketBuffer, sizeof(TfswscPacket));
            inc(aBytesSent, BytesSent); {!!.11}
            Result := 0;
          End;
      End;
  Finally
    If aConnLock Then {!!.06}
      HangupUnlock; {!!.05}
    //    wscPortal.EndWrite;                                              {Deleted !!.05}
  End;
End;
{--------}

Procedure TfsWinsockConnection.StartReceive;
Begin
  FSWSAsyncSelect(Socket, wscNotifyWnd,
    FD_READ Or FD_WRITE Or FD_CLOSE);
End;
{====================================================================}

{===TfsWinsockProtocol===============================================}

Constructor TfsWinsockProtocol.Create(Const aName: TffNetAddress;
  aCSType: TfsClientServerType);
Begin
  {make sure Winsock is installed}
  If Not FSWSInstalled Then
    Raise EfsWinsockException.CreateNoData(fsStrResGeneral, fserrWSNoWinsock);
  {let our ancestor create itself}
  Inherited Create(aName, aCSType);
  FCollectingServerNames := False;
  FDatagramPadlock := TfsPadlock.Create;
  FMaxNetMsgSize := fsc_MaxWinsockMsgSize;
  FServerNames := TStringList.Create;
  FServerNames.Sorted := True;
  FServerNames.Duplicates := dupIgnore;
  {set the sockets we use to default values}
  wspListenSocket := INVALID_SOCKET;
  wspRcvDatagramSocket := INVALID_SOCKET;
  {allocate a receive datagram buffer}
  GetMem(wspRcvDGBuffer, fsc_MaxDatagramSize);
End;
{--------}

Destructor TfsWinsockProtocol.Destroy;
Begin
  If assigned(FServerNames) Then
    FServerNames.Free;
  If assigned(FDatagramPadlock) Then
    FDatagramPadlock.Free;
  FSWSDestroySocket(wspListenSocket);
  FSWSDestroySocket(wspRcvDatagramSocket);
  Inherited Destroy;
  FFShStrFree(FNetName);
  FreeMem(wspRcvDGBuffer, fsc_MaxDatagramSize);
End;
{--------}

Function TfsWinsockProtocol.Call(Const aServerName: TffNetName;
  Var aClientID: TffClientID;
  Const timeout: Longint): TffResult;
Var
  NewSocket: TfswsSocket;
  Conn: TfsWinsockConnection;
  SASize: Integer;
  AddrFamily: Integer;
  Protocol: Integer;
  RemSockAddr: TfswsSockAddr;
  aNetName: TffNetName;
  T: TfsTimer; {!!.05}
  StartTime: DWORD; {!!.05}
Begin

  Result := DBIERR_NONE;

  {servers don't call}
  If (CSType = csServer) Then
    Raise EfsCommsException.CreateNoData(fsStrResGeneral, fserrCommsCannotCall);

  { If no servername then we cannot connect. }
  If (aServerName = '') Then
    Begin
      Result := DBIERR_SERVERNOTFOUND;
      Exit;
    End;

  {either create a socket address record for TCP...}
  If (Family = wfTCP) Then
    Begin
      AddrFamily := AF_INET;
      Protocol := 0;
      SASize := sizeof(TfswsSockAddrIn);
      FillChar(RemSockAddr, SASize, 0);
      With RemSockAddr.TCP Do
        Begin
          sin_family := PF_INET;
          sin_port := fsc_TCPPort;
          If FSWSCvtStrToAddr(aServerName, sin_addr) Then
            //      aNetName := FSWSGetRemoteNameFromAddr(sin_addr)
          Else
            Begin
              If Not FSWSGetRemoteHost(aServerName, aNetName, sin_addr) Then
                Begin
                  Result := DBIERR_SERVERNOTFOUND; {!!.06}
                  Exit;
                End;
            End;
        End;
    End
      {or for IPX...}
  Else {if (Family = wfIPX) then}
    Begin
      AddrFamily := AF_IPX;
      Protocol := NSPROTO_SPX;
      SASize := sizeof(TfswsSockAddrIPX);
      FillChar(RemSockAddr, SASize, 0);
      With RemSockAddr.IPX Do
        Begin
          sipx_family := PF_IPX;
          If Not FSWSCvtStrToIPXAddr(aServerName,
            sipx_netnum,
            sipx_nodenum) Then
            Exit;
          sipx_socket := fsc_SPXSocket;
        End;
    End;
  {open a call socket}
  NewSocket := FSWSCreateSocket(AddrFamily, SOCK_STREAM, Protocol);
  Try
    {set the socket to non-blocking mode}
    FSWSAsyncSelect(NewSocket, FNotifyWindow, FD_CONNECT);
    {try and connect}
    wspWaitingForConnect := True;
    CheckWinsockError(
      WinsockRoutines.connect(NewSocket, RemSockAddr, SASize), False);
    {Begin !!.05}
    //    wspWaitForConnect(timeout, NewSocket);
    StartTime := GetTickCount;
    SetTimer(T, timeout);
    While wspWaitingForConnect And (Not HasTimerExpired(T)) Do
      Begin
        If (GetTickCount - StartTime) > fsc_ConnectRetryTimeout Then
          Begin
            CheckWinsockError(WinsockRoutines.connect(NewSocket, RemSockAddr,
              SASize), True);
            Starttime := GetTickCount;
          End;
        Breathe;
      End;
    {End !!.05}
        {if we connected...}
    If Not wspWaitingForConnect Then
      Begin
        {create a new connection}
        Conn := TfsWinsockConnection.Create(Self, aNetName, NewSocket, Family,
          FNotifyWindow);
        Conn.ClientID := Conn.Handle;
        aClientID := Conn.Handle;
        cpAddConnection(Conn);
        Conn.StartReceive;
      End
    Else
      Begin {we didn't connect}
        FSWSDestroySocket(NewSocket);
        Result := DBIERR_SERVERNOTFOUND;
      End;
  Except
    FSWSDestroySocket(NewSocket);
    Raise;
  End; {try..except}
End;
{--------}

Procedure TfsWinsockProtocol.cpDoReceiveDatagram(Const aName: TffNetName;
  aData: PffByteArray;
  aDataLen: Longint);
Var
  Addr: TffNetAddress; { sender }
  Datagram: PfsnmServerNameReply Absolute aData; { sender }
  Msg: PfsnmRequestServerName Absolute aData; { listener }
  Reply: TfsnmServerNameReply; { listener }
Begin
  Inherited cpDoReceiveDatagram(aName, aData, aDataLen);
  FDatagramPadlock.Lock;
  Try
    { If we are on the sending side, waiting for server names to roll in
      then get the server's reply and add it to our list of server names. }
    If FCollectingServerNames Then
      Begin
        If assigned(aData) And (Datagram^.MsgID = fsnmServerNameReply) Then
          Begin
            FSMakeNetAddress(Addr, Datagram^.ServerLocalName, aName);
            FServerNames.Add(Addr);
          End;
      End
    Else If (aDataLen = sizeof(TfsnmRequestServerName)) And
      (Msg^.MsgID = fsnmRequestServerName) Then
      Begin
        {send a message back to the caller with our name}
        Reply.MsgID := fsnmServerNameReply;
        Reply.ServerLocalName := LocalName;
        Reply.ServerNetName := NetName;
        SendDatagram(aName, @Reply, sizeof(Reply));
      End;
  Finally
    FDatagramPadlock.Unlock;
  End;
End;
{--------}

Procedure TfsWinsockProtocol.cpPerformStartUp;
Var
  AddrFamily: Integer;
  Protocol: Integer;
  SASize: Integer;
  SockAddr: TfswsSockAddr;
Begin
  {create our notify window}
  If Not cpCreateNotifyWindow Then
    Begin
      LogStr('Could not create notification window.');
      Raise EfsCommsException.CreateNoData(fsStrResGeneral, fserrCommsNoWinRes);
    End;

  {create and bind the listen socket if we're a server; for a client,
   we never would listen}
  If (CSType = csServer) Then
    Begin
      {==the listen socket==}
      {create a socket address record}
      If (Family = wfTCP) Then
        Begin
          AddrFamily := AF_INET;
          Protocol := 0;
          SASize := sizeof(TfswsSockAddrIn);
          FillChar(SockAddr, SASize, 0);
          With SockAddr.TCP Do
            Begin
              sin_family := PF_INET;
              sin_port := fsc_TCPPort;
              sin_addr := wspLocalInAddr;
            End;
        End
      Else {if (Family = wfIPX) then}
        Begin
          AddrFamily := AF_IPX;
          Protocol := NSPROTO_SPX;
          SASize := sizeof(TfswsSockAddrIPX);
          FillChar(SockAddr, SASize, 0);
          With SockAddr.IPX Do
            Begin
              sipx_family := PF_IPX;
              sipx_netnum := wspLocalIPXNetNum;
              sipx_nodenum := wspLocalIPXAddr;
              sipx_socket := fsc_SPXSocket;
            End;
        End;
      {open a listen socket}
      wspListenSocket := FSWSCreateSocket(AddrFamily, SOCK_STREAM, Protocol);
      {bind the socket to the address}
      CheckWinsockError(
        WinsockRoutines.bind(wspListenSocket, SockAddr, SASize), False);
    End;
End;
{--------}

Procedure TfsWinsockProtocol.GetServerNames(aList: TStrings; Const timeout: Longint);
Var
  TotalTimer: TfsTimer;
  NameReq: TfsnmRequestServerName;
Begin
  If Not assigned(aList) Then
    Exit;

  { Open and prepare a UDP socket. }
  ReceiveDatagram;
  FCollectingServerNames := True;
  Try
    aList.Clear;
    FServerNames.Clear;
    NameReq.MsgID := fsnmRequestServerName;
    SetTimer(TotalTimer, timeout); {!!.13}
    SendDatagram('', @NameReq, sizeOf(NameReq));
    Repeat
      Breathe;
    Until HasTimerExpired(TotalTimer);
    aList.Assign(FServerNames);
  Finally
    FCollectingServerNames := False;
    StopReceiveDatagram;
  End;

End;
{--------}

Procedure TfsWinsockProtocol.HangUp(aConn: TfsConnection);
Begin
  cpDoHangUp(aConn);
  cpRemoveConnection(aConn.ClientID);
End;
{--------}

Procedure TfsWinsockProtocol.Listen;
Begin
  {clients don't listen}
  If (CSType = csClient) Then
    Raise EfsCommsException.CreateNoData(fsStrResGeneral, fserrCommsCantListen);
  {start listening, if not doing so already}
  If Not wspListening Then
    Begin
      FSWSAsyncSelect(wspListenSocket, FNotifyWindow, FD_ACCEPT);
      CheckWinsockError(WinsockRoutines.listen(wspListenSocket, SOMAXCONN), False);
      wspListening := True;
    End;
End;
{--------}

Procedure TfsWinsockProtocol.ReceiveDatagram;
Var
  AddrFamily: Integer;
  Protocol: Integer;
  SASize: Integer;
  BCastOn: Bool;
  SockAddr: TfswsSockAddr;
Begin
  If Not wspReceivingDatagram Then
    Begin
      {create and bind the receive datagram socket}
      {create a socket address record}
      If (Family = wfTCP) Then
        Begin
          AddrFamily := AF_INET;
          Protocol := 0;
          SASize := sizeof(TfswsSockAddrIn);
          FillChar(SockAddr, SASize, 0);
          With SockAddr.TCP Do
            Begin
              sin_family := PF_INET;
              If (CSType = csClient) Then
                sin_port := fsc_UDPPortClient
              Else
                sin_port := fsc_UDPPortServer;
              sin_addr := wspLocalInAddr;
            End;
        End
      Else {if (Family = wfIPX) then}
        Begin
          AddrFamily := AF_IPX;
          Protocol := NSPROTO_IPX;
          SASize := sizeof(TfswsSockAddrIPX);
          FillChar(SockAddr, SASize, 0);
          With SockAddr.IPX Do
            Begin
              sipx_family := PF_IPX;
              sipx_netnum := wspLocalIPXNetNum;
              sipx_nodenum := wspLocalIPXAddr;
              If (CSType = csClient) Then
                sipx_socket := fsc_IPXSocketClient
              Else
                sipx_socket := fsc_IPXSocketServer;
            End;
        End;
      {open a receivedatagram socket}
      wspRcvDatagramSocket := FSWSCreateSocket(AddrFamily,
        SOCK_DGRAM,
        Protocol);
      {make sure the socket can do broadcasts (req for IPX)}
      If (Family = wfIPX) Then
        Begin
          BCastOn := True;
          FSWSSetSocketOption(wspRcvDatagramSocket, SOL_SOCKET, SO_BROADCAST,
            BCastOn, sizeof(BCastOn));
        End;
      {bind the socket to the address}
      CheckWinsockError(
        WinsockRoutines.bind(wspRcvDatagramSocket, SockAddr, SASize), False);
      FSWSAsyncSelect(wspRcvDatagramSocket, FNotifyWindow, FD_READ Or FD_WRITE);
      wspReceivingDatagram := True;
    End;
End;
{--------}

Procedure TfsWinsockProtocol.SendDatagram(Const aName: TffNetName;
  aData: PffByteArray;
  aDataLen: Longint);
Var
  SockAddr: TfswsSockAddr;
  Socket: TfswsSocket;
  SASize: Integer;
  BCastOn: Bool;
  NetName: TffNetName;
Begin
  {create a send datagram socket}
  If (Family = wfTCP) Then
    Begin
      Socket := FSWSCreateSocket(AF_INET, SOCK_DGRAM, 0);
    End
  Else {Family <> wfTCP}
    Begin
      Socket := FSWSCreateSocket(AF_IPX, SOCK_DGRAM, NSPROTO_IPX);
    End;
  Try
    {create the socket address to bind to}
    If (aName = '') Then
      Begin {a broadcast message}
        {create a socket address record}
        If (Family = wfTCP) Then
          Begin
            SASize := sizeof(TfswsSockAddrIn);
            FillChar(SockAddr, SASize, 0);
            With SockAddr.TCP Do
              Begin
                sin_family := PF_INET;
                If (CSType = csClient) Then
                  sin_port := fsc_UDPPortServer
                Else
                  sin_port := fsc_UDPPortClient;
                sin_addr := INADDR_BROADCAST;
              End;
          End
        Else {Family <> wfTCP}
          Begin
            SASize := sizeof(TfswsSockAddrIPX);
            FillChar(SockAddr, SASize, 0);
            With SockAddr.IPX Do
              Begin
                sipx_family := PF_IPX;
                FillChar(sipx_nodenum, sizeof(sipx_nodenum), $FF);
                If (CSType = csClient) Then
                  sipx_socket := fsc_IPXSocketServer
                Else
                  sipx_socket := fsc_IPXSocketClient;
              End;
          End;
        {make sure the socket can do broadcasts}
        BCastOn := True;
        FSWSSetSocketOption(Socket, SOL_SOCKET, SO_BROADCAST, BCastOn, sizeof(BCastOn));
      End
    Else
      Begin {a specific target}
        {create a socket address record}
        If (Family = wfTCP) Then
          Begin
            SASize := sizeof(TfswsSockAddrIn);
            FillChar(SockAddr, SASize, 0);
            With SockAddr.TCP Do
              Begin
                sin_family := PF_INET;
                If (CSType = csClient) Then
                  sin_port := fsc_UDPPortServer
                Else
                  sin_port := fsc_UDPPortClient;
                If Not FSWSCvtStrToAddr(aName, sin_addr) Then
                  If Not FSWSGetRemoteHost(aName, NetName, sin_addr) Then
                    Exit;
              End;
          End
        Else {Family <> wfTCP}
          Begin
            SASize := sizeof(TfswsSockAddrIPX);
            FillChar(SockAddr, SASize, 0);
            With SockAddr.IPX Do
              Begin
                sipx_family := PF_IPX;
                If Not FSWSCvtStrToIPXAddr(aName, sipx_netnum, sipx_nodenum) Then
                  Exit;
                If (CSType = csClient) Then
                  sipx_socket := fsc_IPXSocketServer
                Else
                  sipx_socket := fsc_IPXSocketClient;
              End;
          End;
      End;
    CheckWinsockError(
      WinsockRoutines.sendto(Socket, aData^, aDataLen, 0, SockAddr, SASize),
      False);
  Finally
    FSWSDestroySocket(Socket);
  End; {try.finally}
End;
{--------}

Function TfsWinsockProtocol.SendMsg(aClientID: TffClientID;
  aData: PffByteArray;
  aDataLen: Longint;
  aConnLock: Boolean): TffResult; {!!.06}
Var
  Conn: TfsWinsockConnection;
  SendResult: Integer;
  BytesSent: Longint;
  SentSoFar: Longint;
  DataPtr: PffByteArray; {!!.06}
  DataLen: Longint; {!!.06}
  TimerExpired: Boolean; {!!.06}
Begin
  Result := DBIERR_NONE;
  Conn := TfsWinsockConnection(cpGetConnection(aClientID));
  If Assigned(Conn) Then
    Begin
      DataPtr := aData; {!!.06}
      DataLen := aDataLen; {!!.06}
      SentSoFar := 0;
      While (SentSoFar < DataLen) Do
        Begin
          SendResult := Conn.Send(DataPtr, DataLen, SentSoFar, BytesSent, {!!.06}
            aConnLock); {!!.06}
          If (SendResult = WSAEWOULDBLOCK) Then
            Begin
              {Begin !!.06}
              TimerExpired := wspWaitForSendToUnblock;
              { The connection has the packet already on its list, waiting to be
                resent. Reset the data pointer & length so that the connection
                does not add a duplicate packet to its list. }
              DataPtr := Nil;
              DataLen := 0;
              { The connection may have been killed (hung up), so recheck. }
              Conn := TfsWinsockConnection(cpGetConnection(aClientID));
              If Conn = Nil Then
                Exit
              Else If TimerExpired Then
                Begin
                  wspWaitingForSendToUnblock := False;
                  Conn.IsSending := False;
                End;
              {End !!.06}
            End
          Else If (SendResult <> 0) Then
            Begin
              LogStrFmt('Unhandled Winsock Exception   %d', [SendResult]);
              Result := SendResult;
              Exit;
            End
          Else
            Begin
              inc(SentSoFar, BytesSent);
            End;
        End; { while }
    End
  Else
    Result := fserrConnectionLost;
End;
{--------}

Procedure TfsWinsockProtocol.SetFamily(F: TfsWinsockFamily);
Var
  LocalHost: TffNetName;
Begin
  If (FNetName <> Nil) Then
    FFShStrFree(FNetName);
  FFamily := F;
  If (F = wfTCP) Then
    Begin
      {get our name and address}
      If Not FSWSGetLocalHostByNum(fsc_TCPInterface, LocalHost,
        wspLocalInAddr) Then
        Raise EfsWinsockException.CreateNoData(fsStrResGeneral, fserrWSNoLocalAddr);
      cpSetNetName(FSWSCvtAddrToStr(wspLocalInAddr));
    End
  Else If (F = wfIPX) Then
    Begin
      {get our IPX address}
      If Not FSWSGetLocalIPXAddr(wspLocalIPXNetNum, wspLocalIPXAddr) Then
        Raise EfsWinsockException.CreateNoData(fsStrResGeneral, fserrWSNoLocalAddr);
      cpSetNetName(FSWSCvtIPXAddrToStr(wspLocalIPXNetNum, wspLocalIPXAddr));
    End;
End;
{--------}

Procedure TfsWinsockProtocol.StopReceiveDatagram;
Begin
  If wspReceivingDatagram Then
    Begin
      FSWSDestroySocket(wspRcvDatagramSocket);
      wspRcvDatagramSocket := INVALID_SOCKET;
      wspReceivingDatagram := False;
    End;
End;
{--------}

Procedure TfsWinsockProtocol.wspConnectCompleted(aSocket: TfswsSocket);
Begin
  wspWaitingForConnect := False;
End;
{--------}

Function TfsWinsockProtocol.cpCreateNotifyWindow: boolean;
Begin
  {$IFDEF DCC6OrLater} {!!.11}
  {$WARN SYMBOL_DEPRECATED OFF}
  {$ENDIF}
  FNotifyWindow := AllocateHWnd(wspWSAEventCompleted);
  {$IFDEF DCC6OrLater} {!!.11}
  {$WARN SYMBOL_DEPRECATED ON}
  {$ENDIF}
  Result := FNotifyWindow <> 0;
  If Result Then
    Begin
      {$IFDEF KALog}
      KALog.WriteStringFmt('Winsock.cpCreateNotifyWindow: protocol %d',
        [Longint(Self)]);
      {$ENDIF}
      Windows.SetTimer(FNotifyWindow, 1, FKeepAliveInterval, Nil); {!!.05}
    End;
End;
{--------}

Function TfsWinsockProtocol.wspGetConnForSocket(aSocket: TfswsSocket): TfsWinsockConnection;
Var
  Inx: Integer;
  T: TfsIntListItem;
Begin

  ConnLock;
  Try
    { If indexing connections then use the index to find the connection. }
    If Assigned(cpIndexByOSConnector) Then
      Begin
        T := TfsIntListItem(cpIndexByOSConnector.Items[cpIndexByOSConnector.Index(aSocket)]);
        If T = Nil Then
          Result := Nil
        Else
          Result := T.ExtraData;
        Exit;
      End;
    For Inx := 0 To pred(cpConnList.Count) Do
      Begin
        Result := TfsWinsockConnection(cpConnList[Inx]);
        If (Result.Socket = aSocket) Then
          Exit;
      End;
  Finally
    ConnUnlock;
  End;
  Result := Nil;
End;
{--------}

Procedure TfsWinsockProtocol.wspHangupDetected(aSocket: TfswsSocket);
{Rewritten !!.06}
Var
  Conn: TfsWinsockConnection;
Begin
  Conn := wspGetConnForSocket(aSocket);
  If (Conn <> Nil) Then
    Begin
      Conn.HangingUp := False;
      HangUp(Conn);
    End;
End;
{--------}

Procedure TfsWinsockProtocol.wspListenCompleted(aSocket: TfswsSocket);
Var
  NewSocket: TfswsSocket;
  SocketAddr: TfswsSockAddr;
  AddrLen: Integer;
  Conn: TfsWinsockConnection;
  RemoteName: TffNetName;
  WasAdded: boolean;
Begin
  AddrLen := sizeof(SocketAddr);
  NewSocket := WinsockRoutines.accept(aSocket, SocketAddr, AddrLen);
  If (NewSocket <> INVALID_SOCKET) Then
    Begin
      {a listen event has been accepted, create a connection}
      WasAdded := False;
      Conn := Nil;
      Try
        RemoteName := ''; {!!!!}
        { When we first create this connection, we don't have a clientID so
          we temporarily use the connection's handle.  There is also a temporary
          clientID on the client-side of things.
          When the client is given a real clientID, the temporary clientIDs on
          both client and server are replaced with the true clientID. }
        Conn := TfsWinsockConnection.Create(Self, RemoteName, NewSocket, Family,
          FNotifyWindow);
        Conn.ClientID := Conn.Handle;
        //      Conn.InitCode(0);                                              {Deleted !!.05}
        cpAddConnection(Conn);
        WasAdded := True; {!!.03}
        Conn.StartReceive;
        cpDoHeardCall(Conn.ClientID);
      Except
        If WasAdded Then
          cpRemoveConnection(Conn.ClientID);
        Raise;
      End;
    End;
End;
{--------}

Procedure TfsWinsockProtocol.wspProcessCompletedWSACall(WParam, LParam: Longint);
Begin
  {check the error code}
  If (WSAGetSelectError(LParam) <> 0) Then
    Begin
      wspHangupDetected(TfswsSocket(WParam));
      wspWaitingForSendToUnblock := False;
      Exit;
    End;
  {check for event completion (note case is in numeric sequence)}
  Case WSAGetSelectEvent(LParam) Of
    FD_READ:
      wspReceiveCompleted(TfswsSocket(WParam));
    FD_WRITE:
      wspSendMsgCompleted(TfswsSocket(WParam));
    FD_OOB:
      {do nothing};
    FD_ACCEPT:
      wspListenCompleted(TfswsSocket(WParam));
    FD_CONNECT:
      wspConnectCompleted(TfswsSocket(WParam));
    FD_CLOSE:
      wspHangupDetected(TfswsSocket(WParam));
  End; {case}
End;
{--------}

Procedure TfsWinsockProtocol.wspSendMsgCompleted(aSocket: TfswsSocket);
Var
  SocketType: Integer;
  Conn: TfsWinsockConnection;
  dummy: Longint;
Begin
  wspWaitingForSendToUnblock := False;
  SocketType := 0;
  FSWSGetSocketOption(aSocket, SOL_SOCKET, SO_TYPE, SocketType,
    sizeof(SocketType));
  If (SocketType = SOCK_STREAM) Then
    Begin
      Conn := wspGetConnForSocket(aSocket);
      If Assigned(Conn) Then
        Begin
          Conn.wscIsSending := False;
          While (Not Conn.wscIsSending) And Assigned(Conn.wscPacketHead) Do
            {try to send all outstanding packets}
            Conn.Send(Nil, 0, 0, dummy, True); {!!.06}
        End;
    End;
End;
{--------}

Procedure TfsWinsockProtocol.wspReceiveCompleted(aSocket: TfswsSocket);
Var
  SocketType: Integer;
Begin
  SocketType := 0;
  FSWSGetSocketOption(aSocket, SOL_SOCKET, SO_TYPE, SocketType, sizeof(SocketType));
  If (SocketType = SOCK_STREAM) Then
    wspReceiveMsgCompleted(aSocket)
  Else If (SocketType = SOCK_DGRAM) Then
    wspReceiveDatagramCompleted(aSocket);
End;
{--------}

Procedure TfsWinsockProtocol.wspReceiveDatagramCompleted(aSocket: TfswsSocket);
Var
  RemNetName: TffNetName;
  BytesAvail: Longint;
  BytesRead: Integer;
  Error: Integer;
  SockAddrLen: Integer;
  SockAddr: TfswsSockAddr;
Begin
  Error := WinsockRoutines.ioctlsocket(aSocket, FIONREAD, BytesAvail);
  If (Error <> SOCKET_ERROR) And (BytesAvail > 0) Then
    Begin
      FillChar(SockAddr, sizeof(SockAddr), 0);
      If (Family = wfTCP) Then
        Begin
          SockAddrLen := sizeof(TfswsSockAddrIn);
        End
      Else {Family <> wfTCP}
        Begin
          SockAddrLen := sizeof(TfswsSockAddrIPX);
        End;
      BytesRead := WinsockRoutines.recvfrom(aSocket,
        wspRcvDGBuffer^,
        fsc_MaxDatagramSize,
        0,
        SockAddr,
        SockAddrLen);
      If (BytesRead <> SOCKET_ERROR) Then
        Begin
          {get our user to process the data}
          If (Family = wfTCP) Then
            Begin
              RemNetName := FSWSCvtAddrToStr(SockAddr.TCP.sin_addr);
            End
          Else {Family <> wfTCP}
            Begin
              With SockAddr.IPX Do
                RemNetName :=
                  FSWSCvtIPXAddrToStr(sipx_netnum, sipx_nodenum);
            End;
          cpDoReceiveDatagram(RemNetName, wspRcvDGBuffer, BytesRead);
        End;
    End;
End;
{--------}

Procedure TfsWinsockProtocol.wspReceiveMsgCompleted(aSocket: TfswsSocket);
Var
  BytesAvail: Longint;
  BytesRead: Integer;
  Conn: TfsWinsockConnection;
  Error: Integer;
  MsgLen: Integer;
  Parsing: boolean;
Begin
  Error := WinsockRoutines.ioctlsocket(aSocket, FIONREAD, BytesAvail);
  If (Error <> SOCKET_ERROR) And (BytesAvail > 0) Then
    Begin
      Conn := wspGetConnForSocket(aSocket);
      If assigned(Conn) Then
        With Conn Do
          Begin
            {read everything we can}
            BytesRead := WinsockRoutines.recv(aSocket,
              RcvBuffer^[RcvBufferOffset],
              fsc_MaxWinsockMsgSize - RcvBufferOffset,
              0);
            If (BytesRead <> SOCKET_ERROR) Then
              Begin
                {calculate the number of valid bytes in our receive buffer}
                RcvBufferOffset := RcvBufferOffset + BytesRead;
                Parsing := True;
                While Parsing Do
                  Begin
                    Parsing := False;
                    {discard check connection (keepalive) messages now, we may
                     have real messages piggybacking one}
                    While (RcvBufferOffset >= sizeof(Longint)) And
                      (PLongint(RcvBuffer)^ = fsnmCheckConnection) Do
                      Begin
                        {move the remainder of the received data up by 4 bytes}
                        RcvBufferOffset := RcvBufferOffset - sizeof(Longint);
                        If (RcvBufferOffset > 0) Then
                          Move(RcvBuffer^[sizeof(Longint)], RcvBuffer^[0], RcvBufferOffset);
                        cpGotCheckConnection(Conn);
                        Parsing := True;
                      End; { while }
                    {if we have something left..., and enough of it...}
                    If (RcvBufferOffset >= fsc_NetMsgHeaderSize) Then
                      Begin
                        MsgLen := PfsnmHeader(RcvBuffer)^.nmhMsgLen;
                        If (RcvBufferOffset >= MsgLen) Then
                          Begin
                            {get our ancestor to process the data}
                            If cpDoReceiveMsg(Conn, RcvBuffer, MsgLen) Then
                              Begin
                                {remove the message}
                                RcvBufferOffset := RcvBufferOffset - MsgLen;
                                If (RcvBufferOffset > 0) Then
                                  Move(RcvBuffer^[MsgLen], RcvBuffer^[0], RcvBufferOffset);
                                Parsing := True;
                              End;
                          End;
                      End; { if }
                  End; { while }
              End; { if }
          End { with }
      Else
        LogStrFmt('Could not find connection for socket %d', [aSocket]);
    End; { if }
End;
{--------}

Procedure TfsWinsockProtocol.wspWaitForConnect(aTimeOut: Integer);
Var
  T: TfsTimer;
Begin
  SetTimer(T, aTimeOut);
  While wspWaitingForConnect And (Not HasTimerExpired(T)) Do
    Begin
      Breathe;
    End;
End;
{--------}

Function TfsWinsockProtocol.wspWaitForSendToUnblock: Boolean;
{ Rewritten !!.06}
Var
  UnblockTimer: TfsTimer;
Begin
  wspWaitingForSendToUnblock := True;
  SetTimer(UnblockTimer, fsc_UnblockWait);
  Repeat
    Breathe;
    Result := HasTimerExpired(UnblockTimer);
  Until (Not wspWaitingForSendToUnblock) Or Result;
End;
{--------}

Procedure TfsWinsockProtocol.wspWSAEventCompleted(Var WSMsg: TMessage);
Begin
  With WSMsg Do
    Begin
      If (Msg = fswscEventComplete) Then
        Begin
          wspProcessCompletedWSACall(WParam, LParam);
          Result := 0;
        End
      Else If (Msg = WM_TIMER) Then
        Begin
          cpTimerTick;
        End
      Else
        Result := DefWindowProc(FNotifyWindow, Msg, WParam, LParam);
    End;
End;
{====================================================================}

{===TfsTCPIPProtocol=================================================}

Constructor TfsTCPIPProtocol.Create(Const aName: TffNetAddress;
  aCSType: TfsClientServerType);
Begin
  Inherited Create(aName, aCSType);
  Family := wfTCP;
End;
{--------}

Class Function TfsTCPIPProtocol.GetProtocolName: String;
Begin
  Result := 'TCP/IP';
End;
{--------}

Class Function TfsTCPIPProtocol.Supported: boolean;
Begin
  If FSWSInstalled Then
    Result := wfTCP In fswsFamiliesInstalled
  Else
    Result := False;
End;
{====================================================================}

{===TfsIPXSPXProtocol================================================}

Constructor TfsIPXSPXProtocol.Create(Const aName: TffNetAddress;
  aCSType: TfsClientServerType);
Begin
  Inherited Create(aName, aCSType);
  Family := wfIPX;
End;
{--------}

Class Function TfsIPXSPXProtocol.GetProtocolName: String;
Begin
  Result := 'IPX/SPX';
End;
{--------}

Class Function TfsIPXSPXProtocol.Supported: boolean;
Begin
  If FSWSInstalled Then
    Result := wfIPX In fswsFamiliesInstalled
  Else
    Result := False;
End;
{====================================================================}

{===Helper routines for single user==================================}
Type
  PffSUEnumData = ^TffSUEnumData;
  TffSUEnumData = Packed Record
    MsgID: Integer;
    OurWnd: HWND;
    SrvWnd: HWND;
  End;
  {====================================================================}

  {===TfsSingleUserConnection==========================================}

Constructor TfsSingleUserConnection.Create(aOwner: TfsBaseCommsProtocol;
  aRemoteName: TffNetAddress;
  aUs: HWND;
  aPartner: HWND);
Begin
  Inherited Create(aOwner, aRemoteName);
  FUs := aUs;
  FPartner := aPartner;
  GetMem(sucSendBuffer, fsc_MaxSingleUserMsgSize);
End;
{--------}

Destructor TfsSingleUserConnection.Destroy;
Var
  CDS: TCopyDataStruct;
  MsgResult: DWORD;
  WinError: TffWord32; {!!.12}
Begin
  { If we are deliberately hanging up then send a message to our partner. }
  If FHangingUp Then
    Begin
      If IsWindow(Partner) Then
        Begin
          CDS.dwData := fssumHangUp;
          CDS.cbData := 0;
          CDS.lpData := Nil;
          {Begin !!.12}
          If Not LongBool(SendMessageTimeout(FPartner, WM_COPYDATA, FClientID,
            Longint(@CDS),
            {$IFDEF RunningUnitTests}
            SMTO_ABORTIFHUNG,
            {$ELSE}
            SMTO_ABORTIFHUNG Or SMTO_BLOCK,
            {$ENDIF}
            fsc_SendMessageTimeout, MsgResult)) Or
            (MsgResult <> 0) Then
            Begin
              Sleep(fsc_SUPErrorTimeout);
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
              If Not LongBool(SendMessageTimeout(FPartner, WM_COPYDATA, FClientID,
                Longint(@CDS),
                {$IFDEF RunningUnitTests}
                SMTO_ABORTIFHUNG,
                {$ELSE}
                SMTO_ABORTIFHUNG Or SMTO_BLOCK,
                {$ENDIF}
                fsc_SendMessageTimeout, MsgResult)) Then
                Begin
                  WinError := GetLastError;
                  FOwner.LogStrFmt('Error %d sending message via SUP connection: %s',
                    [WinError, SysErrorMessage(WinError)]);
                End;
            End;
          {End !!.12}
        End;
    End;
  FreeMem(sucSendBuffer, fsc_MaxSingleUserMsgSize);
  Inherited Destroy;
End;
{--------}

Procedure TfsSingleUserConnection.AddToList(List: TFSNormalList);
Var
  T: TfsIntListItem;
  {$IFNDEF WIN32}
  tmpLongInt: Longint;
  {$ENDIF}
Begin {add a list entry to allow partner hwnd lookups}
  {$IFDEF WIN32}
  T := TfsIntListItem.Create(FPartner);
  {$ELSE}
  { The 16-bit HWND is a Word.  Cast it to a longInt so that
    our TffIntList comparison will work. }
  tmpLongInt := FPartner;
  T := TfsIntListItem.Create(tmpLongInt);
  {$ENDIF}
  T.ExtraData := Self;
  List.Insert(T);
End;
{--------}

Class Function TfsSingleUserProtocol.GetProtocolName: String;
Begin
  Result := 'Single';
End;
{--------}

Procedure TfsSingleUserConnection.RemoveFromList(List: TFSNormalList);
Begin
  List.Delete(FPartner);
End;
{--------}

Procedure TfsSingleUserConnection.Send(aData: PffByteArray;
  aDataLen: Longint;
  aConnLock: Boolean); {!!.06}
Var
  CDS: TCopyDataStruct;
  MsgResult: DWORD;
  WinError: TffWord32; {!!.05}
Begin
  If IsWindow(Partner) Then
    Begin
      If aConnLock Then {!!.06}
        HangupLock; {!!.05}
      Try {!!.05}
        If (aDataLen <> 0) Then
          Begin
            Move(aData^, sucSendBuffer^, aDataLen);
            Owner.cpCodeMessage(Self, sucSendBuffer, aDataLen);
            CDS.lpData := sucSendBuffer;
            CDS.cbData := aDataLen;
          End
        Else
          Begin
            CDS.lpData := Nil;
            CDS.cbData := 0;
          End;
        CDS.dwData := fssumDataMsg;
        {Begin !!.05}
        If Not LongBool(SendMessageTimeout(FPartner, WM_COPYDATA, FClientID,
          Longint(@CDS),
          {$IFDEF RunningUnitTests}
          SMTO_ABORTIFHUNG,
          {$ELSE}
          SMTO_ABORTIFHUNG Or SMTO_BLOCK,
          {$ENDIF}
          fsc_SendMessageTimeout, MsgResult)) Or
          (MsgResult <> 0) Then
          Begin
            {Begin !!.06}
            Sleep(fsc_SUPErrorTimeout);
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
            If Not LongBool(SendMessageTimeout(FPartner, WM_COPYDATA, FClientID,
              Longint(@CDS),
              {$IFDEF RunningUnitTests}
              SMTO_ABORTIFHUNG,
              {$ELSE}
              SMTO_ABORTIFHUNG Or SMTO_BLOCK,
              {$ENDIF}
              fsc_SendMessageTimeout, MsgResult)) Then
              Begin
                WinError := GetLastError;
                FOwner.LogStrFmt('Error %d sending message via SUP connection: %s',
                  [WinError, SysErrorMessage(WinError)]);
              End;
            {End !!.05}
          End;
      Finally {!!.05}
        If aConnLock Then {!!.06}
          HangupUnlock; {!!.05}
      End; {!!.05}
    End;
End;
{====================================================================}

{===TfsSingleUserProtocol============================================}

Constructor TfsSingleUserProtocol.Create(Const aName: TffNetAddress;
  aCSType: TfsClientServerType);
Begin
  Inherited Create(aName, aCSType);
  FMaxNetMsgSize := fsc_MaxSingleUserMsgSize;
  { Create a new Windows message. }
  supMsgID := RegisterWindowMessage('FSSQL1SingleUser');
  supPostMsgID := RegisterWindowMessage('FSSQL1SingleUserPostMessage');
End;
{--------}

Function TfsSingleUserProtocol.Call(Const aServerName: TffNetName;
  Var aClientID: TffClientID;
  Const timeout: Longint): TffResult;
Var
  Conn: TfsSingleUserConnection;
  SUED: TffSUEnumData;
Begin

  Result := DBIERR_NONE;

  {servers don't call}
  If (CSType = csServer) Then
    Raise EfsCommsException.CreateNoData(fsStrResGeneral, fserrCommsCannotCall);
  {assume failure}

  {enumerate the top-level windows, looking for a server}
  SUED.MsgID := supMsgID;
  SUED.OurWnd := FNotifyWindow;
  SUED.SrvWnd := 0;

  { Create a connection object with the assumption we find a server. }
  Conn := TfsSingleUserConnection.Create(Self, '', FNotifyWindow, SUED.SrvWnd);
  Conn.ClientID := Conn.Handle;

  SUED.SrvWnd := supFindPartner(Conn.ClientID, timeout);

  {did we find one?}
  If (SUED.SrvWnd <> 0) Then
    Begin
      Conn.Partner := SUED.SrvWnd;
      cpAddConnection(Conn);
      aClientID := Conn.ClientID;
    End
  Else
    Begin
      Conn.Free;
      Result := DBIERR_SERVERNOTFOUND;
    End;
End;
{--------}

Procedure TfsSingleUserProtocol.cpPerformStartUp;
Begin
  {create our Window}
  If Not cpCreateNotifyWindow Then
    Begin
      LogStr('Could not create notification window.');
      Raise EfsCommsException.CreateNoData(fsStrResGeneral, fserrCommsNoWinRes);
    End;
End;
{--------}

Procedure TfsSingleUserProtocol.GetServerNames(aList: TStrings; Const timeout: Longint);
Begin
  If Not assigned(aList) Then
    Exit;

  aList.Clear;
  aList.Add(fsc_SingleUserServerName);
End;
{--------}

Procedure TfsSingleUserProtocol.HangUp(aConn: TfsConnection);
Begin
  cpDoHangUp(aConn);
  cpRemoveConnection(aConn.ClientID);
End;
{--------}

Procedure TfsSingleUserProtocol.Listen;
Begin
End;
{--------}

Procedure TfsSingleUserProtocol.ReceiveDatagram;
Begin
  If Not supReceivingDatagram Then
    supReceivingDatagram := True;
End;
{--------}

Procedure TfsSingleUserProtocol.SendDatagram(Const aName: TffNetName;
  aData: PffByteArray;
  aDataLen: Longint);
Begin
End;
{--------}

Function TfsSingleUserProtocol.SendMsg(aClientID: TffClientID;
  aData: PffByteArray;
  aDataLen: Longint;
  aConnLock: Boolean): TffResult; {!!.06}
Var
  Conn: TfsSingleUserConnection;
Begin
  Result := DBIERR_NONE;
  Conn := TfsSingleUserConnection(cpGetConnection(aClientID));
  If Assigned(Conn) Then
    Conn.Send(aData, aDataLen, aConnLock) {!!.06}
  Else
    Result := fserrConnectionLost;
End;
{--------}

Procedure TfsSingleUserProtocol.StopReceiveDatagram;
Begin
  If supReceivingDatagram Then
    supReceivingDatagram := False;
End;
{--------}

Function TfsSingleUserProtocol.cpCreateNotifyWindow: boolean;
Begin
  {$IFDEF DCC6OrLater} {!!.11}
  {$WARN SYMBOL_DEPRECATED OFF}
  {$ENDIF}
  FNotifyWindow := AllocateHWnd(supMsgReceived);
  {$IFDEF DCC6OrLater} {!!.11}
  {$WARN SYMBOL_DEPRECATED ON}
  {$ENDIF}
  Result := FNotifyWindow <> 0;
  If Result Then
    Begin
      {$IFDEF KALog}
      KALog.WriteStringFmt('SingleUser.cpCreateNotifyWindow: protocol %d',
        [Longint(Self)]);
      {$ENDIF}
      Windows.SetTimer(FNotifyWindow, 1, FKeepAliveInterval, Nil); {!!.05}
    End;
End;
{--------}

Procedure TfsSingleUserProtocol.supDataMsgReceived(Const aClientID: TffClientID;
  Const aCDS: TCopyDataStruct);
Var
  Conn: TfsSingleUserConnection;
Begin

  Conn := TfsSingleUserConnection(cpGetConnection(aClientID));
  {get our user to process the data}
  If assigned(Conn) Then
    cpDoReceiveMsg(Conn, aCDS.lpData, aCDS.cbData)
  Else
    LogStrFmt('Could not find connection for client %d', [aClientID]);
End;
{--------}

Function TfsSingleUserProtocol.supGetConnForPartner(aPartner: HWND): TfsSingleUserConnection;
Var
  Inx: Integer;
  T: TfsIntListItem;
Begin
  { If we are indexing connections then use the index to locate
    the connection. }
  If Assigned(cpIndexByOSConnector) Then
    Begin
      T := TfsIntListItem(cpIndexByOSConnector.Items[cpIndexByOSConnector.Index(aPartner)]);
      If T = Nil Then
        Result := Nil
      Else
        Result := T.ExtraData;
      Exit;
    End;
  For Inx := 0 To pred(cpConnList.Count) Do
    Begin
      Result := TfsSingleUserConnection(cpConnList[Inx]);
      If (Result.Partner = aPartner) Then
        Exit;
    End;
  Result := Nil;
End;
{--------}

Procedure TfsSingleUserProtocol.supHangupDetected(Const aClientID: TffClientID);
{Rewritten !!.06}
Var
  Conn: TfsSingleUserConnection;
Begin
  Conn := TfsSingleUserConnection(cpGetConnection(aClientID));
  If Conn <> Nil Then
    Begin
      Conn.HangingUp := False;
      HangUp(Conn);
    End;
End;
{--------}

Procedure TfsSingleUserProtocol.supListenCompleted(aClientID: TffClientID;
  Wnd: HWND);
Var
  Conn: TfsSingleUserConnection;
  WasAdded: boolean;
Begin
  {a listen event has been accepted, create a connection}
  WasAdded := False;
  Conn := Nil;
  Try
    { When we first create this connection, we don't have a clientID so
      we temporarily use the connection's handle.  There is also a temporary
      clientID on the client-side of things.
      When the client is given a real clientID, the temporary clientIDs on
      both client and server are replaced with the true clientID. }
    Conn := TfsSingleUserConnection.Create(Self, '', FNotifyWindow, Wnd);
    Conn.ClientID := aClientID;
    //    Conn.InitCode(0);                                                {Deleted !!.05}
    cpAddConnection(Conn);
    WasAdded := True;
    cpDoHeardCall(Conn.ClientID);
  Except
    If WasAdded Then
      cpRemoveConnection(Conn.ClientID);
    Raise;
  End; {try..except}
End;
{--------}

Procedure TfsSingleUserProtocol.supMsgReceived(Var SUMsg: TMessage);
Begin
  With SUMsg Do
    Begin
      If (Msg = supMsgID) Then
        Begin
          If (CSType = csServer) Then
            Begin
              Result := fssumCallServer {'FS'};
              supListenCompleted(WParam, LParam);
            End
          Else
            Result := 0;
        End
      Else If Msg = supPostMsgID Then
        Begin
          If CSType = csServer Then
            Begin
              { Client is trying to initiate conversation with us.  Send back
                a reply. }
              If LParam = fssumCallServer {'FS'} Then
                Begin
                  If IsWindow(WParam) Then
                    PostMessage(WParam, fsm_ServerReply, FNotifyWindow, fssumCallServer);
                End;
            End;
        End
      Else If Msg = fsm_ServerReply Then
        Begin
          If supPartner = 0 Then
            Begin
              If CSType = csClient Then
                Begin
                  If LParam = fssumCallServer {'FS'} Then
                    Begin
                      If IsWindow(WParam) Then
                        supPartner := WParam;
                    End;
                End;
            End;
        End
      Else If (Msg = WM_COPYDATA) Then
        Begin
          Case PCopyDataStruct(LParam)^.dwData Of
            fssumDataMsg: supDataMsgReceived(WParam, PCopyDataStruct(LParam)^);
            fssumHangUp: supHangUpDetected(WParam);
          End;
        End
      Else If (Msg = WM_TIMER) Then
        cpTimerTick
      Else
        Result := DefWindowProc(FNotifyWindow, Msg, WParam, LParam);
    End;
End;
{--------}

Function TfsSingleUserProtocol.supFindPartner(Const aClientID: TffClientID;
  Const timeout: Longint): HWND;

Var
  WaitUntil: Tffword32;
  MsgResult: DWORD;
  Msg: TMsg;
  StartTime: DWORD; {!!.05}
  WinError: TffWord32; {!!.05}
Begin
  supPartner := 0;
  PostMessage(HWND_BROADCAST, supPostMsgID, FNotifyWindow, fssumCallServer);
  WaitUntil := GetTickCount + DWORD(timeout);
  StartTime := GetTickCount; {!!.05}
  While (GetTickCount < WaitUntil) And (supPartner = 0) Do
    Begin
      If PeekMessage(Msg, FNotifyWindow, fsm_ServerReply,
        fsm_ServerReply, PM_REMOVE) Then
        Begin
          TranslateMessage(Msg);
          DispatchMessage(Msg);
          {Begin !!.05}
        End
      Else If GetTickCount - StartTime > fsc_ConnectRetryTimeout Then
        Begin
          PostMessage(HWND_BROADCAST, supPostMsgID, FNotifyWindow, fssumCallServer);
          StartTime := GetTickCount;
        End;
      {End !!.05}
      If supPartner = 0 Then
        Breathe;
    End;
  Result := supPartner;
  If Result <> 0 Then
    Begin
      If LongBool(SendMessageTimeout(Result, supMsgID, aClientID, FNotifyWindow,
        SMTO_ABORTIFHUNG Or SMTO_BLOCK,
        timeout, MsgResult)) Then
        Begin
          If MsgResult <> fssumCallServer {FS} Then
            Begin
              {Begin !!.05}
              If LongBool(SendMessageTimeout(Result, supMsgID, aClientID, FNotifyWindow,
                SMTO_ABORTIFHUNG Or SMTO_BLOCK,
                timeout, MsgResult)) Then
                If MsgResult <> fssumCallServer {FS} Then
                  Begin
                    WinError := GetLastError;
                    LogStrFmt('Error %d when finding SUP partner: %s',
                      [WinError, SysErrorMessage(WinError)]);
                    Result := 0;
                  End; { if }
            End; { if }
          {End !!.05}
        End
      Else
        Result := 0;
    End;
End;
{====================================================================}

{$IFDEF KALog}
Initialization
  KALog := TFSNormalEventLog.Create(Nil);
  KALog.FileName := ChangeFileExt(ParamStr(0), '') + 'KAL.lg';
  KALog.Enabled := True;

Finalization
  KALog.Free;
  {$ENDIF}

End.

