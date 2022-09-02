{*********************************************************}
{* FlashFiler: TffLegacyTransport                        *}
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

unit fflllgcy;

interface

uses
  dialogs,
  classes,
  messages,
  windows,
  ffdtmsgq,
  ffllbase,
  ffllcomp,
  fflleng,
  ffllcomm,
  fflllog,
  ffllprot,
  ffllreq,
  ffnetmsg;

type

  TffLegacyTransportThread = class;  { foward declaration }

  {The purpose of this class is to give us a way of re-using the existing
   protocols until better protocols can be written.  It instantiates a protocol
   object based upon the specified protocol type.

   If Listen := False then the transport is in Send mode and is used to send
   requests to a remote listener.   A sender thread is used to process sending
   of requests and receiving of replies.  A little hoop to jump through: The
   transport must do two things: wait for requests to be submitted to its
   UnsentRequestQueue and allow the legacy protocol to listen for messages.

   If Listen := True then the transport is in Listen mode and starts a thread
   for receiving of requests.  When a request is received, it processes the
   request via a worker thread.
  }
  TffLegacyTransport = class(TffThreadedTransport)
  protected {private}
    { See comments in TFFBaseTransport for _* fields }

    FMsgQueue : TffDataMessageQueue;
      {-When in Listen mode, used to hold partially received messages. }

    FLostConnWindow : HWND;
      {-Used to receive lost connection events from the protocol thread. }

    FProtocol : TffBaseCommsProtocol;
      {-The protocol instantiated by this transport. }

    FProtocolType : TffProtocolType;
    _FProtocolType : TffProtocolType;
      {-The enumeration describing the protocol instantiated by this transport. }

    FSendBuffer : PffnmHeader;
      {-The buffer used to send messages to the remote server. }

    FServerLocalName : TffNetName;
      {-This is the local name portion of FServerName.  For example,
        if we are trying to reach 'prod1@127.0.0.1' then the server's local
        name is 'prod1' and the server's address is '127.0.0.1' }

    FServerAddress : TffNetName;
      {-This is FServerName minus the local name of the server.  For example,
        if we are trying to reach 'prod1@127.0.0.1' then the server's local
        name is 'prod1' and FServerAddress will be '127.0.0.1' }

    FTransportThread : TffLegacyTransportThread;
      {-If in Listen mode, this is the thread that is listening.  If in Send
        mode, this is the thread that will be sending requests. }

  protected

    procedure btBeginUpdatePrim; override;

    procedure btEndUpdatePrim; override;

    function btGetConnectionID(const anIndex : Longint) : TffClientID; override;
      { Used to obtain the IDs of the protocol's connections.  Handy for when
        a server wants to send messages to one or more client connections. }

    procedure btInternalReply(msgID          : Longint;
                              errorCode      : TffResult;
                              replyData      : pointer;
                              replyDataLen   : Longint); override;
      { This method is called from TffBaseTransport.Reply.  It sends the
        reply to the client. }

    procedure btSetRespondToBroadcasts(const respond : boolean); override;
      { This implementation makes sure the legacy protocol is configured
        properly if RespondToBroadcasts is changed while the transport is
        active. }

    procedure btSetServername(const aServername : string); override;   {!!.10}
      {-This method sets the server name.  The implementation for this class
        does not perform any validation.  Transport subclasses should perform
        their own validation. }

    procedure lcLog(const aMsg : string); override;
      { Use this method to write an error string to the event log. }

    procedure ltFreeMsg(msg : PffDataMessage); virtual;                {!!.01}

    function ltGetProtocol : TffProtocolType; virtual;
      { Used to get the legacy protocol. }

{Begin !!.01}
    procedure ltDoHangup(const aClientID : TffClientID);
      { Hangup processing. }
{End !!.01}

    procedure ltLostConnection(var aMsg : TMessage);
      { Message handler for lost connections window. }

    function ltMapProtocolToClass : TffCommsProtocolClass;
      { Maps the transport's protocol to its protocol class. }

    procedure lcSetEventLog(anEventLog : TffBaseLog); override;
      { Set the transport's event log.  This overridden method makes sure the
        protocol's EventLog property is kept up-to-date. }

    procedure lcSetLogEnabled(const aEnabled : Boolean); override;
      { This overridden method updates the logEnabled property of the
        TffBaseCommsProtocol instance created by this component. }

    procedure ltSetProtocol(aProtocol : TffProtocolType); virtual;
      { Used to set the legacy protocol. }

{Begin !!.05}
    procedure ltTerminateThread;
      { Terminate the transport thread if it is active. }
{End !!.05}

    procedure scInitialize; override;
      { Called when the transport is to initialize itself for operation.  This
        implementation creates and initializes the protocol and transport
        thread. }

    procedure scPrepareForShutdown; override;
      { This method is called when the transport is to prepare for shutdown. }

    procedure scShutdown; override;
      { This method is called when the transport is to shut down. }

    procedure scStartup; override;
      { This method is called when the transport is to start up. }

    procedure tpConnectionLost(aSender     : TObject;
                               aClientID : TffClientID);
      {-Called when the transport is sending and the remote server engine
        unexpectedly hangs up on the client. This method is called within the
        context of the transport thread. It sends a message to the
        transports lost connection window and the message is then processed
        by ltLostConnection. }

    procedure tpDatagramReceived(aSender  : TObject;
                           const aName    : TffNetName;
                                 aData    : PffByteArray;
                                 aDataLen : Longint);
      { Used to log receipt of broadcast requests in listening transport. }

    function tpGetCodeStart(const aClientID : TffClientID) : integer;
      { Used to obtain the starting encryption code for the specified client. }

    procedure tpHandleAddClient(aMsg : PffDataMessage);
      {-This method is called by a listening transport to process the adding
        of a new client. }

    procedure tpHandleNextRequest;
      {-This method is used to handle the next unsent request.  The request
        is moved from the unsent queue to the waiting for reply list. }

    procedure tpHandleRemoveClient(aMsg : PffDataMessage);
      {-This method is called by a listening transport to process the removal
        of an existing client. }

    procedure tpInternalRequest(aRequest : TffRequest;
                                timeout  : Longint;
                                aCookie  : HWND); override;
     {-Internal method for sending a request.  This implementation assigns the
       protocol's event log to the request and assigns the protocol's window
       handle to the value of aCookie. }

    function tpMsgReceived(aSender : TObject;
                           clientID : TffClientID;
                           msgData : PffByteArray;
                           msgDataLen : Longint) : boolean;
      {-This method is called when a request is received from a client or
        when a reply is received from a server. }

    procedure tpPrepareThread;
      { Prepares the legacy transport thread for work. }

    procedure tpProcessCallback(const aProcessCookie : Longint); virtual;
      { When in Listen mode, this method is called by the worker thread to
        process the request received from the client.
        This method stores information such as the clientID and requestID in
        threadvars so that it may be used when replying to the client.
        aProcessCookie is a pointer to the message received from the client.
        The method passes the message to the command handler.
       }

    procedure tpRemoteClientHangup(aSender     : TObject;
                                   aClientID : TffClientID);
      {-Called when the transport is listening and a) the client hangs up
        or b) the transport decides to hang up on the client. }

    function tpReplyReceived(aSender : TObject;
                             clientID : TffClientID;
                             replyData : PffByteArray;
                             replyDataLen : Longint) : boolean; virtual;
      { When sending, this method is called when the legacy protocol has
        received a reply from the server.  In this implementation the
        following occurs:
          1. If this reply is acknowledging the last message was received then
             the request is sitting in the pending list.  Find the request and
             put it in the Unsent Requests queue.

          2. If this reply is a full-fledged response to a complete message, do
             the following:

             a. Find the TffRequest.
             b. Place the reply data on the request.
             c. Remove the request from the WaitingForReply list.
             d. Call TffRequest.WakeUpThread.
       }

    function tpRequestReceived(aSender : TObject;
                               clientID : TffClientID;
                               requestData : PffByteArray;
                               requestDataLen : Longint) : boolean; virtual;
      { When listening, this method is called when the protocol thread has
        received a request from a client.  The transport then spawns a worker
        thread that performs the actual work. }

    function tpSendReply(msgID : Longint;
                         clientID : TffClientID;
                         requestID : Longint;
                         errorCode : TffResult;
                         replyData : pointer;
                         replyDataLen : Longint) : TffResult;
      { This method sends the actual reply to the client.  The reply is
        sent in context of the thread that processed the client's request,
        not the listening thread. }

    function tpSendRequest(aRequest : TffRequest) : TffResult;
      { Sends a request via the protocol to the remove server.  This method
        returns DBIERR_NONE if the request was successfully sent. }

    procedure tpShutdownProtocol;
      { This method is called when the protocol thread stops executing.
        It must be run in context of the protocol thread so that any
        thread-specific items (e.g., windows, timers) may be destroyed. }

    procedure tpStartProtocol;
      { This method is called when the protocol thread begins execution.
        It must be run in context of the protocol thread so that the
        window handle created by the protocol is associated with the
        thread. }

    procedure tpThreadTerminated(Sender : TObject);
      { This method is called when the transport thread terminates.
        The purpose of this handler is to detect the case where the
        thread terminates prematurely. }

  public

    constructor Create(aOwner : TComponent); override;
    destructor Destroy; override;

    function ConnectionCount : Longint; override;
      { Returns the number of established connections.  For a sender (i.e.,
        client), this will be the number of connections to the remote server.
        For a listener (i.e., server), this will be the number of
        connections establshed by remote clients. }

    function EstablishConnection(const aUserName : TffName;
                                       aPasswordHash : integer;
                                       aTimeout : Longint;
                                  var aClientID : TffClientID ) : TffResult; override;
      { Use this method to establish a connection with the server.  If the
        return code is DBIERR_NONE then aClientID will contain the clientID
        supplied by the server.  This clientID must be used in all subsequent
        requests to the server. }

    function GetName : string; override;
      { Returns the transport's name. }

    procedure GetServerNames(aList : TStrings; const timeout : Longint); override;
      { Returns the list of servers available via this transport. }

    function IsConnected : boolean; override;
      { Use this method to determine if the transport is connected to a remote
        server.  It is considered connected if a) the transport's State is
        ffesStarted and b) there is at least one established connection.
        If the transport has been started but no connections have been
        established then this method returns False. }

    procedure Request(transportID : Longint;
                      clientID : TffClientID;
                      msgID : Longint;
                      timeout : Longint;
                      requestData : pointer;
                      requestDataLen : Longint;
                      replyCallback : TffReplyCallback;
                      replyCookie : Longint); override;
      { When the transport is in Send mode, call this method in order to
        submit a request to the transport.

        Parameters are as follows:

        @param transportID - For use by future transports.
        @param clientID - The ID of the client submitting the request.  This
          must be the clientID originally supplied by the server or it may be
          zero for unsecured calls (e.g., initially asking for a connection
          to the server).
        @param msgID - The type of message being sent.
        @param timeout - The number of milliseconds in which a reply must be
          received from the server.
        @param requestData - Pointer to a data buffer containing the message
          data.
        @param requestDataLen - The length of requestData.
        @param replyCallback - The procedure to be called when the reply
          has been received from the server.
        @param replyCookie - Whatever the calling object wants it to be.  This
          parameter is supplied to the replyCallback.
       }
       
    function Supported : boolean; override;
      { Use this method to determine if the transport's current protocol is
        supported on the workstation.  Returns True if the protocol is
        supported otherwise returns False. }

    procedure TerminateConnection(const aClientID : TffClientID;
                                  const timeout : Longint); override;
      { Use this method to terminate a connection with the server.  aClientID
        is the clientID originally returned in the call to EstablishConnection.}

    procedure Work; override;
      { Based upon the transport's mode, this method tells the transport to
        perform some work:

        1. When in sending mode, sends requests and processes replies
        2. When in listening mode, listens for requests.

        This method should be structured such that it does a bit of work and
        then returns.  It is to be repeatedly called from the
        TffLegacyTransportThread.Execute method so that the Execute method
        may check the thread's Terminated property.
      }

  published

    property Protocol : TffProtocolType
       read ltGetProtocol
       write ltSetProtocol
       default ptRegistry;
      { The legacy protocol to be used by this transport.  Defaults to
        ptRegistry. }

  end;

  { In order to support sending and receiving of messages without blocking
    the client application or server, the legacy transport carries out
    sending and receiving of messages through an instance of this class.

    The thread is always created in suspended mode.  It is resumed by
    TffLegacyTransport.tpStartup.
  }
  TffLegacyTransportThread = class(TffThread)
  private
    FTransport : TffLegacyTransport;
      { The transport starting this thread. }
    FUnexpectedTermination : boolean;
      { Set to True if thread terminated by an exception. }
  protected

    procedure Execute; override;
      { This method repeatedly calls the transport's Work method.  Execute
        should be called only when the transport's Startup method is called. }

  public

    constructor Create(aTransport : TffLegacyTransport);
      { When creating a listener thread, the parent transport is identified.
        The thread is suspended. }

    property UnexpectedTermination : boolean read FUnexpectedTermination;

  end;

implementation

uses
  forms,
  sysutils,
  ffclcfg,
  ffllexcp,
  ffllthrd,
  ffllwsck,
  ffsrbase,
  ffsrbde;

{$I ffconst.inc}

const
  ffc_ThreadDoneTimeout = 1000;  { # milliseconds to wait for send/listen
                                   thread to shutdown }
  ffc_ThreadStartTimeout = 2000; { # milliseconds to wait for the transport
                                   thread to start }
  ffc_SingleUserServerName = 'Local';

  { Prefixes for logging requests. }
  ffc_Post     = 'Post';
  ffc_PostWait = 'Post&Wait';
  ffc_Request  = 'Req';

  { Error messages. }
  ffc_ErrMsgType = 'Bad msg type %d, Clnt %d, Msg %d';

{===TffLegacyTransport===============================================}

threadvar
  ffitvClientID : TffClientID;
  ffitvRequestID : Longint;

constructor TffLegacyTransport.Create(aOwner : TComponent);
begin
  inherited Create(aOwner);
  FLogEnabled := False;
  {$IFDEF DCC6OrLater}
    {$WARN SYMBOL_DEPRECATED OFF}
  {$ENDIF}
  FLostConnWindow := AllocateHWND(ltLostConnection);
  {$IFDEF DCC6OrLater}
    {$WARN SYMBOL_DEPRECATED ON}
  {$ENDIF}
  FProtocol := nil;
  FProtocolType := ptRegistry;
  FTransportThread := nil;
  FSendBuffer := nil;
end;
{--------}
destructor TffLegacyTransport.Destroy;
begin
  FFNotifyDependents(ffn_Destroy);
  ltTerminateThread;                                                   {!!.05}
  {$IFDEF DCC6OrLater}
    {$WARN SYMBOL_DEPRECATED OFF}
  {$ENDIF}
  DeallocateHWnd(FLostConnWindow);
  {$IFDEF DCC6OrLater}
    {$WARN SYMBOL_DEPRECATED ON}
  {$ENDIF}
  inherited Destroy;
end;
{--------}
procedure TffLegacyTransport.btBeginUpdatePrim;
begin
  inherited btBeginUpdatePrim;

  { Set the _* fields to match their counterparts }
  _FProtocolType  := FProtocolType;
end;
{--------}
procedure TffLegacyTransport.btEndUpdatePrim;
begin
  { Update the fields with their _* counterparts. }
  { All property write methods are required to check that the new value }
  { does not match the old value! }
  Protocol      := _FProtocolType;
  if Protocol = ptSingleUser then
    _FServerName := ServerName;

  if assigned(FProtocol) then
    FProtocol.LogEnabled := _FLogEnabled;

  inherited btEndUpdatePrim;
end;
{--------}
function TffLegacyTransport.btGetConnectionID(const anIndex : Longint) : TffClientID;
begin
  Result := FProtocol.ConnectionIDs[anIndex];
end;
{--------}
function TffLegacyTransport.ConnectionCount : Longint;
begin
  if assigned(FProtocol) then
    Result := FProtocol.ConnectionCount
  else
    Result := 0;
end;
{--------}
function TffLegacyTransport.EstablishConnection
                              (const aUserName : TffName;
                                     aPasswordHash : integer;
                                     aTimeout : Longint;
                                 var aClientID : TffClientID ) : TffResult;
var
  aRequest : TffRequest;
  AttachReq : TffnmAttachServerReq;
  CallReq : TffnmCallServerReq;
  PAttachRpy : PffnmAttachServerRpy;
begin

  btCheckSender;
  btCheckServerName;
  scCheckStarted;

  { Have the protocol contact the server.
    Note that we will get back a temporary clientID from the protocol.
    This temporary ID will be replaced once we have a good one from
    the server. }
  CallReq.ServerName := FServerAddress;
  aRequest := TffRequest.Create(aClientID, ffnmCallServer, @CallReq,
                                sizeOf(CallReq), aTimeout, ffrmReplyExpected);
  try
    tpInternalRequest(aRequest, aTimeout, FProtocol.NotifyWindow);
    Result := aRequest.ErrorCode;
    if Result <> DBIERR_NONE then begin
      aRequest.Free;
      exit;
    end;
  except
    { If an exception occurs then the transport thread is responsible for
      freeing the request. }
    on E:EffException do begin
      Result := E.ErrorCode;
      exit;
    end else
      raise;
  end;

  { Connection successful.  Get the clientID from the server. }
  Assert(assigned(aRequest.ReplyData));
  aClientID := PffnmCallServerRpy(aRequest.ReplyData)^.ClientID;
  aRequest.Free;

  { Obtain permission to attach a client. }
  with AttachReq do begin
    ClientName := aUserName;
    UserID := aUserName;
    Timeout := aTimeout;
    ClientVersion := ffVersionNumber;
  end;

  { Submit the request. }
  aRequest := TffRequest.Create(aClientID, ffnmAttachServer, @AttachReq,
                                sizeOf(AttachReq), aTimeout, ffrmReplyExpected);
  try
    tpInternalRequest(aRequest, aTimeout, FProtocol.NotifyWindow);
  except
    { If an exception occurs then the transport thread is responsible for
      freeing the request. }
    on E:EffException do begin
      Result := E.ErrorCode;
      exit;
    end else
      raise;
  end;

  { Evaluate the reply. }
  Result := aRequest.ErrorCode;
  if Result = DBIERR_NONE then begin
    pAttachRpy := PffnmAttachServerRpy(aRequest.ReplyData);
    with pAttachRpy^ do begin
      { Have the protocol update our connection's clientID. }
      FProtocol.UpdateClientID(aClientID, ClientID);
      aClientID := ClientID;

      if IsSecure then
        FProtocol.InitCode(aClientID, Code xor Longint(aPasswordHash))
      else
        FProtocol.InitCode(aClientID, Code);

      {Update the protocol's keep alive information. }
      FProtocol.KeepAliveInterval := KAIntvl;
      FProtocol.KeepAliveRetries := KARetries;
      FProtocol.LastMsgInterval := LastMsgIntvl;
      FProtocol.ResetKeepAliveTimer;                                   {!!.06}

      { Check secure communications...}
      aRequest.Free;
      aRequest := TffRequest.Create(aClientID, ffnmCheckSecureComms, nil, 0,
                                    aTimeout, ffrmReplyExpected);
      try
        tpInternalRequest(aRequest, aTimeout, FProtocol.NotifyWindow);
      except
        { If an exception occurs then the transport thread is responsible for
          freeing the request. }
        on E:EffException do begin
          Result := E.ErrorCode;
          exit;
        end else
          raise;
      end;
      Result := aRequest.ErrorCode;
      if Result <> DBIERR_NONE then begin
        { The password is bogus. }
        FProtocol.HangUpByClientID(aClientID);
        if Result <> fferrReplyTimeout then                            {!!.06}
          Result := DBIERR_INVALIDUSRPASS;                             {!!.06}
      end;  { if }
    end;  { with }
  end else
    { Server rejected us.  Tell protocol to get rid of the connection. }
    FProtocol.HangUpByClientID(aClientID);

{Begin !!.06}
  { If timed out waiting for a reply then we need to remove this request from
    the waiting for reply queue. }
  if Result = fferrReplyTimeout then
    with FWaitingForReplyList.BeginWrite do
      try
        Delete(Longint(aRequest));
      finally
        EndWrite;
      end;
{End !!.06}

  if assigned(aRequest) then
    aRequest.Free;

end;
{Begin !!.01}
{--------}
procedure TffLegacyTransport.ltFreeMsg(msg : PffDataMessage);
begin
  if Msg^.dmDataLen > 0 then
    FFFreeMem(Msg^.dmData, Msg^.dmDataLen);
  FFFreeMem(Msg, SizeOf(TffDataMessage));
end;
{End !!.01}
{--------}
procedure TffLegacyTransport.ltLostConnection(var aMsg : TMessage);
begin
  { Lost connection message? Event handler declared? }
  if (aMsg.Msg = ffm_LostConnection) then begin                        {!!.01}
    if assigned(FOnConnectionLost) then begin
      FOnConnectionLost(Self, aMsg.wParam)
    end else
      if csDesigning in ComponentState then
        AutoConnectionLost(Self, aMsg.WParam);
  end                                                                  {!!.01}
  else if aMsg.Msg = WM_QUERYENDSESSION then                           {!!.01}
    aMsg.Result := 1                                                   {!!.01}
  else                                                                 {!!.01}
    Dispatch(aMsg);                                                    {!!.01}
end;
{--------}
function TffLegacyTransport.ltMapProtocolToClass : TffCommsProtocolClass;
var
  protName : TffShStr;
begin
  if (FProtocolType <> ptRegistry) then begin
    case Protocol of
      ptSingleUser : Result := TffSingleUserProtocol;
      ptTCPIP      : Result := TffTCPIPProtocol;
      ptIPXSPX     : Result := TffIPXSPXProtocol;
    else
      Result := TffSingleUserProtocol;
    end;
  end
  else
    FFClientConfigReadProtocol(Result, protName);
end;
{--------}
function TffLegacyTransport.GetName : string;
begin
  Result := ltMapProtocolToClass.GetProtocolName;
end;
{--------}
procedure TffLegacyTransport.GetServerNames(aList : TStrings;
                                            const timeout : Longint);
var
  OldServerNameRequired : boolean;
  OldState : TffState;
begin

  if not assigned(aList) then
    Exit;

  OldState := scState;
  OldServerNameRequired := false;

  { If the transport has not been started, temporarily start the transport. }
  if OldState <> ffesStarted then begin
    OldServerNameRequired := FServerNameRequired;
    FServerNameRequired := false;
    State := ffesStarted;
  end;

  { Note: This is done outside the transport's sender thread.  It should
    not interfere with the sender thread's normal operation. }
{Begin !!.05}
  if Assigned(FProtocol) then
    FProtocol.GetServerNames(aList, timeout)
  else
    aList.Clear;
{End !!.05}

  { Restore transport to original state. }
  if OldState <> ffesStarted then begin
    State := OldState;
    FServerNameRequired := OldServerNameRequired;
  end;

end;
{--------}
procedure TffLegacyTransport.btInternalReply(msgID          : Longint;
                                             errorCode      : TffResult;
                                             replyData      : pointer;
                                             replyDataLen   : Longint);
begin
  inherited btInternalReply(msgID, errorCode, replyData, replyDataLen);
  tpSendReply(msgID, ffitvClientID, ffitvRequestID, errorCode,
              replyData, replyDataLen);
end;
{--------}
function TffLegacyTransport.IsConnected : boolean;
begin
  { We are connected if we are in the right state and there is at least one
    established connection. }
  Result := (scState = ffesStarted) and (FProtocol.ConnectionCount > 0);
end;
{--------}
procedure TffLegacyTransport.lcLog(const aMsg : string);
begin
  if FLogEnabled and assigned(FEventLog) and (fftpLogErrors in FLogOptions) then
    FEventLog.WriteString(aMsg);
end;
{--------}
procedure TffLegacyTransport.lcSetLogEnabled(const aEnabled : Boolean);
begin
  inherited lcSetLogEnabled(aEnabled);
  if (UpdateCount = 0) and assigned(FProtocol) then
    FProtocol.LogEnabled := aEnabled;
end;
{--------}
function TffLegacyTransport.ltGetProtocol : TffProtocolType;
begin
  Result := FProtocolType;
end;
{Begin !!.01}
{--------}
type
  ProtocolCracker = class(TffBaseCommsProtocol);

procedure TffLegacyTransport.ltDoHangup(const aClientID : TffClientID);
{Rewritten !!.05}
var
  conn : TffConnection;
  errorCode : TffResult;
begin
  conn := ProtocolCracker(FProtocol).cpGetConnection(aClientID);
  if Assigned(conn) and (not conn.HangupDone) then begin
    if assigned(FOnRemoveClient) then
      FOnRemoveClient(Self, aClientID, errorCode)
    else
      { No handler assigned.  Log an error. }
      lcLogFmt('No RemoveClientHandler for transport %d', [GetName]);
    conn.HangupDone := True;
  end;
end;
{End !!.01}
{--------}
procedure TffLegacyTransport.ltSetProtocol(aProtocol : TffProtocolType);
begin
  if (UpdateCount > 0) then
    _FProtocolType := aProtocol
  else begin
    {Check to make sure the new property is different.}
    if FProtocolType = aProtocol then Exit;

    {Note: If you ever remove the following requirement, update the Supported
           test at the end of this routine. }
    scCheckInactive;
    FProtocolType := aProtocol;
    if FProtocolType = ptSingleUser then begin
      FServerNameRequired := False;
      ServerName := ffc_SingleUserServerName;
    end;

    { Is this protocol supported on the workstation? }
    if Supported then
      scState := ffesInactive
    else
      scState := ffesUnsupported;
  end;
end;
{Begin !!.05}
{--------}
procedure TffLegacyTransport.ltTerminateThread;
begin
  if assigned(FTransportThread) then begin
    FTransportThread.Terminate;
    FTransportThread.WaitForEx(5000);
    FTransportThread.Free;
    FTransportThread := nil;
  end;
end;
{End !!.05}
{--------}
procedure TffLegacyTransport.btSetRespondToBroadcasts(const respond : boolean);
var
  OldValue : boolean;
begin
  OldValue := FRespondToBroadcasts;
  inherited btSetRespondToBroadcasts(respond);
  if (OldValue <> FRespondToBroadcasts) and
     (scState = ffesStarted) then
    if respond then
      FProtocol.ReceiveDatagram
    else
      FProtocol.StopReceiveDatagram;
end;
{--------}
procedure TffLegacyTransport.btSetServerName(const aServerName : string); {!!.10}
begin
  inherited btSetServerName(aServerName);
  FFSplitNetAddress(aServerName, FServerLocalName, FServerAddress);
end;
{--------}
procedure TffLegacyTransport.lcSetEventLog(anEventLog : TffBaseLog);
begin
  inherited lcSetEventLog(anEventLog);
  if assigned(FProtocol) then
    FProtocol.EventLog := anEventLog;
end;
{--------}
procedure TffLegacyTransport.Request(transportID : Longint;
                                     clientID : TffClientID;
                                     msgID : Longint;
                                     timeout : Longint;
                                     requestData : pointer;
                                     requestDataLen : Longint;
                                     replyCallback : TffReplyCallback;
                                     replyCookie : Longint);
var
  aRequest : TffRequest;

begin
  scCheckStarted;
  aRequest := TffRequest.Create(clientID, msgID, requestData, requestDataLen,
                                timeout, ffrmReplyExpected);
  tpInternalRequest(aRequest, timeout, FProtocol.NotifyWindow);
  if assigned(replyCallback) then
    replyCallback(aRequest.ReplyMsgID, aRequest.ErrorCode,
                  aRequest.ReplyData, aRequest.ReplyDataLen,
                  replyCookie);
  if not aRequest.Aborted then
    aRequest.Free
  else
    with aRequest do
      tpLogReqMisc(format(ffc_ReqAborted,[Longint(aRequest), ClientID,
                                          ErrorCode, Timeout]));
end;
{--------}
procedure TffLegacyTransport.scInitialize;
var
  protClass : TffCommsProtocolClass;
  whichSideOfTheCoin : TffClientServerType;
begin

  { Make sure the old protocol is freed. }
  if assigned(FProtocol) then begin
    if FProtocol.IsStarted then begin
      scPrepareForShutdown;
      scShutdown;
    end;
  end;

  { If we are in sending mode then verify we have a target server. }
  if FMode = fftmSend then
    btCheckServerName;

  { Figure out which type of protocol we are to instantiate. }
  protClass := ltMapProtocolToClass;

  if assigned(FMsgQueue) then begin
    FMsgQueue.Free;
    FMsgQueue := nil;
  end;

  { Figure out the protocol's mode. }
  if FMode = fftmListen then
    whichSideOfTheCoin := csServer
  else
    whichSideOfTheCoin := csClient;

  FMsgQueue := TffDataMessageQueue.Create;

  FProtocol := protClass.Create(FServerName, whichSideOfTheCoin);
  if FMode = fftmListen then
    with FProtocol do begin
      OnConnectionLost := tpRemoteClientHangup;
      OnHangUp := tpRemoteClientHangup;
      OnHeardCall := nil;
      OnReceiveDatagram := tpDatagramReceived;
      OnReceiveMsg := tpMsgReceived;
    end
  else
    with FProtocol do begin
      OnConnectionLost := tpConnectionLost;
      OnHangUp := nil;
      OnHeardCall := nil;
      OnReceiveDatagram := nil;
      OnReceiveMsg := tpMsgReceived;
    end;

  FProtocol.EventLog := FEventLog;                                     {!!.01}
  FProtocol.LogEnabled := FLogEnabled;

  { If we are listening then get our servername from the protocol. }
  if FMode = fftmListen then
    FServerName := FProtocol.NetName;

  FFGetMem(FSendBuffer, FProtocol.MaxNetMsgSize);

  tpPrepareThread;

end;
{--------}
procedure TffLegacyTransport.scPrepareForShutdown;
{Rewritten !!.05}
begin
  ltTerminateThread;
end;
{--------}
procedure TffLegacyTransport.scShutdown;
begin
  try
    { Note: We can't free the protocol or the thread until we know they have
      finished or a certain number of milliseconds has elapsed. }
    ltTerminateThread;                                                 {!!.05}
  finally

{Begin !!.03}
    if assigned(FSendBuffer) then begin
      FFFreeMem(FSendBuffer, FProtocol.MaxNetMsgSize);
      FSendBuffer := nil;
    end;
{End !!.03}

    if assigned(FMsgQueue) then begin
      FMsgQueue.Free;
      FMsgQueue := nil;
    end;

    if assigned(FProtocol) then begin
      FProtocol.Free;
      FProtocol := nil;
    end;

    if assigned(FTransportThread) then
      { By this time, the transport thread will have freed itself. }
      FTransportThread := nil;
  end;
end;
{--------}
procedure TffLegacyTransport.scStartup;
begin
  FTransportThread.Resume;

  { An exception during protocol startup might leave the thread in a terminated
    state.  If the thread is still going, wait for the thread to finish or fail
    startup. }
  if (not FTransportThread.Terminated) then
    FProtocol.StartedEvent.WaitFor(ffc_ThreadStartTimeout);

  { If the thread fails then raise an exception. }
  if not FProtocol.IsStarted then
    raise EffException.CreateEx(ffStrResGeneral, fferrProtStartupFail,
                                [(FProtocol as TffBaseCommsProtocol).GetProtocolName]);

end;
{--------}
function TffLegacyTransport.Supported : boolean;
begin
  Result := ltMapProtocolToClass.Supported;
end;
{--------}
procedure TffLegacyTransport.TerminateConnection(const aClientID : TffClientID;
                                                 const timeout : Longint);
begin
{Begin delete !!.05}
  { Post a message to the server stating that we are hanging up. }
//  Post(0, aClientID, ffnmDetachServer, nil, 0, timeout, ffrmNoReplyWaitUntilSent);
  { After we know the message has been sent, tell the protocol to hangup. }
{End delete !!.05}
  { Tell the protocol to hangup. }                                     {!!.05}
  if assigned(FProtocol) then
    FProtocol.HangUpByClientID(aClientID);
end;
{--------}
procedure TffLegacyTransport.tpConnectionLost(aSender     : TObject;
                                              aClientID : TffClientID);
{Begin !!.01}
var
  anInx : Longint;
  aRequest : TffRequest;
{End !!.01}
  RequestFound : Boolean;
begin
//  PostMessage(FLostConnWindow, ffm_LostConnection, aClientID, 0);    {Deleted !!.12}
{Begin !!.01}
  { Abort the request pending for this client. There should be only one pending
    request for the client at any one time. }
  with FWaitingForReplyList.BeginRead do
    try
      RequestFound := False;                                           {!!.13}
      for anInx := 0 to pred(Count) do begin
        aRequest := TffRequest(TffIntListItem(Items[anInx]).KeyAsInt);
        if aRequest.ClientID = aClientID then begin
          RequestFound := True;                                        {!!.13}
{Begin !!.12}
          { If the request was something other than to check secure
            communications (i.e., no password handling involved) then
            post a message to the lost connection window. }
          if aRequest.MsgID <> ffnmCheckSecureComms then
            PostMessage(FLostConnWindow, ffm_LostConnection, aClientID, 0);
{End !!.12}
          { Mark the request as having lost its connection. }
          aRequest.SetReply(aRequest.MsgID, fferrConnectionLost, nil, 0, 0);
          { Remove the request's entry from the list. }
          DeleteAt(anInx);
          aRequest.WakeUpThread;
          break;
        end;  { if }
      end;  { for }
      if not RequestFound then                                         {!!.13}
        PostMessage(FLostConnWindow, ffm_LostConnection, aClientID, 0);{!!.13}
    finally
      EndRead;
    end;
{End !!.01}
end;
{--------}
procedure TffLegacyTransport.tpDatagramReceived(aSender  : TObject;
                                          const aName    : TffNetName;
                                                aData    : PffByteArray;
                                                aDataLen : Longint);
begin
  tpLogReqMisc(format('Rcvd datagram from %s', [aName]));
end;
{--------}
function TffLegacyTransport.tpGetCodeStart(const aClientID : TffClientID) : integer;
begin
  Result := FProtocol.GetCodeStart(aClientID);
end;
{--------}
procedure TffLegacyTransport.tpHandleAddClient(aMsg : PffDataMessage);
var
  aReply : TffnmAttachServerRpy;
  aClientID : TffClientID;
  aVersionNumber : Longint;
  errorCode : TffResult;
  isSecure : boolean;
  passwordHash : TffWord32;
begin
  if assigned(FOnAddClient) then begin
    FOnAddClient(Self, PffnmAttachServerReq(aMsg^.dmData)^.userID,
                 PffnmAttachServerReq(aMsg^.dmData)^.timeout,
                 PffnmAttachServerReq(aMsg^.dmData)^.clientVersion,
                 passwordHash, aClientID, errorCode, isSecure, aVersionNumber);

    { Build a reply. }
    aReply.ClientID := aClientID;
    aReply.VersionNumber := aVersionNumber;
{Begin !!.05}
    FProtocol.ConnLock;
    try
      if isSecure then
        aReply.Code := TffWord32(tpGetCodeStart(aMsg^.dmClientID)) xor passwordHash
      else
        aReply.Code := tpGetCodeStart(aMsg^.dmClientID);
      aReply.IsSecure := isSecure;
      aReply.LastMsgIntvl := ffc_LastMsgInterval;
      aReply.KAIntvl := ffc_KeepAliveInterval;
      aReply.KARetries := ffc_KeepAliveRetries;

      { Send the reply. }
      Reply(aMsg^.dmMsg, errorCode, @aReply, sizeOf(TffnmAttachServerRpy));

      { Update the clientID maintained by the protocol. }
      if errorCode = DBIERR_NONE then
        FProtocol.UpdateClientID(aMsg^.dmClientID, aClientID);
    finally
      FProtocol.ConnUnlock;
    end;
{End !!.05}

  end else
    { No handler assigned.  Send back an error. }
    Reply(aMsg^.dmMsg, DBIERR_FF_NoAddHandler, nil, 0);

  { Free the message data. }                                           {!!.01}
  ltFreeMsg(aMsg);                                                     {!!.01}
end;
{--------}
procedure TffLegacyTransport.tpHandleNextRequest;
var
  anItem : TffIntListItem;
  aRequest : TffRequest;
  Status : TffResult;
begin

  { Any messages in unsent request queue? Note that we don't care about
    thread-safeness to check the count. We want to improve performance
    by locking the queue only when necessary. If something slips into the
    queue, we will run through this loop again very soon and pick it up
    at that point. }
  anItem := nil;
  if FUnsentRequestQueue.Count > 0 then
    { Yes. Grab one. }
    with FUnsentRequestQueue.BeginWrite do
      try
        anItem := TffIntListItem(FUnsentRequestQueue.Dequeue);
      finally
        EndWrite;
      end;

  if assigned(anItem) then begin
    aRequest := TffRequest(anItem.KeyAsInt);
    anItem.Free;
    { If this request has already timed out then ignore it. }
    if aRequest.Aborted then
      aRequest.Free
    else begin
      { Otherwise send the request. }
      Status := tpSendRequest(aRequest);
      if (Status <> DBIERR_NONE) and
         (aRequest.ReplyMode = ffrmReplyExpected) then begin
        aRequest.ErrorCode := Status;
        aRequest.WakeUpThread;
      end;
    end;
  end;

end;
{--------}
procedure TffLegacyTransport.tpHandleRemoveClient(aMsg : PffDataMessage);
//var                                                                  {Deleted !!.01}
//  errorCode : TffResult;                                             {Deleted !!.01}
begin
{Begin !!.01}
  ltDoHangup(aMsg^.dmClientID);
//  if assigned(FOnRemoveClient) then
//    FOnRemoveClient(Self, aMsg^.dmClientID, errorCode)
//  else
    { No handler assigned.  Log an error. }
//    lcLogFmt(('No RemoveClientHandler for transport %d', [GetName]));
{End !!.01}
  { Free the message data. }                                           {!!.01}
  ltFreeMsg(aMsg);                                                     {!!.01}
end;
{--------}
procedure TffLegacyTransport.tpInternalRequest(aRequest : TffRequest;
                                               timeout  : Longint;
                                               aCookie  : HWND);
var
  anItem : TffIntListItem;
begin
  aRequest.EventLog := FEventLog;
  anItem := TffIntListItem.Create(Longint(aRequest));
  anItem.MaintainLinks := False;                                       {!!.01}
  with FUnsentRequestQueue.BeginWrite do
    try
      Enqueue(anItem);
    finally
      EndWrite;
    end;

  if (aCookie <> 0) and IsWindow(aCookie) then
     PostMessage(aCookie, 0, 0, 0);

  { Wait for the reply.  If a timeout occurs, assume the request object
    will be freed by the transport thread at some point.  Timeout exceptions
    are raised to the calling object. }
  if timeout = 0 then
    aRequest.WaitForReply(timeout)
  else
    aRequest.WaitForReply(timeout + ffcl_RequestLatencyAdjustment);

end;
{--------}
function TffLegacyTransport.tpMsgReceived(aSender : TObject;
                                          clientID : TffClientID;
                                          msgData : PffByteArray;
                                          msgDataLen : Longint) : boolean;
var
  MsgHeader : PffnmHeader absolute msgData;
begin
//  Result := False;                                                   {!!.01}
  case MsgHeader^.nmhMsgType of
    ffmtRequest:
      Result := tpRequestReceived(aSender, clientID, msgData, msgDataLen);
    ffmtReply:
      Result := tpReplyReceived(aSender, clientID, msgData, msgDataLen);
{Begin !!.01}
  else begin
    lcLogFmt(ffc_ErrMsgType,
             [MsgHeader^.nmhMsgType, MsgHeader^.nmhClientID,
              MsgHeader^.nmhMsgID]);
    { Pass it on to tpRequestReceived as this may just be the result of a
      user entering a bad password. }
    Result := tpRequestReceived(aSender, clientID, msgData, msgDataLen);
  end;
  end;  { case }
{End !!.01}
end;
{--------}
procedure TffLegacyTransport.tpPrepareThread;
begin
  if assigned(FTransportThread) then
    FTransportThread.Free;
  FTransportThread := TffLegacyTransportThread.Create(Self);
  FTransportThread.FreeOnTerminate := False;
  FTransportThread.OnTerminate := tpThreadTerminated;
end;
{--------}
procedure TffLegacyTransport.tpProcessCallback(const aProcessCookie : Longint);
var
  conn : TffConnection;
  msg : PffDataMessage;
begin

  btStoreSelfInThreadvar;

{Begin !!.05}
  msg := PffDataMessage(aProcessCookie);
  conn := ProtocolCracker(FProtocol).cpGetConnection(msg^.dmClientID);
  if conn <> nil then begin
    conn.HangupLock;
    try
      { Save off some data for when we reply. }
      ffitvClientID := msg^.dmClientID;
      ffitvRequestID := msg^.dmRequestID;

      { Is this a request to add a client? }
      if (msg^.dmMsg = ffnmAttachServer) then begin
        tpHandleAddClient(msg)
      { Remove a client? }
      end else if (msg^.dmMsg = ffnmDetachServer) then begin
        tpHandleRemoveClient(msg)
      end else
        { None of the above.  Call our Process method which will pass the message
          onto the appropriate command handlers. }
        Process(msg)
    finally
      conn.HangupUnlock;
    end;
  end;
{End !!.05}
end;
{--------}
procedure TffLegacyTransport.tpRemoteClientHangup(aSender     : TObject;
                                                  aClientID : TffClientID);
//var                                                                  {Deleted !!.01}
//  errorCode : TffResult;                                             {Deleted !!.01}
begin
  { As a just in case, make sure the client is removed. }
{Begin !!.01}
  ltDoHangup(aClientID);
//  if assigned(FOnRemoveClient) then
//    FOnRemoveClient(Self, aClientID, errorCode);
{End !!.01}
end;
{--------}
function TffLegacyTransport.tpReplyReceived(aSender : TObject;
                                            clientID : TffClientID;
                                            replyData : PffByteArray;
                                            replyDataLen : Longint): boolean;
var
  msgHeader : PffnmHeader absolute replyData;
  anItem : TffIntListItem;
  aRequest : TffRequest;
begin
  Result := True;

  { Find the request. }
  with FWaitingForReplyList.BeginRead do
    try
      anItem := TffIntListItem
                  (FWaitingForReplyList
                    [FWaitingForReplyList.Index(msgHeader^.nmhRequestID)]);
    finally
      EndRead;
    end;

  { Did we find the request?  If so then set its reply data.

    If we did not find the request then the requesting thread timed out
    and we can just toss the reply into the bitbucket. }
  if assigned(anItem) then begin
    aRequest := TffRequest(anItem.keyAsInt);

    with msgHeader^ do
      if msgHeader^.nmhFirstPart then
        aRequest.SetReply(nmhMsgID, nmhErrorCode, @nmhData, nmhTotalSize,
                          replyDataLen - ffc_NetMsgHeaderSize)
      else
        aRequest.AddToReply(@nmhData, replyDataLen - ffc_NetMsgHeaderSize);

    { If this is the last part of the message then remove the request from
      the waiting list. }
    if msgHeader^.nmhLastPart then begin
      with FWaitingForReplyList.BeginWrite do
        try
          Delete(Longint(msgHeader^.nmhRequestID));
        finally
          EndWrite;
        end;
      { If the request has been aborted then get rid of it otherwise
        wake up the requesting thread. }
      if aRequest.Aborted then
        aRequest.Free
      else begin
        tpLogReply(aRequest);
        aRequest.WakeUpThread;
      end;
    end;

  end else begin
    lcLogFmt('Could not find Request %d, msgID %d',
             [msgHeader^.nmhRequestID, msgHeader^.nmhMsgID]);
  end;

end;
{--------}
function TffLegacyTransport.tpRequestReceived(aSender : TObject;
                                              clientID : TffClientID;
                                              requestData : PffByteArray;
                                              requestDataLen : Longint) : boolean;
var
  MsgHeader : PffnmHeader absolute requestData;
  MsgNode : PffDataMessageNode;
begin
  Result := True;

  with MsgHeader^ do begin

    { Verify the client id in the message is correct.  If not then either
      we have a fake client using the wrong encryption or something else is
      goofed up.  Hangup. }
    if (nmhMsgID <> ffnmAttachServer) and
       (clientID <> nmhClientID) then begin

      if FLogEnabled and assigned(FEventLog) and
         (fftpLogErrors in FLogOptions) then
        FEventLog.WriteStrings(['Hanging up due to bad client password',
                                Format('  ClientID (actual) %d', [clientID]),
                                Format('  ClientID (msg)    %d', [nmhClientID]),
                                Format('  MsgID             %d', [nmhMsgID])]);

      FProtocol.HangUpByClientID(clientID);
      Result := false;
      Exit;
    end;

    if FLogEnabled and assigned(FEventLog) and
       (fftpLogRequests in FLogOptions) then
      tpLogReq2(ffc_Request, nmhRequestID, clientID, nmhMsgID, @nmhData,
                requestDataLen - ffc_NetMsgHeaderSize, nmhTimeout);

    with FMsgQueue.BeginWrite do
      try
        if nmhFirstPart then
          MsgNode := Append(nmhMsgID,
                            clientID,
                            nmhRequestID,
                            nmhTimeout,
                            0,
                            @nmhData,
                            requestDataLen - ffc_NetMsgHeaderSize,
                            nmhTotalSize)
        else
          MsgNode := AddToData(nmhMsgID,
                               clientID,
                               nmhRequestID,
                               @nmhData,
                               requestDataLen - ffc_NetMsgHeaderSize);
      finally
        EndWrite;
      end;

    { Is this the last part of the message? }
    if assigned(MsgNode) then begin
      { Yes. Do we have a thead pool? }
      if assigned(FThreadPool) then
        { Yes.  Pass this request off to a worker thread. }
        FThreadPool.ProcessThreaded(tpProcessCallback, Longint(MsgNode^.dmnMsg))
      else
        { No.  Handle this ourselves. }
        tpProcessCallback(Longint(MsgNode^.dmnMsg));

      { Get rid of the request on the message queue. }
      with FMsgQueue.BeginWrite do
        try
          Remove(MsgNode, false);
        finally
          EndWrite;
        end;
    end;
  end;  { with }
end;
{--------}
function TffLegacyTransport.tpSendReply(msgID : Longint;
                                        clientID : TffClientID;
                                        requestID : Longint;
                                        errorCode : TffResult;
                                        replyData : pointer;
                                        replyDataLen : Longint) : TffResult;
var
  BytesToGo : Longint;
  BytesToSend : Longint;
  FirstMsg : boolean;
  LastMsg : boolean;
  StartOffset : Longint;
  SendBuffer : PffnmHeader;
begin

  try

    FFGetMem(SendBuffer, FProtocol.MaxNetMsgSize);

    try
      { Set up the message header. }
      with SendBuffer^ do begin
        nmhMsgType := ffmtReply;
        nmhMsgID := msgID;
        nmhTotalSize := replyDataLen;
        nmhClientID := clientID;
        nmhRequestID := requestID;
        nmhErrorCode := errorCode;
        nmhTimeout := 0;
      end;

      StartOffset := 0;
      BytesToGo := replyDataLen;
      FirstMsg := true;

      { Send data in reasonably-sized chunks }
      repeat
        { Calculate the size of the data to send in this message packet. }
        BytesToSend := FFMinL(BytesToGo,
                              FProtocol.MaxNetMsgSize - ffc_NetMsgHeaderSize);
        LastMsg := (BytesToSend = BytesToGo);
        with SendBuffer^ do begin
          nmhMsgLen := ffc_NetMsgHeaderSize + BytesToSend;
          nmhFirstPart := FirstMsg;
          nmhLastPart := LastMsg;
        end;

        { Copy the data into the send buffer. }
        if (BytesToSend > 0) then
          Move(PffBLOBArray(replyData)^[StartOffset],
                            SendBuffer^.nmhData, BytesToSend);

        { Send the packet. }
        Result := FProtocol.SendMsg(clientID, PffByteArray(SendBuffer),
                                    SendBuffer^.nmhMsgLen, True);     {!!.06}

        { Do we need to get an acknowledgement? }
        if not LastMsg then begin
          { Update bytes sent, etc. }
          dec(BytesToGo, BytesToSend);
          inc(StartOffset, BytesToSend);
          FirstMsg := false;
        end;

      until LastMsg or (Result <> DBIERR_NONE);
{Moved !!.06}
{Begin !!.10}
      if Result <> DBIERR_NONE then
        lcLogFmt(ffc_SendErr,
                 [Result, 'tpSendReply', -1, clientID, msgID, replyDataLen, 0])
      else if FLogEnabled and assigned(FEventLog) and
{End !!.10}
        (fftpLogReplies in FLogOptions) then
        tpLogReply2(requestID, clientID, msgID, replyDataLen, errorCode);
    finally
      FFFreeMem(SendBuffer, FProtocol.MaxNetMsgSize);
    end;
  except
    on E:EffWinsockException do begin
      Result := fferrTransportFail;
      lcLogFmt('Transport failure %d: %s', [E.ErrorCode, E.Message]);
    end;
    on E:EffException do
      Result := E.ErrorCode;
  end;
end;
{--------}
function TffLegacyTransport.tpSendRequest(aRequest : TffRequest) : TffResult;
var
  aClientID : TffClientID;
  anItem : TffIntListItem;
  BytesToSend : Longint;
  FirstMsg : boolean;
  LastMsg : boolean;
  CallRpy : PffnmCallServerRpy;
const
  logPrefixArray : array[TffReplyModeType] of string = (ffc_Request,
                                                        ffc_Post,
                                                        ffc_PostWait);
begin

  Result := DBIERR_NONE;
  anItem := nil;

  try

    tpLogReq(aRequest, logPrefixArray[aRequest.ReplyMode]);

    { Is this a "call server" request or a regular request?  }
    if aRequest.MsgID = ffnmCallServer then begin
{Begin !!.05}
      tpLogReqMisc(Format(ffc_ReqLogString,
                          [ffc_Request, Longint(aRequest), aRequest.ClientID,
                           aRequest.MsgID, aRequest.RequestDataLen,
                           aRequest.Timeout]));
{End !!.05}
      aRequest.ErrorCode :=
        FProtocol.Call(PffnmCallServerReq(aRequest.RequestData).ServerName,
                       aClientID, aRequest.Timeout);
      FFGetMem(CallRpy, SizeOf(TffnmCallServerRpy));
      CallRpy^.ClientID := aClientID;
      aRequest.ReplyData := CallRpy;
      aRequest.ReplyDataLen := SizeOf(TffnmCallServerRpy);
      aRequest.ReplyMsgID := ffnmCallServer;
{Begin !!.05}
      if FLogEnabled and (fftpLogReplies in FLogOptions) and
         (FEventLog <> nil) then
        with aRequest do
         FEventLog.WriteString(Format(ffc_ReplyLogString,
                                      [Longint(aRequest), ClientID,
                                       ReplyMsgID, ReplyDataLen, ErrorCode]));
{End !!.05}
      aRequest.WakeUpThread;
      anItem.Free;
      exit;
    end;

    { Set up the message header. }
    with FSendBuffer^ do begin
      nmhMsgType := ffmtRequest;
      nmhMsgID := aRequest.MsgID;
      nmhTotalSize := aRequest.RequestDataLen;
      nmhClientID := aRequest.ClientID;
      nmhRequestID := Longint(aRequest);
      nmhErrorCode := 0;
      nmhTimeout := aRequest.Timeout;
    end;

    FirstMsg := (aRequest.StartOffset = 0);


    { Obtain exclusive write access.  This is required because a reply
      may be received from the server before an iteration of this repeat..until
      block completes.  In that situation, we want to make sure this method
      finishes before the TffRequest is freed.

      The corresponding call to TffRequest.Lock is in the TffRequest.Destroy
      method. }
    aRequest.Lock;

    try

      { Send data in reasonably-sized chunks }
      repeat

        { Calculate the size of the data to send in this message packet. }
        BytesToSend := FFMinL(aRequest.BytesToGo,
                              FProtocol.MaxNetMsgSize - ffc_NetMsgHeaderSize);
        LastMsg := (BytesToSend = aRequest.BytesToGo);
        with FSendBuffer^ do begin
          nmhMsgLen := ffc_NetMsgHeaderSize + BytesToSend;
          nmhFirstPart := FirstMsg;
          nmhLastPart := LastMsg;
        end;

        { Copy the data into the send buffer. }
        if (BytesToSend > 0) then
          Move(PffBLOBArray(aRequest.RequestData)^[aRequest.StartOffset],
               FSendBuffer^.nmhData, BytesToSend);

        { Update bytes sent, etc. }
        aRequest.BytesToGo := aRequest.BytesToGo - BytesToSend;
        aRequest.StartOffset := aRequest.StartOffset + BytesToSend;

        { If this is the first message and the requesting thread must wait for a
          reply, add the request to the Waiting For Reply list before we actually
          send the message to the server.  We do this to avoid the situation where
          the reply is received before we actually get the request into the
          Waiting For Reply list.

          The request will sit in the Waiting For Reply list until the entire
          reply is received. }
        if FirstMsg and (aRequest.ReplyMode = ffrmReplyExpected) then
          with FWaitingForReplyList.BeginWrite do
            try
              anItem := TffIntListItem.Create(Longint(aRequest));
              Insert(anItem);
            finally
              EndWrite;
            end;

        { Send the packet. }
        Result := FProtocol.SendMsg(aRequest.ClientID, PffByteArray(FSendBuffer),
                                    FSendBuffer^.nmhMsgLen, True);    {!!.06}

        { If the send failed & we were expecting a reply, take the request out
          of the Waiting For Reply list because no reply is forthcoming. }
        if (Result <> DBIERR_NONE) and
           (aRequest.ReplyMode = ffrmReplyExpected) then begin         {!!.03}
          aRequest.ReplyMsgID := aRequest.MsgID;                       {!!.03}
          with FWaitingForReplyList.BeginWrite do
            try
              Delete(Longint(aRequest));
            finally
              EndWrite;
            end;
        end;                                                           {!!.03}

        FirstMsg := False;                                             {!!.01}

      until LastMsg or (Result <> DBIERR_NONE);

      if Result <> DBIERR_NONE then
        lcLogFmt(ffc_SendErr,
                 [Result, 'tpSendRequest', -1, aRequest.ClientID,
                  aRequest.MsgID, aRequest.RequestDataLen, aRequest.Timeout]);

      { Is the requesting thread waiting for the request to be sent to the
        server but not wanting a reply? }
      if aRequest.ReplyMode = ffrmNoReplyWaitUntilSent then begin
        { Yes.  Was the request aborted by the requesting thread
          (i.e, timeout)? }
        if aRequest.Aborted then begin
          { Yes.  Free the request. }
          aRequest.Unlock;
          aRequest.Free;
          aRequest := nil;
        end else
          { No.  Signal the requesting thread. }
          aRequest.WakeUpThread;
      end;

    finally
      if assigned(aRequest) then begin
        aRequest.Unlock;
        if aRequest.ReplyMode = ffrmNoReplyExpected then
          aRequest.Free;
      end;
    end;
  except
    on E:Exception do begin
      { Free the list item. }
      if assigned(anItem) then
        anItem.Free;

      { Handle the exception. }
      if E is EffWinsockException then begin
        Result := fferrTransportFail;
        lcLogFmt('Transport failure %d: %s',
                 [EffWinsockException(E).ErrorCode, E.Message]);
      end
      else if E is EffException then begin
        Result := EffException(E).ErrorCode;
        lcLogFmt('tpSendRequest exception %d: %s',
                 [EffException(E).ErrorCode, E.Message]);
      end
      else begin
        Result := fferrTransportFail;
        lcLogFmt('tpSendRequest general exception %s:', [E.Message]);
      end;
    end;
  end;

end;
{--------}
procedure TffLegacyTransport.tpShutdownProtocol;
begin
  if FLogEnabled and assigned(FEventLog) and
     ((fftpLogRequests in FLogOptions) or
      (fftpLogReplies in FLogOptions)) then
    tpLogReqMisc(format('Transport thread (%s) shut down.', [GetName]));
  FProtocol.Shutdown;
end;
{--------}
procedure TffLegacyTransport.tpStartProtocol;
begin
  FProtocol.Startup;

  { If we are to listen for broadcasts then set up to receive datagrams. }
  //if (FMode = fftmListen) and FRespondToBroadcasts then begin        {!!.05 - Start}
  //  FProtocol.ReceiveDatagram;
  //  FProtocol.Listen;
  //end; }
  if (FMode = fftmListen) then begin
    FProtocol.Listen;
    if (FRespondToBroadcasts) then
      FProtocol.ReceiveDatagram;
  end;                                                                 {!!.05 - End}
end;
{--------}
procedure TffLegacyTransport.tpThreadTerminated(Sender : TObject);
begin
  if TffLegacyTransportThread(Sender).UnexpectedTermination then begin
    { The thread has shutdown prematurely.  Log the event and restart
      the thread. }
    if assigned(FProtocol) then
      lcLogFmt('Transport thread (%s) prematurely stopped.', [GetName]);
    tpPrepareThread;
    FTransportThread.Resume;
  end;
end;
{--------}
procedure TffLegacyTransport.Work;
begin

  { Legacy transports can both send and receive messages
    (i.e., bi-directional). }

  { Give the protocol a chance to receive requests. }
  FProtocol.Breathe;

  { Give the protocol a chance to send a request. }
  tpHandleNextRequest;

end;
{====================================================================}

{===TffLegacyTransportThread=========================================}
constructor TffLegacyTransportThread.Create(aTransport : TffLegacyTransport);
begin
  inherited Create(True);
  FTransport := aTransport;
  FUnexpectedTermination := false;
end;
{--------}
procedure TffLegacyTransportThread.Execute;
begin
  try
    FTransport.tpStartProtocol;
    repeat
      try
        FTransport.Work;
      except
        on E:Exception do
          FTransport.lcLog
            (format('Transport thread (%s) error: %s',
                    [FTransport.GetName, E.message]));
      end;
    until Terminated;
    FTransport.tpShutdownProtocol;
  except
    on E:Exception do begin
      { Signal the primary thread so that it can see our failure to start. }
      FTransport.FProtocol.StartedEvent.SignalEvent;
      FTransport.lcLog
        (format('Transport thread startup (%s) error: %s',
                [FTransport.GetName, E.message]));
    end;
  end;
end;
{====================================================================}
end.
