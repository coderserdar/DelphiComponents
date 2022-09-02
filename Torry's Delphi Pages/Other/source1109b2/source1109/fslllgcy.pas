{$I fsdefine.inc}

Unit fslllgcy;

Interface

Uses
  Dialogs,
  Classes,
  Messages,
  windows,
  fsdtmsgq,
  fsllbase,
  fssrbase,
  fsllcomp,
  fslleng,
  fsllcomm,
  fslllog,
  fsllprot,
  fsllreq,
  fsnetmsg;

Type

  TFSParamConnectThread = Class; { foward declaration }

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
  TFSParamConnect = Class(TfsThreadedTransport)
  Protected {private}
    { See comments in TFFBaseTransport for _* fields }

    FMsgQueue: TffDataMessageQueue;
    {-When in Listen mode, used to hold partially received messages. }

    FLostConnWindow: HWND;
    {-Used to receive lost connection events from the protocol thread. }

    FProtocol: TfsBaseCommsProtocol;
    {-The protocol instantiated by this transport. }

    FProtocolType: TfsProtocolType;
    _FProtocolType: TfsProtocolType;
    {-The enumeration describing the protocol instantiated by this transport. }

    FSendBuffer: PfsnmHeader;
    {-The buffer used to send messages to the remote server. }

    FServerLocalName: TffNetName;
    {-This is the local name portion of FServerName.  For example,
      if we are trying to reach 'prod1@127.0.0.1' then the server's local
      name is 'prod1' and the server's address is '127.0.0.1' }

    FServerAddress: TffNetName;
    {-This is FServerName minus the local name of the server.  For example,
      if we are trying to reach 'prod1@127.0.0.1' then the server's local
      name is 'prod1' and FServerAddress will be '127.0.0.1' }

    FTransportThread: TFSParamConnectThread;
    {-If in Listen mode, this is the thread that is listening.  If in Send
      mode, this is the thread that will be sending requests. }

  Protected

    Procedure btBeginUpdatePrim; Override;

    Procedure btEndUpdatePrim; Override;

    Function btGetConnectionID(Const anIndex: Longint): TffClientID; Override;
    { Used to obtain the IDs of the protocol's connections.  Handy for when
      a server wants to send messages to one or more client connections. }

    Procedure btInternalReply(msgID: Longint;
      errorCode: TffResult;
      replyData: pointer;
      replyDataLen: Longint); Override;
    { This method is called from TffBaseTransport.Reply.  It sends the
      reply to the client. }

    Procedure btSetRespondToBroadcasts(Const respond: boolean); Override;
    { This implementation makes sure the legacy protocol is configured
      properly if RespondToBroadcasts is changed while the transport is
      active. }

    Procedure btSetServername(Const aServername: String); Override; {!!.10}
    {-This method sets the server name.  The implementation for this class
      does not perform any validation.  Transport subclasses should perform
      their own validation. }

    Procedure lcLog(Const aMsg: String); Override;
    { Use this method to write an error string to the event log. }

    Procedure ltFreeMsg(msg: PFSDataMessage); Virtual; {!!.01}

    Function ltGetProtocol: TfsProtocolType; Virtual;
    { Used to get the legacy protocol. }

{Begin !!.01}
    Procedure ltDoHangup(Const aClientID: TffClientID);
    { Hangup processing. }
{End !!.01}

    Procedure ltLostConnection(Var aMsg: TMessage);
    { Message handler for lost connections window. }

    Function ltMapProtocolToClass: TfsCommsProtocolClass;
    { Maps the transport's protocol to its protocol class. }

    Procedure lcSetEventLog(anEventLog: TFSBaseLog); Override;
    { Set the transport's event log.  This overridden method makes sure the
      protocol's EventLog property is kept up-to-date. }

    Procedure lcSetLogEnabled(Const aEnabled: Boolean); Override;
    { This overridden method updates the logEnabled property of the
      TfsBaseCommsProtocol instance created by this component. }

    Procedure ltSetProtocol(aProtocol: TfsProtocolType); Virtual;
    { Used to set the legacy protocol. }

{Begin !!.05}
    Procedure ltTerminateThread;
    { Terminate the transport thread if it is active. }
{End !!.05}

    Procedure scInitialize; Override;
    { Called when the transport is to initialize itself for operation.  This
      implementation creates and initializes the protocol and transport
      thread. }

    Procedure scPrepareForShutdown; Override;
    { This method is called when the transport is to prepare for shutdown. }

    Procedure scShutdown; Override;
    { This method is called when the transport is to shut down. }

    Procedure scStartup; Override;
    { This method is called when the transport is to start up. }

    Procedure tpConnectionLost(aSender: TObject;
      aClientID: TffClientID);
    {-Called when the transport is sending and the remote server engine
      unexpectedly hangs up on the client. This method is called within the
      context of the transport thread. It sends a message to the
      transports lost connection window and the message is then processed
      by ltLostConnection. }

    Procedure tpDatagramReceived(aSender: TObject;
      Const aName: TffNetName;
      aData: PffByteArray;
      aDataLen: Longint);
    { Used to log receipt of broadcast requests in listening transport. }

    Function tpGetCodeStart(Const aClientID: TffClientID): Integer;
    { Used to obtain the starting encryption code for the specified client. }

    Procedure tpHandleAddClient(aMsg: PFSDataMessage);
    {-This method is called by a listening transport to process the adding
      of a new client. }

    Procedure tpHandleNextRequest;
    {-This method is used to handle the next unsent request.  The request
      is moved from the unsent queue to the waiting for reply list. }

    Procedure tpHandleRemoveClient(aMsg: PFSDataMessage);
    {-This method is called by a listening transport to process the removal
      of an existing client. }

    Procedure tpInternalRequest(aRequest: TfsRequest;
      timeout: Longint;
      aCookie: HWND); Override;
    {-Internal method for sending a request.  This implementation assigns the
      protocol's event log to the request and assigns the protocol's window
      handle to the value of aCookie. }

    Function tpMsgReceived(aSender: TObject;
      clientID: TffClientID;
      msgData: PffByteArray;
      msgDataLen: Longint): boolean;
    {-This method is called when a request is received from a client or
      when a reply is received from a server. }

    Procedure tpPrepareThread;
    { Prepares the legacy transport thread for work. }

    Procedure tpProcessCallback(Const aProcessCookie: Longint); Virtual;
    { When in Listen mode, this method is called by the worker thread to
      process the request received from the client.
      This method stores information such as the clientID and requestID in
      threadvars so that it may be used when replying to the client.
      aProcessCookie is a pointer to the message received from the client.
      The method passes the message to the command handler.
     }

    Procedure tpRemoteClientHangup(aSender: TObject;
      aClientID: TffClientID);
    {-Called when the transport is listening and a) the client hangs up
      or b) the transport decides to hang up on the client. }

    Function tpReplyReceived(aSender: TObject;
      clientID: TffClientID;
      replyData: PffByteArray;
      replyDataLen: Longint): boolean; Virtual;
    { When sending, this method is called when the legacy protocol has
      received a reply from the server.  In this implementation the
      following occurs:
        1. If this reply is acknowledging the last message was received then
           the request is sitting in the pending list.  Find the request and
           put it in the Unsent Requests queue.

        2. If this reply is a full-fledged response to a complete message, do
           the following:

           a. Find the TfsRequest.
           b. Place the reply data on the request.
           c. Remove the request from the WaitingForReply list.
           d. Call TfsRequest.WakeUpThread.
     }

    Function tpRequestReceived(aSender: TObject;
      clientID: TffClientID;
      requestData: PffByteArray;
      requestDataLen: Longint): boolean; Virtual;
    { When listening, this method is called when the protocol thread has
      received a request from a client.  The transport then spawns a worker
      thread that performs the actual work. }

    Function tpSendReply(msgID: Longint;
      clientID: TffClientID;
      requestID: Longint;
      errorCode: TffResult;
      replyData: pointer;
      replyDataLen: Longint): TffResult;
    { This method sends the actual reply to the client.  The reply is
      sent in context of the thread that processed the client's request,
      not the listening thread. }

    Function tpSendRequest(aRequest: TfsRequest): TffResult;
    { Sends a request via the protocol to the remove server.  This method
      returns DBIERR_NONE if the request was successfully sent. }

    Procedure tpShutdownProtocol;
    { This method is called when the protocol thread stops executing.
      It must be run in context of the protocol thread so that any
      thread-specific items (e.g., windows, timers) may be destroyed. }

    Procedure tpStartProtocol;
    { This method is called when the protocol thread begins execution.
      It must be run in context of the protocol thread so that the
      window handle created by the protocol is associated with the
      thread. }

    Procedure tpThreadTerminated(Sender: TObject);
    { This method is called when the transport thread terminates.
      The purpose of this handler is to detect the case where the
      thread terminates prematurely. }

  Public

    Constructor Create(aOwner: TComponent); Override;
    Destructor Destroy; Override;

    Function ConnectionCount: Longint; Override;
    { Returns the number of established connections.  For a sender (i.e.,
      client), this will be the number of connections to the remote server.
      For a listener (i.e., server), this will be the number of
      connections establshed by remote clients. }

    Function EstablishConnection(Const aUserName: TffName;
      aPasswordHash: Integer;
      aTimeout: Longint;
      Var aClientID: TffClientID;
      Var aRights: TffUserRights;
      Var aSecurityEnabled: boolean): TffResult; Override;
    { Use this method to establish a connection with the server.  If the
      return code is DBIERR_NONE then aClientID will contain the clientID
      supplied by the server.  This clientID must be used in all subsequent
      requests to the server. }

    Function GetName: String; Override;
    { Returns the transport's name. }

    Procedure GetServerNames(aList: TStrings; Const timeout: Longint); Override;
    { Returns the list of servers available via this transport. }

    Function IsConnected: boolean; Override;
    { Use this method to determine if the transport is connected to a remote
      server.  It is considered connected if a) the transport's State is
      fsesStarted and b) there is at least one established connection.
      If the transport has been started but no connections have been
      established then this method returns False. }

    Procedure Request(transportID: Longint;
      clientID: TffClientID;
      msgID: Longint;
      timeout: Longint;
      requestData: pointer;
      requestDataLen: Longint;
      replyCallback: TfsReplyCallback;
      replyCookie: Longint); Override;
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

    Function Supported: boolean; Override;
    { Use this method to determine if the transport's current protocol is
      supported on the workstation.  Returns True if the protocol is
      supported otherwise returns False. }

    Procedure TerminateConnection(Const aClientID: TffClientID;
      Const timeout: Longint); Override;
    { Use this method to terminate a connection with the server.  aClientID
      is the clientID originally returned in the call to EstablishConnection.}

    Procedure Work; Override;
    { Based upon the transport's mode, this method tells the transport to
      perform some work:

      1. When in sending mode, sends requests and processes replies
      2. When in listening mode, listens for requests.

      This method should be structured such that it does a bit of work and
      then returns.  It is to be repeatedly called from the
      TFSParamConnectThread.Execute method so that the Execute method
      may check the thread's Terminated property.
    }

  Published

    Property Protocol: TfsProtocolType
      Read ltGetProtocol
      Write ltSetProtocol
      Default ptRegistry;
    { The legacy protocol to be used by this transport.  Defaults to
      ptRegistry. }

  End;

  { In order to support sending and receiving of messages without blocking
    the client application or server, the legacy transport carries out
    sending and receiving of messages through an instance of this class.

    The thread is always created in suspended mode.  It is resumed by
    TFSParamConnect.tpStartup.
  }
  TFSParamConnectThread = Class(TfsThread)
  Private
    FTransport: TFSParamConnect;
    { The transport starting this thread. }
    FUnexpectedTermination: boolean;
    { Set to True if thread terminated by an exception. }
  Protected

    Procedure Execute; Override;
    { This method repeatedly calls the transport's Work method.  Execute
      should be called only when the transport's Startup method is called. }

  Public

    Constructor Create(aTransport: TFSParamConnect);
    { When creating a listener thread, the parent transport is identified.
      The thread is suspended. }

    Property UnexpectedTermination: boolean Read FUnexpectedTermination;

  End;

Implementation

Uses
  Forms,
  SysUtils,
  fsclcfg,
  fsllexcp,
  fsllthrd,
  fsllwsck,
  fssrbde;

{$I fsconst.inc}

Const
  fsc_ThreadDoneTimeout = 1000; { # milliseconds to wait for send/listen
  thread to shutdown }
  fsc_ThreadStartTimeout = 2000; { # milliseconds to wait for the transport
  thread to start }
  fsc_SingleUserServerName = 'Local';

  { Prefixes for logging requests. }
  fsc_Post = 'Post';
  fsc_PostWait = 'Post&Wait';
  fsc_Request = 'Req';

  { Error messages. }
  fsc_ErrMsgType = 'Bad msg type %d, Clnt %d, Msg %d';

  {===TFSParamConnect===============================================}

Threadvar
  fsitvClientID: TffClientID;
  fsitvRequestID: Longint;

Constructor TFSParamConnect.Create(aOwner: TComponent);
Begin
  Inherited Create(aOwner);
  FLogEnabled := False;
  {$IFDEF DCC6OrLater}
  {$WARN SYMBOL_DEPRECATED OFF}
  {$ENDIF}
  FLostConnWindow := AllocateHWND(ltLostConnection);
  {$IFDEF DCC6OrLater}
  {$WARN SYMBOL_DEPRECATED ON}
  {$ENDIF}
  FProtocol := Nil;
  FProtocolType := ptRegistry;
  FTransportThread := Nil;
  FSendBuffer := Nil;
End;
{--------}

Destructor TFSParamConnect.Destroy;
Begin
  FFNotifyDependents(ffn_Destroy);
  ltTerminateThread; {!!.05}
  {$IFDEF DCC6OrLater}
  {$WARN SYMBOL_DEPRECATED OFF}
  {$ENDIF}
  DeallocateHWnd(FLostConnWindow);
  {$IFDEF DCC6OrLater}
  {$WARN SYMBOL_DEPRECATED ON}
  {$ENDIF}
  Inherited Destroy;
End;
{--------}

Procedure TFSParamConnect.btBeginUpdatePrim;
Begin
  Inherited btBeginUpdatePrim;

  { Set the _* fields to match their counterparts }
  _FProtocolType := FProtocolType;
End;
{--------}

Procedure TFSParamConnect.btEndUpdatePrim;
Begin
  { Update the fields with their _* counterparts. }
  { All property write methods are required to check that the new value }
  { does not match the old value! }
  Protocol := _FProtocolType;
  If Protocol = ptSingleUser Then
    _FServerName := ServerName;

  If assigned(FProtocol) Then
    FProtocol.LogEnabled := _FLogEnabled;

  Inherited btEndUpdatePrim;
End;
{--------}

Function TFSParamConnect.btGetConnectionID(Const anIndex: Longint): TffClientID;
Begin
  Result := FProtocol.ConnectionIDs[anIndex];
End;
{--------}

Function TFSParamConnect.ConnectionCount: Longint;
Begin
  If assigned(FProtocol) Then
    Result := FProtocol.ConnectionCount
  Else
    Result := 0;
End;
{--------}

Function TFSParamConnect.EstablishConnection
  (Const aUserName: TffName;
  aPasswordHash: Integer;
  aTimeout: Longint;
  Var aClientID: TffClientID;
  Var aRights: TffUserRights;
  Var aSecurityEnabled: boolean): TffResult;
Var
  aRequest: TfsRequest;
  AttachReq: TfsnmAttachServerReq;
  CallReq: TfsnmCallServerReq;
  PAttachRpy: PfsnmAttachServerRpy;
Begin

  btCheckSender;
  btCheckServerName;
  scCheckStarted;

  { Have the protocol contact the server.
    Note that we will get back a temporary clientID from the protocol.
    This temporary ID will be replaced once we have a good one from
    the server. }
  CallReq.ServerName := FServerAddress;
  aRequest := TfsRequest.Create(aClientID, fsnmCallServer, @CallReq,
    sizeOf(CallReq), aTimeout, fsrmReplyExpected);
  Try
    tpInternalRequest(aRequest, aTimeout, FProtocol.NotifyWindow);
    Result := aRequest.ErrorCode;
    If Result <> DBIERR_NONE Then
      Begin
        aRequest.Free;
        Exit;
      End;
  Except
    { If an exception occurs then the transport thread is responsible for
      freeing the request. }
    On E: EfsException Do
      Begin
        Result := E.ErrorCode;
        Exit;
      End
    Else
      Raise;
  End;

  { Connection successful.  Get the clientID from the server. }
  Assert(assigned(aRequest.ReplyData));
  aClientID := PfsnmCallServerRpy(aRequest.ReplyData)^.ClientID;
  aRequest.Free;

  { Obtain permission to attach a client. }
  With AttachReq Do
    Begin
      ClientName := aUserName;
      UserID := aUserName;
      Timeout := aTimeout;
      ClientVersion := fsVersionNumber;
    End;

  { Submit the request. }
  aRequest := TfsRequest.Create(aClientID, fsnmAttachServer, @AttachReq,
    sizeOf(AttachReq), aTimeout, fsrmReplyExpected);
  Try
    tpInternalRequest(aRequest, aTimeout, FProtocol.NotifyWindow);
  Except
    { If an exception occurs then the transport thread is responsible for
      freeing the request. }
    On E: EfsException Do
      Begin
        Result := E.ErrorCode;
        Exit;
      End
    Else
      Raise;
  End;

  { Evaluate the reply. }
  Result := aRequest.ErrorCode;
  If Result = DBIERR_NONE Then
    Begin
      pAttachRpy := PfsnmAttachServerRpy(aRequest.ReplyData);
      With pAttachRpy^ Do
        Begin
          { Have the protocol update our connection's clientID. }
          FProtocol.UpdateClientID(aClientID, ClientID);
          aClientID := ClientID;
          aRights := Rights;
          aSecurityEnabled:= IsSecure;

          If IsSecure Then
            FProtocol.InitCode(aClientID, Code Xor Longint(aPasswordHash))
          Else
            FProtocol.InitCode(aClientID, Code);

          {Update the protocol's keep alive information. }
          FProtocol.KeepAliveInterval := KAIntvl;
          FProtocol.KeepAliveRetries := KARetries;
          FProtocol.LastMsgInterval := LastMsgIntvl;
          FProtocol.ResetKeepAliveTimer; {!!.06}

          { Check secure communications...}
          aRequest.Free;
          aRequest := TfsRequest.Create(aClientID, fsnmCheckSecureComms, Nil, 0,
            aTimeout, fsrmReplyExpected);
          Try
            tpInternalRequest(aRequest, aTimeout, FProtocol.NotifyWindow);
          Except
            { If an exception occurs then the transport thread is responsible for
              freeing the request. }
            On E: EfsException Do
              Begin
                Result := E.ErrorCode;
                Exit;
              End
            Else
              Raise;
          End;
          Result := aRequest.ErrorCode;
          If Result <> DBIERR_NONE Then
            Begin
              { The password is bogus. }
              FProtocol.HangUpByClientID(aClientID);
              If Result <> fserrReplyTimeout Then {!!.06}
                Result := DBIERR_INVALIDUSRPASS; {!!.06}
            End; { if }
        End; { with }
    End
  Else
    { Server rejected us.  Tell protocol to get rid of the connection. }
    FProtocol.HangUpByClientID(aClientID);

  {Begin !!.06}
    { If timed out waiting for a reply then we need to remove this request from
      the waiting for reply queue. }
  If Result = fserrReplyTimeout Then
    With FWaitingForReplyList.BeginWrite Do
      Try
        Delete(Longint(aRequest));
      Finally
        EndWrite;
      End;
  {End !!.06}

  If assigned(aRequest) Then
    aRequest.Free;

End;
{Begin !!.01}
{--------}

Procedure TFSParamConnect.ltFreeMsg(msg: PFSDataMessage);
Begin
  If Msg^.dmDataLen > 0 Then
    FFFreeMem(Msg^.dmData, Msg^.dmDataLen);
  FFFreeMem(Msg, SizeOf(TfsDataMessage));
End;
{End !!.01}
{--------}

Procedure TFSParamConnect.ltLostConnection(Var aMsg: TMessage);
Begin
  { Lost connection message? Event handler declared? }
  If (aMsg.Msg = fsm_LostConnection) Then
    Begin {!!.01}
      If assigned(FOnConnectionLost) Then
        Begin
          FOnConnectionLost(Self, aMsg.wParam)
        End
      Else If csDesigning In ComponentState Then
        AutoConnectionLost(Self, aMsg.WParam);
    End {!!.01}
  Else If aMsg.Msg = WM_QUERYENDSESSION Then {!!.01}
    aMsg.Result := 1 {!!.01}
  Else {!!.01}
    Dispatch(aMsg); {!!.01}
End;
{--------}

Function TFSParamConnect.ltMapProtocolToClass: TfsCommsProtocolClass;
Var
  protName: TffShStr;
Begin
  If (FProtocolType <> ptRegistry) Then
    Begin
      Case Protocol Of
        ptSingleUser: Result := TfsSingleUserProtocol;
        ptTCPIP: Result := TfsTCPIPProtocol;
        ptIPXSPX: Result := TfsIPXSPXProtocol;
        Else
          Result := TfsSingleUserProtocol;
      End;
    End
  Else
    FFClientConfigReadProtocol(Result, protName);
End;
{--------}

Function TFSParamConnect.GetName: String;
Begin
  Result := ltMapProtocolToClass.GetProtocolName;
End;
{--------}

Procedure TFSParamConnect.GetServerNames(aList: TStrings;
  Const timeout: Longint);
Var
  OldServerNameRequired: boolean;
  OldState: TfsState;
Begin

  If Not assigned(aList) Then
    Exit;

  OldState := scState;
  OldServerNameRequired := False;

  { If the transport has not been started, temporarily start the transport. }
  If OldState <> fsesStarted Then
    Begin
      OldServerNameRequired := FServerNameRequired;
      FServerNameRequired := False;
      State := fsesStarted;
    End;

  { Note: This is done outside the transport's sender thread.  It should
    not interfere with the sender thread's normal operation. }
{Begin !!.05}
  If Assigned(FProtocol) Then
    FProtocol.GetServerNames(aList, timeout)
  Else
    aList.Clear;
  {End !!.05}

    { Restore transport to original state. }
  If OldState <> fsesStarted Then
    Begin
      State := OldState;
      FServerNameRequired := OldServerNameRequired;
    End;

End;
{--------}

Procedure TFSParamConnect.btInternalReply(msgID: Longint;
  errorCode: TffResult;
  replyData: pointer;
  replyDataLen: Longint);
Begin
  Inherited btInternalReply(msgID, errorCode, replyData, replyDataLen);
  tpSendReply(msgID, fsitvClientID, fsitvRequestID, errorCode,
    replyData, replyDataLen);
End;
{--------}

Function TFSParamConnect.IsConnected: boolean;
Begin
  { We are connected if we are in the right state and there is at least one
    established connection. }
  Result := (scState = fsesStarted) And (FProtocol.ConnectionCount > 0);
End;
{--------}

Procedure TFSParamConnect.lcLog(Const aMsg: String);
Begin
  If FLogEnabled And assigned(FEventLog) And (fstpLogErrors In FLogOptions) Then
    FEventLog.WriteString(aMsg);
End;
{--------}

Procedure TFSParamConnect.lcSetLogEnabled(Const aEnabled: Boolean);
Begin
  Inherited lcSetLogEnabled(aEnabled);
  If (UpdateCount = 0) And assigned(FProtocol) Then
    FProtocol.LogEnabled := aEnabled;
End;
{--------}

Function TFSParamConnect.ltGetProtocol: TfsProtocolType;
Begin
  Result := FProtocolType;
End;
{Begin !!.01}
{--------}
Type
  ProtocolCracker = Class(TfsBaseCommsProtocol);

Procedure TFSParamConnect.ltDoHangup(Const aClientID: TffClientID);
{Rewritten !!.05}
Var
  conn: TfsConnection;
  errorCode: TffResult;
Begin
  conn := ProtocolCracker(FProtocol).cpGetConnection(aClientID);
  If Assigned(conn) And (Not conn.HangupDone) Then
    Begin
      If assigned(FOnRemoveClient) Then
        FOnRemoveClient(Self, aClientID, errorCode)
      Else
        { No handler assigned.  Log an error. }
        lcLogFmt('No RemoveClientHandler for transport %d', [GetName]);
      conn.HangupDone := True;
    End;
End;
{End !!.01}
{--------}

Procedure TFSParamConnect.ltSetProtocol(aProtocol: TfsProtocolType);
Begin
  If (UpdateCount > 0) Then
    _FProtocolType := aProtocol
  Else
    Begin
      {Check to make sure the new property is different.}
      If FProtocolType = aProtocol Then
        Exit;

      {Note: If you ever remove the following requirement, update the Supported
             test at the end of this routine. }
      scCheckInactive;
      FProtocolType := aProtocol;
      If FProtocolType = ptSingleUser Then
        Begin
          FServerNameRequired := False;
          ServerName := fsc_SingleUserServerName;
        End;

      { Is this protocol supported on the workstation? }
      If Supported Then
        scState := fsesInactive
      Else
        scState := fsesUnsupported;
    End;
End;
{Begin !!.05}
{--------}

Procedure TFSParamConnect.ltTerminateThread;
Begin
  If assigned(FTransportThread) Then
    Begin
      FTransportThread.Terminate;
      FTransportThread.WaitForEx(5000);
      FTransportThread.Free;
      FTransportThread := Nil;
    End;
End;
{End !!.05}
{--------}

Procedure TFSParamConnect.btSetRespondToBroadcasts(Const respond: boolean);
Var
  OldValue: boolean;
Begin
  OldValue := FRespondToBroadcasts;
  Inherited btSetRespondToBroadcasts(respond);
  If (OldValue <> FRespondToBroadcasts) And
    (scState = fsesStarted) Then
    If respond Then
      FProtocol.ReceiveDatagram
    Else
      FProtocol.StopReceiveDatagram;
End;
{--------}

Procedure TFSParamConnect.btSetServerName(Const aServerName: String); {!!.10}
Begin
  Inherited btSetServerName(aServerName);
  FSSplitNetAddress(aServerName, FServerLocalName, FServerAddress);
End;
{--------}

Procedure TFSParamConnect.lcSetEventLog(anEventLog: TFSBaseLog);
Begin
  Inherited lcSetEventLog(anEventLog);
  If assigned(FProtocol) Then
    FProtocol.EventLog := anEventLog;
End;
{--------}

Procedure TFSParamConnect.Request(transportID: Longint;
  clientID: TffClientID;
  msgID: Longint;
  timeout: Longint;
  requestData: pointer;
  requestDataLen: Longint;
  replyCallback: TfsReplyCallback;
  replyCookie: Longint);
Var
  aRequest: TfsRequest;

Begin
  scCheckStarted;
  aRequest := TfsRequest.Create(clientID, msgID, requestData, requestDataLen,
    timeout, fsrmReplyExpected);
  tpInternalRequest(aRequest, timeout, FProtocol.NotifyWindow);
  If assigned(replyCallback) Then
    replyCallback(aRequest.ReplyMsgID, aRequest.ErrorCode,
      aRequest.ReplyData, aRequest.ReplyDataLen,
      replyCookie);
  If Not aRequest.Aborted Then
    aRequest.Free
  Else
    With aRequest Do
      tpLogReqMisc(format(fsc_ReqAborted, [Longint(aRequest), ClientID,
        ErrorCode, Timeout]));
End;
{--------}

Procedure TFSParamConnect.scInitialize;
Var
  protClass: TfsCommsProtocolClass;
  whichSideOfTheCoin: TfsClientServerType;
Begin

  { Make sure the old protocol is freed. }
  If assigned(FProtocol) Then
    Begin
      If FProtocol.IsStarted Then
        Begin
          scPrepareForShutdown;
          scShutdown;
        End;
    End;

  { If we are in sending mode then verify we have a target server. }
  If FMode = fstmSend Then
    btCheckServerName;

  { Figure out which type of protocol we are to instantiate. }
  protClass := ltMapProtocolToClass;

  If assigned(FMsgQueue) Then
    Begin
      FMsgQueue.Free;
      FMsgQueue := Nil;
    End;

  { Figure out the protocol's mode. }
  If FMode = fstmListen Then
    whichSideOfTheCoin := csServer
  Else
    whichSideOfTheCoin := csClient;

  FMsgQueue := TffDataMessageQueue.Create;

  FProtocol := protClass.Create(FServerName, whichSideOfTheCoin);
  If FMode = fstmListen Then
    With FProtocol Do
      Begin
        OnConnectionLost := tpRemoteClientHangup;
        OnHangUp := tpRemoteClientHangup;
        OnHeardCall := Nil;
        OnReceiveDatagram := tpDatagramReceived;
        OnReceiveMsg := tpMsgReceived;
      End
  Else
    With FProtocol Do
      Begin
        OnConnectionLost := tpConnectionLost;
        OnHangUp := Nil;
        OnHeardCall := Nil;
        OnReceiveDatagram := Nil;
        OnReceiveMsg := tpMsgReceived;
      End;

  FProtocol.EventLog := FEventLog; {!!.01}
  FProtocol.LogEnabled := FLogEnabled;

  { If we are listening then get our servername from the protocol. }
  If FMode = fstmListen Then
    FServerName := FProtocol.NetName;

  FFGetMem(FSendBuffer, FProtocol.MaxNetMsgSize);

  tpPrepareThread;

End;
{--------}

Procedure TFSParamConnect.scPrepareForShutdown;
{Rewritten !!.05}
Begin
  ltTerminateThread;
End;
{--------}

Procedure TFSParamConnect.scShutdown;
Begin
  Try
    { Note: We can't free the protocol or the thread until we know they have
      finished or a certain number of milliseconds has elapsed. }
    ltTerminateThread; {!!.05}
  Finally

    {Begin !!.03}
    If assigned(FSendBuffer) Then
      Begin
        FFFreeMem(FSendBuffer, FProtocol.MaxNetMsgSize);
        FSendBuffer := Nil;
      End;
    {End !!.03}

    If assigned(FMsgQueue) Then
      Begin
        FMsgQueue.Free;
        FMsgQueue := Nil;
      End;

    If assigned(FProtocol) Then
      Begin
        FProtocol.Free;
        FProtocol := Nil;
      End;

    If assigned(FTransportThread) Then
      { By this time, the transport thread will have freed itself. }
      FTransportThread := Nil;
  End;
End;
{--------}

Procedure TFSParamConnect.scStartup;
Begin
  FTransportThread.Resume;

  { An exception during protocol startup might leave the thread in a terminated
    state.  If the thread is still going, wait for the thread to finish or fail
    startup. }
  If (Not FTransportThread.Terminated) Then
    FProtocol.StartedEvent.WaitFor(fsc_ThreadStartTimeout);

  { If the thread fails then raise an exception. }
  If Not FProtocol.IsStarted Then
    Raise EfsException.CreateEx(fsStrResGeneral, fserrProtStartupFail,
      [(FProtocol As TfsBaseCommsProtocol).GetProtocolName]);

End;
{--------}

Function TFSParamConnect.Supported: boolean;
Begin
  Result := ltMapProtocolToClass.Supported;
End;
{--------}

Procedure TFSParamConnect.TerminateConnection(Const aClientID: TffClientID;
  Const timeout: Longint);
Begin
  {Begin delete !!.05}
    { Post a message to the server stating that we are hanging up. }
  //  Post(0, aClientID, fsnmDetachServer, nil, 0, timeout, ffrmNoReplyWaitUntilSent);
    { After we know the message has been sent, tell the protocol to hangup. }
  {End delete !!.05}
          { Tell the protocol to hangup. }{!!.05}
  If assigned(FProtocol) Then
    FProtocol.HangUpByClientID(aClientID);
End;
{--------}

Procedure TFSParamConnect.tpConnectionLost(aSender: TObject;
  aClientID: TffClientID);
{Begin !!.01}
Var
  anInx: Longint;
  aRequest: TfsRequest;
  {End !!.01}
  RequestFound: Boolean;
Begin
  //  PostMessage(FLostConnWindow, fsm_LostConnection, aClientID, 0);    {Deleted !!.12}
  {Begin !!.01}
    { Abort the request pending for this client. There should be only one pending
      request for the client at any one time. }
  With FWaitingForReplyList.BeginRead Do
    Try
      RequestFound := False; {!!.13}
      For anInx := 0 To pred(Count) Do
        Begin
          aRequest := TfsRequest(TfsIntListItem(Items[anInx]).KeyAsInt);
          If aRequest.ClientID = aClientID Then
            Begin
              RequestFound := True; {!!.13}
              {Begin !!.12}
                        { If the request was something other than to check secure
                          communications (i.e., no password handling involved) then
                          post a message to the lost connection window. }
              If aRequest.MsgID <> fsnmCheckSecureComms Then
                PostMessage(FLostConnWindow, fsm_LostConnection, aClientID, 0);
              {End !!.12}
                        { Mark the request as having lost its connection. }
              aRequest.SetReply(aRequest.MsgID, fserrConnectionLost, Nil, 0, 0);
              { Remove the request's entry from the list. }
              DeleteAt(anInx);
              aRequest.WakeUpThread;
              break;
            End; { if }
        End; { for }
      If Not RequestFound Then {!!.13}
        PostMessage(FLostConnWindow, fsm_LostConnection, aClientID, 0); {!!.13}
    Finally
      EndRead;
    End;
  {End !!.01}
End;
{--------}

Procedure TFSParamConnect.tpDatagramReceived(aSender: TObject;
  Const aName: TffNetName;
  aData: PffByteArray;
  aDataLen: Longint);
Begin
  tpLogReqMisc(format('Rcvd datagram from %s', [aName]));
End;
{--------}

Function TFSParamConnect.tpGetCodeStart(Const aClientID: TffClientID): Integer;
Begin
  Result := FProtocol.GetCodeStart(aClientID);
End;
{--------}

Procedure TFSParamConnect.tpHandleAddClient(aMsg: PFSDataMessage);
Var
  aReply: TfsnmAttachServerRpy;
  aClientID: TffClientID;
  aVersionNumber: Longint;
  errorCode: TffResult;
  isSecure: boolean;
  passwordHash: TffWord32;
  aRights: TffUserRights;
Begin
  If assigned(FOnAddClient) Then
    Begin
      FOnAddClient(Self, PfsnmAttachServerReq(aMsg^.dmData)^.userID,
        PfsnmAttachServerReq(aMsg^.dmData)^.timeout,
        PfsnmAttachServerReq(aMsg^.dmData)^.clientVersion,
        passwordHash, aClientID, errorCode, isSecure, aVersionNumber, aRights);

      { Build a reply. }
      aReply.ClientID := aClientID;
      aReply.VersionNumber := aVersionNumber;
      aReply.Rights := aRights;
      {Begin !!.05}
      FProtocol.ConnLock;
      Try
        If isSecure Then
          aReply.Code := TffWord32(tpGetCodeStart(aMsg^.dmClientID)) Xor passwordHash
        Else
          aReply.Code := tpGetCodeStart(aMsg^.dmClientID);
        aReply.IsSecure := isSecure;
        aReply.LastMsgIntvl := fsc_LastMsgInterval;
        aReply.KAIntvl := fsc_KeepAliveInterval;
        aReply.KARetries := fsc_KeepAliveRetries;

        { Send the reply. }
        Reply(aMsg^.dmMsg, errorCode, @aReply, sizeOf(TfsnmAttachServerRpy));

        { Update the clientID maintained by the protocol. }
        If errorCode = DBIERR_NONE Then
          FProtocol.UpdateClientID(aMsg^.dmClientID, aClientID);
      Finally
        FProtocol.ConnUnlock;
      End;
      {End !!.05}

    End
  Else
    { No handler assigned.  Send back an error. }
    Reply(aMsg^.dmMsg, DBIERR_FS_NoAddHandler, Nil, 0);

  { Free the message data. }{!!.01}
  ltFreeMsg(aMsg); {!!.01}
End;
{--------}

Procedure TFSParamConnect.tpHandleNextRequest;
Var
  anItem: TfsIntListItem;
  aRequest: TfsRequest;
  Status: TffResult;
Begin

  { Any messages in unsent request queue? Note that we don't care about
    thread-safeness to check the count. We want to improve performance
    by locking the queue only when necessary. If something slips into the
    queue, we will run through this loop again very soon and pick it up
    at that point. }
  anItem := Nil;
  If FUnsentRequestQueue.Count > 0 Then
    { Yes. Grab one. }
    With FUnsentRequestQueue.BeginWrite Do
      Try
        anItem := TfsIntListItem(FUnsentRequestQueue.Dequeue);
      Finally
        EndWrite;
      End;

  If assigned(anItem) Then
    Begin
      aRequest := TfsRequest(anItem.KeyAsInt);
      anItem.Free;
      { If this request has already timed out then ignore it. }
      If aRequest.Aborted Then
        aRequest.Free
      Else
        Begin
          { Otherwise send the request. }
          Status := tpSendRequest(aRequest);
          If (Status <> DBIERR_NONE) And
            (aRequest.ReplyMode = fsrmReplyExpected) Then
            Begin
              aRequest.ErrorCode := Status;
              aRequest.WakeUpThread;
            End;
        End;
    End;

End;
{--------}

Procedure TFSParamConnect.tpHandleRemoveClient(aMsg: PFSDataMessage);
//var                                                                  {Deleted !!.01}
//  errorCode : TffResult;                                             {Deleted !!.01}
Begin
  {Begin !!.01}
  ltDoHangup(aMsg^.dmClientID);
  //  if assigned(FOnRemoveClient) then
  //    FOnRemoveClient(Self, aMsg^.dmClientID, errorCode)
  //  else
      { No handler assigned.  Log an error. }
  //    lcLogFmt(('No RemoveClientHandler for transport %d', [GetName]));
  {End !!.01}
          { Free the message data. }{!!.01}
  ltFreeMsg(aMsg); {!!.01}
End;
{--------}

Procedure TFSParamConnect.tpInternalRequest(aRequest: TfsRequest;
  timeout: Longint;
  aCookie: HWND);
Var
  anItem: TfsIntListItem;
Begin
  aRequest.EventLog := FEventLog;
  anItem := TfsIntListItem.Create(Longint(aRequest));
  anItem.MaintainLinks := False; {!!.01}
  With FUnsentRequestQueue.BeginWrite Do
    Try
      Enqueue(anItem);
    Finally
      EndWrite;
    End;

  If (aCookie <> 0) And IsWindow(aCookie) Then
    PostMessage(aCookie, 0, 0, 0);

  { Wait for the reply.  If a timeout occurs, assume the request object
    will be freed by the transport thread at some point.  Timeout exceptions
    are raised to the calling object. }
  If timeout = 0 Then
    aRequest.WaitForReply(timeout)
  Else
    aRequest.WaitForReply(timeout + ffcl_RequestLatencyAdjustment);

End;
{--------}

Function TFSParamConnect.tpMsgReceived(aSender: TObject;
  clientID: TffClientID;
  msgData: PffByteArray;
  msgDataLen: Longint): boolean;
Var
  MsgHeader: PfsnmHeader Absolute msgData;
Begin
  //  Result := False;                                                   {!!.01}
  Case MsgHeader^.nmhMsgType Of
    fsmtRequest:
      Result := tpRequestReceived(aSender, clientID, msgData, msgDataLen);
    fsmtReply:
      Result := tpReplyReceived(aSender, clientID, msgData, msgDataLen);
    {Begin !!.01}
    Else
      Begin
        lcLogFmt(fsc_ErrMsgType,
          [MsgHeader^.nmhMsgType, MsgHeader^.nmhClientID,
          MsgHeader^.nmhMsgID]);
        { Pass it on to tpRequestReceived as this may just be the result of a
          user entering a bad password. }
        Result := tpRequestReceived(aSender, clientID, msgData, msgDataLen);
      End;
  End; { case }
  {End !!.01}
End;
{--------}

Procedure TFSParamConnect.tpPrepareThread;
Begin
  If assigned(FTransportThread) Then
    FTransportThread.Free;
  FTransportThread := TFSParamConnectThread.Create(Self);
  FTransportThread.FreeOnTerminate := False;
  FTransportThread.OnTerminate := tpThreadTerminated;
End;
{--------}

Procedure TFSParamConnect.tpProcessCallback(Const aProcessCookie: Longint);
Var
  conn: TfsConnection;
  msg: PFSDataMessage;
Begin

  btStoreSelfInThreadvar;

  {Begin !!.05}
  msg := PFSDataMessage(aProcessCookie);
  conn := ProtocolCracker(FProtocol).cpGetConnection(msg^.dmClientID);
  If conn <> Nil Then
    Begin
      conn.HangupLock;
      Try
        { Save off some data for when we reply. }
        fsitvClientID := msg^.dmClientID;
        fsitvRequestID := msg^.dmRequestID;

        { Is this a request to add a client? }
        If (msg^.dmMsg = fsnmAttachServer) Then
          Begin
            tpHandleAddClient(msg)
              { Remove a client? }
          End
        Else If (msg^.dmMsg = fsnmDetachServer) Then
          Begin
            tpHandleRemoveClient(msg)
          End
        Else
          { None of the above.  Call our Process method which will pass the message
            onto the appropriate command handlers. }
          Process(msg)
      Finally
        conn.HangupUnlock;
      End;
    End;
  {End !!.05}
End;
{--------}

Procedure TFSParamConnect.tpRemoteClientHangup(aSender: TObject;
  aClientID: TffClientID);
//var                                                                  {Deleted !!.01}
//  errorCode : TffResult;                                             {Deleted !!.01}
Begin
  { As a just in case, make sure the client is removed. }
{Begin !!.01}
  ltDoHangup(aClientID);
  //  if assigned(FOnRemoveClient) then
  //    FOnRemoveClient(Self, aClientID, errorCode);
  {End !!.01}
End;
{--------}

Function TFSParamConnect.tpReplyReceived(aSender: TObject;
  clientID: TffClientID;
  replyData: PffByteArray;
  replyDataLen: Longint): boolean;
Var
  msgHeader: PfsnmHeader Absolute replyData;
  anItem: TfsIntListItem;
  aRequest: TfsRequest;
Begin
  Result := True;

  { Find the request. }
  With FWaitingForReplyList.BeginRead Do
    Try
      anItem := TfsIntListItem
        (FWaitingForReplyList
        [FWaitingForReplyList.Index(msgHeader^.nmhRequestID)]);
    Finally
      EndRead;
    End;

  { Did we find the request?  If so then set its reply data.

    If we did not find the request then the requesting thread timed out
    and we can just toss the reply into the bitbucket. }
  If assigned(anItem) Then
    Begin
      aRequest := TfsRequest(anItem.keyAsInt);

      With msgHeader^ Do
        If msgHeader^.nmhFirstPart Then
          aRequest.SetReply(nmhMsgID, nmhErrorCode, @nmhData, nmhTotalSize,
            replyDataLen - fsc_NetMsgHeaderSize)
        Else
          aRequest.AddToReply(@nmhData, replyDataLen - fsc_NetMsgHeaderSize);

      { If this is the last part of the message then remove the request from
        the waiting list. }
      If msgHeader^.nmhLastPart Then
        Begin
          With FWaitingForReplyList.BeginWrite Do
            Try
              Delete(Longint(msgHeader^.nmhRequestID));
            Finally
              EndWrite;
            End;
          { If the request has been aborted then get rid of it otherwise
            wake up the requesting thread. }
          If aRequest.Aborted Then
            aRequest.Free
          Else
            Begin
              tpLogReply(aRequest);
              aRequest.WakeUpThread;
            End;
        End;

    End
  Else
    Begin
      lcLogFmt('Could not find Request %d, msgID %d',
        [msgHeader^.nmhRequestID, msgHeader^.nmhMsgID]);
    End;

End;
{--------}

Function TFSParamConnect.tpRequestReceived(aSender: TObject;
  clientID: TffClientID;
  requestData: PffByteArray;
  requestDataLen: Longint): boolean;
Var
  MsgHeader: PfsnmHeader Absolute requestData;
  MsgNode: PffDataMessageNode;
Begin
  Result := True;

  With MsgHeader^ Do
    Begin

      { Verify the client id in the message is correct.  If not then either
        we have a fake client using the wrong encryption or something else is
        goofed up.  Hangup. }
      If (nmhMsgID <> fsnmAttachServer) And
        (clientID <> nmhClientID) Then
        Begin

          If FLogEnabled And assigned(FEventLog) And
            (fstpLogErrors In FLogOptions) Then
            FEventLog.WriteStrings(['Hanging up due to bad client password',
              Format('  ClientID (actual) %d', [clientID]),
                Format('  ClientID (msg)    %d', [nmhClientID]),
                Format('  MsgID             %d', [nmhMsgID])]);

          FProtocol.HangUpByClientID(clientID);
          Result := False;
          Exit;
        End;

      If FLogEnabled And assigned(FEventLog) And
        (fstpLogRequests In FLogOptions) Then
        tpLogReq2(fsc_Request, nmhRequestID, clientID, nmhMsgID, @nmhData,
          requestDataLen - fsc_NetMsgHeaderSize, nmhTimeout);

      With FMsgQueue.BeginWrite Do
        Try
          If nmhFirstPart Then
            MsgNode := Append(nmhMsgID,
              clientID,
              nmhRequestID,
              nmhTimeout,
              0,
              @nmhData,
              requestDataLen - fsc_NetMsgHeaderSize,
              nmhTotalSize)
          Else
            MsgNode := AddToData(nmhMsgID,
              clientID,
              nmhRequestID,
              @nmhData,
              requestDataLen - fsc_NetMsgHeaderSize);
        Finally
          EndWrite;
        End;

      { Is this the last part of the message? }
      If assigned(MsgNode) Then
        Begin
          { Yes. Do we have a thead pool? }
          If assigned(FThreadPool) Then
            { Yes.  Pass this request off to a worker thread. }
            FThreadPool.ProcessThreaded(tpProcessCallback, Longint(MsgNode^.dmnMsg))
          Else
            { No.  Handle this ourselves. }
            tpProcessCallback(Longint(MsgNode^.dmnMsg));

          { Get rid of the request on the message queue. }
          With FMsgQueue.BeginWrite Do
            Try
              Remove(MsgNode, False);
            Finally
              EndWrite;
            End;
        End;
    End; { with }
End;
{--------}

Function TFSParamConnect.tpSendReply(msgID: Longint;
  clientID: TffClientID;
  requestID: Longint;
  errorCode: TffResult;
  replyData: pointer;
  replyDataLen: Longint): TffResult;
Var
  BytesToGo: Longint;
  BytesToSend: Longint;
  FirstMsg: boolean;
  LastMsg: boolean;
  StartOffset: Longint;
  SendBuffer: PfsnmHeader;
Begin

  Try

    FFGetMem(SendBuffer, FProtocol.MaxNetMsgSize);

    Try
      { Set up the message header. }
      With SendBuffer^ Do
        Begin
          nmhMsgType := fsmtReply;
          nmhMsgID := msgID;
          nmhTotalSize := replyDataLen;
          nmhClientID := clientID;
          nmhRequestID := requestID;
          nmhErrorCode := errorCode;
          nmhTimeout := 0;
        End;

      StartOffset := 0;
      BytesToGo := replyDataLen;
      FirstMsg := True;

      { Send data in reasonably-sized chunks }
      Repeat
        { Calculate the size of the data to send in this message packet. }
        BytesToSend := FFMinL(BytesToGo,
          FProtocol.MaxNetMsgSize - fsc_NetMsgHeaderSize);
        LastMsg := (BytesToSend = BytesToGo);
        With SendBuffer^ Do
          Begin
            nmhMsgLen := fsc_NetMsgHeaderSize + BytesToSend;
            nmhFirstPart := FirstMsg;
            nmhLastPart := LastMsg;
          End;

        { Copy the data into the send buffer. }
        If (BytesToSend > 0) Then
          Move(PffBLOBArray(replyData)^[StartOffset],
            SendBuffer^.nmhData, BytesToSend);

        { Send the packet. }
        Result := FProtocol.SendMsg(clientID, PffByteArray(SendBuffer),
          SendBuffer^.nmhMsgLen, True); {!!.06}

        { Do we need to get an acknowledgement? }
        If Not LastMsg Then
          Begin
            { Update bytes sent, etc. }
            dec(BytesToGo, BytesToSend);
            inc(StartOffset, BytesToSend);
            FirstMsg := False;
          End;

      Until LastMsg Or (Result <> DBIERR_NONE);
      {Moved !!.06}
      {Begin !!.10}
      If Result <> DBIERR_NONE Then
        lcLogFmt(fsc_SendErr,
          [Result, 'tpSendReply', -1, clientID, msgID, replyDataLen, 0])
      Else If FLogEnabled And assigned(FEventLog) And
        {End !!.10}
      (fstpLogReplies In FLogOptions) Then
        tpLogReply2(requestID, clientID, msgID, replyDataLen, errorCode);
    Finally
      FFFreeMem(SendBuffer, FProtocol.MaxNetMsgSize);
    End;
  Except
    On E: EfsWinsockException Do
      Begin
        Result := fserrTransportFail;
        lcLogFmt('Transport failure %d: %s', [E.ErrorCode, E.Message]);
      End;
    On E: EfsException Do
      Result := E.ErrorCode;
  End;
End;
{--------}

Function TFSParamConnect.tpSendRequest(aRequest: TfsRequest): TffResult;
Var
  aClientID: TffClientID;
  anItem: TfsIntListItem;
  BytesToSend: Longint;
  FirstMsg: boolean;
  LastMsg: boolean;
  CallRpy: PfsnmCallServerRpy;
Const
  logPrefixArray: Array[TfsReplyModeType] Of String = (fsc_Request,
    fsc_Post,
    fsc_PostWait);
Begin

  Result := DBIERR_NONE;
  anItem := Nil;

  Try

    tpLogReq(aRequest, logPrefixArray[aRequest.ReplyMode]);

    { Is this a "call server" request or a regular request?  }
    If aRequest.MsgID = fsnmCallServer Then
      Begin
        {Begin !!.05}
        tpLogReqMisc(Format(fsc_ReqLogString,
          [fsc_Request, Longint(aRequest), aRequest.ClientID,
          aRequest.MsgID, aRequest.RequestDataLen,
            aRequest.Timeout]));
        {End !!.05}
        aRequest.ErrorCode :=
          FProtocol.Call(PfsnmCallServerReq(aRequest.RequestData).ServerName,
          aClientID, aRequest.Timeout);
        FFGetMem(CallRpy, SizeOf(TfsnmCallServerRpy));
        CallRpy^.ClientID := aClientID;
        aRequest.ReplyData := CallRpy;
        aRequest.ReplyDataLen := SizeOf(TfsnmCallServerRpy);
        aRequest.ReplyMsgID := fsnmCallServer;
        {Begin !!.05}
        If FLogEnabled And (fstpLogReplies In FLogOptions) And
          (FEventLog <> Nil) Then
          With aRequest Do
            FEventLog.WriteString(Format(fsc_ReplyLogString,
              [Longint(aRequest), ClientID,
              ReplyMsgID, ReplyDataLen, ErrorCode]));
        {End !!.05}
        aRequest.WakeUpThread;
        anItem.Free;
        Exit;
      End;

    { Set up the message header. }
    With FSendBuffer^ Do
      Begin
        nmhMsgType := fsmtRequest;
        nmhMsgID := aRequest.MsgID;
        nmhTotalSize := aRequest.RequestDataLen;
        nmhClientID := aRequest.ClientID;
        nmhRequestID := Longint(aRequest);
        nmhErrorCode := 0;
        nmhTimeout := aRequest.Timeout;
      End;

    FirstMsg := (aRequest.StartOffset = 0);

    { Obtain exclusive write access.  This is required because a reply
      may be received from the server before an iteration of this repeat..until
      block completes.  In that situation, we want to make sure this method
      finishes before the TfsRequest is freed.

      The corresponding call to TfsRequest.Lock is in the TfsRequest.Destroy
      method. }
    aRequest.Lock;

    Try

      { Send data in reasonably-sized chunks }
      Repeat

        { Calculate the size of the data to send in this message packet. }
        BytesToSend := FFMinL(aRequest.BytesToGo,
          FProtocol.MaxNetMsgSize - fsc_NetMsgHeaderSize);
        LastMsg := (BytesToSend = aRequest.BytesToGo);
        With FSendBuffer^ Do
          Begin
            nmhMsgLen := fsc_NetMsgHeaderSize + BytesToSend;
            nmhFirstPart := FirstMsg;
            nmhLastPart := LastMsg;
          End;

        { Copy the data into the send buffer. }
        If (BytesToSend > 0) Then
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
        If FirstMsg And (aRequest.ReplyMode = fsrmReplyExpected) Then
          With FWaitingForReplyList.BeginWrite Do
            Try
              anItem := TfsIntListItem.Create(Longint(aRequest));
              Insert(anItem);
            Finally
              EndWrite;
            End;

        { Send the packet. }
        Result := FProtocol.SendMsg(aRequest.ClientID, PffByteArray(FSendBuffer),
          FSendBuffer^.nmhMsgLen, True); {!!.06}

        { If the send failed & we were expecting a reply, take the request out
          of the Waiting For Reply list because no reply is forthcoming. }
        If (Result <> DBIERR_NONE) And
          (aRequest.ReplyMode = fsrmReplyExpected) Then
          Begin {!!.03}
            aRequest.ReplyMsgID := aRequest.MsgID; {!!.03}
            With FWaitingForReplyList.BeginWrite Do
              Try
                Delete(Longint(aRequest));
              Finally
                EndWrite;
              End;
          End; {!!.03}

        FirstMsg := False; {!!.01}

      Until LastMsg Or (Result <> DBIERR_NONE);

      If Result <> DBIERR_NONE Then
        lcLogFmt(fsc_SendErr,
          [Result, 'tpSendRequest', -1, aRequest.ClientID,
          aRequest.MsgID, aRequest.RequestDataLen, aRequest.Timeout]);

      { Is the requesting thread waiting for the request to be sent to the
        server but not wanting a reply? }
      If aRequest.ReplyMode = fsrmNoReplyWaitUntilSent Then
        Begin
          { Yes.  Was the request aborted by the requesting thread
            (i.e, timeout)? }
          If aRequest.Aborted Then
            Begin
              { Yes.  Free the request. }
              aRequest.Unlock;
              aRequest.Free;
              aRequest := Nil;
            End
          Else
            { No.  Signal the requesting thread. }
            aRequest.WakeUpThread;
        End;

    Finally
      If assigned(aRequest) Then
        Begin
          aRequest.Unlock;
          If aRequest.ReplyMode = fsrmNoReplyExpected Then
            aRequest.Free;
        End;
    End;
  Except
    On E: Exception Do
      Begin
        { Free the list item. }
        If assigned(anItem) Then
          anItem.Free;

        { Handle the exception. }
        If E Is EfsWinsockException Then
          Begin
            Result := fserrTransportFail;
            lcLogFmt('Transport failure %d: %s',
              [EfsWinsockException(E).ErrorCode, E.Message]);
          End
        Else If E Is EfsException Then
          Begin
            Result := EfsException(E).ErrorCode;
            lcLogFmt('tpSendRequest exception %d: %s',
              [EfsException(E).ErrorCode, E.Message]);
          End
        Else
          Begin
            Result := fserrTransportFail;
            lcLogFmt('tpSendRequest general exception %s:', [E.Message]);
          End;
      End;
  End;

End;
{--------}

Procedure TFSParamConnect.tpShutdownProtocol;
Begin
  If FLogEnabled And assigned(FEventLog) And
    ((fstpLogRequests In FLogOptions) Or
    (fstpLogReplies In FLogOptions)) Then
    tpLogReqMisc(format('Transport thread (%s) shut down.', [GetName]));
  FProtocol.Shutdown;
End;
{--------}

Procedure TFSParamConnect.tpStartProtocol;
Begin
  FProtocol.Startup;

  { If we are to listen for broadcasts then set up to receive datagrams. }
  //if (FMode = fftmListen) and FRespondToBroadcasts then begin        {!!.05 - Start}
  //  FProtocol.ReceiveDatagram;
  //  FProtocol.Listen;
  //end; }
  If (FMode = fstmListen) Then
    Begin
      FProtocol.Listen;
      If (FRespondToBroadcasts) Then
        FProtocol.ReceiveDatagram;
    End; {!!.05 - End}
End;
{--------}

Procedure TFSParamConnect.tpThreadTerminated(Sender: TObject);
Begin
  If TFSParamConnectThread(Sender).UnexpectedTermination Then
    Begin
      { The thread has shutdown prematurely.  Log the event and restart
        the thread. }
      If assigned(FProtocol) Then
        lcLogFmt('Transport thread (%s) prematurely stopped.', [GetName]);
      tpPrepareThread;
      FTransportThread.Resume;
    End;
End;
{--------}

Procedure TFSParamConnect.Work;
Begin

  { Legacy transports can both send and receive messages
    (i.e., bi-directional). }

  { Give the protocol a chance to receive requests. }
  FProtocol.Breathe;

  { Give the protocol a chance to send a request. }
  tpHandleNextRequest;

End;
{====================================================================}

{===TFSParamConnectThread=========================================}

Constructor TFSParamConnectThread.Create(aTransport: TFSParamConnect);
Begin
  Inherited Create(True);
  FTransport := aTransport;
  FUnexpectedTermination := False;
End;
{--------}

Procedure TFSParamConnectThread.Execute;
Begin
  Try
    FTransport.tpStartProtocol;
    Repeat
      Try
        FTransport.Work;
      Except
        On E: Exception Do
          FTransport.lcLog
            (format('Transport thread (%s) error: %s',
            [FTransport.GetName, E.message]));
      End;
    Until Terminated;
    FTransport.tpShutdownProtocol;
  Except
    On E: Exception Do
      Begin
        { Signal the primary thread so that it can see our failure to start. }
        FTransport.FProtocol.StartedEvent.SignalEvent;
        FTransport.lcLog
          (format('Transport thread startup (%s) error: %s',
          [FTransport.GetName, E.message]));
      End;
  End;
End;
{====================================================================}
End.

