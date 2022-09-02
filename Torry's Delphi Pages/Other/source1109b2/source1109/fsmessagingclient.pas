Unit fsmessagingclient;

Interface

{
  IMPORTANT NOTE!  If you don't read this, you'll be sorry!

  This plugin is not a normal plugin.  Normally, client-side plugins initiate
  all communication with the server.  If the plugin expects a response, it waits
  for it.  In contrast, this plugin gets messages out of the blue without
  initiating communication.  This is why a client-side command handler is
  needed.  If you already have a flashfiler plugin that requires a client-side
  command handler, you will have a problem because the FlashFiler client-side
  code doesn't have anything to route messages to custom command handlers like
  the server-side code does.  Combining the two command handlers shouldn't be
  too hard though.

  Here are the components you need to use the client-side messaging plugin:

  - TFSRemoteMessagingServer.  Set Session property to your session.

  - TFSRemoteMessagingHandler.  Set PluginEngine property to your plugin.

  - TffLegacyTransport.  Set CommandHandler to your TFSRemoteMessagingHandler
    Hook up your remote server engine to this transport too.

  On startup, activate the flashfiler session like usual to connect to a
  flashfiler server.  Then call TFSRemoteMessagingServer.AddMsgClient to
  "log on" to the messaging system.

  Use TFSRemoteMessagingServer.SendData to send messages to other clients.

  There are two ways to be notified of received messages.  One method is to
  register message handling procedures using RegisterProcessMessageProc.  When
  a message is received, all the registered procedures are called.  This is
  useful if different units in your application handle messages with specific
  id's.  The other method is to use the OnProcessMessage event.  If none of the
  registered procedures set Handled to true, the OnProcessMessage event is
  called.
}

Uses
  Windows,
  Classes,
  fsllcomm,
  fsnetmsg,
  fsclPlug,
  fsllbase,
  Messages,
  fsmessagingcommon,
  Forms;

Const
  cm_TIncomingMessage = wm_user + 4000;

Type
  {
  A note about the flow of incoming messages:

  1. A message comes into TFSRemoteMessagingHandler.Process.  This
     procedure calls Dispatch.
  2. TFSRemoteMessagingHandler.nmBroadcast gets the message and calls
     FPluginEngine.ProcessReceivedMessage
  3. TFSRemoteMessagingServer.ProcessReceivedMessage copies the message into
     the FReceivedMessages queue and posts the a message to its window handle.
  4. TFSRemoteMessagingHandler.Process frees the message and returns
     control to the calling thread.

  Note: The reason we post a message to the window handle is so that we don't
        call the event in the context of one of FlashFiler's threads.

  5. The window message that was posted to the window handle comes back into
     msgIncomingBroadcast which calls CallProcessMessageProcs.
  6. TFSRemoteMessagingServer.CallProcessMessageProcs dequeues the received
     message and calls any registered message handling procedures.  If none
     of these procedures handle the message, the OnProcessMessage event is
     called.
  }

  TReceivedMessage = Class(TfsSelfListItem)
  Public
    MsgID: Longint;
    SenderClientID: TffClientID;
    DataLength: Longint;
    Data: Pointer;
    Constructor Create(AMsgID: Longint; ASenderClientID: TffClientID;
      ADataLength: Longint; AData: Pointer);
    Destructor Destroy; Override;
  End;

  TReceivedMessageList = Class(TFSSpecThreadList)
  Private
    Function GetItem(Index: Integer): TReceivedMessage;
    Procedure SetItem(Index: Integer; Const Value: TReceivedMessage);
  Public
    Property Items[Index: Integer]: TReceivedMessage Read GetItem Write SetItem; Default;
  End;

  TProcessMessageProc = Procedure(ReceivedMessage: TReceivedMessage; Var Handled: Boolean) Of Object;
  TProcessMessageEvent = Procedure(Sender: TObject; ReceivedMessage: TReceivedMessage) Of Object;

  { This is the client-side class that implements a remote interface to the
    actual plugin engine.
    This plugin has its own window handle.  This lets us call the callbacks in
    the context of the main application thread instead of in the context of the
    transports' threads. }
  TFSRemoteMessagingServer = Class(TFSClientPluginEngine)
  Private
    FRegisteredProcs: Array Of TProcessMessageProc;
    FReceivedMessages: TfsThreadQueue;
    FHWnd: HWND;
    FOnProcessMessage: TProcessMessageEvent;
    Procedure PluginMsgHandler(Var Message: TMessage);
    Procedure msgIncomingBroadcast(Var Message: TMessage); Message cm_TIncomingMessage;

    Function DequeueReceivedMessage: TReceivedMessage;
    Procedure CallProcessMessageProcs;
  Protected
    Function QueryInterface(Const IID: TGUID; Out Obj): HResult; Override;
    Function _AddRef: Integer; Stdcall;
    Function _Release: Integer; Stdcall;

    Procedure scInitialize; Override;
    Procedure scPrepareForShutdown; Override;
    Procedure scShutdown; Override;
    Procedure scStartup; Override;

    Procedure LogEvent(AString: String);
  Public
    Constructor Create(aOwner: TComponent); Override;
    Destructor Destroy; Override;

    Function SendData(MsgID: Integer; Data: Pointer; DataLength: Integer; RecipientClientID: TffClientID): TffResult;
    Procedure ProcessReceivedMessage(MsgID: Integer; Data: Pointer;
      DataLength: Integer; SenderClientID, RecipientClientID: TffClientID);
    Function AddMsgClient(Description: String; PromiscuousMode: Boolean): TffResult;
    Function RemoveMsgClient: TffResult;
    Function GetMsgClients(AList: TRegMsgClients): TffResult;

    Procedure RegisterProcessMessageProc(Proc: TProcessMessageProc);
    Procedure UnRegisterProcessMessageProc(Proc: TProcessMessageProc);
  Published
    Property OnProcessMessage: TProcessMessageEvent Read FOnProcessMessage Write FOnProcessMessage;
  End;

  TFSRemoteMessagingHandler = Class(TFSBaseCommandHandler)
  Private
    FPluginEngine: TFSRemoteMessagingServer;
    Procedure SetPluginEngine(Const Value: TFSRemoteMessagingServer);
  Protected
    Procedure nmBroadcast(Var Msg: TfsDataMessage); Message fsnmSendMessageData;
    Procedure Notification(AComponent: TComponent; Operation: TOperation); Override;

    Procedure scInitialize; Override;
    Procedure scPrepareForShutdown; Override;
    Procedure scShutdown; Override;
    Procedure scStartup; Override;
  Public
    Procedure Process(Msg: PfsDataMessage); Override;
  Published
    Property PluginEngine: TFSRemoteMessagingServer Read FPluginEngine Write SetPluginEngine;
  End;

Procedure Register;

Implementation

Uses SysUtils,
  fssrbde;

Procedure Register;
Begin
  RegisterComponents('FSSQL Utils',[TFSRemoteMessagingServer, TFSRemoteMessagingHandler]);
End;

{ TFSRemoteMessagingServer }

Procedure TFSRemoteMessagingServer.scInitialize;
Begin
  // do nothing
End;

Procedure TFSRemoteMessagingServer.scPrepareForShutdown;
Begin
  // do nothing
End;

Procedure TFSRemoteMessagingServer.scShutdown;
Begin
  // do nothing
End;

Procedure TFSRemoteMessagingServer.scStartup;
Begin
  // do nothing
End;

Procedure TFSRemoteMessagingServer.LogEvent(AString: String);
Begin
  If Assigned(FEventLog) Then
    FEventLog.WriteString(AString);
End;

Function TFSRemoteMessagingServer.SendData(MsgID: Integer;
  Data: Pointer;
  DataLength: Integer;
  RecipientClientID: TffClientID): TffResult;
// If RecipientClientID = 0, the message will be broadcast to all clients.
Var
  ReqLen: Integer;
  Request: PfsnmSendMessageDataReq;
Begin
  LogEvent('Sending Broadcast');
  ReqLen := SizeOf(TfsnmSendMessageDataReq) - SizeOf(TfsVarMsgField) +
    DataLength + 1; // do we really need + 1?

  FFGetZeroMem(Request, ReqLen);
  Try
    Request^.MsgID := MsgID;
    Request^.DataLength := DataLength;
    Request^.ClientID := RecipientClientID;
    If DataLength > 0 Then
      Move(Data^, Request^.Data, DataLength);
    Result := ProcessRequestNoReply(fsnmSendMessageData, FTimeout, Request, ReqLen);
  Finally
    FFFreeMem(Request, ReqLen);
  End;
End;

Function TFSRemoteMessagingServer.AddMsgClient(Description: String; PromiscuousMode: Boolean): TffResult;
// If PromiscuousMode is true, you will get all the messages, no matter who
// they're for.  This would be useful to implement a debugging tool that simply
// spits out all the messages in a window.  This tool would act like a packet
// sniffer, so I borrowed the term Promiscuous from Ethernet nomenclature.
// If we have powerful debugging tools at our disposal, we'll never get any
// bugs!

Var
  Request: TfsnmAddMsgClientReq;
Begin
  Request.PromiscuousMode := PromiscuousMode;
  Request.Description := Description;
  Result := ProcessRequestNoReply(fsnmAddMsgClient, FTimeout, @Request, SizeOf(Request));
End;

Function TFSRemoteMessagingServer.RemoveMsgClient: TffResult;
Begin
  Result := ProcessRequestNoReply(fsnmRemoveMsgClient, FTimeout, Nil, 0);
End;

Procedure TFSRemoteMessagingServer.ProcessReceivedMessage(MsgID: Integer;
  Data: Pointer; DataLength: Integer; SenderClientID, RecipientClientID: TffClientID);
Var
  anItem: TReceivedMessage;
Begin
  // enqueue the item:
  anItem := TReceivedMessage.Create(MsgID, SenderClientID, DataLength, Data);
  With FReceivedMessages.BeginWrite Do
    Try
      Enqueue(anItem);
    Finally
      EndWrite;
    End;

  // Post a message to our window handle.  This way, we don't call the events
  // in the context of FlashFiler's thread.  Also, we can return control to the
  // calling thread right away.
  Windows.PostMessage(FHWnd, cm_TIncomingMessage, 0, 0);
End;

Constructor TFSRemoteMessagingServer.Create(aOwner: TComponent);
Begin
  Inherited;
  FReceivedMessages := TfsThreadQueue.Create;
  SetLength(FRegisteredProcs, 0);
  FHWnd := AllocateHWnd(PluginMsgHandler);
End;

Destructor TFSRemoteMessagingServer.Destroy;
Begin
  DeallocateHWnd(FHWnd);
  FReceivedMessages.Free;
  Inherited;
End;

Function TFSRemoteMessagingServer.DequeueReceivedMessage: TReceivedMessage;
//  NOTE: Caller is responsable for freeing result!!!!!!!
//        If queue is empty, result will be nil.
Begin
  FReceivedMessages.BeginRead;
  Try
    Result := TReceivedMessage(FReceivedMessages.Dequeue);
  Finally
    FReceivedMessages.EndRead;
  End;
End;

Function TFSRemoteMessagingServer.GetMsgClients(AList: TRegMsgClients): TffResult;
Var
  AStream: TMemoryStream;
  ReplyLen: Integer;
Begin
  AStream := TMemoryStream.Create;
  Try
    Result := ProcessRequest(fsnmGetMsgClients,
      FTimeout,
      Nil,
      0,
      nmdByteArray,
      Pointer(AStream),
      ReplyLen,
      nmdStream);

    If Result = DBIERR_NONE Then
      Begin
        AStream.Seek(0, soFromBeginning);
        AList.LoadFromStream(AStream);
      End;

  Finally
    AStream.Free;
  End;
End;

Function TFSRemoteMessagingServer._AddRef: Integer;
Begin
  Result := -1;
End;

Function TFSRemoteMessagingServer._Release: Integer;
Begin
  Result := -1;
End;

Function TFSRemoteMessagingServer.QueryInterface(Const IID: TGUID;
  Out Obj): HResult;
Begin
  If GetInterface(IID, Obj) Then
    Result := 0
  Else
    Result := Windows.E_NoInterface;
End;

Procedure TFSRemoteMessagingServer.RegisterProcessMessageProc(
  Proc: TProcessMessageProc);
Var
  Index: Integer;
Begin
  // put this new procedure at the end of the list.
  SetLength(FRegisteredProcs, Length(FRegisteredProcs) + 1);
  For Index := Length(FRegisteredProcs) - 1 Downto 1 Do
    FRegisteredProcs[Index] := FRegisteredProcs[Index - 1];
  FRegisteredProcs[0] := Proc
End;

Procedure TFSRemoteMessagingServer.UnRegisterProcessMessageProc(
  Proc: TProcessMessageProc);
Var
  Index: Integer;
  ProcIndex: Integer;
Type
  // A procedure of object is an 8 byte type made up of two pointers.
  // That makes them kind of tricky to deal with in a list.
  T2PointerRec = Record
    Pointer1: Pointer;
    Pointer2: Pointer;
  End;
Begin
  ProcIndex := -1;
  Index := 0;

  While (Index < Length(FRegisteredProcs)) And (ProcIndex = -1) Do
    Begin
      If (T2PointerRec(Proc).Pointer1 = T2PointerRec(FRegisteredProcs[Index]).Pointer1) And
        (T2PointerRec(Proc).Pointer2 = T2PointerRec(FRegisteredProcs[Index]).Pointer2) Then
        ProcIndex := Index;
      Inc(Index);
    End;

  If ProcIndex = -1 Then
    Exit;

  If Length(FRegisteredProcs) > 1 Then
    For Index := ProcIndex + 1 To Length(FRegisteredProcs) - 1 Do
      Begin
        FRegisteredProcs[Index - 1] := FRegisteredProcs[Index];
      End;

  SetLength(FRegisteredProcs, Length(FRegisteredProcs) - 1);
End;

Procedure TFSRemoteMessagingServer.CallProcessMessageProcs;
// NOTE: The most recently registered procedures get a crack at the message
// first.
Var
  Index: Integer;
  Handled: Boolean;
  ReceivedMessage: TReceivedMessage;
Begin
  ReceivedMessage := DequeueReceivedMessage;
  While ReceivedMessage <> Nil Do
    Begin
      Try
        Handled := False;
        Index := 0;
        While (Index < Length(FRegisteredProcs)) And (Not Handled) Do
          Begin
            FRegisteredProcs[Index](ReceivedMessage, Handled);
            Inc(Index);
          End;
        // If none of the registered procedures want this message, give it to the
        // OnProcessMessage event.
        If Assigned(FOnProcessMessage) And Not Handled Then
          FOnProcessMessage(Self, ReceivedMessage);
      Finally
        ReceivedMessage.Free;
      End;
      ReceivedMessage := DequeueReceivedMessage;
    End;
End;

Procedure TFSRemoteMessagingServer.PluginMsgHandler(Var Message: TMessage);
Begin
  If Message.Msg = WM_QUERYENDSESSION Then
    Message.Result := 1
  Else If Message.Msg = cm_TIncomingMessage Then
    Dispatch(Message)
  Else
    DefWindowProc(FHWnd, Message.Msg, Message.wParam, Message.lParam);
End;

Procedure TFSRemoteMessagingServer.msgIncomingBroadcast(Var Message: TMessage);
Begin
  CallProcessMessageProcs;
End;

{ TFSRemoteMessagingHandler }

Procedure TFSRemoteMessagingHandler.nmBroadcast(Var Msg: TfsDataMessage);
Begin
  With Msg, PfsnmSendMessageDataReq(dmData)^ Do
    Begin
      FPluginEngine.ProcessReceivedMessage(MsgID, @Data, DataLength, ClientID, 0);
    End;
End;

Procedure TFSRemoteMessagingHandler.Notification(AComponent: TComponent;
  Operation: TOperation);
Begin
  Inherited Notification(AComponent, Operation);

  If (Operation = opRemove) Then
    If (AComponent = FPluginEngine) Then
      FPluginEngine := Nil;
End;

Procedure TFSRemoteMessagingHandler.Process(Msg: PfsDataMessage);
Begin
  If Msg^.dmMsg <> fsnmSendMessageData Then
    Exit; // This shouldn't ever happen, but just in case...

  Dispatch(Msg^);

  FFFreeMem(Msg.dmData, Msg.dmDataLen);
  FFFreeMem(Msg, SizeOf(TfsDataMessage));
End;

Procedure TFSRemoteMessagingHandler.scInitialize;
Begin
  // override but do nothing
End;

Procedure TFSRemoteMessagingHandler.scPrepareForShutdown;
Begin
  // override but do nothing
End;

Procedure TFSRemoteMessagingHandler.scShutdown;
Begin
  // override but do nothing
End;

Procedure TFSRemoteMessagingHandler.scStartup;
Begin
  // override but do nothing
End;

Procedure TFSRemoteMessagingHandler.SetPluginEngine(
  Const Value: TFSRemoteMessagingServer);
Begin
  If FPluginEngine = Value Then
    Exit;

  { Are we already connected to an engine? }
  If assigned(FPluginEngine) Then
    Begin
      { Yes.  Notify it that we are disconnecting. }
      FPluginEngine.Notification(Self, opRemove);
      { D3 compatible. }
      FPluginEngine := Nil;
    End;

  { Do we have a new value for the Transport property? }
  If assigned(Value) Then
    Begin
      { Yes.  Set our internal variable and make sure the transport tells us
        when it is freed. }
      FPluginEngine := Value;
      FPluginEngine.FreeNotification(Self);
    End;
End;

{ TReceivedMessage }

Constructor TReceivedMessage.Create(AMsgID: Longint; ASenderClientID: TffClientID;
  ADataLength: Longint; AData: Pointer);
Begin
  Inherited Create;
  MsgID := AMsgID;
  SenderClientID := ASenderClientID;
  DataLength := ADataLength;
  If DataLength > 0 Then
    Begin
      FFGetZeroMem(Data, DataLength);
      Move(AData^, Data^, DataLength);
    End;
End;

Destructor TReceivedMessage.Destroy;
Begin
  If DataLength > 0 Then
    FFFreeMem(Data, DataLength);
  Inherited;
End;

{ TReceivedMessageList }

Function TReceivedMessageList.GetItem(
  Index: Integer): TReceivedMessage;
Begin
  Result := TReceivedMessage(Inherited GetItem(Index));
End;

Procedure TReceivedMessageList.SetItem(Index: Integer;
  Const Value: TReceivedMessage);
Begin
  Inherited SetItem(Index, Value);
End;

End.

