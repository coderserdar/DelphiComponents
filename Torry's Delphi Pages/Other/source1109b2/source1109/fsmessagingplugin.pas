Unit fsmessagingplugin;

Interface

Uses
  Classes,
  fsllcomm,
  fsllbase,
  fsmessagingcommon,
  fsserverclass;

Type

  { This is the server-side plugin.  Since
    TffLoggableComponent is one of its ancestors we will log all errors
    to TffLoggableComponent.EventLog. }
  TFSMessagingServer = Class(TFSBasePluginEngine)
  Private
    FActiveMsgClients: TRegMsgClients;
    FEngine: TFSServer;
  Protected
    Procedure scInitialize; Override;
    Procedure scPrepareForShutdown; Override;
    Procedure scShutdown; Override;
    Procedure scStartup; Override;

    Function InternalAddMsgClient(AClientID: TffClientID; ADescription: String; APromiscuousMode: Boolean): TffResult;
    Function InternalGetMsgClients(AStream: TStream): TffResult;

    Procedure ProcessReceivedMessage(MsgID: Integer; Data: Pointer;
      DataLength: Integer;
      SenderClientID, RecipientClientID: TffClientID);

  Public
    Constructor Create(aOwner: TComponent); Override;
    Destructor Destroy; Override;

    Function AddMsgClient(Description: String; PromiscuousMode: Boolean): TffResult;

    Function RemoveMsgClient: TffResult; Overload;
    Function RemoveMsgClient(AClientID: TffClientID): TffResult; Overload;

    Function GetMsgClients(AList: TRegMsgClients): TffResult;
    Function SendData(MsgID: Integer; Data: Pointer; DataLength: Integer; RecipientClientID: TffClientID): TffResult;
  Published
    Property ServerEngine: TFSServer Read FEngine Write FEngine;
  End;

  { This is the plugin command handler that translates messages from the
    remote interface into method calls of the actual plugin engine. }
  TFSMessagingHandler = Class(TFSBasePluginCommandHandler)
  Protected
    Function GetPluginEngine: TFSMessagingServer;
    Procedure SetPluginEngine(anEngine: TFSMessagingServer);

    Procedure nmBroadcast(Var Msg: TfsDataMessage); Message fsnmSendMessageData;
    Procedure nmAddMsgClient(Var Msg: TfsDataMessage); Message fsnmAddMsgClient;
    Procedure nmRemoveMsgClient(Var Msg: TfsDataMessage); Message fsnmRemoveMsgClient;
    Procedure nmGetMsgClients(Var Msg: TfsDataMessage); Message fsnmGetMsgClients;

    { Called by Process for fsnmLogError message. }
    Procedure scInitialize; Override;
    Procedure scPrepareForShutdown; Override;
    Procedure scShutdown; Override;
    Procedure scStartup; Override;
  Public
    Procedure Process(Msg: PfsDataMessage; Var handled: boolean); Override;
    { This method is called by a command handler when it has a message that
      may be processed by a plugin.  If the plugin handles the message,
      set handled to True. }
    Destructor Destroy; Override;
  Published
    Property PluginEngine: TFSMessagingServer
      Read GetPluginEngine Write SetPluginEngine;
  End;

Procedure Register;

Implementation

Uses
  Windows,
  SysUtils,
  fssrbde,
  Math,
  fsllComp,
  Forms,
  fsllreq;

Procedure Register;
Begin
  RegisterComponents('FSSQL Utils',[TFSMessagingServer,TFSMessagingHandler]);
End;

{ TFSMessagingServer }

Procedure TFSMessagingServer.scInitialize;
Begin
  { Do nothing. }
End;

Procedure TFSMessagingServer.scPrepareForShutdown;
Begin
  { Do nothing. }
End;

Procedure TFSMessagingServer.scShutdown;
Begin
  { Do nothing. }
End;

Procedure TFSMessagingServer.scStartup;
Begin
  { Do nothing. }
End;

Constructor TFSMessagingServer.Create(aOwner: TComponent);
Begin
  Inherited;
  FActiveMsgClients := TRegMsgClients.Create;
  FEngine := Nil;
End;

Destructor TFSMessagingServer.Destroy;
Begin
  FActiveMsgClients.Free;
  Inherited;
End;

Function TFSMessagingServer.SendData(MsgID: Integer;
  Data: Pointer;
  DataLength: Integer;
  RecipientClientID: TffClientID): TffResult;
Begin
  Result := DBIERR_NONE;
End;

Procedure TFSMessagingServer.ProcessReceivedMessage(MsgID: Integer;
  Data: Pointer;
  DataLength: Integer;
  SenderClientID,
  RecipientClientID: TffClientID);
Var
  Request: PfsnmSendMessageDataReq;
  ReqLen: Integer;
  Index, i: Integer;
  anItem: TRegMsgClient;
Begin
  // build the reply message
  ReqLen := SizeOf(TfsnmSendMessageDataReq) - SizeOf(TfsVarMsgField) + DataLength + 1;
  FFGetZeroMem(Request, ReqLen);
  Try
    Request^.DataLength := DataLength;
    Request^.MsgID := MsgID;
    Request^.ClientID := SenderClientId;
    If DataLength > 0 Then
      Move(Data^, Request^.Data, DataLength);

    FActiveMsgClients.BeginRead;
    Try

      For Index := 0 To (FActiveMsgClients.Count - 1) Do
        Begin
          anItem := TRegMsgClient(FActiveMsgClients[Index]);
          If ((((RecipientClientID = 0) Or // no recipent
            (anItem.ClientID = RecipientClientID)) And // or recipent matches
            (anItem.ClientID <> SenderClientID)) Or // and client isn't sender
            anItem.PromiscuousMode) Then
            Begin // or always sent
              // broadcast with the transport the client registered with just incase
              // they are a different transport then is currently active
              TFSBaseTransport(anItem.Transport).Post(0,
                anItem.ClientId,
                fsnmSendMessageData,
                Request,
                ReqLen,
                1000,
                fsrmNoReplyExpected);
            End;
        End; // 2
    Finally
      FActiveMsgClients.EndRead;
    End;
  Finally
    FFFreeMem(Request, ReqLen);
  End;
End;

Function TFSMessagingServer.AddMsgClient(Description: String; PromiscuousMode: Boolean): TffResult;
Begin
  // this function is only called in a single-exe app
  InternalAddMsgClient(1, '', PromiscuousMode);
  Result := DBIERR_NONE;
End;

Function TFSMessagingServer.RemoveMsgClient: TffResult;
Begin
  // this function is only called in a single-exe app
  RemoveMsgClient(1);
  Result := DBIERR_NONE;
End;

Function TFSMessagingServer.InternalAddMsgClient(AClientID: TffClientID; ADescription: String; APromiscuousMode: Boolean): TffResult;
Var
  anItem: TRegMsgClient;
Begin
  Result := DBIERR_NONE;
  With FActiveMsgClients.BeginWrite Do
    Try
      // delete from list if it exists
      Delete(AClientId);
      // add to list
      anItem := TRegMsgClient.Create(AClientID, ADescription, APromiscuousMode, TFSBaseTransport.CurrentTransport);
      Insert(anItem);
    Finally
      EndWrite;
    End;
End;

Function TFSMessagingServer.RemoveMsgClient(AClientID: TffClientID): TffResult;
Begin
  Result := DBIERR_NONE;
  With FActiveMsgClients.BeginWrite Do
    Try
      Delete(AClientId);
    Finally
      EndWrite;
    End;
End;

Function TFSMessagingServer.InternalGetMsgClients(AStream: TStream): TffResult;

  Procedure OptimalizeUsers;
  Var
    cl: TffClientID;
    i, c: Integer;

    Function ClId(client: TffClientID): boolean;
    Var
      j: Integer;
    Begin
      Result := False;
      If FEngine.ClientList.ClientCount > 0 Then
        Begin
          For j := 0 To FEngine.ClientList.ClientCount - 1 Do
            Begin
              If FEngine.ClientList.Client[ftFromIndex, j].ClientId = client Then
                Begin
                  Result := True;
                  Exit;
                End;
            End;
        End;
    End;
  Begin
    If FEngine = Nil Then
      Exit;
    If FActiveMsgClients.Count > 0 Then
      Begin
        For i := 0 To FActiveMsgClients.Count - 1 Do
          Begin
            cl := FActiveMsgClients.Items[i].ClientId;
            If Not ClId(cl) Then
              Begin
                With FActiveMsgClients.BeginWrite Do
                  Try
                    Delete(cl);
                  Finally
                    EndWrite;
                  End;
              End;
          End;
      End;

  End;
Begin
  Result := DBIERR_NONE;
  // for crash user if exists
  OptimalizeUsers;
  FActiveMsgClients.SaveToStream(AStream);
End;

Function TFSMessagingServer.GetMsgClients(AList: TRegMsgClients): TffResult;
Begin
  Result := DBIERR_NONE;
End;

{ TFSMessagingHandler }

Function TFSMessagingHandler.GetPluginEngine: TFSMessagingServer;
Begin
  Result := TFSMessagingServer(FPluginEngine);
End;

Procedure TFSMessagingHandler.SetPluginEngine(anEngine: TFSMessagingServer);
Begin
  If Assigned(FPluginEngine) Then
    FPluginEngine.FFRemoveDependent(Self);
  If Assigned(anEngine) Then
    anEngine.FFAddDependent(Self);
  FPluginEngine := anEngine;
End;

Procedure TFSMessagingHandler.nmBroadcast(Var Msg: TfsDataMessage);
Begin
  With Msg, PfsnmSendMessageDataReq(dmData)^ Do
    Begin
      TFSMessagingServer(FPluginEngine).ProcessReceivedMessage(
        MsgID, @Data,
        DataLength, dmClientID, ClientID);
    End;
End;

Procedure TFSMessagingHandler.nmAddMsgClient(Var Msg: TfsDataMessage);
Begin
  With Msg, PfsnmAddMsgClientReq(dmData)^ Do
    Begin
      TFSMessagingServer(FPluginEngine).InternalAddMsgClient(dmClientID, Description, PromiscuousMode);
    End;
End;

Procedure TFSMessagingHandler.nmRemoveMsgClient(Var Msg: TfsDataMessage);
Begin
  With Msg Do
    Begin
      TFSMessagingServer(FPluginEngine).RemoveMsgClient(dmClientID);
    End;
End;

Procedure TFSMessagingHandler.nmGetMsgClients(Var Msg: TfsDataMessage);
Var
  aBuffer: Pointer;
  Stream: TMemoryStream;
  StreamSize: Longint;
  Error: TffResult;
Begin
  Stream := TMemoryStream.Create;
  Try
    Error := TFSMessagingServer(FPluginEngine).InternalGetMsgClients(Stream);

    StreamSize := Stream.Size;
    FFGetMem(aBuffer, StreamSize);
    Try
      Stream.Position := 0;
      Stream.Read(aBuffer^, StreamSize);

      TFSBaseTransport.Reply(fsnmGetMsgClients, Error, aBuffer, StreamSize);
    Finally
      FFFreeMem(aBuffer, StreamSize);
    End;

  Finally
    Stream.Free;
  End;

End;

Procedure TFSMessagingHandler.Process(Msg: PfsDataMessage; Var handled: Boolean);
Begin
  Handled := (Msg^.dmMsg = fsnmSendMessageData) Or
    (Msg^.dmMsg = fsnmAddMsgClient) Or
    (Msg^.dmMsg = fsnmRemoveMsgClient) Or
    (Msg^.dmMsg = fsnmGetMsgClients);
  If Handled Then
    Dispatch(Msg^);
  { Note: We don't have to free the message data because the a) we were
    called by TffBaseCommandHandler.Process and b) that particular method
    frees the data for us. }
End;

Procedure TFSMessagingHandler.scInitialize;
Begin
  // override abstract method, but nothing to do here.
End;

Procedure TFSMessagingHandler.scPrepareForShutdown;
Begin
  // override abstract method, but nothing to do here.
End;

Procedure TFSMessagingHandler.scShutdown;
Begin
  // override abstract method, but nothing to do here.
End;

Procedure TFSMessagingHandler.scStartup;
Begin
  // override abstract method, but nothing to do here.
End;

Destructor TFSMessagingHandler.Destroy;
Begin
  SetPluginEngine(Nil);
  Inherited;
End;

End.

