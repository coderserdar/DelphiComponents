Unit fsmessagingcommon;

Interface

Uses
  Classes,
  fsllBase,
  fsNetMsg,
  fsllComm;

Const
  fsnmAddMsgClient = fsnmUser + 104;
  fsnmRemoveMsgClient = fsnmUser + 105;
  fsnmSendMessageData = fsnmUser + 106;
  fsnmGetMsgClients = fsnmUser + 107;

Type

  PfsnmAddMsgClientReq = ^TfsnmAddMsgClientReq;
  TfsnmAddMsgClientReq = Packed Record
    Description: String[255];
    PromiscuousMode: Boolean;
  End;
  {no reply expected}

  PfsnmSendMessageDataReq = ^TfsnmSendMessageDataReq;
  TfsnmSendMessageDataReq = Packed Record
    MsgID: Longint;
    // ClientID  serves two purposes
    //     from client -> server used to designate destination
    //     from server -> client used to designate the original sender
    ClientID: TffClientID;
    DataLength: Longint;
    Data: TfsVarMsgField;
  End;
  {no reply expected}

  TRegMsgClient = Class(TFSSpecW32ListItem)
  Private
    FDescription: String;
    FPromiscuousMode: Boolean;
    FTransport: TObject;
    Function GetClientId: TffClientId;
  Public
    Constructor Create(AClientID: TffClientID; ADescription: String; APromiscuousMode: Boolean; ATransport: TObject);
  Published
    Property PromiscuousMode: Boolean Read FPromiscuousMode Write FPromiscuousMode;
    Property ClientId: TffClientId Read GetClientId;
    Property Transport: TObject Read FTransport;
    Property Description: String Read FDescription;
  End;

  TRegMsgClients = Class(TFSSpecThreadList)
  Private
    Function GetItem(Index: Integer): TRegMsgClient;
    Procedure SetItem(Index: Integer; Const Value: TRegMsgClient);
  Public
    Procedure LoadFromStream(AStream: TStream);
    Procedure SaveToStream(AStream: TStream);

    Function IndexOfClientID(ClientId: TffClientId): Integer;
    Property Items[Index: Integer]: TRegMsgClient Read GetItem Write SetItem;
  End;

Implementation

{ TRegMsgClient }

Constructor TRegMsgClient.Create(AClientID: TffClientID;
  ADescription: String;
  APromiscuousMode: Boolean;
  ATransport: TObject);
Begin
  Inherited Create(AClientID);
  FDescription := ADescription;
  FPromiscuousMode := APromiscuousMode;
  FTransport := ATransport;
End;

Function TRegMsgClient.GetClientId: TffClientId;
Begin
  Result := KeyValue;
End;

{ TRegMsgClients }

Function TRegMsgClients.GetItem(Index: Integer): TRegMsgClient;
Begin
  Result := TRegMsgClient(Inherited GetItem(Index));
End;

Function TRegMsgClients.IndexOfClientID(ClientId: TffClientId): Integer;
Var
  Index: Integer;
  Item: TRegMsgClient;
Begin
  Result := -1;
  Index := 0;

  While (Index < Count) And (Result = -1) Do
    Begin
      Item := TRegMsgClient(Items[Index]);
      If ClientID = Item.ClientID Then
        Result := Index;
      Inc(Index);
    End;
End;

Procedure TRegMsgClients.LoadFromStream(AStream: TStream);
Var
  Index: Integer;
  Reader: TReader;
  ACount: Integer;
  ClientId: TffClientId;
  Description: String;
  PromiscuousMode: Boolean;
Begin
  BeginWrite;
  Try
    Reader := TReader.Create(AStream, 4096);
    Try
      ACount := Reader.ReadInteger;
      For Index := 0 To (ACount - 1) Do
        Begin
          ClientId := Reader.ReadInteger;
          Description := Reader.ReadString;
          PromiscuousMode := Reader.ReadBoolean;
          Insert(TRegMsgClient.Create(ClientId, Description, PromiscuousMode, Nil));
        End;
    Finally
      Reader.Free;
    End;
  Finally
    EndWrite;
  End;
End;

Procedure TRegMsgClients.SaveToStream(AStream: TStream);
Var
  Index: Integer;
  Writer: TWriter;
  anItem: TRegMsgClient;
Begin
  BeginRead;
  Try
    Writer := TWriter.Create(AStream, 4096);
    Try
      Writer.WriteInteger(Count);
      For Index := 0 To (Count - 1) Do
        Begin
          anItem := TRegMsgClient(Items[Index]);
          Writer.WriteInteger(anItem.ClientId);
          Writer.WriteString(anItem.Description);
          Writer.WriteBoolean(anItem.PromiscuousMode);
        End;
    Finally
      Writer.Free;
    End;
  Finally
    EndRead;
  End;
End;

Procedure TRegMsgClients.SetItem(Index: Integer;
  Const Value: TRegMsgClient);
Begin
  Inherited SetItem(Index, Value);
End;

End.

