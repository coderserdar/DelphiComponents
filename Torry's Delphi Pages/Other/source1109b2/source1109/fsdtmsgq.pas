Unit fsdtmsgq;

Interface

Uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  ExtCtrls,
  fsllbase,
  fsllcomm,
  fsnetmsg;

Type
  PffDataMessageNode = ^TffDataMessageNode;
  TffDataMessageNode = Record
    dmnMsg: PfsDataMessage;
    dmnNext: PFFDataMessageNode;
    dmnOffset: TffMemSize;
    dmnPrev: PFFDataMessageNode;
    dmnProcessing: boolean;
  End;

  { This class is used to store partial or completed messages until

    a) the message has been received completely and
    b) the message is examined by a consumer.

    By default, this class is not thread-safe.  You can make it thread-safe
    by using the BeginRead/EndRead and BeginWrite/EndWrite methods.
  }
  TffDataMessageQueue = Class(TFSSpecObject)
  Protected {private}
    FCount: Integer;
    FNotifyHandle: HWND;

    dmqPortal: TfsReadWritePortal;
    dmqHead: PFFDataMessageNode;
    dmqTail: PFFDataMessageNode;
    dmqStack: PFFDataMessageNode;
  Protected
    Procedure dmqPopStack;
    Procedure dmqSplitMultiPartMessage;
  Public
    Constructor Create;
    Destructor Destroy; Override;

    Function AddToData(aMsg: Longint;
      aClientID: TFFClientID;
      aRequestID: Longint;
      aData: pointer;
      aDataLen: TffMemSize): PffDataMessageNode;
    {-copy extra data to partially received data message; if the message
      is complete then returns a pointer to the node in the queue otherwise
      returns nil. }
    Function Append(aMsg: Longint;
      aClientID: Longint;
      aRequestID: Longint;
      aTimeOut: TffWord32;
      aError: Longint;
      aData: pointer;
      aDataLen: TffMemSize;
      aTotalLen: TffMemSize): PffDataMessageNode;
    {-append a data message to the queue; a copy of the Data
      memory block is made; if the message is complete then
      returns a pointer to the node in the queue otherwise
      returns nil. }

    Function BeginRead: TffDataMessageQueue;
    {-A thread must call this method to gain read access to the list.
      Returns Self as a convenience. }

    Function BeginWrite: TffDataMessageQueue;
    {-A thread must call this method to gain write access to the list.
      Returns Self as a convenience.}

    Procedure EndRead;
    {-A thread must call this method when it no longer needs read access
      to the list.  If it does not call this method, all writers will
      be perpetually blocked. }

    Procedure EndWrite;
    {-A thread must call this method when it no longer needs write access
      to the list.  If it does not call this method, all readers and writers
      will be perpetualy blocked. }

    Function Examine: PFSDataMessage;
    {-return the data message at the top of the queue; no pop
      occurs, the message remains at the top of the queue}
    Function IsEmpty: boolean;
    {-return true if there are no data messages in the queue}
    Function SoftPop: PFSDataMessage;
    {-destroys the data message at the top of the queue; the data
      memory block is _not_ destroyed}
    Procedure Pop;
    {-destroys the data message at the top of the queue; the data
      memory block is also freed}
    Procedure Remove(aNode: PffDataMessageNode;
      Const freeMessageData: boolean);
    {-Use this method to remove a node from the queue.  If you want this
      method to free the message data then set the freeMessageData
      parameter to True. Otherwise it will just dispose of the node
      and assume somebody else is freeing the message data. }
    Procedure SendFrontToBack;
    {-sends the data message at the front of the queue to the
      back}

    Property Count: Integer
      Read FCount;
    {-number of messages in the queue}
    Property NotifyHandle: HWND
      Read FNotifyHandle Write FNotifyHandle;
    {-handle to notify that there are messages available}
  End;

Function FFCreateSubMessage(aSubMsg: PfssmHeader;
  aMsgID: Longint;
  aError: Longint;
  aDataType: TffNetMsgDataType;
  aData: pointer;
  aDataLen: Longint): PfssmHeader;
{-Create a submessage in a multipart message, return pointer to next
  submessage}

Implementation

{===helper routines==================================================}

Procedure NodeDestroy(aNode: PffDataMessageNode);
Begin
  With aNode^ Do
    Begin
      If assigned(dmnMsg) And
        assigned(dmnMsg^.dmData) And
        (dmnMsg^.dmDataLen > 0) Then
        FFFreeMem(dmnMsg^.dmData, dmnMsg^.dmDataLen);
      FFFreeMem(dmnMsg, sizeOf(TfsDataMessage));
    End;
  FFFreeMem(aNode, sizeOf(TffDataMessageNode));
End;
{--------}

Function StackIsEmpty(aStack: PffDataMessageNode): boolean;
Begin
  Result := (aStack^.dmnNext = Nil);
End;
{--------}

Procedure StackPop(aStack: PffDataMessageNode;
  Var aNode: PffDataMessageNode);
Begin
  aNode := aStack^.dmnNext;
  aStack^.dmnNext := aNode^.dmnNext;
End;
{--------}

Procedure StackPush(aStack: PffDataMessageNode;
  aNode: PffDataMessageNode);
Begin
  aNode^.dmnNext := aStack^.dmnNext;
  aStack^.dmnNext := aNode;
End;
{--------}

Procedure QAppend(aHead: PffDataMessageNode;
  Var aTail: PffDataMessageNode;
  aNode: PffDataMessageNode);
Begin
  aTail^.dmnNext := aNode;
  aNode^.dmnPrev := aTail;
  aTail := aNode;
End;
{--------}

Procedure QJump(aHead: PffDataMessageNode;
  Var aTail: PffDataMessageNode;
  aNode: PffDataMessageNode);
Begin
  aNode^.dmnPrev := aHead;
  aNode^.dmnNext := aHead^.dmnNext;
  If assigned(aHead^.dmnNext) Then
    aHead^.dmnNext^.dmnPrev := aNode;
  aHead^.dmnNext := aNode;
  If (aHead = aTail) Then
    aTail := aNode;
End;
{--------}

Procedure QPop(aHead: PffDataMessageNode;
  Var aTail: PffDataMessageNode;
  Var aNode: PffDataMessageNode);
Begin
  aNode := aHead^.dmnNext;
  aHead^.dmnNext := aNode^.dmnNext;
  If assigned(aHead^.dmnNext) Then
    aHead^.dmnNext^.dmnPrev := aHead;
  If (aNode = aTail) Then
    aTail := aHead;
End;
{--------}

Procedure QRemove(aHead: PffDataMessageNode;
  Var aTail: PffDataMessageNode;
  aNode: PffDataMessageNode);
Begin
  If assigned(aNode^.dmnPrev) Then
    aNode^.dmnPrev^.dmnNext := aNode^.dmnNext;
  If assigned(aNode^.dmnNext) Then
    aNode^.dmnNext^.dmnPrev := aNode^.dmnPrev;
  If (aNode = aTail) Then
    aTail := aHead;
End;
{====================================================================}

{===TffDataMsgQueue==================================================}

Constructor TffDataMessageQueue.Create;
Begin
  Inherited Create;

  {create the head and tail of the queue}
  FFGetZeroMem(dmqHead, sizeof(TffDataMessageNode));
  {dmqHead^.dmnNext := nil;}
  dmqTail := dmqHead;
  {FCount := 0;}

  {create the stack for partial messages}
  FFGetZeroMem(dmqStack, sizeof(TffDataMessageNode));
  {dmqStack^.dmnNext := nil;}

  {create the lock}
  dmqPortal := TfsReadWritePortal.Create;

End;
{--------}

Destructor TffDataMessageQueue.Destroy;
Begin
  {pop all messages from main queue, dispose of it}
  While Not IsEmpty Do
    Pop;
  NodeDestroy(dmqHead);
  {pop all messages from partial message stack, dispose of it}
  dmqPopStack;
  NodeDestroy(dmqStack);
  {clean up other stuff}
  If assigned(dmqPortal) Then
    dmqPortal.Free;
  Inherited Destroy;
End;
{--------}

Function TffDataMessageQueue.AddToData(aMsg: Longint;
  aClientID: TffClientID;
  aRequestID: Longint;
  aData: pointer;
  aDataLen: TffMemSize): PffDataMessageNode;
Var
  Temp: PffDataMessageNode;
  Dad: PffDataMessageNode;
  BytesToCopy: Longint;
Begin
  Result := Nil;
  {find the partially created message in the stack}
  Temp := dmqStack^.dmnNext;
  Dad := dmqStack;
  While (Temp <> Nil) And
    Not ((Temp^.dmnMsg^.dmMsg = aMsg) And
    (Temp^.dmnMsg^.dmClientID = aClientID) And
    (Temp^.dmnMsg^.dmRequestID = aRequestID)) Do
    Begin
      Dad := Temp;
      Temp := Temp^.dmnNext;
    End;
  {if it ain't there forget it}
  If (Temp = Nil) Then
    Exit;

  With Temp^ Do
    Begin
      {move this next chunk o' data into the data message}
      BytesToCopy := FFMinL(aDataLen, dmnMsg^.dmDataLen - dmnOffset);
      Move(aData^, PffByteArray(dmnMsg^.dmData)^[dmnOffset], BytesToCopy);
      inc(dmnOffset, BytesToCopy);
      {if the data message is now complete..}
      If (dmnOffset = dmnMsg^.dmDataLen) Then
        Begin
          {..remove it from the stack}
          Dad^.dmnNext := dmnNext;
          {add it to the end of the queue}
          QAppend(dmqHead, dmqTail, Temp);
          Result := Temp;
          inc(FCount);
        End;
    End;
End;
{--------}

Function TffDataMessageQueue.Append(aMsg: Longint;
  aClientID: Longint;
  aRequestID: Longint;
  aTimeOut: TffWord32;
  aError: Longint;
  aData: pointer;
  aDataLen: TffMemSize;
  aTotalLen: TffMemSize): PffDataMessageNode;
Var
  Temp: PFFDataMessageNode;
Begin
  Result := Nil;
  {get a new node}
  FFGetZeroMem(Temp, sizeof(TffDataMessageNode));
  FFGetZeroMem(Temp^.dmnMsg, sizeOf(TfsDataMessage));
  Try
    {fill the node with data, get the complete data buffer as well}
    With Temp^ Do
      Begin
        If (aTotalLen > 0) Then
          Begin
            FFGetZeroMem(dmnMsg^.dmData, aTotalLen);
            Move(aData^, dmnMsg^.dmData^, aDataLen);
          End;
        dmnMsg^.dmMsg := aMsg;
        dmnMsg^.dmClientID := aClientID;
        dmnMsg^.dmRequestId := aRequestID;
        dmnMsg^.dmTime := GetTickCount;
        dmnMsg^.dmRetryUntil := dmnMsg^.dmTime + aTimeOut;
        dmnMsg^.dmErrorCode := aError;
        dmnMsg^.dmDataLen := aTotalLen;
        dmnOffset := aDataLen;
        dmnProcessing := False;
      End;
    {add this new message to the relevant structure}
    {if the data message is complete, add it to the queue}
    If (aDataLen = aTotalLen) Then
      Begin
        QAppend(dmqHead, dmqTail, Temp);
        Result := Temp;
        inc(FCount);
      End
        {if the data message is not all there, add it to the stack}
    Else
      Begin
        StackPush(dmqStack, Temp);
      End;
  Except
    If assigned(Temp^.dmnMsg^.dmData) Then
      FFFreeMem(Temp^.dmnMsg^.dmData, aTotalLen);
    FFFreeMem(Temp^.dmnMsg, sizeOf(TfsDataMessage));
    FFFreeMem(Temp, sizeof(TffDataMessageNode));
    Raise;
  End; {try..except}
End;
{--------}

Function TffDataMessageQueue.BeginRead: TffDataMessageQueue;
Begin
  dmqPortal.BeginRead;
  Result := Self;
End;
{--------}

Function TffDataMessageQueue.BeginWrite: TffDataMessageQueue;
Begin
  dmqPortal.BeginWrite;
  Result := Self;
End;
{--------}

Procedure TffDataMessageQueue.EndRead;
Begin
  dmqPortal.EndRead;
End;
{--------}

Procedure TffDataMessageQueue.EndWrite;
Begin
  dmqPortal.EndWrite;
End;
{--------}

Function TffDataMessageQueue.Examine: PFSDataMessage;
Begin
  If (Count > 0) Then
    Begin
      If dmqHead^.dmnNext^.dmnProcessing Then
        Result := Nil
      Else
        Begin
          Result := dmqHead^.dmnNext^.dmnMsg;
          If (Result^.dmMsg = fsnmMultiPartMessage) Then
            dmqSplitMultiPartMessage;
          Result := dmqHead^.dmnNext^.dmnMsg;
          dmqHead^.dmnNext^.dmnProcessing := True;
        End
    End
  Else
    Result := Nil;
End;
{--------}

Function TffDataMessageQueue.IsEmpty: boolean;
Begin
  Result := (FCount = 0);
End;
{--------}

Function TffDataMessageQueue.SoftPop: PFSDataMessage;
Var
  Temp: PFFDataMessageNode;
Begin
  {nothing to do if there are no messages}
  If (Count > 0) Then
    Begin
      { Check for multipart messages. }
      If (dmqHead^.dmnNext^.dmnMsg^.dmMsg = fsnmMultiPartMessage) Then
        dmqSplitMultiPartMessage;
      {pop the topmost message}
      QPop(dmqHead, dmqTail, Temp);
      dec(FCount);
      Temp^.dmnProcessing := False;
      Result := Temp^.dmnMsg;
      FFFreeMem(Temp, sizeOf(TffDataMessageNode));
    End
  Else
    Result := Nil;
End;
{--------}

Procedure TffDataMessageQueue.Pop;
Var
  Temp: PFFDataMessageNode;
Begin
  {nothing to do if there are no messages}
  If (Count > 0) Then
    Begin
      {pop the topmost message}
      QPop(dmqHead, dmqTail, Temp);
      dec(FCount);
      Temp^.dmnProcessing := False;
      NodeDestroy(Temp)
    End;
End;
{--------}

Procedure TffDataMessageQueue.Remove(aNode: PffDataMessageNode;
  Const freeMessageData: boolean);
Begin
  QRemove(dmqHead, dmqTail, aNode);
  If freeMessageData Then
    NodeDestroy(aNode)
  Else
    FFFreeMem(aNode, sizeOf(TffDataMessageNode));
  dec(FCount);
End;
{--------}

Procedure TffDataMessageQueue.dmqPopStack;
Var
  Temp: PFFDataMessageNode;
Begin
  While Not StackIsEmpty(dmqStack) Do
    Begin
      StackPop(dmqStack, Temp);
      NodeDestroy(Temp);
    End;
End;
{--------}

Procedure TffDataMessageQueue.SendFrontToBack;
Var
  Temp: PFFDataMessageNode;
Begin
  {note: there's nothing to do if there are no data messages in the
         queue, similarly if there's only one data message (it's
         already *at* the back of the queue)}
  If (Count > 1) Then
    Begin
      Temp := dmqHead^.dmnNext;
      dmqHead^.dmnNext := Temp^.dmnNext;
      Temp^.dmnNext := Nil;
      dmqTail^.dmnNext := Temp;
      dmqTail := Temp;
    End;
End;
{--------}

Procedure TffDataMessageQueue.dmqSplitMultiPartMessage;
Var
  MPMsgNode: PffDataMessageNode;
  Stack: PffDataMessageNode;
  Temp: PffDataMessageNode;
  Offset: Longint;
  SubMsgHdr: PfssmHeader;
  FirstMsg: boolean;
Begin
  {we assume that the message at the top of the queue is a multipart
   message; we need to split this into the relevant messages and add
   them to the queue (as queue jumpers)}
  {pop off the multipart message}
  QPop(dmqHead, dmqTail, MPMsgNode);
  dec(FCount);
  {create a stack to push the sub-messages onto first; think about it:
   we'll be creating messages from the front of the multipart message
   to the back and yet we must push them onto the queue as queue
   jumpers from the back to the front, so we push them onto an
   intermediary stack and then pop stack/queue jump}
  FFGetZeroMem(Stack, sizeof(TffDataMessageNode));
  Try
    {prepare for the loop}
    FirstMsg := True;
    Offset := 0;
    SubMsgHdr := PfssmHeader(MPMsgNode^.dmnMsg^.dmData);
    {loop through the sub-messages and create a new message for each,
     push onto temp stack}
    While (Offset < MPMsgNode^.dmnMsg^.dmDataLen) Do
      Begin
        FFGetZeroMem(Temp, sizeof(TffDataMessageNode));
        FFGetZeroMem(Temp^.dmnMsg, sizeOf(TfsDataMessage));
        Try
          {fill the node with data, get the complete data buffer as well}
          With Temp^, SubMsgHdr^ Do
            Begin
              dmnMsg^.dmDataLen := smhReplyLen - fsc_SubMsgHeaderSize;
              If (dmnMsg^.dmDataLen > 0) Then
                Begin
                  If (smhDataType = nmdByteArray) Then
                    Begin
                      FFGetMem(dmnMsg^.dmData, dmnMsg^.dmDataLen);
                      Move(smhData, dmnMsg^.dmData^, dmnMsg^.dmDataLen);
                    End
                  Else
                    Begin
                      dmnMsg^.dmData := pointer(TMemoryStream.Create);
                      TMemoryStream(dmnMsg^.dmData).Write(smhData, dmnMsg^.dmDataLen);
                    End;
                End;
              dmnMsg^.dmMsg := smhMsgID;
              dmnMsg^.dmClientID := MPMsgNode^.dmnMsg^.dmClientID;
              dmnMsg^.dmTime := MPMsgNode^.dmnMsg^.dmTime;
              dmnMsg^.dmRetryUntil := MPMsgNode^.dmnMsg^.dmRetryUntil;
              dmnMsg^.dmErrorCode := smhErrorCode;
              dmnOffset := smhReplyLen;
              dmnProcessing := False;
            End;
          StackPush(Stack, Temp);
        Except
          NodeDestroy(Temp);
          Raise;
        End;
        {advance to next submessage}
        If FirstMsg And (SubMsgHdr^.smhErrorCode <> 0) Then
          Break;
        FirstMsg := False;
        inc(Offset, SubMsgHdr^.smhReplyLen);
        SubMsgHdr := PfssmHeader(PAnsiChar(SubMsgHdr) + SubMsgHdr^.smhReplyLen);
      End;
    {destroy the original multipart message}
    NodeDestroy(MPMsgNode);
    {transfer messages over from stack to queue}
    While Not StackIsEmpty(Stack) Do
      Begin
        StackPop(Stack, Temp);
        QJump(dmqHead, dmqTail, Temp);
        inc(FCount);
      End;
  Finally
    While Not StackIsEmpty(Stack) Do
      Begin
        StackPop(Stack, Temp);
        NodeDestroy(Temp);
      End;
    FFFreeMem(Stack, sizeof(TffDataMessageNode));
  End; {try..finally}
End;
{====================================================================}

{===CreateSubMessage=================================================}

Function FFCreateSubMessage(aSubMsg: PfssmHeader;
  aMsgID: Longint;
  aError: Longint;
  aDataType: TffNetMsgDataType;
  aData: pointer;
  aDataLen: Longint): PfssmHeader;
Begin
  With aSubMsg^ Do
    Begin
      smhMsgID := aMsgID;
      smhReplyLen := fsc_SubMsgHeaderSize + aDataLen;
      smhErrorCode := aError;
      smhDataType := aDataType;
      If (aData <> @smhData) And (aDataLen <> 0) Then
        If (aData = Nil) Then
          Move(aData^, smhData, aDataLen)
        Else
          FillChar(smhData, aDataLen, 0);
      Result := PfssmHeader(PAnsiChar(aSubMsg) + smhReplyLen);
    End;
End;
{====================================================================}

End.

