{*********************************************************}
{* FlashFiler: Data message queue class                  *}
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

unit ffdtmsgq;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  ExtCtrls,
  ffllbase,
  ffllcomm,
  ffnetmsg;

type
  PffDataMessageNode = ^TffDataMessageNode;
  TffDataMessageNode = record
    dmnMsg    : PffDataMessage;
    dmnNext   : PFFDataMessageNode;
    dmnOffset : TffMemSize;
    dmnPrev   : PFFDataMessageNode;
    dmnProcessing : boolean;
  end;

  { This class is used to store partial or completed messages until

    a) the message has been received completely and
    b) the message is examined by a consumer.

    By default, this class is not thread-safe.  You can make it thread-safe
    by using the BeginRead/EndRead and BeginWrite/EndWrite methods.
  }
  TffDataMessageQueue = class(TffObject)
    protected {private}
      FCount     : integer;
      FNotifyHandle : HWND;

      dmqPortal    : TffReadWritePortal;
      dmqHead      : PFFDataMessageNode;
      dmqTail      : PFFDataMessageNode;
      dmqStack     : PFFDataMessageNode;
    protected
      procedure dmqPopStack;
      procedure dmqSplitMultiPartMessage;
    public
      constructor Create;
      destructor Destroy; override;

      function AddToData(aMsg       : longint;
                         aClientID  : TFFClientID;
                         aRequestID : longint;
                         aData      : pointer;
                         aDataLen   : TffMemSize) : PffDataMessageNode;
        {-copy extra data to partially received data message; if the message
          is complete then returns a pointer to the node in the queue otherwise
          returns nil. }
      function Append(aMsg       : longint;
                      aClientID  : longint;
                      aRequestID : longint;
                      aTimeOut   : TffWord32;
                      aError     : longint;
                      aData      : pointer;
                      aDataLen   : TffMemSize;
                      aTotalLen   : TffMemSize) : PffDataMessageNode;
        {-append a data message to the queue; a copy of the Data
          memory block is made; if the message is complete then
          returns a pointer to the node in the queue otherwise
          returns nil. }

      function BeginRead : TffDataMessageQueue;
        {-A thread must call this method to gain read access to the list.
          Returns Self as a convenience. }

      function BeginWrite : TffDataMessageQueue;
        {-A thread must call this method to gain write access to the list.
          Returns Self as a convenience.}

      procedure EndRead;
        {-A thread must call this method when it no longer needs read access
          to the list.  If it does not call this method, all writers will
          be perpetually blocked. }

      procedure EndWrite;
        {-A thread must call this method when it no longer needs write access
          to the list.  If it does not call this method, all readers and writers
          will be perpetualy blocked. }

      function Examine : PFFDataMessage;
        {-return the data message at the top of the queue; no pop
          occurs, the message remains at the top of the queue}
      function IsEmpty : boolean;
        {-return true if there are no data messages in the queue}
      function SoftPop : PFFDataMessage;
        {-destroys the data message at the top of the queue; the data
          memory block is _not_ destroyed}
      procedure Pop;
        {-destroys the data message at the top of the queue; the data
          memory block is also freed}
      procedure Remove(aNode : PffDataMessageNode;
                       const freeMessageData : boolean);
        {-Use this method to remove a node from the queue.  If you want this
          method to free the message data then set the freeMessageData
          parameter to True. Otherwise it will just dispose of the node
          and assume somebody else is freeing the message data. }
      procedure SendFrontToBack;
        {-sends the data message at the front of the queue to the
          back}

      property Count : integer
         read FCount;
        {-number of messages in the queue}
      property NotifyHandle  : HWND
         read FNotifyHandle write FNotifyHandle;
        {-handle to notify that there are messages available}
  end;

function FFCreateSubMessage(aSubMsg   : PffsmHeader;
                            aMsgID    : longint;
                            aError    : longint;
                            aDataType : TffNetMsgDataType;
                            aData     : pointer;
                            aDataLen  : longint) : PffsmHeader;
  {-Create a submessage in a multipart message, return pointer to next
    submessage}

implementation

{===helper routines==================================================}
procedure NodeDestroy(aNode : PffDataMessageNode);
begin
  with aNode^ do begin
    if assigned(dmnMsg) and
       assigned(dmnMsg^.dmData) and
       (dmnMsg^.dmDataLen > 0) then
      FFFreeMem(dmnMsg^.dmData, dmnMsg^.dmDataLen);
    FFFreeMem(dmnMsg, sizeOf(TffDataMessage));
  end;
  FFFreeMem(aNode, sizeOf(TffDataMessageNode));
end;
{--------}
function StackIsEmpty(aStack : PffDataMessageNode) : boolean;
begin
  Result := (aStack^.dmnNext = nil);
end;
{--------}
procedure StackPop(aStack : PffDataMessageNode;
               var aNode  : PffDataMessageNode);
begin
  aNode := aStack^.dmnNext;
  aStack^.dmnNext := aNode^.dmnNext;
end;
{--------}
procedure StackPush(aStack : PffDataMessageNode;
                    aNode  : PffDataMessageNode);
begin
  aNode^.dmnNext := aStack^.dmnNext;
  aStack^.dmnNext := aNode;
end;
{--------}
procedure QAppend(aHead : PffDataMessageNode;
              var aTail : PffDataMessageNode;
                  aNode : PffDataMessageNode);
begin
  aTail^.dmnNext := aNode;
  aNode^.dmnPrev := aTail;
  aTail := aNode;
end;
{--------}
procedure QJump(aHead : PffDataMessageNode;
            var aTail : PffDataMessageNode;
                aNode : PffDataMessageNode);
begin
  aNode^.dmnPrev := aHead;
  aNode^.dmnNext := aHead^.dmnNext;
  if assigned(aHead^.dmnNext) then
    aHead^.dmnNext^.dmnPrev := aNode;
  aHead^.dmnNext := aNode;
  if (aHead = aTail) then
    aTail := aNode;
end;
{--------}
procedure QPop(aHead : PffDataMessageNode;
           var aTail : PffDataMessageNode;
           var aNode : PffDataMessageNode);
begin
  aNode := aHead^.dmnNext;
  aHead^.dmnNext := aNode^.dmnNext;
  if assigned(aHead^.dmnNext) then
    aHead^.dmnNext^.dmnPrev := aHead;
  if (aNode = aTail) then
    aTail := aHead;
end;
{--------}
procedure QRemove(aHead : PffDataMessageNode;
              var aTail : PffDataMessageNode;
                  aNode : PffDataMessageNode);
begin
  if assigned(aNode^.dmnPrev) then
    aNode^.dmnPrev^.dmnNext := aNode^.dmnNext;
  if assigned(aNode^.dmnNext) then
    aNode^.dmnNext^.dmnPrev := aNode^.dmnPrev;
  if (aNode = aTail) then
    aTail := aHead;
end;
{====================================================================}


{===TffDataMsgQueue==================================================}
constructor TffDataMessageQueue.Create;
begin
  inherited Create;

  {create the head and tail of the queue}
  FFGetZeroMem(dmqHead, sizeof(TffDataMessageNode));
  {dmqHead^.dmnNext := nil;}
  dmqTail := dmqHead;
  {FCount := 0;}

  {create the stack for partial messages}
  FFGetZeroMem(dmqStack, sizeof(TffDataMessageNode));
  {dmqStack^.dmnNext := nil;}

  {create the lock}
  dmqPortal := TffReadWritePortal.Create;

end;
{--------}
destructor TffDataMessageQueue.Destroy;
begin
  {pop all messages from main queue, dispose of it}
  while not IsEmpty do
    Pop;
  NodeDestroy(dmqHead);
  {pop all messages from partial message stack, dispose of it}
  dmqPopStack;
  NodeDestroy(dmqStack);
  {clean up other stuff}
  if assigned(dmqPortal) then
    dmqPortal.Free;
  inherited Destroy;
end;
{--------}
function TffDataMessageQueue.AddToData(aMsg : longint;
                                       aClientID : TffClientID;
                                       aRequestID : longint;
                                       aData : pointer;
                                       aDataLen : TffMemSize) : PffDataMessageNode;
var
  Temp : PffDataMessageNode;
  Dad  : PffDataMessageNode;
  BytesToCopy : longint;
begin
  Result := nil;
  {find the partially created message in the stack}
  Temp := dmqStack^.dmnNext;
  Dad := dmqStack;
  while (Temp <> nil) and
        not ((Temp^.dmnMsg^.dmMsg = aMsg) and
             (Temp^.dmnMsg^.dmClientID = aClientID) and
             (Temp^.dmnMsg^.dmRequestID = aRequestID)) do begin
    Dad := Temp;
    Temp := Temp^.dmnNext;
  end;
  {if it ain't there forget it}
  if (Temp = nil) then
    Exit;

  with Temp^ do begin
    {move this next chunk o' data into the data message}
    BytesToCopy := FFMinL(aDataLen, dmnMsg^.dmDataLen - dmnOffset);
    Move(aData^, PffByteArray(dmnMsg^.dmData)^[dmnOffset], BytesToCopy);
    inc(dmnOffset, BytesToCopy);
    {if the data message is now complete..}
    if (dmnOffset = dmnMsg^.dmDataLen) then begin
      {..remove it from the stack}
      Dad^.dmnNext := dmnNext;
      {add it to the end of the queue}
      QAppend(dmqHead, dmqTail, Temp);
      Result := Temp;
      inc(FCount);
    end;
  end;
end;
{--------}
function TffDataMessageQueue.Append(aMsg       : longint;
                                    aClientID  : longint;
                                    aRequestID : longint;
                                    aTimeOut   : TffWord32;
                                    aError     : longint;
                                    aData      : pointer;
                                    aDataLen   : TffMemSize;
                                    aTotalLen   : TffMemSize) : PffDataMessageNode;
var
  Temp : PFFDataMessageNode;
begin
  Result := nil;
  {get a new node}
  FFGetZeroMem(Temp, sizeof(TffDataMessageNode));
  FFGetZeroMem(Temp^.dmnMsg, sizeOf(TffDataMessage));
  try
    {fill the node with data, get the complete data buffer as well}
    with Temp^ do begin
      if (aTotalLen > 0) then begin
        FFGetZeroMem(dmnMsg^.dmData, aTotalLen);
        Move(aData^, dmnMsg^.dmData^, aDataLen);
      end;
      dmnMsg^.dmMsg := aMsg;
      dmnMsg^.dmClientID := aClientID;
      dmnMsg^.dmRequestId := aRequestID;
      dmnMsg^.dmTime := GetTickCount;
      dmnMsg^.dmRetryUntil := dmnMsg^.dmTime + aTimeOut;
      dmnMsg^.dmErrorCode := aError;
      dmnMsg^.dmDataLen := aTotalLen;
      dmnOffset := aDataLen;
      dmnProcessing := false;
    end;
    {add this new message to the relevant structure}
    {if the data message is complete, add it to the queue}
    if (aDataLen = aTotalLen) then begin
      QAppend(dmqHead, dmqTail, Temp);
      Result := Temp;
      inc(FCount);
    end
    {if the data message is not all there, add it to the stack}
    else begin
      StackPush(dmqStack, Temp);
    end;
  except
    if assigned(Temp^.dmnMsg^.dmData) then
      FFFreeMem(Temp^.dmnMsg^.dmData, aTotalLen);
    FFFreeMem(Temp^.dmnMsg, sizeOf(TffDataMessage));
    FFFreeMem(Temp, sizeof(TffDataMessageNode));
    raise;
  end;{try..except}
end;
{--------}
function TffDataMessageQueue.BeginRead : TffDataMessageQueue;
begin
  dmqPortal.BeginRead;
  Result := Self;
end;
{--------}
function TffDataMessageQueue.BeginWrite : TffDataMessageQueue;
begin
  dmqPortal.BeginWrite;
  Result := Self;
end;
{--------}
procedure TffDataMessageQueue.EndRead;
begin
  dmqPortal.EndRead;
end;
{--------}
procedure TffDataMessageQueue.EndWrite;
begin
  dmqPortal.EndWrite;
end;
{--------}
function TffDataMessageQueue.Examine : PFFDataMessage;
begin
  if (Count > 0) then begin
    if dmqHead^.dmnNext^.dmnProcessing then
      Result := nil
    else begin
      Result := dmqHead^.dmnNext^.dmnMsg;
      if (Result^.dmMsg = ffnmMultiPartMessage) then
        dmqSplitMultiPartMessage;
      Result := dmqHead^.dmnNext^.dmnMsg;
      dmqHead^.dmnNext^.dmnProcessing := true;
    end
  end
  else
    Result := nil;
end;
{--------}
function TffDataMessageQueue.IsEmpty : boolean;
begin
  Result := (FCount = 0);
end;
{--------}
function TffDataMessageQueue.SoftPop : PFFDataMessage;
var
  Temp : PFFDataMessageNode;
begin
  {nothing to do if there are no messages}
  if (Count > 0) then begin
    { Check for multipart messages. }
    if (dmqHead^.dmnNext^.dmnMsg^.dmMsg = ffnmMultiPartMessage) then
      dmqSplitMultiPartMessage;
    {pop the topmost message}
    QPop(dmqHead, dmqTail, Temp);
    dec(FCount);
    Temp^.dmnProcessing := false;
    Result := Temp^.dmnMsg;
    FFFreeMem(Temp, sizeOf(TffDataMessageNode));
  end else
    Result := nil;
end;
{--------}
procedure TffDataMessageQueue.Pop;
var
  Temp : PFFDataMessageNode;
begin
  {nothing to do if there are no messages}
  if (Count > 0) then begin
    {pop the topmost message}
    QPop(dmqHead, dmqTail, Temp);
    dec(FCount);
    Temp^.dmnProcessing := false;
    NodeDestroy(Temp)
  end;
end;
{--------}
procedure TffDataMessageQueue.Remove(aNode : PffDataMessageNode;
                                     const freeMessageData : boolean);
begin
  QRemove(dmqHead, dmqTail, aNode);
  if freeMessageData then
    NodeDestroy(aNode)
  else
    FFFreeMem(aNode, sizeOf(TffDataMessageNode));
  dec(FCount);
end;
{--------}
procedure TffDataMessageQueue.dmqPopStack;
var
  Temp : PFFDataMessageNode;
begin
  while not StackIsEmpty(dmqStack) do begin
    StackPop(dmqStack, Temp);
    NodeDestroy(Temp);
  end;
end;
{--------}
procedure TffDataMessageQueue.SendFrontToBack;
var
  Temp : PFFDataMessageNode;
begin
  {note: there's nothing to do if there are no data messages in the
         queue, similarly if there's only one data message (it's
         already *at* the back of the queue)}
  if (Count > 1) then begin
    Temp := dmqHead^.dmnNext;
    dmqHead^.dmnNext := Temp^.dmnNext;
    Temp^.dmnNext := nil;
    dmqTail^.dmnNext := Temp;
    dmqTail := Temp;
  end;
end;
{--------}
procedure TffDataMessageQueue.dmqSplitMultiPartMessage;
var
  MPMsgNode : PffDataMessageNode;
  Stack     : PffDataMessageNode;
  Temp      : PffDataMessageNode;
  Offset    : longint;
  SubMsgHdr : PffsmHeader;
  FirstMsg  : boolean;                                                
begin
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
  try
    {prepare for the loop}
    FirstMsg := true;                                                 
    Offset := 0;
    SubMsgHdr := PffsmHeader(MPMsgNode^.dmnMsg^.dmData);
    {loop through the sub-messages and create a new message for each,
     push onto temp stack}
    while (Offset < MPMsgNode^.dmnMsg^.dmDataLen) do begin
      FFGetZeroMem(Temp, sizeof(TffDataMessageNode));
      FFGetZeroMem(Temp^.dmnMsg, sizeOf(TffDataMessage));
      try
        {fill the node with data, get the complete data buffer as well}
        with Temp^, SubMsgHdr^ do begin
          dmnMsg^.dmDataLen := smhReplyLen - ffc_SubMsgHeaderSize;
          if (dmnMsg^.dmDataLen > 0) then begin
            if (smhDataType = nmdByteArray) then begin
              FFGetMem(dmnMsg^.dmData, dmnMsg^.dmDataLen);
              Move(smhData, dmnMsg^.dmData^, dmnMsg^.dmDataLen);
            end
            else begin
              dmnMsg^.dmData := pointer(TMemoryStream.Create);
              TMemoryStream(dmnMsg^.dmData).Write(smhData, dmnMsg^.dmDataLen);
            end;
          end;
          dmnMsg^.dmMsg := smhMsgID;
          dmnMsg^.dmClientID := MPMsgNode^.dmnMsg^.dmClientID;
          dmnMsg^.dmTime := MPMsgNode^.dmnMsg^.dmTime;
          dmnMsg^.dmRetryUntil := MPMsgNode^.dmnMsg^.dmRetryUntil;
          dmnMsg^.dmErrorCode := smhErrorCode;
          dmnOffset := smhReplyLen;
          dmnProcessing := false;
        end;
        StackPush(Stack, Temp);
      except
        NodeDestroy(Temp);
        raise;
      end;
      {advance to next submessage}
      if FirstMsg and (SubMsgHdr^.smhErrorCode <> 0) then             
        Break;                                                        
      FirstMsg := false;                                              
      inc(Offset, SubMsgHdr^.smhReplyLen);
      SubMsgHdr := PffsmHeader(PAnsiChar(SubMsgHdr) + SubMsgHdr^.smhReplyLen);
    end;
    {destroy the original multipart message}
    NodeDestroy(MPMsgNode);
    {transfer messages over from stack to queue}
    while not StackIsEmpty(Stack) do begin
      StackPop(Stack, Temp);
      QJump(dmqHead, dmqTail, Temp);
      inc(FCount);
    end;
  finally
    while not StackIsEmpty(Stack) do begin
      StackPop(Stack, Temp);
      NodeDestroy(Temp);
    end;
    FFFreeMem(Stack, sizeof(TffDataMessageNode));
  end;{try..finally}
end;
{====================================================================}


{===CreateSubMessage=================================================}
function FFCreateSubMessage(aSubMsg   : PffsmHeader;
                            aMsgID    : longint;
                            aError    : longint;
                            aDataType : TffNetMsgDataType;
                            aData     : pointer;
                            aDataLen  : longint) : PffsmHeader;
begin
  with aSubMsg^ do begin
    smhMsgID := aMsgID;
    smhReplyLen := ffc_SubMsgHeaderSize + aDataLen;
    smhErrorCode := aError;
    smhDataType := aDataType;
    if (aData <> @smhData) and (aDataLen <> 0) then
      if (aData = nil) then
        Move(aData^, smhData, aDataLen)
      else
        FillChar(smhData, aDataLen, 0);
    Result := PffsmHeader(PAnsiChar(aSubMsg) + smhReplyLen);
  end;
end;
{====================================================================}

end.
