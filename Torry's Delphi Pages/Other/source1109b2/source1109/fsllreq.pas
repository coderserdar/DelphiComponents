{*********************************************************}
{* FSSQL: TfsRequest                                     *}
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

{$I fsdefine.inc}

Unit fsllreq;

Interface

Uses
  fsllbase,
  fslllog;

Type

  { This enumerated type tells a transport whether or not a reply is expected
    for a request.  Values:

    ffrmReplyExpected - The requesting thread will be locked until the request
      has been sent and a reply to the request has been received.

    ffrmNoReplyExpected - The requesting thread does not expect a reply to the
      request.  The requesting thread will continue processing as
      soon as the request has been submitted to the sending thread.

    ffrmNoReplyWaitUntilSent - The requesting thread does not expect a reply.
      The requesting thread will continue processing as soon as the request
      has been sent to the remote server. }
  TfsReplyModeType = (fsrmReplyExpected, fsrmNoReplyExpected,
    fsrmNoReplyWaitUntilSent);

  { This class is used by TffThreadedTransport to manage requests sent to the
    server.  A request instance is placed in the transport's pending queue.
    The transport's send thread retrieves the request from the queue.
    If a reply is expected, the send thread puts the request "on hold" until a
    reply is received.  The reply is stored on the request via the SetReply
    method and the request is awakened.

    Note: An instance of this class stores the reply data received from the
    server.  When the instance is destroyed, it is responsible for freeing the
    reply data.
  }
  TfsRequest = Class(TFSSpecObject)
  Private

    FAborted: boolean;
    { Flag set when an exception occurs during Self.WaitForReply.  This is
      typically raised due to a timeout. }

    FBytesToGo: Longint;
    { The number of bytes in the request data remaining to be sent.
      If a request must be sent across multiple packets, this value is
      incremented for each send. }

    FClientID: TffClientID;
    { The client submitting the request. }

    FErrorCode: TffResult;
    { The error code returned from the server. }

    FEvent: TFSNormalEvent;
    { Used to wait for a reply. }

    FEventLog: TFSBaseLog;
    { For debugging: The event log to which events are written. }

    FMsgID: Longint;
    { The type of message being sent. }

    FPadlock: TfsPadlock;
    { Can be used to control read/write access to the request. }

    FReplyData: pointer;
    { The reply data returned from the server.  This is sized to hold
      the entire reply, which may come across in several messages. }

    FReplyDataLen: Longint;
    { The total length of the reply data. }

    FReplyMode: TfsReplyModeType;
    { Indicates whether or not the requesting thread expects a reply. }

    FReplyMsgID: Longint;
    { The message ID returned in the reply.  Important because the
      reply may be a multipart message. }

    FReplyOffset: Longint;
    {-In situations where multiple packets are received, this variable
      is used to determine the offset into the FReplyData buffer in which
      the next portion of data should be moved. }

    FRequestData: pointer;
    { The data being sent from client to server. }

    FRequestDataLen: Longint;
    { The length of the data being sent. }

    FStartOffset: Longint;
    { The position in the request data from which the next send will occur.
      This variable is used only when a request must be sent across
      multiple packets. }

    FTimeout: Longint;
    { The number of seconds in which the operation must complete. }

  Protected

    Procedure rqWriteString(Const aMsg: String);
    {-Use this method to write a string to the event log. }

  Public

    Constructor Create(clientID: TffClientID;
      msgID: Longint;
      requestData: pointer;
      requestDataLen: Longint;
      timeout: Longint;
      Const replyMode: TfsReplyModeType); Virtual;
    { Creates a new request. }

    Destructor Destroy; Override;

    Procedure AddToReply(replyData: pointer;
      replyDataLen: Longint); Virtual;
    { If a reply is so big as to occupy multiple packets, the first
      packet is moved to the reply using the SetReply method.  Data from
      subsequent packets is added to the reply using this method. }

    Procedure Lock;
    { Use this method to have a thread obtain exclusive access to the
      request. }

    Procedure SetReply(replyMsgID: Longint;
      errorCode: TffResult;
      replyData: pointer;
      totalReplyLen: Longint;
      replyDataLen: Longint); Virtual;
    { Used by the transport to set the reply data.  The TfsRequest takes
      ownership of the memory or stream passed in replyData and will free
      it when TfsRequest.Destroy is executed (or if recycling of TfsRequest
      is implemented in the future. }

    Procedure Unlock;
    { Use this method to have a thread release exclusive access to the
      request. }

    Procedure WakeUpThread; Virtual;
    { This method is called by the transport when a reply has been received
      from the server.  Prior to calling this method, the reply will have
      been placed on the client's message queue via the reply callback. }

    Procedure WaitForReply(Const timeout: TffWord32); Virtual;
    { This method is called by the transport when it has placed a request
      on its Unsent Request Queue.  This method notifies the sender thread
      that a request is ready.  The calling thread is blocked in this
      method until WakeUpThread is called.

      Raises an exception if a timeout occurs or a failure occurs when the
      wait is attempted.
    }

    Property Aborted: boolean Read FAborted;
    { If set to True then Self.WaitForReply encountered an exception
      (e.g., timeout). }

    Property BytesToGo: Longint Read FBytesToGo Write FBytesToGo;
    { The number of bytes of request data remaining to be sent. }

    Property ClientID: TffClientID Read FClientID;
    { The client submitting the request. }

    Property ErrorCode: TffResult Read FErrorCode Write FErrorCode;
    { The error code returned from the server. }

    Property EventLog: TFSBaseLog Read FEventLog Write FEventLog;
    { The event log to which debugging messages should be written. }

    Property MsgID: Longint Read FMsgID;
    { The type of message being sent. }

    Property ReplyData: pointer Read FReplyData Write FReplyData;
    { The reply received from the server.  Will be nil if a timeout or
      some other failure occurs. }

    Property ReplyDataLen: Longint Read FReplyDataLen Write FReplyDataLen;
    { The length of the reply. }

    Property ReplyMode: TfsReplyModeType Read FReplyMode;
    { Indicates whether or not a reply is expected for this request. }

    Property ReplyMsgID: Longint Read FReplyMsgID Write FReplyMsgID;
    { The message ID returned in the reply.  Important because it
      may be a multipart message. }

    Property RequestData: pointer Read FRequestData;
    { The buffer containing the data to be sent. }

    Property RequestDataLen: Longint Read FRequestDataLen;
    { The length of the request data. }

    Property StartOffset: Longint Read FStartOffset Write FStartOffset;
    { The position within request data from which the next send is to
      draw data.  Used when the request is to be sent across multiple
      packets. }

    Property Timeout: Longint Read FTimeout;
    { The number of seconds in which the operation must complete. }

  End;

Implementation

Uses
  SysUtils,
  fsconst,
  fsllexcp;

{===TffBaseTransport=================================================}

Constructor TfsRequest.Create(clientID: TffClientID;
  msgID: Longint;
  requestData: pointer;
  requestDataLen: Longint;
  timeout: Longint;
  Const replyMode: TfsReplyModeType);
Begin
  Inherited Create;
  FAborted := False;
  FBytesToGo := requestDataLen;
  FClientID := clientID;
  FErrorCode := 0;
  FEvent := TFSNormalEvent.Create;
  FMsgID := msgID;
  FPadlock := TfsPadlock.Create;
  FReplyData := Nil;
  FReplyDataLen := -1;
  FReplyMode := replyMode;
  FReplyMsgID := -1;
  { Copy the request data. }
  FRequestDataLen := requestDataLen;
  If FRequestDataLen > 0 Then
    Begin
      FFGetMem(FRequestData, FRequestDataLen);
      Move(requestData^, FRequestData^, FRequestDataLen);
    End
  Else
    FRequestData := Nil;
  FStartOffset := 0;
  FTimeout := timeout;
End;
{--------}

Destructor TfsRequest.Destroy;
Begin

  { Make sure we can get exclusive access to this object. }
  FPadlock.Lock;

  FEvent.Free;
  FPadlock.Free;
  { We are responsible for the request and reply data.  Free it.
    Note: Assumes it was created using FFGetMem. }
  If assigned(FRequestData) Then
    FFFreeMem(FRequestData, FRequestDataLen);
  If assigned(FReplyData) Then
    FFFreeMem(FReplyData, FReplyDataLen);
  Inherited Destroy;
End;
{--------}

Procedure TfsRequest.AddToReply(replyData: pointer;
  replyDataLen: Longint);
Var
  BytesToCopy: Longint;
Begin
  { Move this chunk of data into the reply buffer. }
  BytesToCopy := FFMinL(replyDataLen, FReplyDataLen - FReplyOffset);
  Move(replyData^, PffBLOBArray(FReplyData)^[FReplyOffset], BytesToCopy);
  inc(FReplyOffset, BytesToCopy);
End;
{--------}

Procedure TfsRequest.Lock;
Begin
  FPadlock.Lock;
End;
{--------}

Procedure TfsRequest.rqWriteString(Const aMsg: String);
Begin
  If assigned(FEventLog) Then
    FEventLog.WriteString(aMsg);
End;
{--------}

Procedure TfsRequest.SetReply(replyMsgID: Longint;
  errorCode: TffResult;
  replyData: pointer;
  totalReplyLen: Longint;
  replyDataLen: Longint);
Begin
  FReplyMsgID := replyMsgID;
  FErrorCode := errorCode;
  FReplyDataLen := totalReplyLen;
  { Obtain space to store the entire reply. }
  FFGetMem(FReplyData, totalReplyLen);
  { Move in the portion of the reply just received. }
  Move(replyData^, FReplyData^, replyDataLen);
  FReplyOffset := replyDataLen;
End;
{--------}

Procedure TfsRequest.Unlock;
Begin
  FPadlock.Unlock;
End;
{--------}

Procedure TfsRequest.WakeUpThread;
Begin
  FEvent.SignalEvent;
End;
{--------}

Procedure TfsRequest.WaitForReply(Const timeout: TffWord32);
Begin
  Try
    FEvent.WaitFor(timeout);
  Except
    On E: Exception Do
      Begin
        If E Is EfsException Then
          ErrorCode := EfsException(E).ErrorCode;
        FReplyMsgID := FMsgID;
        FAborted := True;
      End;
  End;
End;
{====================================================================}

End.

