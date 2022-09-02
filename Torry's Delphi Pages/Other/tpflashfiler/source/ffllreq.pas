{*********************************************************}
{* FlashFiler: TffRequest                                *}
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

unit ffllreq;

interface

uses
  ffllbase,
  fflllog;

type

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
  TffReplyModeType = (ffrmReplyExpected, ffrmNoReplyExpected,
                      ffrmNoReplyWaitUntilSent);

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
  TffRequest = class(TffObject)
  private

    FAborted : boolean;
      { Flag set when an exception occurs during Self.WaitForReply.  This is
        typically raised due to a timeout. }

    FBytesToGo : longInt;
      { The number of bytes in the request data remaining to be sent.
        If a request must be sent across multiple packets, this value is
        incremented for each send. }

    FClientID : TffClientID;
      { The client submitting the request. }

    FErrorCode : TffResult;
      { The error code returned from the server. }

    FEvent : TffEvent;
      { Used to wait for a reply. }

    FEventLog : TffBaseLog;
      { For debugging: The event log to which events are written. }

    FMsgID : longInt;
      { The type of message being sent. }

    FPadlock : TffPadlock;
      { Can be used to control read/write access to the request. }

    FReplyData : pointer;
      { The reply data returned from the server.  This is sized to hold
        the entire reply, which may come across in several messages. }

    FReplyDataLen : longInt;
      { The total length of the reply data. }

    FReplyMode : TffReplyModeType;
      { Indicates whether or not the requesting thread expects a reply. }

    FReplyMsgID : longInt;
      { The message ID returned in the reply.  Important because the
        reply may be a multipart message. }

    FReplyOffset : longInt;
      {-In situations where multiple packets are received, this variable
        is used to determine the offset into the FReplyData buffer in which
        the next portion of data should be moved. }

    FRequestData : pointer;
      { The data being sent from client to server. }

    FRequestDataLen : longInt;
      { The length of the data being sent. }

    FStartOffset : longInt;
      { The position in the request data from which the next send will occur.
        This variable is used only when a request must be sent across
        multiple packets. }

    FTimeout : longInt;
      { The number of seconds in which the operation must complete. }

  protected

    procedure rqWriteString(const aMsg : string);
      {-Use this method to write a string to the event log. }

  public

    constructor Create(clientID : TffClientID;
                       msgID : longint;
                       requestData : pointer;
                       requestDataLen  : LongInt;
                       timeout : longInt;
                       const replyMode : TffReplyModeType); virtual;
      { Creates a new request. }

    destructor Destroy; override;

    procedure AddToReply(replyData : pointer;
                         replyDataLen : longInt); virtual;
      { If a reply is so big as to occupy multiple packets, the first
        packet is moved to the reply using the SetReply method.  Data from
        subsequent packets is added to the reply using this method. }

    procedure Lock;
      { Use this method to have a thread obtain exclusive access to the
        request. }

    procedure SetReply(replyMsgID : longInt;
                       errorCode : TffResult;
                       replyData : pointer;
                       totalReplyLen : longInt;
                       replyDataLen : longInt); virtual;
      { Used by the transport to set the reply data.  The TffRequest takes
        ownership of the memory or stream passed in replyData and will free
        it when TffRequest.Destroy is executed (or if recycling of TffRequest
        is implemented in the future. }

    procedure Unlock;
      { Use this method to have a thread release exclusive access to the
        request. }

    procedure WakeUpThread; virtual;
      { This method is called by the transport when a reply has been received
        from the server.  Prior to calling this method, the reply will have
        been placed on the client's message queue via the reply callback. }

    procedure WaitForReply(const timeout : TffWord32); virtual;
      { This method is called by the transport when it has placed a request
        on its Unsent Request Queue.  This method notifies the sender thread
        that a request is ready.  The calling thread is blocked in this
        method until WakeUpThread is called.

        Raises an exception if a timeout occurs or a failure occurs when the
        wait is attempted.
      }

    property Aborted : boolean read FAborted;
      { If set to True then Self.WaitForReply encountered an exception
        (e.g., timeout). }

    property BytesToGo : longInt read FBytesToGo write FBytesToGo;
      { The number of bytes of request data remaining to be sent. }

    property ClientID : TffClientID read FClientID;
      { The client submitting the request. }

    property ErrorCode : TffResult read FErrorCode write FErrorCode;
      { The error code returned from the server. }

    property EventLog : TffBaseLog read FEventLog write FEventLog;
      { The event log to which debugging messages should be written. }

    property MsgID : longInt read FMsgID;
      { The type of message being sent. }

    property ReplyData : pointer read FReplyData write FReplyData;
      { The reply received from the server.  Will be nil if a timeout or
        some other failure occurs. }

    property ReplyDataLen : longInt read FReplyDataLen write FReplyDataLen;
      { The length of the reply. }

    property ReplyMode : TffReplyModeType read FReplyMode;
      { Indicates whether or not a reply is expected for this request. }

    property ReplyMsgID : longInt read FReplyMsgID write FReplyMsgID;
      { The message ID returned in the reply.  Important because it
        may be a multipart message. }

    property RequestData : pointer read FRequestData;
      { The buffer containing the data to be sent. }

    property RequestDataLen : longInt read FRequestDataLen;
      { The length of the request data. }

    property StartOffset : longInt read FStartOffset write FStartOffset;
      { The position within request data from which the next send is to
        draw data.  Used when the request is to be sent across multiple
        packets. }

    property Timeout : longInt read FTimeout;
      { The number of seconds in which the operation must complete. }

  end;

implementation

uses
  SysUtils,
  ffconst,
  ffllexcp;

{===TffBaseTransport=================================================}
constructor TffRequest.Create(clientID : TffClientID;
                              msgID : longint;
                              requestData : pointer;
                              requestDataLen  : LongInt;
                              timeout : longInt;
                              const replyMode : TffReplyModeType);
begin
  inherited Create;
  FAborted := False;
  FBytesToGo := requestDataLen;
  FClientID := clientID;
  FErrorCode := 0;
  FEvent := TffEvent.Create;
  FMsgID := msgID;
  FPadlock := TffPadlock.Create;
  FReplyData := nil;
  FReplyDataLen := -1;
  FReplyMode := replyMode;
  FReplyMsgID := -1;
  { Copy the request data. }
  FRequestDataLen := requestDataLen;
  if FRequestDataLen > 0 then begin
    FFGetMem(FRequestData, FRequestDataLen);
    Move(requestData^, FRequestData^, FRequestDataLen);
  end else
    FRequestData := nil;
  FStartOffset := 0;
  FTimeout := timeout;
end;
{--------}
destructor TffRequest.Destroy;
begin

  { Make sure we can get exclusive access to this object. }
  FPadlock.Lock;

  FEvent.Free;
  FPadlock.Free;
  { We are responsible for the request and reply data.  Free it.
    Note: Assumes it was created using FFGetMem. }
  if assigned(FRequestData) then
    FFFreeMem(FRequestData, FRequestDataLen);
  if assigned(FReplyData) then
    FFFreeMem(FReplyData, FReplyDataLen);
  inherited Destroy;
end;
{--------}
procedure TffRequest.AddToReply(replyData : pointer;
                                replyDataLen : longInt);
var
  BytesToCopy : longInt;
begin
  { Move this chunk of data into the reply buffer. }
  BytesToCopy := FFMinL(replyDataLen, FReplyDataLen - FReplyOffset);
  Move(replyData^, PffBLOBArray(FReplyData)^[FReplyOffset], BytesToCopy);
  inc(FReplyOffset, BytesToCopy);
end;
{--------}
procedure TffRequest.Lock;
begin
  FPadlock.Lock;
end;
{--------}
procedure TffRequest.rqWriteString(const aMsg : string);
begin
  if assigned(FEventLog) then
    FEventLog.WriteString(aMsg);
end;
{--------}
procedure TffRequest.SetReply(replyMsgID : longInt;
                              errorCode : TffResult;
                              replyData : pointer;
                              totalReplyLen : longInt;
                              replyDataLen : longInt);
begin
  FReplyMsgID := replyMsgID;
  FErrorCode := errorCode;
  FReplyDataLen := totalReplyLen;
  { Obtain space to store the entire reply. }
  FFGetMem(FReplyData, totalReplyLen);
  { Move in the portion of the reply just received. }
  Move(replyData^, FReplyData^, replyDataLen);
  FReplyOffset := replyDataLen;
end;
{--------}
procedure TffRequest.Unlock;
begin
  FPadlock.Unlock;
end;
{--------}
procedure TffRequest.WakeUpThread;
begin
  FEvent.SignalEvent;
end;
{--------}
procedure TffRequest.WaitForReply(const timeout : TffWord32);
begin
  try
    FEvent.WaitFor(timeout);
  except
    on E:Exception do begin
      if E is EffException then
        ErrorCode := EffException(E).ErrorCode;
      FReplyMsgID := FMsgID;
      FAborted := True;
    end;
  end;
end;
{====================================================================}

end.
