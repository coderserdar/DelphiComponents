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
 * The Original Code is TurboPower B-Tree Filer
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Based in part on code written by Ralf Nagel
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{--Conditional defines and compiler options}
  {$I BTDefine.Inc}
  {$B+,F-,I-,R-,S-,V-}
  {$IFDEF CanAllowOverlays}
    {$F+,O+,A-}
  {$ENDIF}
  {$IFDEF CanSetOvrflowCheck}
    {$Q-,P-}
  {$ENDIF}
  {all other compiler options are 'don't care'}

{$IFDEF Win32}
  !! Error - this unit cannot be compiled for 32-bit
{$ENDIF}

unit NWMsg;
  {-Unit to provide NetWare messaging functionality.}

interface

uses
  NWBase,
  NWConn;

type
  {Different broadcast message modes}
  TnwBroadcastMode = (bmDisplayBoth,   {display both server and user messages}
                      bmDisplayServer, {display only server messages}
                      bmStoreServer,   {store only server messages}
                      bmStoreBoth);    {store both server and user messages}

{---Broadcast mode routines---}
function nwGetBroadcastMode(Server : TnwServer;
                            var Mode : TnwBroadcastMode) : TnwErrorCode;
  {-Get the broadcast mode.}

function nwSetBroadcastMode(Server : TnwServer;
                            Mode : TnwBroadcastMode) : TnwErrorCode;
  {-Set the broadcast mode.}

{---Message routines---}
function nwSendMessageToConsole(Server : TnwServer;
                                Message : string) : TnwErrorCode;
  {-Send a message to the console.}

function nwSendBroadcastMessage(Server : TnwServer;
                                Message : string;
                                var ToList : TnwConnList) : TnwErrorCode;
  {-Broadcast a message to a group of connections.
    Note: For NetWare 3.20 and above, the maximum number of connections
          that can be sent to at once is 62 and the message length is 254
          characters; for earlier servers the corresponding values are 256
          connections and 58 characters.}

function nwGetBroadcastMessage(Server : TnwServer;
                               var Message : string) : TnwErrorCode;
  {-Get a broadcast message.}

implementation

function nwSendMessageToConsole(Server : TnwServer;
                                Message : string) : TnwErrorCode;
  var
    Request : record
      Len  : word;
      Func : byte;
      Msg  : string[58];
    end;
    Dummy : word;
  begin
    {API: Broadcast To Console}
    with Request do
      begin
        Msg  := Message;
        Len  := Length(Msg) + 2;
        Func := $09;
      end;
    nwSendMessageToConsole := nwServerCall(Server, $15,
                                           Request.Len+2, Request,
                                           0, Dummy);
  end;

function NETXGetBroadcastMode(Server : TnwServer;
                              var Mode : TnwBroadcastMode) : TnwErrorCode;
  begin
    {API: Get Broadcast Mode}
    nwNETXPushServer(Server);
    asm
      mov ax, $DE00
      mov dl, $04
      int $21
      les di, Mode
      mov es:[di], al
    end;
    nwNETXPopServer;
    NETXGetBroadcastMode := 0;
  end;

function VLMGetBroadcastMode(Server : TnwServer;
                             var Mode : TnwBroadcastMode) : TnwErrorCode;
  var
    vlmResult : word;
    Regs : TnwRegisters;
  begin
    {Note: the Novell Client API docs are ambiguous about whether a
           workstation can have a different mode per server.}
    {API: _NWP Message Handler}
    nwInitRegs(Regs);
    Regs.BX := $02;
    Regs.DX := 4;
    Regs.CX := Server;
    vlmResult := vlmCall(vlmNWP, $0A, Regs);
    if (vlmResult = 0) then
      Mode := TnwBroadcastMode(Regs.DL);
    VLMGetBroadcastMode := vlmResult;
  end;

function nwGetBroadcastMode(Server : TnwServer;
                            var Mode : TnwBroadcastMode) : TnwErrorCode;
  begin
    case nwShellType of
      nsNETX : nwGetBroadcastMode := NETXGetBroadcastMode(Server, Mode);
      nsVLM  : nwGetBroadcastMode := VLMGetBroadcastMode(Server, Mode);
    else
      begin
        nwGetBroadcastMode := nwErrShell;
      end;
    end;{case}
  end;

function NETXSetBroadcastMode(Server : TnwServer;
                              Mode : TnwBroadcastMode) : TnwErrorCode;
  begin
    {API: Set Broadcast Mode}
    nwNETXPushServer(Server);
    asm
      mov ax, $DE00
      mov dl, Mode
      int $21
    end;
    nwNETXPopServer;
    NETXSetBroadcastMode := 0;
  end;

function VLMSetBroadcastMode(Server : TnwServer;
                             Mode : TnwBroadcastMode) : TnwErrorCode;
  var
    Regs : TnwRegisters;
  begin
    {Note: the Novell Client API docs are ambiguous about whether a
           workstation can have a different mode per server.}
    {API: _NWP Message Handler}
    nwInitRegs(Regs);
    Regs.BX := $02;
    Regs.DX := ord(Mode);
    Regs.CX := Server;
    VLMSetBroadcastMode := vlmCall(vlmNWP, $0A, Regs);
  end;

function nwSetBroadcastMode(Server : TnwServer;
                            Mode : TnwBroadcastMode) : TnwErrorCode;
  begin
    case nwShellType of
      nsNETX : nwSetBroadcastMode := NETXSetBroadcastMode(Server, Mode);
      nsVLM  : nwSetBroadcastMode := VLMSetBroadcastMode(Server, Mode);
    else
      nwSetBroadcastMode := nwErrShell;
    end;{case}
  end;

function nwGetBroadcastMessage(Server : TnwServer;
                               var Message : string) : TnwErrorCode;
  var
    Request : record
      Len  : word;
      Func : byte;
    end;
  begin
    {Note: the NWCALLS DLL uses server version 3.20 as the break point
           between using function $01 or function $0B.}
    {API: Get Broadcast Message}
    with Request do
      begin
        Len  := 1;
        if (nwServerVersion(Server) >= $0320) then
             Func := $0B
        else Func := $01;
      end;
    nwGetBroadcastMessage := nwServerCall(Server, $15,
                                          sizeof(Request), Request,
                                          sizeof(Message), Message);
  end;

function Send1000UserMsg(Server : TnwServer;
                         Message : string;
                         var ToList : TnwConnList) : TnwErrorCode;
  type
    PRequest = ^TRequest;
    TRequest = record                   {variably sized}
      Len  : word;
      Func : byte;
      Count: word;
      Rest : array [0..199] of nwLong;
    end;
  var
    Status  : word;
    ReqLen  : word;
    Request : PRequest;
    i       : word;
    Reply   : array [0..255] of byte;
  begin
    {API: Send Broadcast Message}
    {Calc the size of the request packet: sizeof(Len+Func+Count) +
     4 bytes for each connection number + the message length + 1.}
    ReqLen := 5 + (ToList.Count * 4) + 1 + length(Message);
    if not nwGetMem(Request, ReqLen) then
      Status := nwErrMemory
    else
      begin
        with Request^ do
          begin
            Len := ReqLen - 2;
            Func := $0A;
            Count := ToList.Count;
            for i := 0 to pred(Count) do
              Rest[i] := ToList.List[i];
            Move(Message, Rest[Count], succ(length(Message)));
          end;
        Status := nwServerCall(Server, $15, ReqLen, Request^,
                                            sizeof(Reply), Reply);
        FreeMem(Request, ReqLen);
      end;
    Send1000UserMsg := Status;
  end;

function SendMsgOldStyle(Server : TnwServer;
                         Message : string;
                         var ToList : TnwConnList) : TnwErrorCode;
  type
    PRequest = ^TRequest;
    TRequest = record                {variably sized}
      Len  : word;
      Func : byte;
      Count: byte;
      Rest : array [0..999] of byte;
    end;
  var
    Status  : word;
    ReqLen  : word;
    Request : PRequest;
    i       : word;
    Reply   : array [0..255] of byte;
  begin
    {API: Send Broadcast Message (old)}
    {Calc the size of the request packet: sizeof(Len+Func+Count) +
     1 byte for each connection number + the message length + 1.}
    ReqLen := 5 + (ToList.Count) + 1 + length(Message);
    if not nwGetMem(Request, ReqLen) then
      Status := nwErrMemory
    else
      begin
        with Request^ do
          begin
            Len := ReqLen - 2;
            Func := $00;
            Count := ToList.Count;
            for i := 0 to pred(Count) do
              Rest[i] := ToList.List[i];
            Move(Message, Rest[Count], nwMinI(59, succ(length(Message))));
          end;
        Status := nwServerCall(Server, $15, ReqLen, Request^,
                                            sizeof(Reply), Reply);
        FreeMem(Request, ReqLen);
      end;
    SendMsgOldStyle := Status;
  end;

function nwSendBroadcastMessage(Server : TnwServer;
                                Message : string;
                                var ToList : TnwConnList) : TnwErrorCode;
  begin
    {Note: the NWCALLS DLL uses server version 3.20 as the break point
           between using old style and new style calls.}
    if (ToList.Count = 0) or (Message = '') then
      begin
        nwSendBroadcastMessage := 0; {success!}
        Exit;
      end;
    if (nwServerVersion(Server) >= $0320) then
      if (ToList.Count > 62) then {max num for 3.20+}
        nwSendBroadcastMessage := nwErrTooManyConns
      else
        nwSendBroadcastMessage := Send1000UserMsg(Server, Message, ToList)
    else {server is earlier than 3.20}
      if (ToList.Count > 256) then {max num}
        nwSendBroadcastMessage := nwErrTooManyConns
      else
        nwSendBroadcastMessage := SendMsgOldStyle(Server, Message, ToList);
  end;

end.
