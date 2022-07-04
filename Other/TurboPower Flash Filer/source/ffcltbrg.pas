{*********************************************************}
{* FlashFiler: Range support for Client Tables           *}
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

unit ffcltbrg;

interface

uses
  ffllbase;

type
  TffTableRangeStack = class
    private
      trsStack : pointer;
      trsSavedRequest : pffByteArray;
      trsSavedReqLen  : integer;
    protected

      function GetSavedRequest : boolean;

    public
      constructor Create;
      destructor Destroy; override;

      procedure Clear;
      procedure ClearSaved;
        { Use this method to clear out the saved request bucket. }
      function IsEmpty : boolean;
      procedure Pop(var aRequestPacket : PffByteArray;
                    var aPacketLen     : integer);

      procedure PopSavedRequest(var aRequestPacket : PffByteArray;
                                var aPacketLen     : integer);
        { Use this method to pop the top of the stack into the
          saved request bucket.  This method also returns the
          request and its length so that the caller may resend
          the request to the server.  However, the caller must not
          free this request because it is still in the saved
          bucket. }

      procedure Push(aRequestPacket : PffByteArray;
                     aPacketLen     : integer);


      procedure PushSavedRequest;
        { Use this method to push the last saved request onto the
          range stack.  After it is pushed onto the stack, the last
          saved request is removed from the save bucket. }

      procedure SaveLastRequest(aRequestPacket : PffByteArray;
                                aPacketLen     : integer);
        { This method is used as a bucket to hold the last range
          request.  If a request is already in the bucket, we dispose
          of it prior to saving the new request.
          @param aRequestPacket The setRange message sent to the
            server.
          @param aPacketLen The length of the setRange message sent
            to the server. }

      property SavedRequest : boolean read GetSavedRequest;
        { Returns True if a setRange request is in the saved bucket. }

    end;

implementation

type
  PStackNode = ^TStackNode;
  TStackNode = packed record
    snNext : PStackNode;
    snReq  : PffByteArray;
    snLen  : integer;
  end;

{===TffTableRangeStack===============================================}
constructor TffTableRangeStack.Create;
begin
  inherited Create;
  trsStack := nil; {this means the stack is empty}
  trsSavedRequest := nil;
  trsSavedReqLen := -1;
end;
{--------}
destructor TffTableRangeStack.Destroy;
begin
  Clear;
  inherited Destroy;
end;
{--------}
procedure TffTableRangeStack.Clear;
var
  Req : PffByteArray;
  Len : integer;
begin
  while not IsEmpty do begin
    Pop(Req, Len);
    FreeMem(Req, Len);
  end;
  ClearSaved;
end;
{--------}
procedure TffTableRangeStack.ClearSaved;
begin
  if assigned(trsSavedRequest) then begin
    FFFreeMem(trsSavedRequest, trsSavedReqLen);
    trsSavedRequest := nil;
    trsSavedReqLen := -1;
  end;
end;
{--------}
function TffTableRangeStack.getSavedRequest : boolean;
begin
  result := assigned(trsSavedRequest);
end;                                                                   
{--------}
function TffTableRangeStack.IsEmpty : boolean;
begin
  Result := trsStack = nil;
end;
{--------}
procedure TffTableRangeStack.Pop(var aRequestPacket : PffByteArray;
                                 var aPacketLen     : integer);
var
  Temp : PStackNode;
begin
  Temp := trsStack;
  if (Temp <> nil) then begin
    aRequestPacket := Temp^.snReq;
    aPacketLen := Temp^.snLen;
    trsStack := Temp^.snNext;
    Dispose(Temp);
  end
  else begin
    aRequestPacket := nil;
    aPacketLen := 0;
  end;
end;
{--------}
procedure TffTableRangeStack.PopSavedRequest
                                (var aRequestPacket : PffByteArray;
                                 var aPacketLen     : integer);
var
  Temp : PStackNode;
begin
  Temp := trsStack;
  if (Temp <> nil) then begin
    aRequestPacket := Temp^.snReq;
    aPacketLen := Temp^.snLen;
    trsSavedRequest := aRequestPacket;
    trsSavedReqLen := aPacketLen;
    trsStack := Temp^.snNext;
    Dispose(Temp);
  end
  else begin
    aRequestPacket := nil;
    aPacketLen := 0;
  end;
end;
{--------}
procedure TffTableRangeStack.Push(aRequestPacket : PffByteArray;
                                  aPacketLen     : integer);
var
  Temp : PStackNode;
begin
  New(Temp);
  Temp^.snNext := trsStack;
  Temp^.snReq := aRequestPacket;
  Temp^.snLen := aPacketLen;
  trsStack := Temp;
end;
{--------}
procedure TffTableRangeStack.PushSavedRequest;
var
  Temp : PStackNode;
begin
  New(Temp);
  Temp^.snNext := trsStack;
  Temp^.snReq := trsSavedRequest;
  Temp^.snLen := trsSavedReqLen;
  trsStack := Temp;
  trsSavedRequest := nil;
  trsSavedReqLen := -1;
end;
{--------}
procedure TffTableRangeStack.SaveLastRequest
                               (aRequestPacket : PffByteArray;
                                aPacketLen     : integer);
begin

  if assigned(trsSavedRequest) then
    FFFreeMem(trsSavedRequest, trsSavedReqLen);

  trsSavedRequest := aRequestPacket;
  trsSavedReqLen := aPacketLen;

end;
{====================================================================}

end.
