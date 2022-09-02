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

{$I fsdefine.inc}

Unit fscltbrg;

Interface

Uses
  fsllbase;

Type
  TfsTableRangeStack = Class
  Private
    trsStack: pointer;
    trsSavedRequest: pffByteArray;
    trsSavedReqLen: Integer;
  Protected

    Function GetSavedRequest: boolean;

  Public
    Constructor Create;
    Destructor Destroy; Override;

    Procedure Clear;
    Procedure ClearSaved;
    { Use this method to clear out the saved request bucket. }
    Function IsEmpty: boolean;
    Procedure Pop(Var aRequestPacket: PffByteArray;
      Var aPacketLen: Integer);

    Procedure PopSavedRequest(Var aRequestPacket: PffByteArray;
      Var aPacketLen: Integer);
    { Use this method to pop the top of the stack into the
      saved request bucket.  This method also returns the
      request and its length so that the caller may resend
      the request to the server.  However, the caller must not
      free this request because it is still in the saved
      bucket. }

    Procedure Push(aRequestPacket: PffByteArray;
      aPacketLen: Integer);

    Procedure PushSavedRequest;
    { Use this method to push the last saved request onto the
      range stack.  After it is pushed onto the stack, the last
      saved request is removed from the save bucket. }

    Procedure SaveLastRequest(aRequestPacket: PffByteArray;
      aPacketLen: Integer);
    { This method is used as a bucket to hold the last range
      request.  If a request is already in the bucket, we dispose
      of it prior to saving the new request.
      @param aRequestPacket The setRange message sent to the
        server.
      @param aPacketLen The length of the setRange message sent
        to the server. }

    Property SavedRequest: boolean Read GetSavedRequest;
    { Returns True if a setRange request is in the saved bucket. }

  End;

Implementation

Type
  PStackNode = ^TStackNode;
  TStackNode = Packed Record
    snNext: PStackNode;
    snReq: PffByteArray;
    snLen: Integer;
  End;

  {===TfsTableRangeStack===============================================}

Constructor TfsTableRangeStack.Create;
Begin
  Inherited Create;
  trsStack := Nil; {this means the stack is empty}
  trsSavedRequest := Nil;
  trsSavedReqLen := -1;
End;
{--------}

Destructor TfsTableRangeStack.Destroy;
Begin
  Clear;
  Inherited Destroy;
End;
{--------}

Procedure TfsTableRangeStack.Clear;
Var
  Req: PffByteArray;
  Len: Integer;
Begin
  While Not IsEmpty Do
    Begin
      Pop(Req, Len);
      FreeMem(Req, Len);
    End;
  ClearSaved;
End;
{--------}

Procedure TfsTableRangeStack.ClearSaved;
Begin
  If assigned(trsSavedRequest) Then
    Begin
      FFFreeMem(trsSavedRequest, trsSavedReqLen);
      trsSavedRequest := Nil;
      trsSavedReqLen := -1;
    End;
End;
{--------}

Function TfsTableRangeStack.getSavedRequest: boolean;
Begin
  Result := assigned(trsSavedRequest);
End;
{--------}

Function TfsTableRangeStack.IsEmpty: boolean;
Begin
  Result := trsStack = Nil;
End;
{--------}

Procedure TfsTableRangeStack.Pop(Var aRequestPacket: PffByteArray;
  Var aPacketLen: Integer);
Var
  Temp: PStackNode;
Begin
  Temp := trsStack;
  If (Temp <> Nil) Then
    Begin
      aRequestPacket := Temp^.snReq;
      aPacketLen := Temp^.snLen;
      trsStack := Temp^.snNext;
      Dispose(Temp);
    End
  Else
    Begin
      aRequestPacket := Nil;
      aPacketLen := 0;
    End;
End;
{--------}

Procedure TfsTableRangeStack.PopSavedRequest
  (Var aRequestPacket: PffByteArray;
  Var aPacketLen: Integer);
Var
  Temp: PStackNode;
Begin
  Temp := trsStack;
  If (Temp <> Nil) Then
    Begin
      aRequestPacket := Temp^.snReq;
      aPacketLen := Temp^.snLen;
      trsSavedRequest := aRequestPacket;
      trsSavedReqLen := aPacketLen;
      trsStack := Temp^.snNext;
      Dispose(Temp);
    End
  Else
    Begin
      aRequestPacket := Nil;
      aPacketLen := 0;
    End;
End;
{--------}

Procedure TfsTableRangeStack.Push(aRequestPacket: PffByteArray;
  aPacketLen: Integer);
Var
  Temp: PStackNode;
Begin
  New(Temp);
  Temp^.snNext := trsStack;
  Temp^.snReq := aRequestPacket;
  Temp^.snLen := aPacketLen;
  trsStack := Temp;
End;
{--------}

Procedure TfsTableRangeStack.PushSavedRequest;
Var
  Temp: PStackNode;
Begin
  New(Temp);
  Temp^.snNext := trsStack;
  Temp^.snReq := trsSavedRequest;
  Temp^.snLen := trsSavedReqLen;
  trsStack := Temp;
  trsSavedRequest := Nil;
  trsSavedReqLen := -1;
End;
{--------}

Procedure TfsTableRangeStack.SaveLastRequest
  (aRequestPacket: PffByteArray;
  aPacketLen: Integer);
Begin

  If assigned(trsSavedRequest) Then
    FFFreeMem(trsSavedRequest, trsSavedReqLen);

  trsSavedRequest := aRequestPacket;
  trsSavedReqLen := aPacketLen;

End;
{====================================================================}

End.

