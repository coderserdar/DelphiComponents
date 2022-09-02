{*********************************************************}
{* FlashFiler: Client plugin engine                      *}
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

Unit fsclplug;

Interface

Uses
  Classes,
  fsdb,
  FsLLBase,
  FsLLComm;

Type
  TFSClientPluginEngine = Class(TFSBasePluginEngine)
  Protected
    FSession: TFSSession;
    FTimeout: Longint;

    Procedure cpSetSession(aSession: TFSSession);

    Function ProcessRequest(aMsgID: Longint;
      aTimeout: Longint;
      aRequestData: Pointer;
      aRequestDataLen: Longint;
      aRequestDataType: TffNetMsgDataType;
      Var aReply: Pointer;
      Var aReplyLen: Longint;
      aReplyType: TffNetMsgDataType): TffResult;

    Function ProcessRequestNoReply(aMsgID: Longint;
      aTimeout: Longint;
      aRequestData: Pointer;
      aRequestDataLen: Longint): TffResult;
  Public

    Constructor Create(aOwner: TComponent); Override;
    Destructor Destroy; Override;

    Procedure FFNotificationEx(Const aOp: Byte; aFrom: TFSSpecComp;
      Const aData: TffWord32); Override;
    { Method used to detect loss of connection. }

  Published

    Property Timeout: Longint
      Read FTimeout Write FTimeout Default 10000;

    Property Session: TFSSession
      Read FSession Write cpSetSession;

  End;

Implementation

Uses
  SysUtils,
  Windows,
  FsLLReq,
  FsSrBDE;

{===TFSClientPluginEngine============================================}

Constructor TFSClientPluginEngine.Create(aOwner: TComponent);
Begin
  Inherited;
  FTimeout := 10000;
End;
{--------}

Destructor TFSClientPluginEngine.Destroy;
Begin
  If FSession <> Nil Then
    Begin
      FSession.FFRemoveDependent(Self);
      FSession := Nil;
    End;
  Inherited;
End;
{--------}

Procedure TFSClientPluginEngine.cpSetSession(aSession: TFSSession);
Begin
  If FSession = aSession Then
    Exit;

  FFNotifyDependents(ffn_Deactivate);
  If Assigned(FSession) Then
    Begin
      FSession.FFRemoveDependent(Self);
      FSession := Nil;
    End;

  FSession := aSession;
  If Assigned(FSession) Then
    FSession.FFAddDependent(Self);
End;
{--------}

Procedure TFSClientPluginEngine.FFNotificationEx(Const aOp: Byte;
  aFrom: TFSSpecComp;
  Const aData: TffWord32);
Begin
  If (aFrom = FSession) Then
    If ((aOp = ffn_Destroy) Or (aOp = ffn_Remove)) Then
      Begin
        FFNotifyDependents(ffn_Deactivate);
        FSession := Nil;
      End
    Else If (aOp = ffn_Deactivate) Then
      FFNotifyDependents(ffn_Deactivate);
End;
{----------}
Type
  TffSessionCracker = Class(TFSSession);
  {----------}

Function TFSClientPluginEngine.ProcessRequest(aMsgID: Longint;
  aTimeout: Longint;
  aRequestData: Pointer;
  aRequestDataLen: Longint;
  aRequestDataType: TffNetMsgDataType;
  Var aReply: Pointer;
  Var aReplyLen: Longint;
  aReplyType: TffNetMsgDataType
  ): TffResult;
Begin
  Result := TffSessionCracker(FSession).ProcessRequest(aMsgID,
    aTimeout,
    aRequestData,
    aRequestDataLen,
    aRequestDataType,
    aReply,
    aReplyLen,
    aReplyType);
End;
{----------}

Function TFSClientPluginEngine.ProcessRequestNoReply(aMsgID: Longint;
  aTimeout: Longint;
  aRequestData: Pointer;
  aRequestDataLen: Longint
  ): TffResult;
Begin
  Result := TffSessionCracker(FSession).ProcessRequestNoReply(aMsgID,
    aTimeout,
    aRequestData,
    aRequestDataLen);
End;
{====================================================================}

End.

