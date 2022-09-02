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

{$I ffdefine.inc}

unit FFClPlug;

interface

uses
  Classes,
  FFDB,
  FFLLBase,
  FFLLComm;

type
  TffClientPluginEngine = class(TffBasePluginEngine)
  protected
    FSession : TffSession;
    FTimeout : Longint;

    procedure cpSetSession(aSession : TffSession);

    function ProcessRequest(aMsgID           : Longint;
                            aTimeout         : Longint;
                            aRequestData     : Pointer;
                            aRequestDataLen  : Longint;
                            aRequestDataType : TffNetMsgDataType;
                        var aReply           : Pointer;
                        var aReplyLen        : Longint;
                            aReplyType       : TffNetMsgDataType) : TffResult;

    function ProcessRequestNoReply(aMsgID          : Longint;
                                   aTimeout        : Longint;
                                   aRequestData    : Pointer;
                                   aRequestDataLen : Longint ) : TffResult;
  public

    constructor Create(aOwner : TComponent); override;
    destructor Destroy; override;

    procedure FFNotificationEx(const aOp : Byte; aFrom : TffComponent;
                               const aData : TffWord32); override;
      { Method used to detect loss of connection. }


  published

    property Timeout : Longint
      read FTimeout write FTimeout default 10000;

    property Session : TffSession
      read FSession write cpSetSession;

  end;

implementation

uses
  SysUtils,
  Windows,
  FFLLReq,
  FFSrBDE;

{===TffClientPluginEngine============================================}
constructor TffClientPluginEngine.Create(aOwner : TComponent);
begin
  inherited;
  FTimeout := 10000;
end;
{--------}
destructor TffClientPluginEngine.Destroy;
begin
  if FSession <> nil then begin
    FSession.FFRemoveDependent(Self);
    FSession := nil;
  end;
  inherited;
end;
{--------}
procedure TffClientPluginEngine.cpSetSession(aSession : TffSession);
begin
  if FSession = aSession then
    Exit;

  FFNotifyDependents(ffn_Deactivate);
  if Assigned(FSession) then begin
    FSession.FFRemoveDependent(Self);
    FSession := nil;
  end;

  FSession := aSession;
  if Assigned(FSession) then
    FSession.FFAddDependent(Self);
end;
{--------}
procedure TffClientPluginEngine.FFNotificationEx(const aOp   : Byte;
                                                       aFrom : TffComponent;
                                                 const aData : TffWord32);
begin
  if (aFrom = FSession) then
    if ((aOp = ffn_Destroy) or (aOp = ffn_Remove))  then begin
      FFNotifyDependents(ffn_Deactivate);
      FSession := nil;
    end else if (aOp = ffn_Deactivate) then
      FFNotifyDependents(ffn_Deactivate);
end;
{----------}
type
  TffSessionCracker = class(TffSession);
{----------}
function TffClientPluginEngine.ProcessRequest(aMsgID           : longInt;
                                              aTimeout         : longInt;
                                              aRequestData     : Pointer;
                                              aRequestDataLen  : longInt;
                                              aRequestDataType : TffNetMsgDataType;
                                          var aReply           : Pointer;
                                          var aReplyLen        : longInt;
                                              aReplyType       : TffNetMsgDataType
                                             ) : TffResult;
begin
  Result := TffSessionCracker(FSession).ProcessRequest(aMsgID,
                                                       aTimeout,
                                                       aRequestData,
                                                       aRequestDataLen,
                                                       aRequestDataType,
                                                       aReply,
                                                       aReplyLen,
                                                       aReplyType);
end;
{----------}
function TffClientPluginEngine.ProcessRequestNoReply(aMsgID          : Longint;
                                                     aTimeout        : Longint;
                                                     aRequestData    : Pointer;
                                                     aRequestDataLen : Longint
                                                    ) : TffResult;
begin
  Result := TffSessionCracker(FSession).ProcessRequestNoReply(aMsgID,
                                                              aTimeout,
                                                              aRequestData,
                                                              aRequestDataLen);
end;
{====================================================================}

end.
