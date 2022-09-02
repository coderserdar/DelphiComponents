{ Notes:
  The purpose of this unit is to allow a server command
  handler to register itself with a server engine.
  We couldn't embed this functionality into TfFServerEngine
  and TffServerCommandHandler because they are declared in separate
  units. }

{*********************************************************}
{* FlashFiler: Server intermediate classes               *}
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

unit ffsrintm;

interface

uses
  Classes,
  ffllbase,
  ffllcomm,
  ffllcomp,
  fflleng,
  fflllgcy;

type

  TffIntermediateCommandHandler = class;  { forward declaration }

  TffIntermediateServerEngine = class(TffBaseServerEngine)
  protected
    FCmdHandlers : TffThreadList;

    function  iseGetCmdHandler(aInx : Longint) : TffIntermediateCommandHandler;

    function  iseGetCmdHandlerCount : Longint;

    procedure scSetState(const aState : TffState); override;

  public

    constructor Create(aOwner : TComponent); override;

    destructor Destroy; override;

    procedure FFAddDependent(ADependent : TffComponent); override;     {!!.11}
    procedure FFRemoveDependent(ADependent : TffComponent); override;  {!!.11}

    property CmdHandler[aInx : Longint] : TffIntermediateCommandHandler
      read iseGetCmdHandler;

    property CmdHandlerCount : Longint read iseGetCmdHandlerCount;

  end;


  TffIntermediateCommandHandler = class(TffBaseCommandHandler)
  protected

    FServerEngine : TffIntermediateServerEngine;

    procedure ichLog(const aMsg : string); virtual;
      {-Use this method to log a string to the event log. }

    procedure ichLogFmt(const aMsg : string; args : array of const); virtual;
      {-Use this method to log a formatted string to the event log. }

    procedure ichLogAll(const Msgs : array of string); virtual;
      {-Use this method to log multiple strings to the event log. }

    procedure ichLogBlock(const S : string; Buf : pointer;
                          BufLen : TffMemSize); virtual;

    procedure ichSetServerEngine(anEngine : TffIntermediateServerEngine); virtual;
      {-Sets the server engine to be used by this command handler. }

  public

    constructor Create(aOwner : TComponent); override;

    destructor Destroy; override;

    procedure FFNotificationEx(const AOp : Byte; AFrom : TffComponent; {!!.11}
                               const AData : TffWord32); override;     {!!.11}

  published

    property ServerEngine : TffIntermediateServerEngine read FServerEngine
                                                        write ichSetServerEngine;
      { The server engine handling requests received by this command handler. }

  end;

implementation

uses
  SysUtils;

{===TffIntermediateServerEngine===========================================}
constructor TffIntermediateServerEngine.Create(aOwner : TComponent);
begin
  inherited Create(aOwner);
  FCmdHandlers := TffThreadList.Create;
end;
{--------}
destructor TffIntermediateServerEngine.Destroy;
{Rewritten !!.11}
begin
  FFNotifyDependents(ffn_Destroy);
  FCmdHandlers.Free;
  inherited Destroy;
end;
{Begin !!.11}
{--------}
procedure TffIntermediateServerEngine.FFAddDependent(ADependent : TffComponent);
var
  aListItem : TffIntListItem;
begin
  inherited;
  if ADependent is TffIntermediateCommandHandler then begin
    aListItem := TffIntListItem.Create(LongInt(ADependent));
    with FCmdHandlers.BeginWrite do
      try
        Insert(aListItem);
      finally
        EndWrite;
      end;
  end;
end;
{--------}
procedure TffIntermediateServerEngine.FFRemoveDependent(ADependent : TffComponent);
begin
  inherited;
  if ADependent is TffIntermediateCommandHandler then
    with FCmdHandlers.BeginWrite do
      try
        Delete(LongInt(ADependent));
      finally
        EndWrite;
      end;
end;
{End !!.11}
{--------}
function TffIntermediateServerEngine.iseGetCmdHandler(aInx: Integer) : TffIntermediateCommandHandler;
begin
  Result := TffIntermediateCommandHandler(TffIntListItem(FCmdHandlers[aInx]).KeyAsInt);
end;
{--------}
function TffIntermediateServerEngine.iseGetCmdHandlerCount: Longint;
begin
  Result := FCmdHandlers.Count;
end;
{--------}
procedure TffIntermediateServerEngine.scSetState(const aState : TffState);
var
  Idx       : longInt;
  NextState : TffState;
  Handler   : TffBaseCommandHandler;
begin
  if aState = State then exit;

  if Assigned(FCmdHandlers) then
    with FCmdHandlers.BeginRead do
      try
        while State <> aState do begin
          { Based upon our current state & the target state, get the next state. }
          NextState := ffStateDiagram[State, aState];

          { Move all command handlers to the specified state. }
          for Idx := Pred(Count) downto 0 do begin
            Handler := TffBaseCommandHandler(TffIntListItem(Items[Idx]).KeyAsInt);
            Handler.State := NextState;
          end;

          {For each step, call the inherited SetState method. The inherited
           method is responsible for calling the state methods for the
           engine}
          inherited scSetState(NextState);
        end;
      finally
        EndRead;
      end;
end;

{=========================================================================}

{===TffIntermediateCommandHandler=========================================}
constructor TffIntermediateCommandHandler.Create(aOwner : TComponent);
begin
  inherited Create(aOwner);
  FServerEngine := nil;
end;
{--------}
destructor TffIntermediateCommandHandler.Destroy;
begin
  if assigned(FServerEngine) then
    FServerEngine.FFRemoveDependent(Self);

  inherited Destroy;
end;
{--------}
procedure TffIntermediateCommandHandler.ichLog(const aMsg : string);
begin
  if FLogEnabled and assigned(FEventLog) then
    FEventLog.WriteString(aMsg);
end;
{--------}
procedure TffIntermediateCommandHandler.ichLogAll(const Msgs : array of string);
begin
  if FLogEnabled and assigned(FEventLog) then
    FEventLog.WriteStrings(Msgs);
end;
{--------}
procedure TffIntermediateCommandHandler.ichLogBlock(const S : string; Buf : pointer;
                                                    BufLen : TffMemSize);
begin
  if FLogEnabled and assigned(FEventLog) then
    FEventLog.WriteBlock(S, Buf, BufLen);
end;
{--------}
procedure TffIntermediateCommandHandler.ichLogFmt(const aMsg : string; args : array of const);
begin
  if FLogEnabled and assigned(FEventLog) then
    FEventLog.WriteString(format(aMsg, args));
end;
{--------}
procedure TffIntermediateCommandHandler.ichSetServerEngine(anEngine : TffIntermediateServerEngine);
begin
  if FServerEngine = anEngine then Exit;

  scCheckInactive;

  if assigned(FServerEngine) then
    FServerEngine.FFRemoveDependent(Self);                             {!!.11}

  if assigned(anEngine) then
    anEngine.FFAddDependent(Self);                                     {!!.11}

  FServerEngine := anEngine;

end;
{--------}
{Rewritten !!.11}
procedure TffIntermediateCommandHandler.FFNotificationEx
            (const AOp : Byte; AFrom : TffComponent;
             const AData : TffWord32);
begin
  inherited;
  if (AFrom = FServerEngine) and
     (AOp in [ffn_Destroy, ffn_Remove]) then begin
    State := ffesStopped;
    FServerEngine.FFRemoveDependent(Self);
    FServerEngine := nil;
  end;
end;
{=========================================================================}
end.
