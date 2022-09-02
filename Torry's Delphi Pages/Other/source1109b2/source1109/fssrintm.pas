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

{$I fsdefine.inc}

Unit fssrintm;

Interface

Uses
  Classes,
  fsllbase,
  fsllcomm,
  fsllcomp,
  fslleng,
  fslllgcy;

Type

  TfsIntermediateCommandHandler = Class; { forward declaration }

  TfsIntermediateServerEngine = Class(TFSBaseServerEngine)
  Protected
    FCmdHandlers: TFSSpecThreadList;
    FListEvents: TFSSpecStringList;
    FListEventsTransaction: TFSSpecStringList;

    Function iseGetCmdHandler(aInx: Longint): TfsIntermediateCommandHandler;

    Function iseGetCmdHandlerCount: Longint;

    Procedure scSetState(Const aState: TfsState); Override;

  Public
    Constructor Create(aOwner: TComponent); Override;

    Destructor Destroy; Override;

    Procedure FFAddDependent(ADependent: TFSSpecComp); Override; {!!.11}
    Procedure FFRemoveDependent(ADependent: TFSSpecComp); Override; {!!.11}

    Property CmdHandler[aInx: Longint]: TfsIntermediateCommandHandler
    Read iseGetCmdHandler;

    Property CmdHandlerCount: Longint Read iseGetCmdHandlerCount;
    Property ListEvents: TFSSpecStringList Read FListEvents Write FListEvents;
    Property ListEventsTransaction: TFSSpecStringList Read FListEventsTransaction Write FListEventsTransaction;
  End;

  TfsIntermediateCommandHandler = Class(TFSBaseCommandHandler)
  Protected

    FServerEngine: TfsIntermediateServerEngine;

    Procedure ichLog(Const aMsg: String); Virtual;
    {-Use this method to log a string to the event log. }

    Procedure ichLogFmt(Const aMsg: String; args: Array Of Const); Virtual;
    {-Use this method to log a formatted string to the event log. }

    Procedure ichLogAll(Const Msgs: Array Of String); Virtual;
    {-Use this method to log multiple strings to the event log. }

    Procedure ichLogBlock(Const S: String; Buf: pointer;
      BufLen: TffMemSize); Virtual;

    Procedure ichSetServerEngine(anEngine: TfsIntermediateServerEngine); Virtual;
    {-Sets the server engine to be used by this command handler. }

  Public

    Constructor Create(aOwner: TComponent); Override;

    Destructor Destroy; Override;

    Procedure FFNotificationEx(Const AOp: Byte; AFrom: TFSSpecComp; {!!.11}
      Const AData: TffWord32); Override; {!!.11}

  Published

    Property ServerEngine: TfsIntermediateServerEngine Read FServerEngine
      Write ichSetServerEngine;
    { The server engine handling requests received by this command handler. }

  End;

Implementation

Uses
  SysUtils;

{===TfsIntermediateServerEngine===========================================}

Constructor TfsIntermediateServerEngine.Create(aOwner: TComponent);
Begin
  Inherited Create(aOwner);
  FCmdHandlers := TFSSpecThreadList.Create;
  FListEvents := TFSSpecStringList.Create;
  FListEvents.CaseSensitive := False;
  FListEventsTransaction := TFSSpecStringList.Create;
  FListEventsTransaction.CaseSensitive := False;
End;
{--------}

Destructor TfsIntermediateServerEngine.Destroy;
{Rewritten !!.11}
Begin
  FFNotifyDependents(ffn_Destroy);
  FCmdHandlers.Free;
  FListEvents.free;
  FListEventsTransaction.Free;
  Inherited Destroy;
End;
{Begin !!.11}
{--------}

Procedure TfsIntermediateServerEngine.FFAddDependent(ADependent: TFSSpecComp);
Var
  aListItem: TfsIntListItem;
Begin
  Inherited;
  If ADependent Is TfsIntermediateCommandHandler Then
    Begin
      aListItem := TfsIntListItem.Create(Longint(ADependent));
      With FCmdHandlers.BeginWrite Do
        Try
          Insert(aListItem);
        Finally
          EndWrite;
        End;
    End;
End;
{--------}

Procedure TfsIntermediateServerEngine.FFRemoveDependent(ADependent: TFSSpecComp);
Begin
  Inherited;
  If ADependent Is TfsIntermediateCommandHandler Then
    With FCmdHandlers.BeginWrite Do
      Try
        Delete(Longint(ADependent));
      Finally
        EndWrite;
      End;
End;
{End !!.11}
{--------}

Function TfsIntermediateServerEngine.iseGetCmdHandler(aInx: Integer): TfsIntermediateCommandHandler;
Begin
  Result := TfsIntermediateCommandHandler(TfsIntListItem(FCmdHandlers[aInx]).KeyAsInt);
End;
{--------}

Function TfsIntermediateServerEngine.iseGetCmdHandlerCount: Longint;
Begin
  Result := FCmdHandlers.Count;
End;
{--------}

Procedure TfsIntermediateServerEngine.scSetState(Const aState: TfsState);
Var
  Idx: Longint;
  NextState: TfsState;
  Handler: TFSBaseCommandHandler;
Begin
  If aState = State Then Exit;

  If Assigned(FCmdHandlers) Then
    With FCmdHandlers.BeginRead Do
      Try
        While State <> aState Do
          Begin
            { Based upon our current state & the target state, get the next state. }
            NextState := fsStateDiagram[State, aState];

            { Move all command handlers to the specified state. }
            For Idx := Pred(Count) Downto 0 Do
              Begin
                Handler := TFSBaseCommandHandler(TfsIntListItem(Items[Idx]).KeyAsInt);
                Handler.State := NextState;
              End;

            {For each step, call the inherited SetState method. The inherited
             method is responsible for calling the state methods for the
             engine}
            Inherited scSetState(NextState);
          End;
      Finally
        EndRead;
      End;
End;

{=========================================================================}

{===TfsIntermediateCommandHandler=========================================}

Constructor TfsIntermediateCommandHandler.Create(aOwner: TComponent);
Begin
  Inherited Create(aOwner);
  FServerEngine := Nil;
End;
{--------}

Destructor TfsIntermediateCommandHandler.Destroy;
Begin
  If assigned(FServerEngine) Then
    FServerEngine.FFRemoveDependent(Self);

  Inherited Destroy;
End;
{--------}

Procedure TfsIntermediateCommandHandler.ichLog(Const aMsg: String);
Begin
  If FLogEnabled And assigned(FEventLog) Then
    FEventLog.WriteString(aMsg);
End;
{--------}

Procedure TfsIntermediateCommandHandler.ichLogAll(Const Msgs: Array Of String);
Begin
  If FLogEnabled And assigned(FEventLog) Then
    FEventLog.WriteStrings(Msgs);
End;
{--------}

Procedure TfsIntermediateCommandHandler.ichLogBlock(Const S: String; Buf: pointer;
  BufLen: TffMemSize);
Begin
  If FLogEnabled And assigned(FEventLog) Then
    FEventLog.WriteBlock(S, Buf, BufLen);
End;
{--------}

Procedure TfsIntermediateCommandHandler.ichLogFmt(Const aMsg: String; args: Array Of Const);
Begin
  If FLogEnabled And assigned(FEventLog) Then
    FEventLog.WriteString(format(aMsg, args));
End;
{--------}

Procedure TfsIntermediateCommandHandler.ichSetServerEngine(anEngine: TfsIntermediateServerEngine);
Begin
  If FServerEngine = anEngine Then Exit;

  scCheckInactive;

  If assigned(FServerEngine) Then
    FServerEngine.FFRemoveDependent(Self); {!!.11}

  If assigned(anEngine) Then
    anEngine.FFAddDependent(Self); {!!.11}

  FServerEngine := anEngine;

End;
{--------}
{Rewritten !!.11}

Procedure TfsIntermediateCommandHandler.FFNotificationEx
  (Const AOp: Byte; AFrom: TFSSpecComp;
  Const AData: TffWord32);
Begin
  Inherited;
  If (AFrom = FServerEngine) And
    (AOp In [ffn_Destroy, ffn_Remove]) Then
    Begin
      State := fsesStopped;
      FServerEngine.FFRemoveDependent(Self);
      FServerEngine := Nil;
    End;
End;
{=========================================================================}
End.

