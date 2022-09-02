{*********************************************************}
{* FlashFiler: Server Rebuild status                     *}
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

Unit fssrstat;

Interface

Uses
  Windows,
  SysUtils,
  Classes,
  fsconst,
  fsllbase,
  fssrbase;

Type
  TfsSrcRebuildStatus = Class(TfsSelfListItem)
  Protected {private}
    rsPadlock: TfsPadlock;
    rsStatus: TffRebuildStatus;
    rsClientID: Longint;
  Protected
  Public
    Constructor Create(aClientID: Longint; aTotalRecords: Longint);
    Destructor Destroy; Override;

    Procedure GetLastSnapshot(Var aRebuildStatus: TffRebuildStatus);
    Procedure MakeSnapshot(aRecsRead: Longint; aRecsWritten: Longint; aErrorCode: TffResult);
    Procedure MarkFinished;
    Property ClientID: Longint Read rsClientID;
    Property RebuildID: Longint Read KeyAsInt;
  End;

  TfsSrcRebuildStatusList = Class(TFSSpecObject)
  Protected {private}
    FList: TFSNormalList;
    rslPadlock: TfsPadlock;
  Public
    Constructor Create;
    Destructor Destroy; Override;

    Function AddRebuildStatus(aClientID: Longint; aTotalRecs: Longint): TfsSrcRebuildStatus;
    Procedure DeleteAllForClient(aClientID: Longint);
    Function GetRebuildStatus(aRebuildID: Longint;
      Var aStatus: TffRebuildStatus): boolean;
    Procedure MarkRebuildStatusFinished(aRebuildID: Longint);
  End;

Implementation

{===TfsSrcRebuildStatus===============================================}

Constructor TfsSrcRebuildStatus.Create(aClientID: Longint; aTotalRecords: Longint);
Begin
  Inherited Create;
  rsPadlock := TfsPadlock.Create;
  rsStatus.rsTotalRecs := aTotalRecords;
  rsStatus.rsStartTime := GetTickCount;
  rsClientID := aClientID;
End;
{--------}

Destructor TfsSrcRebuildStatus.Destroy;
Begin
  rsPadlock.Free;
  Inherited Destroy;
End;
{--------}

Procedure TfsSrcRebuildStatus.GetLastSnapshot(Var aRebuildStatus: TffRebuildStatus);
Begin
  rsPadlock.Lock;
  Try
    aRebuildStatus := rsStatus;
  Finally
    rsPadlock.Unlock;
  End; {try..finally}
End;
{--------}

Procedure TfsSrcRebuildStatus.MakeSnapshot(aRecsRead: Longint;
  aRecsWritten: Longint;
  aErrorCode: TffResult);
Var
  Dividend: Longint;
  Divisor: Longint;
Begin
  rsPadlock.Lock;
  Try
    With rsStatus Do
      Begin
        rsRecsRead := aRecsRead;
        rsRecsWritten := aRecsWritten;
        rsErrorCode := aErrorCode;
        rsSnapshotTime := GetTickCount;
        If (rsRecsRead >= $1000000) Then
          Begin
            Dividend := (rsRecsRead Shr 7) * 100;
            Divisor := rsTotalRecs Shr 7;
          End
        Else
          Begin
            Dividend := rsRecsRead * 100;
            Divisor := rsTotalRecs;
          End;
        If Divisor <> 0 Then
          rsPercentDone := Dividend Div Divisor;
      End;
  Finally
    rsPadlock.Unlock;
  End; {try..finally}
End;
{--------}

Procedure TfsSrcRebuildStatus.MarkFinished;
Begin
  rsStatus.rsFinished := True;
End;
{====================================================================}

{===TfsSrcRebuildStatusList===========================================}

Constructor TfsSrcRebuildStatusList.Create;
Begin
  Inherited Create;
  FList := TFSNormalList.Create;
  rslPadlock := TfsPadlock.Create;
End;
{--------}

Destructor TfsSrcRebuildStatusList.Destroy;
Begin
  rslPadlock.Free;
  FList.Free;
  Inherited Destroy;
End;
{--------}

Function TfsSrcRebuildStatusList.AddRebuildStatus(aClientID: Longint;
  aTotalRecs: Longint): TfsSrcRebuildStatus;
Begin
  rslPadlock.Lock;
  Try
    Result := TfsSrcRebuildStatus.Create(aClientID, aTotalRecs);
    Try
      FList.Insert(Result);
    Except
      Result.Free;
      Raise;
    End; {try..except}
  Finally
    rslPadlock.Unlock;
  End; {try..finally}
End;
{--------}

Procedure TfsSrcRebuildStatusList.DeleteAllForClient(aClientID: Longint);
Var
  Inx: Integer;
  TempStat: TfsSrcRebuildStatus;
Begin
  rslPadlock.Lock;
  Try
    For Inx := pred(FList.Count) Downto 0 Do
      Begin
        TempStat := TfsSrcRebuildStatus(FList[Inx]);
        If (TempStat.ClientID = aClientID) Then
          FList.DeleteAt(Inx);
      End;
  Finally
    rslPadlock.Unlock;
  End; {try..finally}
End;
{--------}

Function TfsSrcRebuildStatusList.GetRebuildStatus(aRebuildID: Longint;
  Var aStatus: TffRebuildStatus): boolean;
Var
  Inx: Integer;
  TempStat: TfsSrcRebuildStatus;
Begin
  rslPadlock.Lock;
  Try
    Inx := FLIst.Index(aRebuildID);
    If (Inx = -1) Then
      Result := False
    Else
      Begin
        Result := True;
        TempStat := TfsSrcRebuildStatus(FList[Inx]);
        TempStat.GetLastSnapshot(aStatus);
        If (aStatus.rsFinished = True) Then
          FList.DeleteAt(Inx);
      End;
  Finally
    rslPadlock.Unlock;
  End; {try..finally}
End;
{--------}

Procedure TfsSrcRebuildStatusList.MarkRebuildStatusFinished(aRebuildID: Longint);
Var
  Inx: Integer;
  TempStat: TfsSrcRebuildStatus;
Begin
  rslPadlock.Lock;
  Try
    Inx := FLIst.Index(aRebuildID);
    If (Inx <> -1) Then
      Begin
        TempStat := TfsSrcRebuildStatus(FList[Inx]);
        TempStat.MarkFinished;
      End;
  Finally
    rslPadlock.Unlock;
  End; {try..finally}
End;
{====================================================================}

End.

