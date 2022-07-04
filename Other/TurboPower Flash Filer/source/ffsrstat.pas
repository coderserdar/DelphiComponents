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

{$I ffdefine.inc}

unit ffsrstat;

interface

uses
  Windows,
  SysUtils,
  Classes,
  ffconst,
  ffllbase,
  ffsrbase;

type
  TffSrRebuildStatus = class(TffSelfListItem)
    protected {private}
      rsPadlock : TffPadlock;
      rsStatus  : TffRebuildStatus;
      rsClientID: longint;
    protected
    public
      constructor Create(aClientID : longint; aTotalRecords : longint);
      destructor Destroy; override;

      procedure GetLastSnapshot(var aRebuildStatus : TffRebuildStatus);
      procedure MakeSnapshot(aRecsRead : longint; aRecsWritten : longint; aErrorCode : TffResult);
      procedure MarkFinished;
      property ClientID : longint read rsClientID;
      property RebuildID : longint read KeyAsInt;
  end;

  TffSrRebuildStatusList = class(TffObject)                           
    protected {private}
      FList : TffList;
      rslPadlock     : TffPadlock;
    public
      constructor Create;
      destructor Destroy; override;

      function AddRebuildStatus(aClientID : longint; aTotalRecs : longint) : TffSrRebuildStatus;
      procedure DeleteAllForClient(aClientID : longint);
      function GetRebuildStatus(aRebuildID : longint;
                            var aStatus    : TffRebuildStatus) : boolean;
      procedure MarkRebuildStatusFinished(aRebuildID : longint);
  end;

implementation

{===TffSrRebuildStatus===============================================}
constructor TffSrRebuildStatus.Create(aClientID : longint; aTotalRecords : longint);
begin
  inherited Create;
  rsPadlock := TffPadlock.Create;
  rsStatus.rsTotalRecs := aTotalRecords;
  rsStatus.rsStartTime := GetTickCount;
  rsClientID := aClientID;
end;
{--------}
destructor TffSrRebuildStatus.Destroy;
begin
  rsPadlock.Free;
  inherited Destroy;
end;
{--------}
procedure TffSrRebuildStatus.GetLastSnapshot(var aRebuildStatus : TffRebuildStatus);
begin
  rsPadlock.Lock;
  try
    aRebuildStatus := rsStatus;
  finally
    rsPadlock.Unlock;
  end;{try..finally}
end;
{--------}
procedure TffSrRebuildStatus.MakeSnapshot(aRecsRead    : longint;
                                          aRecsWritten : longint;
                                          aErrorCode   : TffResult);
var
  Dividend: LongInt;
  Divisor: LongInt;
begin
  rsPadlock.Lock;
  try
    with rsStatus do begin
      rsRecsRead := aRecsRead;
      rsRecsWritten := aRecsWritten;
      rsErrorCode := aErrorCode;
      rsSnapshotTime := GetTickCount;
      if (rsRecsRead >= $1000000) then begin
        Dividend := (rsRecsRead shr 7) * 100;
        Divisor := rsTotalRecs shr 7;
      end
      else begin
        Dividend := rsRecsRead * 100;
        Divisor := rsTotalRecs;
      end;
      if Divisor <> 0 then
        rsPercentDone := Dividend div Divisor;
    end;
  finally
    rsPadlock.Unlock;
  end;{try..finally}
end;
{--------}
procedure TffSrRebuildStatus.MarkFinished;
begin
  rsStatus.rsFinished := true;
end;
{====================================================================}


{===TffSrRebuildStatusList===========================================}
constructor TffSrRebuildStatusList.Create;
begin
  inherited Create;
  FList := TffList.Create;
  rslPadlock := TffPadlock.Create;
end;
{--------}
destructor TffSrRebuildStatusList.Destroy;
begin
  rslPadlock.Free;
  FList.Free;
  inherited Destroy;
end;
{--------}
function TffSrRebuildStatusList.AddRebuildStatus(aClientID  : longint;
                                                 aTotalRecs : longint) : TffSrRebuildStatus;
begin
  rslPadlock.Lock;
  try
    Result := TffSrRebuildStatus.Create(aClientID, aTotalRecs);
    try
      FList.Insert(Result);
    except
      Result.Free;
      raise;
    end;{try..except}
  finally
    rslPadlock.Unlock;
  end;{try..finally}
end;
{--------}
procedure TffSrRebuildStatusList.DeleteAllForClient(aClientID : longint);
var
  Inx      : integer;
  TempStat : TffSrRebuildStatus;
begin
  rslPadlock.Lock;
  try
    for Inx := pred(FList.Count) downto 0 do begin
      TempStat := TffSrRebuildStatus(FList[Inx]);
      if (TempStat.ClientID = aClientID) then
        FList.DeleteAt(Inx);
    end;
  finally
    rslPadlock.Unlock;
  end;{try..finally}
end;
{--------}
function TffSrRebuildStatusList.GetRebuildStatus(aRebuildID : longint;
                                             var aStatus    : TffRebuildStatus) : boolean;
var
  Inx : integer;
  TempStat : TffSrRebuildStatus;
begin
  rslPadlock.Lock;
  try
    Inx := FLIst.Index(aRebuildID);
    if (Inx = -1) then
      Result := false
    else begin
      Result := true;
      TempStat := TffSrRebuildStatus(FList[Inx]);
      TempStat.GetLastSnapshot(aStatus);
      if (aStatus.rsFinished = true) then
        FList.DeleteAt(Inx);
    end;
  finally
    rslPadlock.Unlock;
  end;{try..finally}
end;
{--------}
procedure TffSrRebuildStatusList.MarkRebuildStatusFinished(aRebuildID : longint);
var
  Inx : integer;
  TempStat : TffSrRebuildStatus;
begin
  rslPadlock.Lock;
  try
    Inx := FLIst.Index(aRebuildID);
    if (Inx <> -1) then begin
      TempStat := TffSrRebuildStatus(FList[Inx]);
      TempStat.MarkFinished;
    end;
  finally
    rslPadlock.Unlock;
  end;{try..finally}
end;
{====================================================================}

end.
