{*********************************************************}
{* FlashFiler: Order Processing main form                *}
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

unit OpMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ffllbase;

type
  opState = (opsWaiting, opsProcessing, opsShuttingDown, opsPaused);

type
  TfrmMain = class(TForm)
    pnlTop: TPanel;
    pnlCenter: TPanel;
    pnlBottom: TPanel;
    memLog: TMemo;
    pbClose: TButton;
    pbProcess: TButton;
    lblClients: TLabel;
    timStatus: TTimer;
    pbPause: TButton;
    procedure FormCreate(Sender: TObject);
    procedure timStatusTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pbCloseClick(Sender: TObject);
    procedure pbProcessClick(Sender: TObject);
    procedure pbPauseClick(Sender: TObject);
  private
    { Private declarations }
    FForceRun : Boolean;
    FNextRun : TDateTime;
    FState : opState;
    FTransports : TffList;

    procedure CalcNextRun;
    function  ElapsedTimeToStr(T : TDateTime) : string;
    procedure OnMsg(Sender : TObject; Msg : string);
    procedure ProcessOrders;
    procedure ShowClients;
    procedure ShowState;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses OpEngMgr, ffllcomm, OpDM;

{$R *.DFM}

const
  csClients = 'Order Entry clients: %d';
  csStatus = 'Status: %s';
  clProcessFreq = 30000;
    { Number of milliseconds between each order processing session. }

procedure TfrmMain.CalcNextRun;
begin
  FNextRun := Now + (clProcessFreq / (86400 * 1000));
end;
{--------}
procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FForceRun := False;
  FTransports := TffList.Create;
end;
{--------}
function TfrmMain.ElapsedTimeToStr(T : TDateTime) : string;
var
  Hr : integer;
  Mi : integer;
  Se : integer;
  WorkSt : string[9];
begin
  T := frac(T) * 24.0;
  Hr := trunc(T);
  T := frac(T) * 60.0;
  Mi := trunc(T);
  Se := trunc(frac(T) * 60.0);
            {12345678}
  Result := '00:00:00';
  Result[3] := TimeSeparator;
  Result[6] := TimeSeparator;
  Str(Hr:2, WorkSt);
  Result[2] := WorkSt[2];
  if (Hr > 9) then
    Result[1] := WorkSt[1];
  Str(Mi:2, WorkSt);
  Result[5] := WorkSt[2];
  if (Mi > 9) then
    Result[4] := WorkSt[1];
  Str(Se:2, WorkSt);
  Result[8] := WorkSt[2];
  if (Se > 9) then
    Result[7] := WorkSt[1];
end;
{--------}
procedure TfrmMain.timStatusTimer(Sender: TObject);
var
  aProcess : Boolean;
begin
  { Is it time to process? }
  aProcess := FForceRun or ((FState = opsWaiting) and (Now > FNextRun));
  if aProcess then begin
    CalcNextRun;
    FState := opsProcessing;
  end;

  { Display current status. }
  ShowState;

  { Display number of clients connected via engine manager. }
  ShowClients;

  { Process orders. }
  if aProcess then begin
    pbPause.Enabled := False;
    pbProcess.Enabled := False;
    ProcessOrders;
    pbPause.Enabled := True;
    pbProcess.Enabled := True;
    FForceRun := False;
  end;

end;
{--------}
procedure TfrmMain.ShowClients;
var
  i, aCount, Clients : Integer;
begin
  Clients := 0;
  for i := 0 to pred(FTransports.Count) do begin
    aCount := TffBaseTransport(TffIntListItem(FTransports[i]).KeyAsInt).ConnectionCount;
    inc(Clients, aCount);
  end;
  lblClients.Caption := format(csClients, [Clients]);
end;
{--------}
procedure TfrmMain.ShowState;
var
  aStatus : string;
begin
  case FState of
    opsWaiting :
      aStatus := format('Waiting, next run in %s',
                        [ElapsedTimeToStr(FNextRun - Now)]);
    opsProcessing :
      aStatus := 'Processing orders';
    opsShuttingDown :
      aStatus := 'Shutting down';
    opsPaused :
      aStatus := 'Paused';
  end;  { case }
  pnlTop.Caption := format(csStatus, [aStatus]);
end;
{--------}
procedure TfrmMain.FormShow(Sender: TObject);
begin
  { Get the list of transports. }
  OpEngineMgr.GetTransports(OpEngineMgr.ServerEngine, FTransports);

  CalcNextRun;
  FState := opsWaiting;

  { Start the engine manager. }
  OpEngineMgr.Startup;

  { Connect our internal client components to the server engine. }
  OpData.OnMsg := OnMsg;
  OpData.Connect(OpEngineMgr.ServerEngine);

  ShowState;
  ShowClients;
  timStatus.Enabled := True;
end;
{--------}
procedure TfrmMain.ProcessOrders;
begin
  OpData.ProcessOrders;
  
  { After all orders have been processed, switch state back to waiting. }
  if FState <> opsPaused then
    FState := opsWaiting;
  ShowState;
end;
{--------}
procedure TfrmMain.pbCloseClick(Sender: TObject);
begin
  { Change state to shutting down. }
  FState := opsShuttingDown;
  ShowState;

  { Disconnect the client components. }
  OpData.Disconnect;

  { Tell engine manager to shutdown }
  OpEngineMgr.Shutdown;

  { Disable the timer. }
  timStatus.Enabled := False;

  Close;

end;
{--------}
procedure TfrmMain.pbProcessClick(Sender: TObject);
begin
  FForceRun := True;
  FNextRun := Now;
end;
{--------}
procedure TfrmMain.OnMsg(Sender : TObject; Msg : string);
var
  anInx : Integer;
begin
  memLog.Lines.Add(Msg);
  if memLog.Lines.Count > 5000 then
    for anInx := 1 to 100 do
      memLog.Lines.Delete(0);
end;

procedure TfrmMain.pbPauseClick(Sender: TObject);
begin
  if FState = opsPaused then begin
    pbPause.Caption := 'Pa&use';
    FState := opsWaiting;
    CalcNextRun;
  end
  else begin
    pbPause.Caption := '&Resume';
    FState := opsPaused;
  end;
end;

end.
