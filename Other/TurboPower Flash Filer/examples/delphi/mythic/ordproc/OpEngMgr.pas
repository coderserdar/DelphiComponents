{*********************************************************}
{* FlashFiler: Order Processing engine manager           *}
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
 
unit OpEngMgr;

interface

uses
  windows, messages, sysutils, classes, controls, forms, fflleng, ffsreng, 
  ffllcomm, fflllgcy, fflllog, ffllthrd, ffnetmsg, ffsrintm, ffsrcmd, ffllbase,
  ffsrsec, ffsqlbas, ffsqleng, ffllcomp, ffsrjour;

type
  TOpEngineMgr = class(TFFBaseEngineManager)
    ServerEngine : TFFServerEngine;
    EventLog : TffEventLog;
    CommandHandler : TFFServerCommandHandler;
    SecurityMonitor : TFFSecurityMonitor;
    ThreadPool : TFFThreadPool;
    SUPTransport : TFFLegacyTransport;
    TCPIPTransport : TFFLegacyTransport;
    SQLEngine: TffSqlEngine;
    procedure RecoveryCheck(Sender : TObject);
  private
    { private declarations }
  protected
    FScriptFile : TffFullFileName;
    function GetLogEnabled : boolean;
    procedure SetLogEnabled(const aEnabled : boolean);
    procedure SetScriptFile(const aFileName : TffFullFileName);
  public
    constructor Create(Sender: TComponent); override;
    procedure GetServerEngines(var aServerList : TffList);
    procedure GetTransports(aServer : TffIntermediateServerEngine; var aTransList : TffList);
    procedure Process(Msg : PffDataMessage; var Handled : Boolean); override;
    procedure Restart; override;
    procedure ShutDown; override;
    procedure StartUp; override;
    procedure Stop; override;

    { Properties }
    property EventLogEnabled : boolean
             read  GetLogEnabled
             write SetLogEnabled;

    property ScriptFile : TffFullFileName
             read FScriptFile
             write SetScriptFile;

  end;

var
  OpEngineMgr: TOpEngineMgr;

implementation

{$R *.DFM}

{====================================================================}
constructor TOpEngineMgr.Create(Sender: TComponent);
begin
  inherited Create(Sender);
  EventLog.FileName := ExtractFilePath(Application.ExeName) + 'FFServer.log';
end;
{--------}
function TOpEngineMgr.GetLogEnabled : boolean;
var
  Idx : Integer;
begin
  Result := False;
  { Assumption: Event log is enabled if we find a server engine
    that is routing events to the log. }
  for Idx := 0 to Pred(ComponentCount) do
    if (Components[Idx] is TffBaseServerEngine) then begin
      Result := TffBaseServerEngine(Components[Idx]).EventLogEnabled;
      break;
    end;
end;
{--------}
procedure TOpEngineMgr.GetServerEngines(var aServerList: TffList);
var
  ServerListItem : TffIntListItem;
  i              : Integer;
begin
  for I := 0 to Pred(ComponentCount) do
    if (Components[i] is TffBaseServerEngine) then begin
      ServerListItem := TffIntListItem.Create(longint(Components[i]));
      aServerList.Insert(ServerListItem);
    end;
end;
{--------}
procedure TOpEngineMgr.GetTransports(aServer    : TffIntermediateServerEngine;
                                     var aTransList : TffList);
var
  TransportItem : TffIntListItem;
  i, k          : Integer;
begin
  for i := 0 to Pred(aServer.CmdHandlerCount) do begin
    for k := 0 to Pred(aServer.CmdHandler[i].TransportCount) do begin
      TransportItem := TffIntListItem.Create(Integer(aServer.CmdHandler[i].Transports[k]));
      aTransList.Insert(TransportItem);
    end;
  end;
end;
{--------}
procedure TOpEngineMgr.Process(Msg : PffDataMessage; var Handled : Boolean);
begin
  Handled := True;
  case Msg.dmMsg of
    ffnmServerRestart  : Restart;
    ffnmServerShutdown : ShutDown;
    ffnmServerStartUp  : StartUp;
    ffnmServerStop     : Stop;
  else
    Handled := False;
  end;
end;
{--------}
procedure TOpEngineMgr.RecoveryCheck(Sender : TObject);
begin
  with FFRecoveryClass.Create do
    try
      Check(TffServerEngine(Sender));
    finally
      Free;
    end;
end;
{--------}
procedure TOpEngineMgr.Restart;
begin
  Shutdown;
  Startup;
end;
{--------}
procedure TOpEngineMgr.SetLogEnabled(const aEnabled : boolean);
var
  Idx : Integer;
begin
  { Assumption: TffBaseLog is always enabled.  We just control which
    components are issuing messages to the log. }
  for Idx := 0 to Pred(ComponentCount) do
    if (Components[Idx] is TffLoggableComponent) then
      TffLoggableComponent(Components[Idx]).EventLogEnabled := aEnabled
end;
{--------}
procedure TOpEngineMgr.SetScriptFile(const aFileName : TffFullFileName);
var
  Idx : Integer;
begin
  FScriptFile := aFileName;
  for Idx := 0 to Pred(ComponentCount) do
    if (Components[Idx] is TffServerEngine) then
      TffServerEngine(Components[Idx]).ScriptFile := aFileName;
end;
{--------}
procedure TOpEngineMgr.ShutDown;
var
  Idx : Integer;
begin
  for Idx := 0 to Pred(ComponentCount) do
    if ((Components[Idx] is TFFBaseServerEngine) or
        (Components[Idx] is TFFBasePluginEngine)) and
        not (TffStateComponent(Components[Idx]).State in
          [ffesInactive, ffesStopped]) then
      TffStateComponent(Components[Idx]).Shutdown;
end;
{--------}
procedure TOpEngineMgr.StartUp;
var
  Idx : Integer;
begin
  for Idx := 0 to Pred(ComponentCount) do
    if (Components[Idx] is TFFBaseServerEngine) or
       (Components[Idx] is TFFBasePluginEngine) then
      TffStateComponent(Components[Idx]).Startup;
end;
{--------}
procedure TOpEngineMgr.Stop;
var
  Idx : Integer;
begin
  for Idx := 0 to Pred(ComponentCount) do
    if (Components[Idx] is TFFBaseServerEngine) or
       (Components[Idx] is TFFBasePluginEngine) then
      TffStateComponent(Components[Idx]).Stop;
end;
{====================================================================}

end.
