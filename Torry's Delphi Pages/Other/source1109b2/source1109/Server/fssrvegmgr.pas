{$I fsdefine.inc}

Unit fssrvegmgr;

Interface

Uses
  windows,
  Messages,
  SysUtils,
  Classes,
  Controls,
  Forms,
  fslleng,
  fsserverclass,
  fsllcomm,
  fslllgcy,
  fslllog,
  fsllthrd,
  fsnetmsg,
  fssrintm,
  fssrcmd,
  fsllbase,
  fssrsec,
  fsllcomp,
  fssrjour, fsadminplug;

Type
  TfsEngineManager = Class(TFSBaseEngineManager)
    EventLog: TFSEventLog;
    CommandHandler: TFSHandler;
    SecurityMonitor: TFSMonitor;
    ThreadPool: TFSThreadPool;
    SUPTransport: TFSParamConnect;
    IPXSPXTransport: TFSParamConnect;
    TCPIPTransport: TFSParamConnect;
    AdminServer: TFSAdminServer;
    AdminHandler: TFSAdminHandler;
    ServerEngine: TFSServer;
  Private
    { private declarations }
  Protected
    FScriptFile: TffFullFileName;
    Function GetLogEnabled: boolean;
    Procedure SetLogEnabled(Const aEnabled: boolean);
    Procedure SetScriptFile(Const aFileName: TffFullFileName);
  Public
    Constructor Create(Sender: TComponent); Override;
    Procedure GetServerEngines(Var aServerList: TFSNormalList);
    Procedure GetTransports(aServer: TfsIntermediateServerEngine; Var aTransList: TFSNormalList);
    Procedure Process(Msg: PFSDataMessage; Var Handled: Boolean); Override;
    Procedure Restart; Override;
    Procedure Shutdown; Override;
    Procedure Startup; Override;
    Procedure Stop; Override;

    { Properties }
    Property EventLogEnabled: boolean
      Read GetLogEnabled
      Write SetLogEnabled;

    Property ScriptFile: TffFullFileName
      Read FScriptFile
      Write SetScriptFile;

  End;

Var
  fsEngineManager: TfsEngineManager;

Implementation

{$R *.DFM}

{====================================================================}

Constructor TfsEngineManager.Create(Sender: TComponent);
Begin
  Inherited Create(Sender);
  If SysUtils.FileExists(ExtractFilePath(Application.ExeName) + 'server.lg') Then
    Begin
      EventLog.FileName := ExtractFilePath(Application.ExeName) + 'server.lg';
      EventLog.Enabled := True;
    End
  Else
    EventLog.Enabled := False;
End;
{--------}

Function TfsEngineManager.GetLogEnabled: boolean;
Var
  Idx: Integer;
Begin
  Result := False;
  { Assumption: Event log is enabled if we find a server engine
    that is routing events to the log. }
  For Idx := 0 To Pred(ComponentCount) Do
    If (Components[Idx] Is TFSBaseServerEngine) Then
      Begin
        Result := TFSBaseServerEngine(Components[Idx]).EventLogEnabled;
        break;
      End;
End;
{--------}

Procedure TfsEngineManager.GetServerEngines(Var aServerList: TFSNormalList);
Var
  ServerListItem: TfsIntListItem;
  i: Integer;
Begin
  For I := 0 To Pred(ComponentCount) Do
    If (Components[i] Is TFSBaseServerEngine) Then
      Begin
        ServerListItem := TfsIntListItem.Create(Longint(Components[i]));
        aServerList.Insert(ServerListItem);
      End;
End;
{--------}

Procedure TfsEngineManager.GetTransports(aServer: TfsIntermediateServerEngine;
  Var aTransList: TFSNormalList);
Var
  TransportItem: TfsIntListItem;
  i, k: Integer;
Begin
  For i := 0 To Pred(aServer.CmdHandlerCount) Do
    Begin
      For k := 0 To Pred(aServer.CmdHandler[i].TransportCount) Do
        Begin
          TransportItem := TfsIntListItem.Create(Integer(aServer.CmdHandler[i].Transports[k]));
          aTransList.Insert(TransportItem);
        End;
    End;
End;
{--------}

Procedure TfsEngineManager.Process(Msg: PFSDataMessage; Var Handled: Boolean);
Begin
  Handled := True;
  Case Msg.dmMsg Of
    fsnmServerRestart: Restart;
    fsnmServerShutdown: Shutdown;
    fsnmServerStartUp: Startup;
    fsnmServerStop: Stop;
    Else
      Handled := False;
  End;
End;
{--------}

Procedure TfsEngineManager.Restart;
Begin
  Shutdown;
  Startup;
End;
{--------}

Procedure TfsEngineManager.SetLogEnabled(Const aEnabled: boolean);
Var
  Idx: Integer;
Begin
  { Assumption: TffBaseLog is always enabled.  We just control which
    components are issuing messages to the log. }
  For Idx := 0 To Pred(ComponentCount) Do
    Begin
      If (Components[Idx] Is TfsLoggableComponent) And
        Not (Components[Idx] Is TFSBaseTransport) Then
        TfsLoggableComponent(Components[Idx]).EventLogEnabled := aEnabled;
    End;
End;
{--------}

Procedure TfsEngineManager.SetScriptFile(Const aFileName: TffFullFileName);
Var
  Idx: Integer;
Begin
  FScriptFile := aFileName;
  For Idx := 0 To Pred(ComponentCount) Do
    If (Components[Idx] Is TFSServer) Then
      TFSServer(Components[Idx]).ScriptFile := aFileName;
End;
{--------}

Procedure TfsEngineManager.Shutdown;
Var
  Idx: Integer;
Begin
  For Idx := 0 To Pred(ComponentCount) Do
    If ((Components[Idx] Is TFSBaseServerEngine) Or
      (Components[Idx] Is TFSBasePluginEngine)) And
      Not (TfsStateComponent(Components[Idx]).State In
      [fsesInactive, fsesStopped]) Then
      TfsStateComponent(Components[Idx]).Shutdown;
End;
{--------}

Procedure TfsEngineManager.Startup;
Var
  Idx: Integer;
Begin
  For Idx := 0 To Pred(ComponentCount) Do
    If (Components[Idx] Is TFSBaseServerEngine) Or
      (Components[Idx] Is TFSBasePluginEngine) Then
      TfsStateComponent(Components[Idx]).Startup;
End;
{--------}

Procedure TfsEngineManager.Stop;
Var
  Idx: Integer;
Begin
  For Idx := 0 To Pred(ComponentCount) Do
    If (Components[Idx] Is TFSBaseServerEngine) Or
      (Components[Idx] Is TFSBasePluginEngine) Then
      TfsStateComponent(Components[Idx]).Stop;
End;
{====================================================================}

End.
