(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at babikov@mail.ru

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit NMMCustomServer;

interface
uses Classes,
     ExtCtrls, IdBaseComponent, IdComponent, IdTCPServer, IdThreadMgrDefault,
     NMMCommon,
     NMMCustomAuthChecker, SyncObjs, SysUtils, NMMUserData;

type
 TNMMConnectionGroupThreads= class(TThreadList)
 end;

 TNMMServerThread = class(TIdPeerThread)
 public
   DontCallOnDisconnect: Boolean;
 end;

 TThreadInfo = class(TObject)
 protected
   function GetConnectionGroupThreads: Integer;
   function GetWriteStreamThreads: Integer;
 public
   property ConnectionGroupThreads: Integer read GetConnectionGroupThreads;
   property WriteStreamThreads: Integer read GetWriteStreamThreads;
 end;

 TCustomCommandEvent= procedure(Sender: TObject; Command: string) of object;

 TNMMCustomServer= class(TComponent)
 protected
   FActive: Boolean;
   FTCPServer: TIdTCPServer;
   FIdThreadMgrDefault: TIdThreadMgrDefault;
   FCS_Login: TCriticalSection;

   FPort: Integer;
   FAuthChecker: TNMMCustomAuthChecker;
   FThreadInfo: TThreadInfo;
   
   FOnCustomCommand: TCustomCommandEvent;
   FOnAuthentication: TAuthenticationEvent;

   function GetActive: Boolean;
   procedure SetActive(AActive: Boolean); virtual;
   procedure SetPort(AValue: Integer);

   procedure OnTCPServerConnect(AThread: TIdPeerThread); virtual; abstract;
   procedure OnTCPServerDisconnect(AThread: TIdPeerThread); virtual;
   procedure OnTCPServerExecute(AThread: TIdPeerThread); virtual;
   procedure OnTCPServerException(AThread: TIdPeerThread; AException: Exception); virtual;

   procedure HandleCustomCommand(ACommand: String); virtual;
   function CreateAuthChecker: TNMMCustomAuthChecker; virtual; abstract;
 public
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;
   procedure RefusePeer(AThread: TIdPeerThread; ARefuseReason: String);
   procedure DisconnectPeer(AThread: TIdPeerThread); virtual;

   property Active: Boolean read GetActive write SetActive;
   property TCPServer: TIdTCPServer read FTCPServer;
   property ThreadInfo: TThreadInfo read FThreadInfo;
 published
   property Port: Integer read FPort write SetPort default DefaultServerPort;
   property OnCustomCommand: TCustomCommandEvent
            read FOnCustomCommand write FOnCustomCommand;
   property OnAuthentication: TAuthenticationEvent
            read FOnAuthentication write FOnAuthentication;
 end;


implementation

uses NMMConnectionHandle, NMMCustomConnectionProcessor, IdException,
     IdGlobal, NMMServerGlobals;


{ThreadInfo}
function TThreadInfo.GetConnectionGroupThreads: Integer;
begin
 result:= GcgtInstances.Value;
end;

function TThreadInfo.GetWriteStreamThreads: Integer;
begin
 result:= GchwstInstances.Value;
end;


{TNMMCustomServer}
constructor TNMMCustomServer.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 FPort:= DefaultServerPort;
 FIdThreadMgrDefault:= TIdThreadMgrDefault.Create(self);
 FIdThreadMgrDefault.Name:= 'ThreadMgrDefault';
 FTCPServer:= TIdTCPServer.Create(self);
 FTCPServer.Name:= 'TCPServer';
 FTCPServer.ThreadMgr:= FIdThreadMgrDefault;
 FTCPServer.OnConnect:= OnTCPServerConnect;
 FTCPServer.OnDisconnect:= OnTCPServerDisconnect;
 FTCPServer.OnExecute:= OnTCPServerExecute;
 FTCPServer.ThreadClass := TNMMServerThread;
{$IFDEF VER150}
 FTCPServer.OnException:= OnTCPServerException;
{$ENDIF}
 if (not (csDesigning in ComponentState)) and
    (not (csLoading in ComponentState)) then
 begin
   FCS_Login:= TCriticalSection.Create;
   FThreadInfo:= TThreadInfo.Create;
   FAuthChecker:= CreateAuthChecker;
   FAuthChecker.OnAuthentication:= FOnAuthentication;
 end;
end;


destructor TNMMCustomServer.Destroy;
begin
  if (not (csDesigning in ComponentState)) and
     (not (csLoading in ComponentState)) then
  begin
    Active:= false;
    FreeAndNil(FThreadInfo);
    FreeAndNil(FCS_Login);
    FreeAndNil(FAuthChecker);
  end;
  
  FreeAndNil(FTCPServer);
  FreeAndNil(FIdThreadMgrDefault);
  inherited Destroy;
end;

function TNMMCustomServer.GetActive: Boolean;
begin
 result:= FActive;
end;

procedure TNMMCustomServer.SetActive(AActive: Boolean);
begin
 FTCPServer.DefaultPort:= FPort;
 FTCPServer.Active:= AActive;
 FActive:= AActive;
end;

procedure TNMMCustomServer.SetPort( AValue: Integer );
begin
 if FPort<>AValue then
 begin
   FPort:= AValue;
   FTCPServer.DefaultPort:= AValue;
 end;
end;

function MakeHostAndPortString( Host: String; Port: Integer): String;
begin
 if Port<>0 then
 begin
   result:= Host +':'+ IntToStr(Port)
 end
 else
 begin
   result:= Host;
 end;
end;

procedure TNMMCustomServer.OnTCPServerDisconnect(AThread: TIdPeerThread);
begin
  DisconnectPeer(AThread);
end;

procedure TNMMCustomServer.OnTCPServerException(AThread: TIdPeerThread;
                                                  AException: Exception);
begin
 if (AException is EIdConnClosedGracefully) then
 begin
   DisconnectPeer(AThread);
 end else
 begin
   AddErrorToLog(AException.Message);
 end;
end;

procedure TNMMCustomServer.HandleCustomCommand(ACommand: String);
begin
  if Assigned(FOnCustomCommand) then
     FOnCustomCommand(self,ACommand);
end;

procedure TNMMCustomServer.RefusePeer(AThread: TIdPeerThread; ARefuseReason: String);
begin
 if AThread.Connection.Connected then
 begin
   AThread.Connection.WriteLn(ARefuseReason); 
   AThread.Connection.Disconnect; 
 end;
end;

procedure TNMMCustomServer.DisconnectPeer(AThread: TIdPeerThread);
begin
  try
    AThread.Connection.Disconnect;
    AThread.Terminate;
    if not AThread.FreeOnTerminate then
    begin
      FreeAndNil(AThread);
    end;
  except
  end;
end;

procedure TNMMCustomServer.OnTCPServerExecute(AThread: TIdPeerThread);
var
  s: String;
begin
  S:= AThread.Connection.ReadLn;

  if Pos(cmdDisconnect,S)>0 then
     DisconnectPeer(AThread)
  else
     HandleCustomCommand(S);
end;


end.
