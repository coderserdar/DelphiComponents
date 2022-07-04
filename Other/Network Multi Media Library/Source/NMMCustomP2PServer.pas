(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at babikov@mail.ru

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit NMMCustomP2PServer;

interface

uses Classes, IdTCPServer,
     NMMCustomConnectionHandle, NMMConnectionHandle, NMMCustomConnectionProcessor,
     NMMCustomServer, NMMCommon,  NMMUserData;

type
 TNMMCustomP2PServer = class(TNMMCustomServer)
 protected
   FUser: String;
   FPassword: String;
   FConnectionHandle: TNMMCustomConnectionHandle;
   FConnectionProcessor: TNMMCustomConnectionProcessor;
   procedure OnTCPServerConnect(AThread: TIdPeerThread); override;
   function CreateConnectionProcessor(AUserData: TNMMUserData): TNMMCustomConnectionProcessor; virtual; abstract;
   function CreateConnectionHandle(AUserData: TNMMUserData; APeerThread: TIdPeerThread): TNMMCustomConnectionHandle; virtual;
 public
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;
   procedure DisconnectPeer(AThread: TIdPeerThread); override;
 published
   property User: String read FUser write FUser;
   property Password: String read FPassword write FPassword;
 end;


implementation

uses IdGlobal, SysUtils;


constructor TNMMCustomP2PServer.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TNMMCustomP2PServer.Destroy;
begin
  if FConnectionProcessor<>nil then
     FreeAndNil(FConnectionProcessor);
  if FConnectionHandle<>nil then
     FreeAndNil(FConnectionHandle);
  inherited;
end;

function TNMMCustomP2PServer.CreateConnectionHandle(AUserData: TNMMUserData;
              APeerThread: TIdPeerThread): TNMMCustomConnectionHandle;
begin
  result:= TNMMConnectionHandle.Create(AUserData,APeerThread,self);
end;

procedure TNMMCustomP2PServer.OnTCPServerConnect(AThread: TIdPeerThread);
var LUserData: TNMMUserData;
    LRefuseReason: String;
begin
  AThread.FreeOnTerminate:= true;
  if FConnectionHandle<>nil then
  begin
    LRefuseReason:= 'Only one connection allowed';
    RefusePeer(AThread,LRefuseReason);
    Exit;
  end;

  try
    LUserData:= TNMMUserData.Create;
    try
      LUserData.UserData:= AThread.Connection.ReadLn(LF);


      if not FAuthChecker.AcceptClient(LUserData,LRefuseReason) then
      begin
        RefusePeer(AThread,LRefuseReason);
        Exit;
      end;

      if FConnectionProcessor<>nil then
         FreeAndNil(FConnectionProcessor);

      FConnectionHandle:= CreateConnectionHandle(LUserData,AThread);
      FConnectionProcessor:= CreateConnectionProcessor(LUserData);
      FConnectionProcessor.State:= [cpsStarting];
      FConnectionHandle.Period:= LUserData.Period;
      FConnectionProcessor.AddConnectionHandle(FConnectionHandle);

      AThread.Connection.SendBufferSize:= ServerSendBufferSize;
      FConnectionHandle.ConnectionTime:= Now;
    finally
    end;
  except
    on E: Exception do
    begin
      AddErrorToLog(E.Message);
    end;
  end;
end;

procedure TNMMCustomP2PServer.DisconnectPeer(AThread: TIdPeerThread);
begin
  FConnectionProcessor.RemoveConnectionHandle(FConnectionHandle);
  if FConnectionHandle<>nil then
     FreeAndNil(FConnectionHandle);
  inherited;
end;

end.
