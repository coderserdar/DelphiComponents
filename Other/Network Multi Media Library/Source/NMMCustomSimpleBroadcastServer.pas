(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at babikov@mail.ru

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit NMMCustomSimpleBroadcastServer;

interface
uses Classes, NMMCustomConnectionHandle,
     NMMConnectionHandles,
     IdTCPServer,
     NMMCommon,
     NMMCustomAuthChecker, NMMCustomBroadcastServer, NMMCustomConnectionProcessor,
     NMMCustomConnectionGroupThread,
     SyncObjs, SysUtils, NMMUserData;

type
 TOnConnectEvent= procedure(Sender: TObject; User: String; IdPeerThread:TIdPeerThread; var Accept: Boolean; var RefuseReason: String) of object;

 TNMMCustomSimpleBroadcastServer= class(TNMMCustomBroadcastServer)
 protected
   FConnectionProcessor: TNMMCustomConnectionGroupThread;
   FBeforeConnect: TOnConnectEvent;
   FOnDisconnect: TNotifyEvent;

   procedure SetActive(AActive: Boolean); override;
   procedure OnTCPServerConnect(AThread: TIdPeerThread); override;
   procedure OnTCPServerDisconnect(AThread: TIdPeerThread); override;

   procedure DeleteConnectionProcessor;
   function CreateConnectionProcessor: TNMMCustomConnectionGroupThread; virtual; abstract;

   property BeforeConnect: TOnConnectEvent read FBeforeConnect write FBeforeConnect;
   property OnDisconnect: TNotifyEvent read FOnDisconnect write FOnDisconnect;
 public
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;
   function CreateConnectionHandle(AUserData: TNMMUserData; AThread: TIdPeerThread): TNMMCustomConnectionHandle; virtual;
   procedure RemoveConnectionHandle(AConnectionHandle: TNMMCustomConnectionHandle);
   procedure DisconnectThread(AThread: TIdPeerThread);
 published
   property MaxConnections;
 end;


implementation

uses NMMConnectionHandle, IdException,
     IdGlobal, NMMServerGlobals, NMMCustomServer;


{TNMMCustomSimpleBroadcastServer}
constructor TNMMCustomSimpleBroadcastServer.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
  if (not (csDesigning in ComponentState)) and
     (not (csLoading in ComponentState)) then
  begin
  end;
end;

destructor TNMMCustomSimpleBroadcastServer.Destroy;
begin
  Active:= false;
  if (not (csDesigning in ComponentState)) and
     (not (csLoading in ComponentState)) then
  begin
    if Assigned(FConnectionProcessor) then
    begin
    end;
  end;
  inherited Destroy;
end;

procedure TNMMCustomSimpleBroadcastServer.DeleteConnectionProcessor;
var j: Integer;
begin
  if Assigned(FConnectionProcessor) then
  begin
    FConnectionProcessor.ConnectionHandles.DisconnectAll;
    FConnectionProcessor.State:= FConnectionProcessor.State+[cpsTerminating];
    if not FConnectionProcessor.Suspended then
    begin
      for j:=1 to 200 do
      begin
        if CheckIfTermitated(FConnectionProcessor) then
          break
        else
          Sleep(10);
      end;
    end;
    FConnectionProcessor.Terminate;
    if not FConnectionProcessor.FreeOnTerminate then
    begin
      try
        FreeAndNil(FConnectionProcessor);
      except
      end;
    end;
  end;
  FConnectionHandles.GarbageConnectionHandles;
end;

procedure TNMMCustomSimpleBroadcastServer.SetActive(AActive: Boolean);
begin
 inherited;
end;

function TNMMCustomSimpleBroadcastServer.CreateConnectionHandle(AUserData: TNMMUserData;
         AThread: TIdPeerThread): TNMMCustomConnectionHandle;
begin
 result:= TNMMConnectionHandle.Create(AUserData,AThread,self);
end;

procedure TNMMCustomSimpleBroadcastServer.RemoveConnectionHandle(AConnectionHandle: TNMMCustomConnectionHandle);
begin
 if FConnectionProcessor <> nil then
    FConnectionProcessor.RemoveConnectionHandle(AConnectionHandle);
end;

procedure TNMMCustomSimpleBroadcastServer.OnTCPServerConnect(AThread: TIdPeerThread);
var ConnectionHandle: TNMMCustomConnectionHandle;
    LUserData: TNMMUserData;
    LRefuseReason: String;
    LAccept: Boolean;
begin
 AThread.FreeOnTerminate:= true;
 try
   LUserData:= TNMMUserData.Create;
   LUserData.UserData:= AThread.Connection.ReadLn(LF);

   if (FMaxConnections <> -1{unlimited}) and
      not (ConnectionHandles.GetCount < FMaxConnections) then
   begin
     (AThread as TNMMServerThread).DontCallOnDisconnect:= true;
     RefusePeer(AThread,'Too many connections');
     FreeAndNil(LUserData);
     Exit;
   end;

   if not FAuthChecker.AcceptClient(LUserData,LRefuseReason) then
   begin
     (AThread as TNMMServerThread).DontCallOnDisconnect:= true;
     RefusePeer(AThread,LRefuseReason);
     FreeAndNil(LUserData);
     Exit;
   end;

   if Assigned(FBeforeConnect) then
   begin
     FBeforeConnect(self,LUserData.User,AThread,LAccept,LRefuseReason);
     if not LAccept then
     begin
      (AThread as TNMMServerThread).DontCallOnDisconnect:= true;
       RefusePeer(AThread,LRefuseReason);
       FreeAndNil(LUserData);
       Exit;
     end;
   end;
   
   ConnectionHandle:= CreateConnectionHandle(LUserData,AThread);
   ConnectionHandle.Period:= LUserData.Period;

   FCS_Login.Enter;
   try
     if not Assigned(FConnectionProcessor) then
        FConnectionProcessor:= CreateConnectionProcessor;
     FConnectionProcessor.AddConnectionHandle(ConnectionHandle);
     try
       FConnectionHandles.Add(ConnectionHandle);
     except
       RemoveConnectionHandle(ConnectionHandle);
       try
        (AThread as TNMMServerThread).DontCallOnDisconnect:= true;
         RefusePeer(AThread, cmdRefused);
       finally
         ConnectionHandle.Free;
       end;
     end;
   finally
     FCS_Login.Leave; 
   end;

   AThread.Connection.SendBufferSize:= ServerSendBufferSize;
   ConnectionHandle.ConnectionTime:= Now;
 except
   on E: Exception do
   begin
     AddErrorToLog(E.Message);
   end;
 end;
end;

procedure TNMMCustomSimpleBroadcastServer.OnTCPServerDisconnect(AThread: TIdPeerThread);
begin
  DisconnectThread(AThread);
//  if AThread.Connection.Connected then
  begin
    if Assigned(FOnDisconnect) and not (AThread as TNMMServerThread).DontCallOnDisconnect then
       FOnDisconnect(self);
  end;
end;

procedure TNMMCustomSimpleBroadcastServer.DisconnectThread(AThread: TIdPeerThread);
var j: Integer;
    LConnectionHandles: TList;
    LConnectionHandle: TNMMCustomConnectionHandle;
begin
 LConnectionHandle:= nil;
 FCS_Login.Enter;
 try
   LConnectionHandles:= FConnectionProcessor.ConnectionHandles.List.LockList;
   try
     for j:=0 to LConnectionHandles.Count-1 do
     begin
       if TNMMCustomConnectionHandle(LConnectionHandles[j]).PeerThread = AThread then
       begin
         LConnectionHandle:= LConnectionHandles[j];
         break;
       end;
     end;
   finally
     FConnectionProcessor.ConnectionHandles.List.UnlockList;
   end;

   if (LConnectionHandle<>nil) then
   begin
     FConnectionProcessor.RemoveConnectionHandle(LConnectionHandle);
     FConnectionHandles.Remove(LConnectionHandle);
   end;
 finally
   try
     AThread.Connection.Disconnect;
     AThread.Terminate;
     if not AThread.FreeOnTerminate then
     begin
       FreeAndNil(AThread);
     end;
   except
   end;
   FCS_Login.Leave;
 end;
end;

end.
