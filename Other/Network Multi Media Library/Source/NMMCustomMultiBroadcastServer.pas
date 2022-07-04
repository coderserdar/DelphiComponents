(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at babikov@mail.ru

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit NMMCustomMultiBroadcastServer;

interface
uses Classes, NMMCustomConnectionHandle,
     NMMConnectionHandles, 
     IdTCPServer,
     NMMCommon,
     NMMCustomAuthChecker, NMMCustomServer, NMMCustomConnectionGroupThread,
     SyncObjs, SysUtils, NMMUserData;

type

 TNMMCustomMultiBroadcastServer= class(TNMMCustomServer)
 protected
   FConnectionHandles: TNMMConnectionHandles;
   FConnectionGroupThreads: TNMMConnectionGroupThreads;
   FCS_RefuseOldConnection: TCriticalSection;

   procedure SetActive(AActive: Boolean); override;
   procedure OnTCPServerConnect(AThread: TIdPeerThread); override;
   procedure OnTCPServerDisconnect(AThread: TIdPeerThread); override;
   procedure OnTCPServerException(AThread: TIdPeerThread; AException: Exception); override;

   procedure DeleteConnectionGroupThreads;
 public
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;

   function CreateConnectionHandle(AUserData: TNMMUserData; AThread: TIdPeerThread): TNMMCustomConnectionHandle; virtual;
   function CreateNewConnectionGroupThread(APeriod: Int64): TNMMCustomConnectionGroupThread; virtual; abstract;

   function PutIntoAppropriateGroup(AConnectionHandle: TNMMCustomConnectionHandle): Boolean; virtual;
   function FindConnectionGroupThread(AConnectionHandle: TNMMCustomConnectionHandle): TNMMCustomConnectionGroupThread;
   procedure RemoveConnectionHandle(AConnectionHandle: TNMMCustomConnectionHandle);
   procedure RefusePeer(AThread: TIdPeerThread; ARefuseReason: String);
   procedure DisconnectThread(AThread: TIdPeerThread);
   function CheckIfTermitated(AConnectionGroupThread: TNMMCustomConnectionGroupThread): Boolean;
   function CheckIfTermitating(AConnectionGroupThread: TNMMCustomConnectionGroupThread): Boolean;
   procedure GarbageConnectionGroupThreads;
   procedure BroadcastMsg(Msg: string);
   procedure FillUserList(AUserList: TStringList);
   function SendTextMessageToUser( APtrConnectionHandle: Pointer; AMessage: String): Boolean;

   property ConnectionGroupThreads: TNMMConnectionGroupThreads read FConnectionGroupThreads;
 published
 end;


implementation

uses NMMConnectionHandle, NMMCustomConnectionProcessor, IdException,
     IdGlobal, NMMServerGlobals;


{TNMMCustomMultiBroadcastServer}
constructor TNMMCustomMultiBroadcastServer.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 FConnectionGroupThreads:= TNMMConnectionGroupThreads.Create;
 FConnectionHandles:= TNMMConnectionHandles.Create;
 FCS_RefuseOldConnection:= TCriticalSection.Create;
end;


destructor TNMMCustomMultiBroadcastServer.Destroy;
begin
 Active:= false;
 DeleteConnectionGroupThreads;
 FreeAndNil(FConnectionGroupThreads);
 FreeAndNil(FConnectionHandles);
 FreeAndNil(FCS_RefuseOldConnection);
 inherited Destroy;
end;


procedure TNMMCustomMultiBroadcastServer.DeleteConnectionGroupThreads;
var i,j: Integer;
    LConnectionGroupThreads: TList;
    LConnectionGroupThread: TNMMCustomConnectionGroupThread;
begin
 LConnectionGroupThreads:= FConnectionGroupThreads.LockList;
 try
   for i:= LConnectionGroupThreads.Count-1 downto 0 do
   begin
     LConnectionGroupThread:= TNMMCustomConnectionGroupThread( LConnectionGroupThreads[i] );
     LConnectionGroupThread.ConnectionHandles.DisconnectAll;
     LConnectionGroupThreads.Remove(LConnectionGroupThread);
     LConnectionGroupThread.State:= LConnectionGroupThread.State+[cpsTerminating];
     for j:=1 to 20 do
     begin
       if CheckIfTermitated(LConnectionGroupThread) then
         break
       else
         Sleep(100);
     end;
     LConnectionGroupThread.Terminate;
     if not LConnectionGroupThread.FreeOnTerminate then
     begin
       try
         LConnectionGroupThread.Free;
       except
       end;  
     end;
   end;
 finally
   FConnectionGroupThreads.UnlockList;
 end;
 FConnectionHandles.GarbageConnectionHandles;
end;

procedure TNMMCustomMultiBroadcastServer.SetActive(AActive: Boolean);
begin
 if not AActive then
 begin
   DeleteConnectionGroupThreads;
 end;
 inherited;
end;


function TNMMCustomMultiBroadcastServer.CheckIfTermitated(AConnectionGroupThread: TNMMCustomConnectionGroupThread): Boolean;
begin
 result:= (AConnectionGroupThread = nil) or
          (cpsTerminated in AConnectionGroupThread.State) or
           AConnectionGroupThread.Suspended;
end;


function TNMMCustomMultiBroadcastServer.CheckIfTermitating(AConnectionGroupThread: TNMMCustomConnectionGroupThread): Boolean;
begin
 result:= (cpsTerminated in AConnectionGroupThread.State) or
          (cpsTerminating in AConnectionGroupThread.State) or
           AConnectionGroupThread.Suspended;
end;


procedure TNMMCustomMultiBroadcastServer.GarbageConnectionGroupThreads;
var i,j,LWaitTo: Integer;
    LConnectionGroupThreads: TList;
    LConnectionGroupThread: TNMMCustomConnectionGroupThread;
begin
 LConnectionGroupThreads:= FConnectionGroupThreads.LockList;
 try
   for i:=LConnectionGroupThreads.Count-1 downto 0 do
   begin
     LConnectionGroupThread:= TNMMCustomConnectionGroupThread( LConnectionGroupThreads[i] );
     if CheckIfTermitating(LConnectionGroupThread) then
     begin
       LConnectionGroupThreads.Remove(LConnectionGroupThread);
       LConnectionGroupThread.State:= LConnectionGroupThread.State + [cpsTerminating];
       try
         LConnectionGroupThread.ConnectionHandles.DisconnectAll;
         if cpsCalculations in LConnectionGroupThread.State then
         begin
           LWaitTo:= 150;
         end
         else
         begin
           LWaitTo:= 20;
         end;

         for j:=1 to LWaitTo do
         begin
           if CheckIfTermitated(LConnectionGroupThread) then
             break
           else
             Sleep(100);
         end;
       finally
         LConnectionGroupThread.Terminate;
       end;
       
       if not LConnectionGroupThread.FreeOnTerminate then
       begin
         try
           LConnectionGroupThread.Free;
         except
         end;
       end;
     end;
   end;
 finally
   FConnectionGroupThreads.UnlockList;
 end;

 FConnectionHandles.GarbageConnectionHandles;
end;


function TNMMCustomMultiBroadcastServer.CreateConnectionHandle(AUserData: TNMMUserData;
         AThread: TIdPeerThread): TNMMCustomConnectionHandle;
begin
 result:= TNMMConnectionHandle.Create(AUserData,AThread,self);
end;


function TNMMCustomMultiBroadcastServer.PutIntoAppropriateGroup(AConnectionHandle: TNMMCustomConnectionHandle): Boolean;
var i: Integer;
    LConnectionGroupThreads: TList;
    LConnectionGroupThread, LNewConnectionGroupThread: TNMMCustomConnectionGroupThread;
begin
 result:= false;
 LConnectionGroupThreads:= FConnectionGroupThreads.LockList;
 try
   for i:=0 to LConnectionGroupThreads.Count-1 do
   begin
     LConnectionGroupThread:= TNMMCustomConnectionGroupThread( LConnectionGroupThreads[i] );
     if CheckIfTermitating(LConnectionGroupThread) then
     begin
       Continue;
     end;
     if LConnectionGroupThread.Period = AConnectionHandle.Period then
     begin
       LConnectionGroupThread.AddConnectionHandle(AConnectionHandle);
       result:= true;
       break;
     end;
   end;
 finally
   FConnectionGroupThreads.UnlockList;
 end;

 if not result then
 begin
   LNewConnectionGroupThread:= CreateNewConnectionGroupThread(AConnectionHandle.Period);
   result:= LNewConnectionGroupThread<>nil;
   if result then
   begin
     FConnectionGroupThreads.Add(LNewConnectionGroupThread);
     LNewConnectionGroupThread.AddConnectionHandle(AConnectionHandle);
   end;
 end;
end;

function TNMMCustomMultiBroadcastServer.FindConnectionGroupThread(AConnectionHandle: TNMMCustomConnectionHandle): TNMMCustomConnectionGroupThread;
var i: Integer;
    LConnectionGroupThreads: TList;
    LConnectionGroupThread: TNMMCustomConnectionGroupThread;
begin
 result:= nil;
 LConnectionGroupThreads:= FConnectionGroupThreads.LockList;
 try
   for i:=0 to LConnectionGroupThreads.Count-1 do
   begin
     LConnectionGroupThread:= TNMMCustomConnectionGroupThread( LConnectionGroupThreads[i] );
     if LConnectionGroupThread.ConnectionHandles.IndexOf(AConnectionHandle) <> -1 then
     begin
       result:= LConnectionGroupThread;
       break;
     end;
   end;
 finally
   FConnectionGroupThreads.UnlockList;
 end;
end;

procedure TNMMCustomMultiBroadcastServer.RemoveConnectionHandle(AConnectionHandle: TNMMCustomConnectionHandle);
var ConnectionGroupThread: TNMMCustomConnectionGroupThread;
begin
 ConnectionGroupThread:= FindConnectionGroupThread(AConnectionHandle);
 if ConnectionGroupThread <> nil then
   ConnectionGroupThread.RemoveConnectionHandle(AConnectionHandle);
 GarbageConnectionGroupThreads;
end;

procedure TNMMCustomMultiBroadcastServer.RefusePeer(AThread: TIdPeerThread; ARefuseReason: String);
begin
 if AThread.Connection.Connected then
 begin
   AThread.Connection.WriteLn(ARefuseReason); // Send refusing message
   AThread.Connection.Disconnect; // RefuseThread;
 end;
end;

procedure TNMMCustomMultiBroadcastServer.OnTCPServerConnect(AThread: TIdPeerThread);
var ConnectionHandle: TNMMCustomConnectionHandle;
    LUserData: TNMMUserData;
    LRefuseReason: String;
begin
 AThread.FreeOnTerminate:= true;
 try
   try
     LUserData:= TNMMUserData.Create;
     try
       LUserData.UserData:= AThread.Connection.ReadLn(LF);

       if not FAuthChecker.AcceptClient(LUserData,LRefuseReason) then
       begin
         RefusePeer(AThread,LRefuseReason);
         Exit;
       end;

       GarbageConnectionGroupThreads; 
       ConnectionHandle:= CreateConnectionHandle(LUserData,AThread);
       ConnectionHandle.Period:= LUserData.Period;

       FCS_Login.Enter;
       try
         if PutIntoAppropriateGroup(ConnectionHandle) then
         begin
           try
             FConnectionHandles.Add(ConnectionHandle);
           except
             RemoveConnectionHandle(ConnectionHandle);
             try 
               RefusePeer(AThread, cmdRefused);
             finally
               ConnectionHandle.Free;
             end;
           end;
         end
         else
         begin
           try
             RefusePeer(AThread,cmdRefused);
           finally
             ConnectionHandle.Free;
           end;
         end;
       finally
         FCS_Login.Leave; 
       end;

       AThread.Connection.SendBufferSize:= ServerSendBufferSize;
       ConnectionHandle.ConnectionTime:= Now;
     finally
     end;
   except
     on E: Exception do
     begin
       AddErrorToLog(E.Message);
     end;  
   end;
 finally
   GarbageConnectionGroupThreads; 
 end;
end;


procedure TNMMCustomMultiBroadcastServer.OnTCPServerDisconnect(AThread: TIdPeerThread);
begin
 DisconnectThread(AThread);
end;


procedure TNMMCustomMultiBroadcastServer.DisconnectThread(AThread: TIdPeerThread);
var i,j: Integer;
    LConnectionGroupThreads, LConnectionHandles: TList;
    LConnectionGroupThread: TNMMCustomConnectionGroupThread;
    LConnectionHandle: TNMMCustomConnectionHandle;
begin
 LConnectionHandle:= nil;
 LConnectionGroupThread:= nil;
 FCS_Login.Enter; 
 try
   LConnectionGroupThreads:= FConnectionGroupThreads.LockList;
   try
     for i:=0 to LConnectionGroupThreads.Count-1 do
     begin
       LConnectionGroupThread:= TNMMCustomConnectionGroupThread( LConnectionGroupThreads[i] );
       LConnectionHandles:= LConnectionGroupThread.ConnectionHandles.List.LockList;
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
         LConnectionGroupThread.ConnectionHandles.List.UnlockList;
       end;
       if LConnectionHandle<> nil then
         break;
     end;
   finally
     FConnectionGroupThreads.UnlockList;
   end;

   if (LConnectionHandle<>nil) and (LConnectionGroupThread<>nil) then
   begin
     LConnectionGroupThread.RemoveConnectionHandle(LConnectionHandle);
     FConnectionHandles.Remove(LConnectionHandle);
   end;
   GarbageConnectionGroupThreads;
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

procedure TNMMCustomMultiBroadcastServer.OnTCPServerException(AThread: TIdPeerThread;
                                                  AException: Exception);
begin
 if (AException is EIdConnClosedGracefully) then
 begin
   DisconnectThread(AThread);
 end else
 begin
   AddErrorToLog(AException.Message);
 end;
end;


procedure TNMMCustomMultiBroadcastServer.BroadcastMsg(Msg: string);
begin
 FConnectionHandles.BroadcastMsg(Msg);
end;


procedure TNMMCustomMultiBroadcastServer.FillUserList(AUserList: TStringList);
var i: Integer;
    LConnectionHandle: TNMMCustomConnectionHandle;
    LList: TList;
begin
 AUserList.Clear;
 if FActive then
 begin
   LList:= FConnectionHandles.List.LockList;
   try
     for i:= 0 to LList.Count - 1 do
     begin
       LConnectionHandle:= TNMMCustomConnectionHandle( LList[i] );
       AUserList.AddObject( LConnectionHandle.User+' '+
                            TimeToStr(LConnectionHandle.ConnectionTime),
                            LConnectionHandle );
     end;
   finally
     FConnectionHandles.List.UnlockList;
   end;
 end;
end;


function TNMMCustomMultiBroadcastServer.SendTextMessageToUser(
  APtrConnectionHandle: Pointer; AMessage: String): Boolean;
var LConnectionHandleIndex: Integer;
    LConnectionHandles: TList;
begin
 LConnectionHandles:= FConnectionHandles.List.LockList;
 try
   LConnectionHandleIndex:= LConnectionHandles.IndexOf(APtrConnectionHandle);
   if LConnectionHandleIndex > -1 then
   begin
     try
       TNMMCustomConnectionHandle(LConnectionHandles[LConnectionHandleIndex]).SendRawText(
              cmdSingleTxtMsg+DateTimeToStr(Now)+': '+AMessage
              );
       result:= true;
     except
       result:= false;
       exit;
     end;
   end
   else
   begin
     result:= false;
   end;
 finally
   FConnectionHandles.List.UnlockList;
 end;
end;

end.
