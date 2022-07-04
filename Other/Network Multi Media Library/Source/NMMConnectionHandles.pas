(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at babikov@mail.ru

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit NMMConnectionHandles;

interface
uses Classes, NMMCustomConnectionHandle, Graphics, IdTCPServer, SysUtils, NMMReceiverList;

type
 TNMMConnectionHandlesState = Set of (chssNeedsGarbage);

 TNMMConnectionHandles = class(TObject)
 protected
   FItems: TThreadList;
   FState: TNMMConnectionHandlesState;
 public
   constructor Create;
   destructor Destroy; override;
   function GetCount: Integer;
   procedure Add(AConnectionHandle: TNMMCustomConnectionHandle);
   procedure Remove(AConnectionHandle: TNMMCustomConnectionHandle);
   procedure GarbageConnectionHandles;
   function IndexOf(AConnectionHandle: TNMMCustomConnectionHandle): Integer;
   function FindConnectionHandleByUser(User: string): TNMMCustomConnectionHandle;
   function FindConnectionHandleByThread(APeerThread: TIdPeerThread): TNMMCustomConnectionHandle;
   procedure BroadcastRawText(AText: string; AReceiverList: TNMMReceiverList);
   procedure BroadcastMsg(AMsg: string);
   procedure DisconnectAll;
   property List: TThreadList read FItems;
   property State: TNMMConnectionHandlesState read FState write FState;
 end;


implementation
uses NMMCommon;

{TNMMConnectionHandles}
constructor TNMMConnectionHandles.Create;
begin
 inherited Create;
 FItems:= TThreadList.Create;
end;

destructor TNMMConnectionHandles.Destroy;
var i: Integer;
    LConnectionHandle: TNMMCustomConnectionHandle;
    LList: TList;
begin
 LList:= FItems.LockList;
 try
   for i:=LList.Count-1 downto 0 do
   begin
     LConnectionHandle:= TNMMCustomConnectionHandle( LList[i] );
     if (csInvalidConnection in LConnectionHandle.State) then
     begin
       LConnectionHandle.Release;
       LConnectionHandle.Disconnect;

       if LConnectionHandle.RefCount=0 then 
       begin
         FreeAndNil(LConnectionHandle); 
       end;
     end;
   end;
 finally
   FItems.UnlockList;
 end;
 FreeAndNil(FItems);
 inherited Destroy;
end;

procedure TNMMConnectionHandles.Add(AConnectionHandle: TNMMCustomConnectionHandle);
begin
 FItems.Add(AConnectionHandle);
 AConnectionHandle.State:= [csNewConnection];
 AConnectionHandle.AddRef;
end;

procedure TNMMConnectionHandles.Remove(AConnectionHandle: TNMMCustomConnectionHandle);
var LList: TList;
    LIndex: Integer;
begin
 LList:= FItems.LockList;
 try
   LIndex:= LList.IndexOf(AConnectionHandle);
   if LIndex <> -1 then
   begin
     LList.Delete(LIndex);
     AConnectionHandle.Release;
     if AConnectionHandle.RefCount<=0 then
     begin
       FreeAndNil(AConnectionHandle);
     end;
   end;
 finally
   FItems.UnlockList;
 end;
end;

function TNMMConnectionHandles.IndexOf(AConnectionHandle: TNMMCustomConnectionHandle): Integer;
var LList: TList;
begin
 LList:= FItems.LockList;
 try
   Result:= LList.IndexOf(AConnectionHandle);
 finally
   FItems.UnlockList;
 end;
end;

function TNMMConnectionHandles.GetCount: Integer;
var LList: TList;
begin
 LList:= FItems.LockList;
 try
   Result:= LList.Count;
 finally
   FItems.UnlockList;
 end;
end;

procedure TNMMConnectionHandles.BroadcastRawText(AText: string; AReceiverList: TNMMReceiverList);
var i: Integer;
    LConnectionHandle: TNMMCustomConnectionHandle;
    LList: TList;
begin
 LList:= FItems.LockList;
 try
   for i:= 0 to LList.Count - 1 do
   begin
     LConnectionHandle:= TNMMCustomConnectionHandle( LList[i] );
     if not (csInvalidConnection in LConnectionHandle.State) then
     begin
       if (AReceiverList=nil) or
          AReceiverList.CheckUser(LConnectionHandle.User) then
       begin
         LConnectionHandle.SendRawText(AText);
       end;
     end
     else
     begin
       FState:= FState + [chssNeedsGarbage];
     end;
   end;
 finally
   FItems.UnlockList;
 end;
end;

procedure TNMMConnectionHandles.BroadcastMsg(AMsg: string);
begin
 BroadcastRawText(cmdTxtMsg+AMsg,nil);
end;

function TNMMConnectionHandles.FindConnectionHandleByUser(User: string): TNMMCustomConnectionHandle;
var i: Integer;
    LConnectionHandle: TNMMCustomConnectionHandle;
    LList: TList;
begin
 result:= nil;
 LList:= FItems.LockList;
 try
   for i:= 0 to LList.Count - 1 do
   begin
     LConnectionHandle:= TNMMCustomConnectionHandle( LList[i] );
     if not (csInvalidConnection in LConnectionHandle.State) and
       (LConnectionHandle.PeerThread <> nil) and
        not LConnectionHandle.PeerThread.Terminated and
       (LConnectionHandle.PeerThread.Connection <> nil) then
     begin
       if UpperCase(LConnectionHandle.User) = UpperCase(User) then
       begin
         result:= LConnectionHandle;
         break;
       end;
     end
     else
     begin
       FState:= FState + [chssNeedsGarbage];
     end;
   end;
 finally
   FItems.UnlockList;
 end;
end;


function TNMMConnectionHandles.FindConnectionHandleByThread(APeerThread: TIdPeerThread): TNMMCustomConnectionHandle;
var i: Integer;
    LConnectionHandle: TNMMCustomConnectionHandle;
    LList: TList;
begin
 result:= nil;
 LList:= FItems.LockList;
 try
   for i:= 0 to LList.Count - 1 do
   begin
     LConnectionHandle:= TNMMCustomConnectionHandle( LList[i] );
     if not (csInvalidConnection in LConnectionHandle.State) then
     begin
       if LConnectionHandle.PeerThread = APeerThread then
         result:= LConnectionHandle;
     end else
     begin
       FState:= FState + [chssNeedsGarbage];
     end;
   end;
 finally
   FItems.UnlockList;
 end;
end;

procedure TNMMConnectionHandles.GarbageConnectionHandles;
var i: Integer;
    LConnectionHandle: TNMMCustomConnectionHandle;
    LList: TList;
 procedure DoRemoveConnectionHandle;
 begin
   Remove(LConnectionHandle);
   if LConnectionHandle.RefCount<=0 then
   begin
     FreeAndNil(LConnectionHandle);
   end;
 end;
begin
 LList:= FItems.LockList;
 try
   for i:=LList.Count-1 downto 0 do
   begin
     LConnectionHandle:= TNMMCustomConnectionHandle( LList[i] );
     try
       if (csInvalidConnection in LConnectionHandle.State) or
          (LConnectionHandle.PeerThread = nil) or
           LConnectionHandle.PeerThread.Terminated or
          (LConnectionHandle.PeerThread.Connection = nil) or
          (LConnectionHandle.PeerThread.Connection.Connected = false) then
       begin
         DoRemoveConnectionHandle;
       end;
     except
       DoRemoveConnectionHandle;
     end;
   end;
 finally
   FItems.UnlockList;
 end;
 FState:= FState - [chssNeedsGarbage];
end;

procedure TNMMConnectionHandles.DisconnectAll;
var i: Integer;
    LConnectionHandle: TNMMCustomConnectionHandle;
    LList: TList;
begin
 LList:= FItems.LockList;
 try
   for i:= 0 to LList.Count - 1 do
   begin
     LConnectionHandle:= TNMMCustomConnectionHandle( LList[i] );
     if not (csInvalidConnection in LConnectionHandle.State) then
     begin
       LConnectionHandle.Disconnect;
     end;
   end;
 finally
   FItems.UnlockList;
 end;
 FState:= FState - [chssNeedsGarbage];
end;

end.
