(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at babikov@mail.ru

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit NMMCustomBroadcastServer;

interface
uses Classes, NMMCustomConnectionHandle,
     NMMConnectionHandles, 
     IdTCPServer,
     NMMCommon, NMMCustomConnectionGroupThread,
     NMMCustomAuthChecker, NMMCustomServer,
     SyncObjs, SysUtils, NMMUserData;

type
 TNMMCustomBroadcastServer= class(TNMMCustomServer)
 protected
   FConnectionHandles: TNMMConnectionHandles;
   FCS_RefuseOldConnection: TCriticalSection;
   FMaxConnections: Integer;
   procedure SetActive(AActive: Boolean); override;
   property ConnectionHandles: TNMMConnectionHandles read FConnectionHandles write FConnectionHandles;
   property MaxConnections: Integer read FMaxConnections write FMaxConnections;
 public
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;
   function CheckIfTermitated(AConnectionGroupThread: TNMMCustomConnectionGroupThread): Boolean;
   procedure BroadcastMsg(Msg: string);
   procedure FillUserList(AUserList: TStringList);
   function SendTextMessageToUser( APtrConnectionHandle: Pointer; AMessage: String): Boolean;
 published
 end;


implementation

uses NMMConnectionHandle, NMMCustomConnectionProcessor, IdException,
     IdGlobal, NMMServerGlobals;


{TNMMCustomBroadcastServer}
constructor TNMMCustomBroadcastServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMaxConnections:= -1; // -1 - unlimited

  if (not (csDesigning in ComponentState)) and
     (not (csLoading in ComponentState)) then
  begin
    FConnectionHandles:= TNMMConnectionHandles.Create;
    FCS_RefuseOldConnection:= TCriticalSection.Create;
  end;
end;


destructor TNMMCustomBroadcastServer.Destroy;
begin
  if (not (csDesigning in ComponentState)) and
     (not (csLoading in ComponentState)) then
  begin
    Active:= false;
    FreeAndNil(FConnectionHandles);
    FreeAndNil(FCS_RefuseOldConnection);
  end;
  
  inherited Destroy;
end;

procedure TNMMCustomBroadcastServer.SetActive(AActive: Boolean);
begin
 inherited;
end;


function TNMMCustomBroadcastServer.CheckIfTermitated(AConnectionGroupThread: TNMMCustomConnectionGroupThread): Boolean;
begin
 result:= (AConnectionGroupThread = nil) or
          (cpsTerminated in AConnectionGroupThread.State);
end;

procedure TNMMCustomBroadcastServer.BroadcastMsg(Msg: string);
begin
 FConnectionHandles.BroadcastMsg(Msg);
end;

procedure TNMMCustomBroadcastServer.FillUserList(AUserList: TStringList);
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

function TNMMCustomBroadcastServer.SendTextMessageToUser(
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
              cmdTxtMsg+AMessage
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
