(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at babikov@mail.ru

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit NMMConnectionHandle;

interface
uses 
  Classes, 
  NMMConnectionHandles, NMMCustomConnectionHandle, 
  NMMCustomDataProcessors, NMMCustomServer, NMMUserData,
  IdTCPConnection, IdTCPServer, IdThread, IdThreadSafe,
  Graphics, SyncObjs;

{ TNMMConnectionHandle }
type
 TNMMConnectionHandleWriteStreamThread= class;

 TNMMConnectionHandle = class(TNMMCustomConnectionHandle)
 protected
   FWriteStreamThread: TNMMConnectionHandleWriteStreamThread;
   FServer: TNMMCustomServer;
 public
   constructor Create(AUserData: TNMMUserData; APeerThread: TIdPeerThread;
                      AServer: TNMMCustomServer; AConnection: TIdTCPConnection = nil);
   destructor Destroy; override;
   procedure SendIniData(AIniDataStream: TStream; AWidth, AHeight: Integer); override;
   procedure SendData(AData: TStream); override;
   procedure SendRawText(AText: String); override; //todo ->SendText
   function Busy: Boolean; override;
   function Overloaded: Boolean; override;
   procedure Disconnect; override;
   function TestConnectionObjects: Boolean;
 end;

 TNMMConnectionHandleWriteStreamThread = class(TThread)
 protected
   FParent: TNMMConnectionHandle;
   FStreamCash: TThreadList;
   FMsg: String;
   FMsgCash: TThreadList;
   FIniStreamWidth: Integer;
   FIniStreamHeight: Integer;
   FStreamPacket: TMemoryStream;
 public
   Working: Boolean;
   constructor Create(AParent: TNMMConnectionHandle);
   destructor Destroy; override;
   procedure AlertSendMsgTask(AMsg: String);
   procedure AlertWriteStreamTask( AStream: TStream; AHeader: String);
   procedure Execute; override;
   procedure SetInvalid;
 end;

 TStreamTask = class(TObject)
 public
   Header: String;
   Stream: TStream;
   constructor Create;
   destructor Destroy; override;
 end;

implementation
uses NMMCommon, Math, NMMServerGlobals, SysUtils;

{TStreamTask}

constructor TStreamTask.Create;
begin
 inherited;
 Stream:= TMemoryStream.Create;
end;

destructor TStreamTask.Destroy;
begin
 FreeAndNil(Stream);
 inherited;
end;


{TNMMConnectionHandleWriteStreamThread}
constructor TNMMConnectionHandleWriteStreamThread.Create(AParent: TNMMConnectionHandle);
begin
 inherited Create(true);
 FreeOnTerminate:= true;
 Working:= false;
 FParent:= AParent;
 FStreamCash:= TThreadList.Create;
 FMsgCash:= TThreadList.Create;
 FStreamPacket:= TMemoryStream.Create;
 FreeOnTerminate:= true;
 GchwstInstances.Increment;
 Resume;
end;

destructor TNMMConnectionHandleWriteStreamThread.Destroy;
var LStream: TStream;
    LStringWrapper: TStringWrapper;
    LList: TList;
    i: Integer;
begin
 GchwstInstances.Decrement;
 LList:= FStreamCash.LockList;
 try
   for i:=LList.Count-1 downto 0 do
   begin
     LStream:= LList[0];
     LList.Delete(0);
     LStream.Free;
   end;
 finally
   FStreamCash.UnlockList;
 end;
 FreeAndNil(FStreamCash);

 LList:= FMsgCash.LockList;
 try
   for i:=LList.Count-1 downto 0 do
   begin
     LStringWrapper:= LList[0];
     LList.Delete(0);
     LStringWrapper.Free;
   end;
 finally
   FMsgCash.UnlockList;
 end;

 FreeAndNil(FMsgCash);
 FreeAndNil(FStreamPacket);
 inherited;
end;

procedure TNMMConnectionHandleWriteStreamThread.AlertWriteStreamTask( AStream: TStream; AHeader: String);
var
 LList: TList;
 LStreamTask: TStreamTask;
begin
 LList:= FStreamCash.LockList;
 try
   LStreamTask:= TStreamTask.Create;
   LStreamTask.Stream.CopyFrom(AStream,AStream.Size);
   LStreamTask.Stream.Seek(0, soFromBeginning);
   LStreamTask.Header:= AHeader;
   LList.Add(LStreamTask);
   AStream.Seek(0, soFromBeginning);
 finally
   FStreamCash.UnlockList;
 end;
end;

procedure TNMMConnectionHandleWriteStreamThread.AlertSendMsgTask(AMsg: String);
var
 LList: TList;
begin
 LList:= FMsgCash.LockList;
 try
   LList.Add(TStringWrapper.Create(AMsg));
 finally
   FMsgCash.UnlockList;
 end;
end;

function ThreadListCount(AThreadList: TThreadList): Integer;
var LList: TList;
begin
 LList:= AThreadList.LockList;
 try
   result:= LList.Count;
 finally
   AThreadList.UnlockList;
 end;
end;

function ThreadListIsEmpty(AThreadList: TThreadList): Boolean;
begin
 result:= not (ThreadListCount(AThreadList) > 0);
end;

procedure TNMMConnectionHandleWriteStreamThread.Execute;
 procedure Done;
 begin
   Working:= false;
 end;
var LList: TList;
    LStringWrapper: TStringWrapper;
    LBytesSent: Integer;
    LStreamTask: TStreamTask;
    LHeader: String;
begin
 FreeOnTerminate:= true; 
 try
   while not Terminated and not (csInvalidConnection in FParent.State) do
   begin
     Working:= true;
     if not FParent.TestConnectionObjects then
     begin
       Terminate;
       Exit;
     end;
     if (ThreadListIsEmpty(FStreamCash) and ThreadListIsEmpty(FMsgCash)) then
     begin
       Done;
       Sleep(250);
       Continue;
     end;
     try
       try
         with FParent.Connection do
         begin
           if Connected then
           begin
             if not ThreadListIsEmpty(FMsgCash) then
             begin
               LList:= FMsgCash.LockList;
               try
                 LStringWrapper:= LList[0];
                 OpenWriteBuffer;
                 try
                   WriteLn(LStringWrapper.Value);
                   CloseWriteBuffer;
                 except
                   on E: Exception do
                   begin
                     CancelWriteBuffer;
                     AddErrorToLog(E.Message);
                   end;
                 end;
                 LList.Delete(0);
                 FreeAndNil(LStringWrapper);
               finally
                 FMsgCash.UnlockList;
               end;
             end;

             if not ThreadListIsEmpty(FStreamCash) then
             begin
               LList:= FStreamCash.LockList;
               try
                 LStreamTask:= TStreamTask(LList[0]);
                 LHeader:= LStreamTask.Header; 
                 WriteLn(LHeader);

                 LBytesSent:= 0;
                 while LBytesSent < LStreamTask.Stream.Size do
                 begin
                   FStreamPacket.Clear;
                   FStreamPacket.CopyFrom(LStreamTask.Stream,LStreamTask.Stream.Size-LBytesSent);

                   FStreamPacket.Seek(0, soFromBeginning);

                   OpenWriteBuffer;
                   try
{$IFDEF VER140}      WriteStream(FStreamPacket,True,True); {$ENDIF}
{$IFDEF VER150}      WriteStream(FStreamPacket,True,True,FStreamPacket.Size); {$ENDIF}
                     CloseWriteBuffer;
                   except
                     if Connected then
                     begin
                       CancelWriteBuffer;
                     end;
                     raise;
                   end;
                   inc(LBytesSent,FStreamPacket.Size);
                 end;

                 if LList.Count > 0 then LList.Delete(0);
                 FreeAndNil(LStreamTask);
               finally
                 FStreamCash.UnlockList;
               end;
             end;

             Done;
           end
           else
           begin
             FParent.SetInvalid;
             Terminate; 
           end;
         end;
       except
         on E:Exception do
         begin
           if FParent<>nil then
           begin
             FParent.SetInvalid;
             Terminate; 
           end;
           Done;
           break;
         end;
       end;
     finally
       Done;
     end;
   end; 
 finally
   Terminate; 
 end;
end;

procedure TNMMConnectionHandleWriteStreamThread.SetInvalid;
begin
 FParent.SetInvalid;
end;


{TNMMConnectionHandle}
constructor TNMMConnectionHandle.Create(AUserData: TNMMUserData;
            APeerThread: TIdPeerThread; AServer: TNMMCustomServer;
            AConnection: TIdTCPConnection = nil);
begin
 inherited Create(AUserData);
 FPeerThread:= APeerThread;
 FConnection:= AConnection;
 if (FConnection=nil) and (FPeerThread<>nil) then
   FConnection:= FPeerThread.Connection;
 FServer:= AServer;
 FWriteStreamThread:= TNMMConnectionHandleWriteStreamThread.Create(self);
// FWriteStreamThread.OnTerminate:= OnWriteStreamThreadTerminate;
 GchInstances.Increment;
end;

destructor TNMMConnectionHandle.Destroy;
begin
 GchInstances.Decrement;
 try
   if (FWriteStreamThread<>nil) and
      not FWriteStreamThread.Terminated {shouldn't occur but...} then
   begin
     FWriteStreamThread.Terminate;
     if not FWriteStreamThread.FreeOnTerminate then
     begin
       FreeAndNil(FWriteStreamThread);
     end;
     Sleep(50);
   end;
 except
 end;

 try
   if (FPeerThread<>nil) and
      not FPeerThread.Terminated  then
   begin
     FPeerThread.Terminate;
     if not FPeerThread.FreeOnTerminate then
     begin
       FreeAndNil(FPeerThread);
     end;
   end;
 except
 end;

 inherited Destroy;
end;

{
procedure TNMMConnectionHandle.OnWriteStreamThreadTerminate(Sender: TObject) ;
begin
 FWriteStreamThread:= nil;
end;
 }

function TNMMConnectionHandle.TestConnectionObjects: Boolean;
begin
 try
   result:= ((FPeerThread = nil) or not PeerThread.Terminated) and
             (FConnection <> nil) and FConnection.Connected;
 except
   result:= false;
 end;  
end;

procedure TNMMConnectionHandle.SendIniData(AIniDataStream: TStream; AWidth, AHeight: Integer);
var LHeader: String;
begin
 inherited;
 FDeltasCount:= 0;
{
 if (FPeerThread = nil) or PeerThread.Terminated or
    (FPeerThread.Connection = nil) or
     not FPeerThread.Connection.Connected then
     }
 if not TestConnectionObjects then
 begin
   SetInvalid;
   Exit;
 end
 else if Overloaded then
 begin
   FConnection.WriteLn(cmdDisconnectSlowClient);
   SetInvalid;
   Exit;
 end;

 AIniDataStream.Seek(0, soFromBeginning);
 LHeader:= cmdIniData + ' ' +
           EncodeParam(prmWidth,IntToStr(AWidth)) +
           EncodeParam(prmHeight,IntToStr(AHeight)) +
           EncodeParam(prmImageSize,IntToStr(AIniDataStream.Size));

 FWriteStreamThread.AlertWriteStreamTask(AIniDataStream,LHeader);
 State:= State - [csNewConnection];
end;

procedure TNMMConnectionHandle.SendData(AData: TStream);
begin
 if not TestConnectionObjects then
 begin
   SetInvalid;
   Exit;
 end
 else if Overloaded and (FDeltasCount>0){Connection cannot cope with data} then
 begin
   FWriteStreamThread.Terminate; Sleep(15);
   FConnection.WriteLn(cmdDisconnectSlowClient); Sleep(15);
   FConnection.Disconnect;
   SetInvalid;
   Exit;
 end;
 inc(FDeltasCount);
 AData.Seek(0, soFromBeginning);
 FWriteStreamThread.AlertWriteStreamTask(AData,cmdData);
end;

procedure TNMMConnectionHandle.SendRawText(AText: String);
begin
 inherited;

 if not TestConnectionObjects then
 begin
   SetInvalid;
   Exit;
 end
 else if Overloaded and (FDeltasCount>0){Connection cannot cope with data} then
 begin
   FWriteStreamThread.Terminate; Sleep(15);
   FConnection.WriteLn(cmdDisconnectSlowClient); Sleep(15);
   FConnection.Disconnect;
   SetInvalid;
   Exit;
 end;
 FWriteStreamThread.AlertSendMsgTask(AText);
end;

function TNMMConnectionHandle.Busy: Boolean;
begin
 result:= false;
 try
   if FWriteStreamThread <> nil then
   begin
     result:=
       (ThreadListCount(FWriteStreamThread.FStreamCash) > 0) or
       (ThreadListCount(FWriteStreamThread.FMsgCash) > 0);
   end;
 except
   SetInvalid;
 end;
end;

function TNMMConnectionHandle.Overloaded: Boolean;
begin
 result:= false;
 try
   if FWriteStreamThread <> nil then
   begin
     result:=
       (ThreadListCount(FWriteStreamThread.FStreamCash) > MaxDeltasInCash) or
       (ThreadListCount(FWriteStreamThread.FMsgCash) > 1000) or
        FWriteStreamThread.Terminated;
   end;     
 except
   SetInvalid;
 end;
end;

procedure TNMMConnectionHandle.Disconnect;
begin
 FWriteStreamThread.Terminate;
 if not FWriteStreamThread.FreeOnTerminate then
 begin
   FreeAndNil(FWriteStreamThread);
 end;

 try
   if TestConnectionObjects then
   try
     if FConnection.Connected then
     begin
       FConnection.WriteLn(cmdDisconnect);
       Sleep(10);

       FConnection.Disconnect;
       Sleep(10);
     end;
   except
     //it's already disconnected 
   end;
 finally
   FPeerThread:= nil;
 end;
end;

end.
