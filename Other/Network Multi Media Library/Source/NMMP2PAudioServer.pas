(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at babikov@mail.ru

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit NMMP2PAudioServer;

interface
uses  Classes, IdTCPServer,
      NMMCustomAuthChecker, NMMCustomConnectionProcessor, NMMCustomConnectionHandle,
      NMMUserData, NMMCustomServer, NMMP2PServer, NMMAudioDataPacket, NMMCustomAudioThread;

type
 TNMMP2PVoiceServerThread= class(TNMMServerThread)
 public
   ConnectionHandle: TNMMCustomConnectionHandle;
 end;

 TNMMP2PVoiceServer= class(TNMMP2PServer)
 protected
   function CreateAuthChecker: TNMMCustomAuthChecker; override;
   function CreateConnectionProcessor(AUserData: TNMMUserData): TNMMCustomConnectionProcessor; override;
   function CreateConnectionHandle(AUserData: TNMMUserData; APeerThread: TIdPeerThread): TNMMCustomConnectionHandle; override;
   procedure OnTCPServerExecute(AThread: TIdPeerThread); override;
   procedure OnWaveDataAvailable(Sender: TNMMCustomAudioThread; DataPacket: TNMMAudioDataPacketInfo);
 public
   constructor Create(AOwner: TComponent); override;
 end;


implementation

uses IdGlobal, 
     NMMBasicAuthChecker, NMMCommon,
     NMMAudioGroupProcessor, NMMAudioConnectionHandle;


constructor TNMMP2PVoiceServer.Create(AOwner: TComponent);
begin
  inherited;
  FTCPServer.ThreadClass:= TNMMP2PVoiceServerThread;
end;

function TNMMP2PVoiceServer.CreateAuthChecker: TNMMCustomAuthChecker;
begin
  result:= TNMMBasicAuthChecker.Create;
end;

function TNMMP2PVoiceServer.CreateConnectionProcessor(AUserData: TNMMUserData): TNMMCustomConnectionProcessor;
begin
  result:= TNMMAudioGroupProcessor.Create;
end;

function TNMMP2PVoiceServer.CreateConnectionHandle(AUserData: TNMMUserData; APeerThread: TIdPeerThread): TNMMCustomConnectionHandle;
begin
  result:= TNMMAudioConnectionHandle.Create(AUserData,APeerThread,self);
  (APeerThread As TNMMP2PVoiceServerThread).ConnectionHandle:= result;
  (result As TNMMAudioConnectionHandle).OnWaveDataAvailable:= OnWaveDataAvailable;
end;

procedure TNMMP2PVoiceServer.OnTCPServerExecute(AThread: TIdPeerThread);
var
  s: String;
begin
  S:= AThread.Connection.ReadLn;
  if Pos(cmdData,S)>0 then
  begin
    (AThread As TNMMP2PVoiceServerThread).ConnectionHandle.PullData;
  end;
end;

procedure TNMMP2PVoiceServer.OnWaveDataAvailable(Sender: TNMMCustomAudioThread; DataPacket: TNMMAudioDataPacketInfo);
begin
end;

end.

