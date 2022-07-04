(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at babikov@mail.ru

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit NMMP2PVoiceServer;

interface
uses  Classes, IdTCPServer,
      NMMCustomAuthChecker, NMMCustomConnectionProcessor, NMMCustomConnectionHandle,
      NMMUserData, NMMCustomServer, NMMVoiceBroadcastServer, NMMAudioDataPacket,
      NMMCustomAudioThread, NMMAudioPlayThread, NMMAudioQueues, NMMACMThread;

type
 TNMMP2PVoiceServerThread= class(TNMMServerThread)
 public
   ConnectionHandle: TNMMCustomConnectionHandle;
 end;

  TNMMP2PVoiceServer= class(TNMMVoiceBroadcastServer)
 protected
   FAudioPlayThread: TNMMAudioPlayThread;
   FAudioPlayQueue: TNMMAudioQueue;
   procedure OnTCPServerExecute(AThread: TIdPeerThread); override;
   procedure OnWaveDataAvailable(Sender: TNMMCustomAudioThread; DataPacket: TNMMAudioDataPacketInfo);
   procedure OnPlayingFinished(Sender: TObject);
 public
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;
   procedure SetActive(AActive: Boolean); override;
   function CreateConnectionHandle(AUserData: TNMMUserData; APeerThread: TIdPeerThread): TNMMCustomConnectionHandle; override;
   property ConnectionHandles;
 published
   property BeforeConnect;
   property OnDisconnect;
 end;


implementation

uses IdGlobal,
     NMMBasicAuthChecker, NMMCommon,
     NMMAudioGroupProcessor, NMMAudioConnectionHandle;


constructor TNMMP2PVoiceServer.Create(AOwner: TComponent);
begin
  inherited;
  FTCPServer.ThreadClass:= TNMMP2PVoiceServerThread;
  FAudioPlayQueue:= TNMMAudioQueue.Create;
end;

destructor TNMMP2PVoiceServer.Destroy;
begin
  inherited;
end;

function TNMMP2PVoiceServer.CreateConnectionHandle(AUserData: TNMMUserData; APeerThread: TIdPeerThread): TNMMCustomConnectionHandle;
begin
  result:= TNMMAudioConnectionHandle.Create(AUserData,APeerThread,self,
           FAudioDataParams.PWfxCode,FAudioDataParams.PWfxInOut);
  (APeerThread As TNMMP2PVoiceServerThread).ConnectionHandle:= result;
  (result As TNMMAudioConnectionHandle).OnWaveDataAvailable:= OnWaveDataAvailable;
end;

procedure TNMMP2PVoiceServer.OnTCPServerExecute(AThread: TIdPeerThread);
var
  s: String;
begin
  S:= AThread.Connection.ReadLn;
  if Pos(cmdData,S)>0 then
    (AThread As TNMMP2PVoiceServerThread).ConnectionHandle.PullData
  else
    inherited;
end;

procedure TNMMP2PVoiceServer.OnWaveDataAvailable(Sender: TNMMCustomAudioThread; DataPacket: TNMMAudioDataPacketInfo);
begin
  with (Sender As TNMMACMThread).OutQueue do
    while Count>0 do
    begin
      FAudioPlayQueue.Add(Get(0));
      Remove(0);
    end;
end;

procedure TNMMP2PVoiceServer.OnPlayingFinished(Sender: TObject);
begin
  FAudioPlayThread:= nil;
end;

procedure TNMMP2PVoiceServer.SetActive(AActive: Boolean);
begin
  if (not (csDesigning in ComponentState)) and
     (not (csLoading in ComponentState)) and
     (AActive<>Active) then
  begin
    try
      if AActive then
      begin
        FAudioPlayQueue.Finished:= false;
        FAudioPlayQueue.Clear;
        FAudioPlayThread:= TNMMAudioPlayThread.Create(true);
        with FAudioPlayThread do
        try
          pwfx:= FAudioDataParams.PWfxInOut;
          DeviceID:= FInOutDevice;
          DataQueue:= FAudioPlayQueue;
          OnFinished:= OnPlayingFinished;
          Resume;
        except
          Free;
          raise;
        end;
      end
      else
      begin
        FAudioPlayQueue.Finished:= true;
      end;
    finally
      inherited;
    end;
  end;
end;


end.

