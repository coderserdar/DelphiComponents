(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at babikov@mail.ru

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit NMMVoiceClient;

interface

uses
  SysUtils, Classes,
  NMMCustomClient, NMMVoiceInClient, NMMAudioRecordThread, NMMACMThread,
  NMMAudioQueues, NMMAudioConnectionHandle, NMMCustomAudioThread,
  NMMAudioDataPacket;

type
  TNMMVoiceClient = class(TNMMVoiceInClient)
  protected
    FAudioRecordThread: TNMMAudioRecordThread;
    FCodeThread: TNMMACMThread;
    FAudioRecordQueue: TNMMAudioRecordQueue;
    FCodedOutQueue: TNMMAudioQueue;
    FConnectionHandle: TNMMAudioConnectionHandle;
    FOutStream: TMemoryStream;
    procedure OnRecordingFinished(Sender: TObject);
    procedure OnCodingFinished(Sender: TObject);
    procedure ClearData; override;
    procedure Play; override;
    procedure DoOnDisconnect; override;
    procedure OnDataPacketCoded(Sender: TNMMCustomAudioThread; ADataPacket: TNMMAudioDataPacketInfo);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses NMMUserData, NMMCommon;

constructor TNMMVoiceClient.Create(AOwner: TComponent);
begin
  inherited;
  FAudioRecordQueue:= TNMMAudioRecordQueue.Create;
  FCodedOutQueue:= TNMMAudioQueue.Create;
  FOutStream:= TMemoryStream.Create;
end;

destructor TNMMVoiceClient.Destroy;
begin
  FreeAndNil(FOutStream);
  inherited;
  FreeAndNil(FCodedOutQueue);
  FreeAndNil(FAudioRecordQueue);
end;

procedure TNMMVoiceClient.Play;
begin
  inherited;
  FAudioRecordQueue.Clear;
  FCodedOutQueue.Clear;

  FAudioRecordThread:= TNMMAudioRecordThread.Create(true);
  with FAudioRecordThread, FAudioDataParams do
  begin
    DeviceID:= FInOutDevice;
    DataQueue:= FAudioRecordQueue;
    pwfx:= PWfxInOut;
    OnFinished:= OnRecordingFinished;
  end;
  FAudioRecordThread.Resume;
  Sleep(3);

  FCodeThread:= TNMMACMThread.Create(true);
  with FCodeThread, FAudioDataParams do
  try
    ThreadType:= attEncode;
    wfxIn:= PWfxInOut;
    wfxOut:= PWfxCode;
    InQueue:= FAudioRecordQueue;
    OutQueue:= FCodedOutQueue;
    OnOutData:= OnDataPacketCoded;
    OnFinished:= OnCodingFinished;
    Resume;
  except
    Free;
    raise;
  end;

  FConnectionHandle:= TNMMAudioConnectionHandle.Create(TNMMUserData.Create,nil,nil,nil,nil,FTCPClient);
end;

procedure TNMMVoiceClient.DoOnDisconnect;
begin
  if Assigned(FAudioRecordThread) then
    FAudioRecordThread.Stop;
  Sleep(10);
  inherited;
end;

procedure TNMMVoiceClient.ClearData;
begin
  if Assigned(FCodedOutQueue) then
    FCodedOutQueue.Finished:= true;
  if Assigned(FAudioRecordQueue) then
    FAudioRecordQueue.Finished:= true;
  inherited;
end;

procedure TNMMVoiceClient.OnDataPacketCoded(Sender: TNMMCustomAudioThread; ADataPacket: TNMMAudioDataPacketInfo);
begin
  with (Sender As TNMMACMThread).OutQueue do
    while Count>0 do
    begin
      Get(0).GetPackedData(FOutStream);
      FConnectionHandle.SendData(FOutStream);
      Remove(0);
    end;
end;

procedure TNMMVoiceClient.OnRecordingFinished(Sender: TObject);
begin
  FAudioRecordThread:= nil;
end;

procedure TNMMVoiceClient.OnCodingFinished(Sender: TObject);
begin
  FCodeThread:= nil;
end;


end.
