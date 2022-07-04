(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at babikov@mail.ru

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit NMMAudioConnectionHandle;

interface

uses NMMACMThread, NMMAudioQueues, NMMCustomConnectionHandle, NMMConnectionHandle,
     NMMUserData, NMMCustomServer, NMMAudioDataPacket, NMMCustomAudioThread,
     NMMVoiceBroadcastServer,
     IdTCPServer, Classes, SysUtils, IdTCPConnection, MMSystem;

type
  TNMMAudioConnectionHandle = class(TNMMConnectionHandle)
  protected
    FInEncQueue: TNMMAudioQueue;
    FInWaveQueue: TNMMAudioQueue;
  //  FOutWaveQueue: TNMMAudioQueue;
  //  FOutEncQueue: TNMMAudioQueue;
    FInDecThread: TNMMACMThread;
    FOnWaveDataAvailable: TOnDataEvent;
  //  FOutCodeThread: TACMThread;
    FPWfxIn: PWAVEFORMATEX;
    FPWfxOut: PWAVEFORMATEX;

    procedure OnEncodingFinished(Sender: TObject);
    procedure SetOnWaveDataAvailable(AOnWaveDataAvailable: TOnDataEvent);
  public
    constructor Create(AUserData: TNMMUserData; APeerThread:
                TIdPeerThread; AServer: TNMMCustomServer;
                APWfxIn, APWfxOut: PWAVEFORMATEX;
                AConnection: TIdTCPConnection = nil);
    destructor Destroy; override;
    procedure ReceiveData(AData: TStream); override;

    property InEncQueue: TNMMAudioQueue read FInEncQueue write FInEncQueue;
    property InWaveQueue: TNMMAudioQueue read FInWaveQueue write FInWaveQueue;
 //   property FOutWaveQueue: TNMMAudioQueue read FOutWaveQueue write FOutWaveQueue;
 //   property FOutEncQueue: TNMMAudioQueue read FOutEncQueue write FOutEncQueue;
    property OnWaveDataAvailable: TOnDataEvent
             read FOnWaveDataAvailable write SetOnWaveDataAvailable;
  end;

implementation

uses NMMCommon;


constructor TNMMAudioConnectionHandle.Create(AUserData: TNMMUserData;
            APeerThread: TIdPeerThread; AServer: TNMMCustomServer;
            APWfxIn, APWfxOut: PWAVEFORMATEX;
            AConnection: TIdTCPConnection = nil);
begin
  inherited Create(AUserData, APeerThread, AServer, AConnection);
  FInEncQueue:= TNMMAudioQueue.Create;
  FInWaveQueue:= TNMMAudioQueue.Create;
//  FOutWaveQueue:= TNMMAudioQueue.Create;
//  FOutEncQueue:= TNMMAudioQueue.Create;

  if (AServer<>nil) and (APWfxIn<>nil) and (APWfxOut<>nil) then
  begin
    FInDecThread:= TNMMACMThread.Create(true);
    FInDecThread.ThreadType:= attDecode;
    FInDecThread.wfxIn:= APWfxIn;
    FInDecThread.wfxOut:= APWfxOut;
    FInDecThread.InQueue:= FInEncQueue;
    FInDecThread.OutQueue:= FInWaveQueue;
    FInDecThread.OnFinished:= OnEncodingFinished;
    FInDecThread.Resume;
  end;
end;

destructor TNMMAudioConnectionHandle.Destroy;
var i: Integer;
begin
  FInEncQueue.Finished:= true;
  FInWaveQueue.Finished:= true;
  if Assigned(FInDecThread) then
    for i:=0 to 100 do
    begin
      if FInDecThread=nil then
         break;
      Sleep(10);
    end;
  FreeAndNil(FInEncQueue);
  FreeAndNil(FInWaveQueue);
//  FreeAndNil(FOutWaveQueue);
//  FreeAndNil(FOutEncQueue);
  inherited;
end;

procedure TNMMAudioConnectionHandle.SetOnWaveDataAvailable(AOnWaveDataAvailable: TOnDataEvent);
begin
  FOnWaveDataAvailable:= AOnWaveDataAvailable;
  FInDecThread.OnOutData:= FOnWaveDataAvailable;
end;

procedure TNMMAudioConnectionHandle.OnEncodingFinished(Sender: TObject);
begin
  FInDecThread:= nil;
end;

procedure TNMMAudioConnectionHandle.ReceiveData(AData: TStream);
var LNMMAudioDataPacket: TNMMAudioDataPacket;
begin
  LNMMAudioDataPacket:= TNMMAudioDataPacket.Create;
  LNMMAudioDataPacket.SetPackedData(AData);
  FInEncQueue.Add(LNMMAudioDataPacket);
end;

end.
