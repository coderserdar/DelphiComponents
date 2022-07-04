(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at babikov@mail.ru

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit AudioClient;

interface
uses NMMCustomDataProcessors, NMMCustomClient,
     Classes, ExtCtrls,
     NMMCommon, Types
     ;

type
 TAudioClientThread = class(TNMMCustomClientThread)
 protected
   FStream: TStream;
   procedure DoHandleCommand(S: String); override;
   procedure Idle; override;
 public
   constructor Create(CreateSuspended: Boolean; AParent: TNMMCustomClient);
   destructor Destroy; override;
 end;

 TAudioClient = class(TNMMCustomClient)
 protected
   FAudioIn: TACSStreamedInput;
   FAudioOut: TACSOutput;
   FDataQueue: TNMMDataQueue;
   FStarted: Boolean;
   FStream: TStream;
   FAudioOutputDone: Boolean;
   function CreateThread(CreateSuspended: Boolean; AParent: TNMMCustomClient): TNMMCustomClientThread; override;
   procedure ClearData; override;
   procedure DoOnConnect; override;
   procedure DoOnDisconnect; override;
   procedure SetPeriod(AValue: Integer ); override;
   procedure AudioOutputDone(Sender: TComponent);
   procedure PlayAudio;
 public
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;
 published
 end;


implementation

uses NMMAudioDataPacket,  SysUtils;


{TAudioClient}
constructor TAudioClient.Create(AOwner: TComponent);
begin
 inherited;
 if (not (csDesigning in ComponentState)) and
    (not (csLoading in ComponentState)) then
 begin
   FDataQueue:= TNMMDataQueue.Create;
 end;
end;

destructor TAudioClient.Destroy;
begin
 if (not (csDesigning in ComponentState)) and
    (not (csLoading in ComponentState)) then
 begin
   DoDisconnect;
   FreeAndNil(FDataQueue);
 end;
 inherited;
end;


function TAudioClient.CreateThread(CreateSuspended: Boolean; AParent: TNMMCustomClient): TNMMCustomClientThread;
begin
 result:= TAudioClientThread.Create(CreateSuspended,AParent);
end;

procedure TAudioClient.ClearData;
begin
 inherited;
end;


{TAudioClientThread}
constructor TAudioClientThread.Create(CreateSuspended: Boolean; AParent: TNMMCustomClient);
begin
 inherited Create(CreateSuspended, AParent);
 FStream:= TMemoryStream.Create;
 (FParent as TAudioClient).FAudioOutputDone:= False;
end;

destructor TAudioClientThread.Destroy;
begin
 FreeAndNil(FStream);
 inherited;
end;

procedure TAudioClientThread.DoHandleCommand(S: String);
var LDataPacket: TNMMDataPacket;
begin
  if Pos(cmdData,S)=1 then
  begin
    ChangeStatus('Loading new frame');

    FStream.Size:= 0;
    ReadStream(S,cmdData,FStream);

    inc(FParent.Statistics.DeltasReceivedCount);
    FParent.Statistics.LastDeltaSize:= FStream.Size;
    inc(FParent.Statistics.DeltasProcessedCount);
    inc(FParent.Statistics.DeltaBytesReceived, FParent.Statistics.LastDeltaSize);

    LDataPacket:= TNMMAudioDataPacket.Create;
    LDataPacket.SetPackedData(FStream);
    (FParent as TAudioClient).FDataQueue.Add(LDataPacket);

    ChangeStatus('');
    FParent.LastDeltaTime:= Now;
  end;
end;

end.
