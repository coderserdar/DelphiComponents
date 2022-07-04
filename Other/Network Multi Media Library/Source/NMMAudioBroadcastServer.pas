(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit NMMAudioBroadcastServer;

interface

uses NMMCustomSimpleBroadcastServer,  NMMCustomAuthChecker,
     NMMCustomConnectionGroupThread,
     NMMAudioQueues, NMMACMThread, NMMAudioRecordThread,
     Classes, SysUtils;

type
  TNMMAudioBroadcastServer = class(TNMMCustomSimpleBroadcastServer)
  protected
    NMMAudioRecordThread: TNMMAudioRecordThread;
    //NMMAudioPlayThread: TNMMAudioPlayThread;
    EncodeThread: TNMMACMThread;
    FAudioRecordQueue: TNMMAudioRecordQueue;
    FCodedQueue: TNMMAudioQueue;
    function CreateAuthChecker: TNMMCustomAuthChecker; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CreateNewConnectionGroupThread(APeriod: Int64): TCustomConnectionGroupThread; override;
    procedure SetActive(AActive: Boolean); override;
  end;

implementation

uses NMMAudioGroupProcessor, NMMBasicAuthChecker,
     NMMAudioSettings;


constructor TNMMAudioBroadcastServer.Create(AOwner: TComponent);
begin
  inherited;
  FAudioRecordQueue:= TNMMAudioRecordQueue.Create;
  FCodedQueue:= TNMMAudioQueue.Create;
end;

destructor TNMMAudioBroadcastServer.Destroy;
begin
  FreeAndNil(FAudioRecordQueue);
  FreeAndNil(FCodedQueue);
  inherited;
end;

procedure TNMMAudioBroadcastServer.SetActive(AActive: Boolean);
begin
  if AActive then
  begin
//    SelectAudioInput.SetupWFX(FwfxIn);
    NMMAudioRecordThread:= TNMMAudioRecordThread.Create(true);
//    SelectAudioInput.SetupWFX(NMMAudioRecordThread.wfx);
    with NMMAudioRecordThread do
    begin
      DeviceID:= GInputDevice;
      DataQueue:= FAudioRecordQueue;
     // OnOutData:= OnDataPacketRecorded;
     // OnFinished:= RecordingFinished;
    end;
    NMMAudioRecordThread.Resume;

    //FillChar(FCodingInfo,SizeOf(FCodingInfo),0);
    with TNMMACMThread.Create(true) do
    try
      ThreadType:= attEncode;
      wfxIn:= @FwfxIn;
      wfxOut:= FWfxCode;
      InQueue:= FAudioRecordQueue;
      OutQueue:= FCodedQueue;
      //OnOutData:= OnDataPacketCoded;
      //OnFinished:= OnCodingFinished;
      Resume;
    except
      Free;
      raise;
    end;
  end
  else
  begin
    NMMAudioRecordThread.Stop;
    Sleep(50);
  end;
end;

function TNMMAudioBroadcastServer.CreateNewConnectionGroupThread(APeriod: Int64): TNMMCustomConnectionGroupThread;
begin
  result:= TNMMAudioGroupProcessor.Create;
end;

function TNMMAudioBroadcastServer.CreateAuthChecker: TNMMCustomAuthChecker;
begin
  result:= TNMMBasicAuthChecker.Create('','');
end;

end.
