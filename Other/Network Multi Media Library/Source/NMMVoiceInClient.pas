(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at babikov@mail.ru

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit NMMVoiceInClient;

interface
uses NMMCustomDataProcessors, NMMCustomClient,
     Classes, ExtCtrls, Controls,
     NMMCommon, Types,
     MsACM, MMSystem,
     NMMACMThread, NMMAudioPlayThread, NMMAudioQueues, NMMAudioCommon;

type
 TNMMVoiceInClientThread = class(TNMMCustomClientThread)
 protected
   FStream: TStream;
   procedure DoHandleCommand(S: String); override;
 public
   constructor Create(CreateSuspended: Boolean; AParent: TNMMCustomClient);
   destructor Destroy; override;
 end;

 TNMMVoiceInClient = class(TNMMCustomClient)
 protected
   FAudioPlayThread: TNMMAudioPlayThread;
   FDecodeThread: TNMMACMThread;
   FAudioDataParams: TAudioDataParams;
   FInOutDevice: Integer;
   FCodedInQueue: TNMMAudioQueue;
   FAudioPlayQueue: TNMMAudioQueue;
   FPlaying: Boolean;
   FDataExchangeStarted: Boolean;
   function CreateThread(CreateSuspended: Boolean; AParent: TNMMCustomClient): TNMMCustomClientThread; override;
   procedure ClearData; override;
   procedure Play; virtual;
   procedure OnPlayingFinished(Sender: TObject);
   procedure OnDecodingFinished(Sender: TObject);
 public
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;
   procedure LoadSettings(AFileName: String);
   procedure SaveSettings(AFileName: String);
   function RunAudioSetupDlg: TModalResult;
   procedure Connect; override;
   procedure Disconnect; override;

   property AudioDataParams: TAudioDataParams read FAudioDataParams write FAudioDataParams;
   property InOutDevice: Integer read FInOutDevice write FInOutDevice;
   property Playing: Boolean read FPlaying;
   property DataExchangeStarted: Boolean read FDataExchangeStarted;
 published
 end;


implementation

uses NMMAudioDataPacket, DateUtils, NMMAudioSetup, SysUtils;


{TNMMVoiceInClient}
constructor TNMMVoiceInClient.Create(AOwner: TComponent);
begin
 inherited;
 if (not (csDesigning in ComponentState)) and
    (not (csLoading in ComponentState)) then
 begin
   FInOutDevice:= WAVE_MAPPER;
   FAudioDataParams:= TAudioDataParams.Create;
   FAudioPlayQueue:= TNMMAudioQueue.Create;
   FCodedInQueue:= TNMMAudioQueue.Create;
 end;
 FReduceLoadOnDisconnect:= false;
end;

destructor TNMMVoiceInClient.Destroy;
begin
 if (not (csDesigning in ComponentState)) and
    (not (csLoading in ComponentState)) then
 begin
   FOnStatusChanged:= nil;
   FOnConnected:= nil;
   FOnDisconnected:= nil;
   FOnConnectionTeminated:= nil;
   FPeriodChangedEvent:= nil;
   FOnConnectionRefused:= nil;
   FOnConnectionTooSlow:= nil;
   FPeriodChangedEvent:= nil;
   FOnConnectionTeminated:= nil;
   DoDisconnect;
   FreeAndNil(FAudioPlayQueue);
   FreeAndNil(FCodedInQueue);
   FreeAndNil(FAudioDataParams);
 end;
 inherited;
end;

function TNMMVoiceInClient.CreateThread(CreateSuspended: Boolean; AParent: TNMMCustomClient): TNMMCustomClientThread;
begin
 result:= TNMMVoiceInClientThread.Create(CreateSuspended,AParent);
end;

procedure TNMMVoiceInClient.ClearData;
begin
  FAudioPlayQueue.Finished:= true;
  FCodedInQueue.Finished:= true;
  inherited;
end;

procedure TNMMVoiceInClient.LoadSettings(AFileName: String);
var LFS: TFileStream;
begin
  if FileExists(AFileName) then
  begin
    LFS:= TFileStream.Create(AFileName,fmOpenRead);
    try
      LFS.ReadBuffer(FInOutDevice,SizeOf(FInOutDevice));
    finally
      FreeAndNil(LFS);
    end;
  end;
end;

procedure TNMMVoiceInClient.SaveSettings(AFileName: String);
var LFS: TFileStream;
begin
  if FileExists(AFileName) then
     DeleteFile(AFileName);

  LFS:= TFileStream.Create(AFileName,fmCreate);
  try
    LFS.WriteBuffer(FInOutDevice,SizeOf(FInOutDevice));
  finally
    FreeAndNil(LFS);
  end;
end;

function TNMMVoiceInClient.RunAudioSetupDlg: TModalResult;
var
  frmAudioSettings: TfrmAudioSettings;
begin
  Result:= mrNone;
  if (not (csDesigning in ComponentState)) and
     (not (csLoading in ComponentState)) then
  begin
    with FAudioDataParams do
    begin
      if WfxCodeMaxSize<>0 then // was loaded
         FreeMem(PWfxCode,WfxCodeMaxSize);
      PWfxCode:= nil;

      WfxCodeMaxSize:= sizeof(TWAVEFORMATEX);
      acmMetrics(nil, ACM_METRIC_MAX_SIZE_FORMAT, WfxCodeMaxSize);
      GetMem(PWfxCode,WfxCodeMaxSize);
      FillChar(PWfxCode^, WfxCodeMaxSize, 0);
      PWfxCode.cbSize:= 0;
      
      frmAudioSettings:= TfrmAudioSettings.Create(nil);
      try
        frmAudioSettings.SetWfxCode(PWfxCode,WfxCodeMaxSize);
        frmAudioSettings.WfxIn:= PWfxInOut;
        Result:= frmAudioSettings.ShowModal;
      finally
        FreeAndNil(frmAudioSettings);
      end;
    end;
  end;
end;

procedure TNMMVoiceInClient.OnPlayingFinished;
begin
  FAudioPlayThread:= nil;
end;

procedure TNMMVoiceInClient.OnDecodingFinished;
begin
  FDecodeThread:= nil;
end;

procedure TNMMVoiceInClient.Play;
begin
  if not FPlaying then
  begin
    FCodedInQueue.Finished:= false;
    FAudioPlayQueue.Finished:= false;
    FDecodeThread:= TNMMACMThread.Create(true);
    with FDecodeThread do
    try
      ThreadType:= attDecode;
      wfxIn:= FAudioDataParams.PWfxCode;
      wfxOut:= FAudioDataParams.PWfxInOut;
      InQueue:= FCodedInQueue;
      OutQueue:= FAudioPlayQueue;
      OnFinished:= OnDecodingFinished;
      InitialDelay:= GBlockSize*1000 div FAudioDataParams.PWfxInOut^.nAvgBytesPerSec;
      Resume;
    except
      Free;
      raise;
    end;

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
  end;
  FPlaying:= true;
end;

procedure TNMMVoiceInClient.Connect;
begin
  FCodedInQueue.Clear;
  FAudioPlayQueue.Clear;
  FPlaying:= false;
  inherited;
  FDataExchangeStarted:= false;
end;

procedure TNMMVoiceInClient.Disconnect;
begin
  inherited;
  FDataExchangeStarted:= false;
end;

{TNMMVoiceInClientThread}
constructor TNMMVoiceInClientThread.Create(CreateSuspended: Boolean; AParent: TNMMCustomClient);
begin
 inherited Create(CreateSuspended, AParent);
 FStream:= TMemoryStream.Create;
end;

destructor TNMMVoiceInClientThread.Destroy;
begin
 FreeAndNil(FStream);
 inherited;
end;

procedure TNMMVoiceInClientThread.DoHandleCommand(S: String);
var LDataPacket: TNMMDataPacket;
    hStream: HACMSTREAM;
    LError: String;
  function CheckCodec(ACodecName: String): Boolean;
  var LSL: TStringList;
      i: Integer;
  begin
    result:= false;
    LSL:= TStringList.Create;
    try
      GetCodecs(LSL);
      for i:=0 to LSL.Count-1 do
      begin
        if Pos(ACodecName,LSL[i])<>0 then
           result:= true;
      end;
    finally
      LSL.Free;
    end;
  end;
begin
  inherited;
  (FParent as TNMMVoiceInClient).FDataExchangeStarted:= true;

  if Pos(cmdIniData,S)=1 then
  begin
    ChangeStatus('Loading voice settings');
    FStream.Size:= 0;
    ReadStream(S,cmdIniData,FStream);
    with FParent as TNMMVoiceInClient do
    begin
      FAudioDataParams.LoadFromStream(FStream);
      try
        acmCheck(acmStreamOpen(hStream, nil,
                               FAudioDataParams.PWfxCode^, FAudioDataParams.PWfxInOut^,
                               nil, 0, 0, ACM_STREAMOPENF_QUERY));
      except
        on E: Exception do
        begin
          LError:= E.Message + ' for the codec "' +
                   StrPas(FAudioDataParams.szFormatTag) + '" in "' +
                   StrPas(FAudioDataParams.szFormat) + '" format.';
          if not CheckCodec(StrPas(FAudioDataParams.szFormat)) then
             LError:= LError + ' This codec in not present in your system.';
          raise NMMExceptionForUser.Create(LError);
        end;
      end;
    end;
  end;

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
    with FParent as TNMMVoiceInClient do
    begin
      FCodedInQueue.Add(LDataPacket);
      if not FPlaying then
         Play;
    end;
    ChangeStatus('');
    FParent.LastDeltaTime:= Now;
  end;
end;


end.
