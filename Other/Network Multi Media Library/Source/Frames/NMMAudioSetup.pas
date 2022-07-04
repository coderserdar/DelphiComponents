(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at babikov@mail.ru

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit NMMAudioSetup;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  frSelectAudioInput, NMMAudioRecordThread, NMMAudioPlayThread,
  NMMAudioQueues, frSelectAudio, NMMACMThread, NMMAudioDataPacket,
  NMMAudioCommon, NMMCustomAudioThread, NMMAudioSettings,
  Menus, Spin, SyncObjs, MsACM, MMSystem, Buttons, ComCtrls;

type
  TProcessingInfo = record
    LastPacketTime, PrevPacketTime, Finished, Started: TDateTime;
    MaxPacket: Integer;
    Bytes: Longint;
  end;

  TfrmAudioSettings = class(TForm)
    MainMenu: TMainMenu;
    mnuOptions: TMenuItem;
    mnuViewAllCodecs: TMenuItem;
    PageControl: TPageControl;
    tsSettings: TTabSheet;
    tsTests: TTabSheet;
    gbCodec: TGroupBox;
    Label3: TLabel;
    memCodecInfo: TMemo;
    bChooseFormat: TButton;
    cbSameBits: TCheckBox;
    cbSameFrequency: TCheckBox;
    gbTestAudio: TGroupBox;
    bStartRec: TButton;
    bStopRec: TButton;
    bStartPlay: TButton;
    cbCodec: TGroupBox;
    Label2: TLabel;
    Label1: TLabel;
    bStartTest: TButton;
    bFinishTest: TButton;
    edIniDelay: TSpinEdit;
    memResults: TMemo;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    SelectAudioDevice: TSelectAudioInput;
    procedure bStartRecClick(Sender: TObject);
    procedure bStopRecClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure bStartPlayClick(Sender: TObject);
    procedure bStartTestClick(Sender: TObject);
    procedure bChooseFormatClick(Sender: TObject);
    procedure mnuViewAllCodecsClick(Sender: TObject);
    procedure bFinishTestClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FAudioRecordQueue: TNMMAudioRecordQueue;
    FCodedQueue, FDecodedQueue: TNMMAudioQueue;
    FLastPacketTime, FPrevPacketTime, FRecordingStarted: TDateTime;
    FByteRecorded, FDeltaTime, FRecordLen: Longint;
    FCodingInfo, FDecodingInfo: TProcessingInfo;
    FCS: TCriticalSection;

    FPwfxIn: PWAVEFORMATEX;
    FPAfc: PACMFORMATCHOOSE;

    procedure OnPlayingFinished(Sender: TObject);
  public
    { Public declarations }
    NMMAudioRecordThread: TNMMAudioRecordThread;
    NMMAudioPlayThread: TNMMAudioPlayThread;
    EncodeThread: TNMMACMThread;
    procedure InitAFC;
    procedure UpdateCodecInfo;
    procedure OnDataPacketRecorded(Sender: TNMMCustomAudioThread; ADataPacket: TNMMAudioDataPacketInfo);
    procedure RecordingFinished(Sender: TObject);
    procedure OnDataPacketCoded(Sender: TNMMCustomAudioThread; ADataPacket: TNMMAudioDataPacketInfo);
    procedure OnCodingFinished(Sender: TObject);
    procedure OnDataPacketDecoded(Sender: TNMMCustomAudioThread; ADataPacket: TNMMAudioDataPacketInfo);
    procedure OnDecodingFinished(Sender: TObject);
    procedure SetWfxCode(APWfxCode: PWAVEFORMATEX; AAfcMaxSize: Integer);
    procedure GetWfxCode(var APWfxCode: PWAVEFORMATEX; var AAfcMaxSize: Integer);

    property WfxIn: PWAVEFORMATEX read FPwfxIn write FPwfxIn;
    property PAfc: PACMFORMATCHOOSE read FPAfc write FPAfc;
  end;


implementation

uses CodecsInfo, DateUtils, Math;



{$R *.dfm}

procedure TfrmAudioSettings.FormCreate(Sender: TObject);
begin
  LongTimeFormat:= 'hh:nn:ss:zzz';
  FAudioRecordQueue:= TNMMAudioRecordQueue.Create;
  FCodedQueue:= TNMMAudioQueue.Create;
  FDecodedQueue:= TNMMAudioQueue.Create;

//  InitAFC;
  FCS:= TCriticalSection.Create;
//  Load;
  PageControl.ActivePage:= tsSettings;
end;

procedure TfrmAudioSettings.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FAudioRecordQueue);
  FreeAndNil(FCodedQueue);
  FreeAndNil(FDecodedQueue);
  FreeAndNil(FCS);
end;

procedure TfrmAudioSettings.bStartRecClick(Sender: TObject);
begin
  memResults.Clear;
  FDeltaTime:= 0;
  FRecordLen:= 0;
  FRecordingStarted:= 0;
  FByteRecorded:= 0;
  FLastPacketTime:= 0;
  FPrevPacketTime:= 0;

  bStartRec.Enabled:= false;
  bStartTest.Enabled:= false;
  bStopRec.Enabled:= true;
  bFinishTest.Enabled:= true;

  NMMAudioRecordThread:= TNMMAudioRecordThread.Create(true);
  SelectAudioDevice.SetupWfx(FPwfxIn^);
  NMMAudioRecordThread.pwfx:= FPwfxIn;
  with SelectAudioDevice,NMMAudioRecordThread do
  begin
    DeviceID:= GetDeviceID;
    DataQueue:= FAudioRecordQueue;
    OnOutData:= OnDataPacketRecorded;
    OnFinished:= RecordingFinished;
  end;

  NMMAudioRecordThread.Resume;
end;

procedure TfrmAudioSettings.bStopRecClick(Sender: TObject);
begin
  NMMAudioRecordThread.Stop;
  Sleep(50);
  bStartRec.Enabled:= true;
  bStartTest.Enabled:= true;
  bStopRec.Enabled:= false;
  bFinishTest.Enabled:= false;
end;

procedure TfrmAudioSettings.bStartPlayClick(Sender: TObject);
begin
  bStartPlay.Enabled:= false;
  NMMAudioPlayThread:= TNMMAudioPlayThread.Create(true);
  NMMAudioPlayThread.pwfx:= FPwfxIn;
  SelectAudioDevice.SetupWFX(NMMAudioPlayThread.pwfx^);
  NMMAudioPlayThread.DeviceID:= SelectAudioDevice.GetDeviceID;
  NMMAudioPlayThread.DataQueue:= FAudioRecordQueue;
  NMMAudioPlayThread.OnFinished:= OnPlayingFinished;
  NMMAudioPlayThread.Resume;
//  NMMAudioPlayThread.OnTerminate:= OnPlayingFinished;
//  AudioOutputParams.RunTimeInit(,NMMAudioPlayThread.wfx);
end;

procedure TfrmAudioSettings.OnPlayingFinished(Sender: TObject);
begin
  bStartPlay.Enabled:= true;
end;

procedure TfrmAudioSettings.bStartTestClick(Sender: TObject);
begin
//  memResults.Clear;
  SelectAudioDevice.SetupWFX(FPwfxIn^);

  bStartRecClick(nil);

  Sleep(edIniDelay.Value);

  FillChar(FCodingInfo,SizeOf(FCodingInfo),0);
  with TNMMACMThread.Create(true) do
  try
    ThreadType:= attEncode;
    wfxIn:= FPwfxIn;
    wfxOut:= FPAfc^.pwfx;
    InQueue:= FAudioRecordQueue;
    OutQueue:= FCodedQueue;
    OnOutData:= OnDataPacketCoded;
    OnFinished:= OnCodingFinished;
    Resume;
  except
    Free;
    raise;
  end;

  FillChar(FDecodingInfo,SizeOf(FCodingInfo),0);
  with TNMMACMThread.Create(true) do
  try
    ThreadType:= attDecode;
    wfxIn:= FPAfc^.pwfx;
    wfxOut:= FPwfxIn;
    InQueue:= FCodedQueue;
    OutQueue:= FDecodedQueue;
    OnOutData:= OnDataPacketDecoded;
    OnFinished:= OnDecodingFinished;
    Resume;
  except
    Free;
    raise;
  end;

  with TNMMAudioPlayThread.Create(true) do
  try
    pwfx:= FPwfxIn;
    DeviceID:= SelectAudioDevice.GetDeviceID;
    DataQueue:= FDecodedQueue;
    Resume;
  except
    Free;
    raise;
  end;

end;

procedure TfrmAudioSettings.bChooseFormatClick(Sender: TObject);
var
  LAcmRes: Integer;
  hStream: HACMSTREAM;
begin
  SelectAudioDevice.SetupWFX(FPwfxIn^);
  try
    if FPAfc^.cbwfx<>0 then // was loaded
       FreeMem(FPAfc^.pwfx,FPAfc^.cbwfx);
    FPAfc^.pwfx:= nil;
    FPAfc^.cbwfx:= sizeof(TWAVEFORMATEX);
    acmMetrics(nil, ACM_METRIC_MAX_SIZE_FORMAT, FPAfc^.cbwfx);
    GetMem(FPAfc^.pwfx,FPAfc^.cbwfx);
    FillChar(FPAfc^.pwfx^, FPAfc^.cbwfx, 0);
    FPAfc^.pwfx^.cbSize:= 0;

    InitAFC;
    LAcmRes:= acmFormatChoose(FPAfc^);
    if MMSYSERR_NOERROR = LAcmRes then
    begin
      UpdateCodecInfo;
      hStream:= nil;
      acmCheck(acmStreamOpen(hStream, nil, FPwfxIn^, FPAfc^.pwfx^, nil, 0,
               0, ACM_STREAMOPENF_QUERY));
    end
    else
      AcmCheck(LAcmRes);
  except
    on E: Exception do
       ShowMessage(E.Message);
  end;

end;

procedure TfrmAudioSettings.UpdateCodecInfo;
begin
  with memCodecInfo.Lines do
  begin
    Clear;
    Add('Format: '+FPAfc^.szFormatTag);
    Add('Format details: '+FPAfc^.szFormat);
  end;
end;

procedure TfrmAudioSettings.mnuViewAllCodecsClick(Sender: TObject);
begin
  with TfrmCodecsInfo.Create(self) do
  begin
    try
      ShowModal;
    finally
      Free;
    end;    
  end;
end;

procedure TfrmAudioSettings.SetWfxCode(APWfxCode: PWAVEFORMATEX; AAfcMaxSize: Integer);
begin
  FPAfc^.pwfx:= APWfxCode;
  FPAfc^.cbwfx:= AAfcMaxSize;
end;

procedure TfrmAudioSettings.GetWfxCode(var APWfxCode: PWAVEFORMATEX; var AAfcMaxSize: Integer);
begin
  APWfxCode:= FPAfc^.pwfx;
  AAfcMaxSize:= FPAfc^.cbwfx;
end;

procedure TfrmAudioSettings.InitAFC;
begin
  FPAfc^.cbStruct:= sizeof(FPAfc^);
  FPAfc^.hwndOwner:= Handle;
  FPAfc^.cbStruct:= sizeof(FPAfc^);
  FPAfc^.fdwEnum:= ACM_FORMATENUMF_NCHANNELS
                 {or ACMFORMATCHOOSE_STYLEF_INITTOWFXSTRUCT};

  if cbSameBits.Checked then
     FPAfc^.fdwEnum:= FPAfc^.fdwEnum or ACM_FORMATENUMF_WBITSPERSAMPLE;
  if cbSameFrequency.Checked then
     FPAfc^.fdwEnum:= FPAfc^.fdwEnum or ACM_FORMATENUMF_NSAMPLESPERSEC;

  FPAfc^.pwfxEnum:= FPwfxIn;
  FPAfc^.pszTitle:= 'Choose a format';
end;

procedure TfrmAudioSettings.OnDataPacketRecorded(Sender: TNMMCustomAudioThread; ADataPacket: TNMMAudioDataPacketInfo);
begin
  FLastPacketTime:= Now;
  if FRecordingStarted=0 then FRecordingStarted:= FLastPacketTime;
  inc(FByteRecorded,ADataPacket.Size);
  if (FPrevPacketTime<>0) and (FDeltaTime=0) then
    FDeltaTime:= MillisecondsBetween(FPrevPacketTime,FLastPacketTime);
  FPrevPacketTime:= FLastPacketTime;
end;

procedure TfrmAudioSettings.RecordingFinished(Sender: TObject);
var LFinished: TDateTime;
begin
  LFinished:= Now;
  FRecordLen:= (MillisecondsBetween(FRecordingStarted,LFinished));
  FCS.Enter;
  try
    with memResults.Lines do
    begin
      Add('------- Audio data recording -------');
      Add(' Started at: '+TimeToStr(FRecordingStarted));
      Add(' Finished at: '+TimeToStr(LFinished));
      Add(' Time spent: '+TimeToStr(LFinished-FRecordingStarted));
      Add(' Bytes recorded: '+IntToStr(FByteRecorded));
      if FRecordLen<>0 then
        Add(' Avg bits per sec: ' +
              IntToStr( FByteRecorded div FRecordLen*8*1000  ));
      Add(' Data packet length (msecs): ' + IntToStr(FDeltaTime) );
      Add(' Data packet size (bytes): ' + IntToStr(GBlockSize) );
      Add('');
    end;
  finally
    FCS.Leave;
  end;
end;

procedure TfrmAudioSettings.OnDataPacketCoded(Sender: TNMMCustomAudioThread; ADataPacket: TNMMAudioDataPacketInfo);
begin
  with FCodingInfo do
  begin
    LastPacketTime:= Now;
    if Started=0 then Started:= LastPacketTime;
    MaxPacket:= Max(ADataPacket.Size,MaxPacket);
    PrevPacketTime:= LastPacketTime;
    inc(Bytes,ADataPacket.Size);
  end;
end;

procedure TfrmAudioSettings.OnCodingFinished(Sender: TObject);
begin
  Sleep(100);
  FCS.Enter;
  try
    with FCodingInfo, memResults.Lines do
    begin
      if Started<>0 then
      begin
        Finished:= Now;
        Add('------- Audio data coding -------');
        Add(' Started at: '+TimeToStr(Started));
        Add(' Finished at: '+TimeToStr(Finished));
        Add(' Time spent: '+TimeToStr(Finished-Started));
        Add(' Bytes coded: '+IntToStr(Bytes));
        if FRecordLen<>0 then
          Add(' Avg bits per sec: ' + IntToStr( Bytes div FRecordLen*8*1000 ));
        if FDeltaTime<>0 then
          Add(' Max bits per sec: ' + IntToStr( MaxPacket*8*1000 div FDeltaTime) );
        Add('');
      end;
    end;
  finally
    FCS.Leave;
  end;
end;

procedure TfrmAudioSettings.OnDataPacketDecoded(Sender: TNMMCustomAudioThread; ADataPacket: TNMMAudioDataPacketInfo);
begin
  with FDecodingInfo do
  begin
    LastPacketTime:= Now;
    if Started=0 then Started:= LastPacketTime;
    MaxPacket:= Max(ADataPacket.Size,MaxPacket);
    PrevPacketTime:= LastPacketTime;
    inc(Bytes,ADataPacket.Size);
  end;
end;

procedure TfrmAudioSettings.OnDecodingFinished(Sender: TObject);
begin
  Sleep(200);
  FCS.Enter;
  try
    with FDecodingInfo, memResults.Lines do
    begin
      if Started<>0 then
      begin
        Finished:= Now;
        Add('------- Audio data decoding -----');
        Add(' Started at: '+TimeToStr(Started));
        Add(' Finished at: '+TimeToStr(Finished));
        Add(' Time spent: '+TimeToStr(Finished-Started));
        Add(' Bytes coded: '+IntToStr(Bytes));
        if FRecordLen<>0 then
          Add(' Avg bits per sec: ' + IntToStr( Bytes div FRecordLen*8*1000 ));
        if FDeltaTime<>0 then
          Add(' Max bits per sec: ' + IntToStr( MaxPacket*8*1000 div FDeltaTime) );
        Add('');
      end;
    end;
  finally
    FCS.Leave;
  end;
end;

procedure TfrmAudioSettings.bFinishTestClick(Sender: TObject);
begin
  bStopRecClick(nil);
end;

procedure TfrmAudioSettings.BitBtn1Click(Sender: TObject);
begin
  SelectAudioDevice.SetupWFX(FPwfxIn^);
//  SelectAudioDevice.SetupWFX(FPAfc^.pwfx^);
end;

procedure TfrmAudioSettings.FormShow(Sender: TObject);
begin
  UpdateCodecInfo;
end;

end.
