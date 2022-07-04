(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at babikov@mail.ru

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit NMMVoiceBroadcastServer;

interface

uses NMMCustomSimpleBroadcastServer,  NMMCustomAuthChecker,
     NMMCustomConnectionGroupThread,
     NMMAudioQueues, NMMACMThread, NMMAudioRecordThread, NMMAudioCommon,
     MsACM, MMSystem,
     Classes, Controls, SysUtils;

type
  TNMMVoiceBroadcastServer = class(TNMMCustomSimpleBroadcastServer)
  protected
    FAudioRecordThread: TNMMAudioRecordThread;
    FAudioDataParams: TAudioDataParams;
    FAfc: TACMFORMATCHOOSE;
    FInOutDevice: Integer;
    FCodeThread: TNMMACMThread;
    FAudioRecordQueue: TNMMAudioRecordQueue;
    FCodedQueue: TNMMAudioQueue;
    function CreateAuthChecker: TNMMCustomAuthChecker; override;
    procedure OnRecordingFinished(Sender: TObject);
    procedure OnCodingFinished(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CreateConnectionProcessor: TNMMCustomConnectionGroupThread; override;
    procedure SetActive(AActive: Boolean); override;
    function RunAudioSetupDlg: TModalResult;
    procedure LoadSettings(AFileName: String);
    procedure SaveSettings(AFileName: String);
    property InOutDevice: Integer read FInOutDevice write FInOutDevice;
  end;


implementation

uses NMMAudioGroupProcessor, NMMBasicAuthChecker,
     NMMAudioSetup;


constructor TNMMVoiceBroadcastServer.Create(AOwner: TComponent);
begin
  inherited;
  if (not (csDesigning in ComponentState)) and
     (not (csLoading in ComponentState)) then
  begin
    FInOutDevice:= WAVE_MAPPER;
    FAudioRecordQueue:= TNMMAudioRecordQueue.Create;
    FCodedQueue:= TNMMAudioQueue.Create;
    FAudioDataParams:= TAudioDataParams.Create;
  end;
end;

destructor TNMMVoiceBroadcastServer.Destroy;
begin
  if (not (csDesigning in ComponentState)) and
     (not (csLoading in ComponentState)) then
  begin
    Active:= false;
    Sleep(10);
    FreeAndNil(FAudioRecordQueue);
    FreeAndNil(FCodedQueue);
    FreeAndNil(FAudioDataParams);
  end;
  inherited;
end;

function TNMMVoiceBroadcastServer.RunAudioSetupDlg: TModalResult;
var
  frmAudioSettings: TfrmAudioSettings;
begin
  Result:= mrNone;
  if (not (csDesigning in ComponentState)) and
     (not (csLoading in ComponentState)) then
  begin
    with FAudioDataParams do
    begin
      frmAudioSettings:= TfrmAudioSettings.Create(nil);
      try
        frmAudioSettings.PAfc:= @FAfc;
        frmAudioSettings.SetWfxCode(PWfxCode,WfxCodeMaxSize);
        frmAudioSettings.WfxIn:= PWfxInOut;
        frmAudioSettings.SelectAudioDevice.Setup(PWfxInOut^,FInOutDevice,false);
        Result:= frmAudioSettings.ShowModal;
        frmAudioSettings.GetWfxCode(PWfxCode,WfxCodeMaxSize);
        FInOutDevice:= frmAudioSettings.SelectAudioDevice.GetDeviceID;
      finally
        FreeAndNil(frmAudioSettings);
      end;
    end;
  end;
end;

procedure TNMMVoiceBroadcastServer.SetActive(AActive: Boolean);
begin
  if (not (csDesigning in ComponentState)) and
     (not (csLoading in ComponentState)) and
     (AActive<>Active) then
  begin
    if AActive then
    begin
      FAudioRecordQueue.Clear;
      FCodedQueue.Clear;
      FConnectionProcessor:= CreateConnectionProcessor;
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
        OutQueue:= FCodedQueue;
        OnFinished:= OnCodingFinished;
        Resume;
      except
        Free;
        raise;
      end;
      inherited;
    end
    else
    begin
      try
        if Assigned(FAudioRecordThread) then
          FAudioRecordThread.Stop;
        Sleep(10);
        inherited;
      finally
        DeleteConnectionProcessor;
      end;
    end;
  end;
end;

procedure TNMMVoiceBroadcastServer.OnRecordingFinished(Sender: TObject);
begin
  FAudioRecordThread:= nil;
end;

procedure TNMMVoiceBroadcastServer.OnCodingFinished(Sender: TObject);
begin
  FCodeThread:= nil;
end;

function TNMMVoiceBroadcastServer.CreateConnectionProcessor: TNMMCustomConnectionGroupThread;
begin
  result:= TNMMAudioGroupProcessor.Create;
  (result as TNMMAudioGroupProcessor).DataQueue:= FCodedQueue;
  FAudioDataParams.SaveToStream((result as TNMMAudioGroupProcessor).IniDataStream);
end;

function TNMMVoiceBroadcastServer.CreateAuthChecker: TNMMCustomAuthChecker;
begin
  result:= TNMMBasicAuthChecker.Create;
end;

procedure TNMMVoiceBroadcastServer.LoadSettings(AFileName: String);
var LFS: TFileStream;
begin
  if FileExists(AFileName) then
  begin
    LFS:= TFileStream.Create(AFileName,fmOpenRead);
    try
      FAudioDataParams.LoadFromStream(LFS);
      LFS.ReadBuffer(FInOutDevice,SizeOf(FInOutDevice));
      with FAfc,LFS do
      begin
        ReadBuffer(cbStruct,SizeOf(cbStruct));
        ReadBuffer(fdwStyle,SizeOf(fdwStyle));
        ReadBuffer(cbwfx,SizeOf(cbwfx));
        ReadBuffer(pszTitle,SizeOf(pszTitle^));
        ReadBuffer(szFormatTag,SizeOf(szFormatTag));
        ReadBuffer(szFormat,SizeOf(szFormat));
        ReadBuffer(pszName,SizeOf(pszName^));
        ReadBuffer(cchName,SizeOf(cchName));
      end;
    finally
      FreeAndNil(LFS);
    end;
  end;
end;

procedure TNMMVoiceBroadcastServer.SaveSettings(AFileName: String);
var LFS: TFileStream;
begin
  if FileExists(AFileName) then
     DeleteFile(AFileName);

  LFS:= TFileStream.Create(AFileName,fmCreate);
  try
    StrCopy(FAudioDataParams.szFormatTag,FAfc.szFormatTag);
    StrCopy(FAudioDataParams.szFormat,FAfc.szFormat);
    FAudioDataParams.SaveToStream(LFS);
    LFS.WriteBuffer(FInOutDevice,SizeOf(FInOutDevice));
    with FAfc,LFS do
    begin
      WriteBuffer(cbStruct,SizeOf(cbStruct));
      WriteBuffer(fdwStyle,SizeOf(fdwStyle));
      WriteBuffer(cbwfx,SizeOf(cbwfx));
      WriteBuffer(pszTitle,SizeOf(pszTitle^));
      WriteBuffer(szFormatTag,SizeOf(szFormatTag));
      WriteBuffer(szFormat,SizeOf(szFormat));
      WriteBuffer(pszName,SizeOf(pszName^));
      WriteBuffer(cchName,SizeOf(cchName));
    end;
  finally
    FreeAndNil(LFS);
  end;
end;

end.
