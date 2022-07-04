(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at babikov@mail.ru

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit frSelectAudioInput;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frSelectAudio, StdCtrls, MMSystem, ExtCtrls;

type
  TSelectAudioInput = class(TSelectAudio)
    Label2: TLabel;
    cbBits: TComboBox;
    cbFrequency: TComboBox;
    Label3: TLabel;
    Label4: TLabel;
    cbChannels: TComboBox;
  private
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure SetupWfx(var wfx: TWAVEFORMATEX);
    procedure GetSettings(var wfx: TWAVEFORMATEX; var ADeviceId: Integer);
    procedure Setup(wfx: TWAVEFORMATEX; ADeviceId: Integer; ADisableCombos: Boolean=false);
  end;

var
  SelectAudioInput: TSelectAudioInput;

implementation

{$R *.dfm}

constructor TSelectAudioInput.Create(AOwner: TComponent);
var
  i,ndevs: integer;
  wiCaps: TWaveInCaps;
begin
  inherited;
  ndevs := waveInGetNumDevs;
  for i:=0 to ndevs-1 do begin
    if waveInGetDevCaps(i,@wiCaps,sizeof(wiCaps))=MMSYSERR_NOERROR then begin
      lbWaveDevices.Items.AddObject(wiCaps.szPname,Pointer(wiCaps.wPid));
    end;
  end;
  if ndevs>0 then lbWaveDevices.ItemIndex:= 0;

  cbBits.Items.AddObject('8 bits',Pointer(8));
  cbBits.Items.AddObject('16 bits',Pointer(16));
  cbBits.ItemIndex:= 1;

  cbFrequency.Items.AddObject('8000',Pointer(8000));
  cbFrequency.Items.AddObject('11025',Pointer(11025));
  cbFrequency.Items.AddObject('16000',Pointer(16000));
  cbFrequency.Items.AddObject('22050',Pointer(22050));
  cbFrequency.Items.AddObject('32000',Pointer(32000));
  cbFrequency.Items.AddObject('44100',Pointer(44100));
  cbFrequency.ItemIndex:= 0;

  cbChannels.Items.AddObject('Mono',Pointer(1));
  cbChannels.Items.AddObject('Stereo',Pointer(2));
  cbChannels.ItemIndex:= 0;
end;

procedure TSelectAudioInput.SetupWfx(var wfx: TWAVEFORMATEX);
begin
  wfx.wFormatTag:= WAVE_FORMAT_PCM;
  wfx.nChannels:= Integer(cbChannels.Items.Objects[cbChannels.ItemIndex]);
  wfx.nSamplesPerSec:= Integer(cbFrequency.Items.Objects[cbFrequency.ItemIndex]);
  wfx.wBitsPerSample:= Integer(cbBits.Items.Objects[cbBits.ItemIndex]);
  wfx.nBlockAlign:= (wfx.wBitsPerSample div 8)*wfx.nChannels;
  wfx.nAvgBytesPerSec:= wfx.nSamplesPerSec*wfx.nBlockAlign;
  wfx.cbSize:= 0;
end;

procedure TSelectAudioInput.GetSettings(var wfx: TWAVEFORMATEX; var ADeviceId: Integer);
begin
  SetupWfx(wfx);
  if cbWaveMapper.Checked then
     ADeviceId:= WAVE_MAPPER
  else
     ADeviceId:= lbWaveDevices.ItemIndex;
end;

procedure TSelectAudioInput.Setup(wfx: TWAVEFORMATEX; ADeviceId: Integer; ADisableCombos: Boolean=false);
  procedure SetCombo(ACombo: TComboBox; AValue: Integer);
  begin
    ACombo.ItemIndex:= ACombo.Items.IndexOfObject(Pointer(AValue));
    if ACombo.ItemIndex=-1 then ACombo.ItemIndex:= 0; 
  end;
begin
  SetCombo(cbChannels,wfx.nChannels);
  SetCombo(cbFrequency,wfx.nSamplesPerSec);
  SetCombo(cbBits,wfx.wBitsPerSample);
  cbChannels.Enabled:= not ADisableCombos;
  cbFrequency.Enabled:= not ADisableCombos;
  cbBits.Enabled:= not ADisableCombos;
  cbWaveMapper.Checked:= (ADeviceId = -1);
  if not cbWaveMapper.Checked then
     lbWaveDevices.ItemIndex:= ADeviceId;
end;

end.
