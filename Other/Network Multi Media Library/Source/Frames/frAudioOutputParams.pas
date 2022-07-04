(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at babikov@mail.ru

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit frAudioOutputParams;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, MMSystem;

type
  TAudioOutputParams = class(TFrame)
    cbVolume: TGroupBox;
    tbRight: TTrackBar;
    tbLeft: TTrackBar;
    tbFrequency: TTrackBar;
    tbPitch: TTrackBar;
    cbMono: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    GroupBox3: TGroupBox;
    Label5: TLabel;
    Label6: TLabel;
    procedure tbVolumeChange(Sender: TObject);
    procedure tbPitchChange(Sender: TObject);
    procedure tbFrequencyChange(Sender: TObject);
  private
    { Private declarations }
  protected
    FSavedVolume,
    FSavedPitch,
    FSavedPlaybackRate: DWORD;
  public
    { Public declarations }
    hwo: HWAVEOUT;
    wfx: PWAVEFORMATEX;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RunTimeInit(Ahwo: HWAVEOUT; Awfx: PWAVEFORMATEX);
  end;

implementation

{$R *.dfm}

const DispTrackBarMax=11;
      TrackBarScale= $FFFF div (DispTrackBarMax+1);

constructor TAudioOutputParams.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  hwo:= 0;
  tbRight.Min:= -DispTrackBarMax;
  tbLeft.Min:= -DispTrackBarMax;
  tbFrequency.Min:= -DispTrackBarMax;
  tbPitch.Min:= -DispTrackBarMax;

  tbRight.Enabled:= false;
  tbLeft.Enabled:= false;
  tbFrequency.Enabled:= false;
  tbPitch.Enabled:= false;
end;

procedure TAudioOutputParams.RunTimeInit(Ahwo: HWAVEOUT; Awfx: PWAVEFORMATEX);
//var
begin
  hwo:= Ahwo;  wfx:= Awfx;
  if hwo<>0 then
  begin
    waveOutGetVolume(hwo, @FSavedVolume);
    //todo: use inifile
    tbRight.Position:= -(FSavedVolume shr 16) div TrackBarScale;
    tbLeft.Position:= -((FSavedVolume shl 16) shr 16) div TrackBarScale;

    if Assigned(wfx) then
    begin
      tbPitch.Enabled:= true;
      waveOutGetPitch(hwo, @FSavedPitch);
      tbPitch.Position:= -FSavedPitch*wfx^.nSamplesPerSec div TrackBarScale;
    end;

    waveOutGetPlaybackRate(hwo, @FSavedPlaybackRate);
    tbFrequency.Position:= -FSavedPlaybackRate*100 div TrackBarScale;

    tbLeft.Enabled:= true;
    tbRight.Enabled:= true;
    tbFrequency.Enabled:= true;
  end;
end;

destructor TAudioOutputParams.Destroy;
begin
  if hwo<>0 then
  begin
    waveOutSetVolume(hwo,FSavedVolume);
    if Assigned(wfx) then waveOutSetPitch(hwo, FSavedPitch);
    waveOutSetPlaybackRate(hwo, FSavedPlaybackRate);
  end;
  inherited;
end;

procedure TAudioOutputParams.tbVolumeChange(Sender: TObject);
var LeftVolume, RightVolume: DWORD;
begin
  if hwo<>0 then
  begin
    LeftVolume:= $FFFF - TrackBarScale*(-tbLeft.Position);
    RightVolume:= $FFFF - TrackBarScale*(-tbRight.Position);
    //RightVolume shl 16) + LeftVolume
    waveOutSetVolume(hwo, MAKELONG(LeftVolume,RightVolume));
  end;
end;

procedure TAudioOutputParams.tbPitchChange(Sender: TObject);
begin
  if (hwo<>0) and Assigned(wfx) then
     waveOutSetPitch(hwo, TrackBarScale*(-tbPitch.Position) div wfx^.nSamplesPerSec);
end;

procedure TAudioOutputParams.tbFrequencyChange(Sender: TObject);
begin
  if hwo<>0 then
     waveOutSetPlaybackRate(hwo, TrackBarScale*(-tbFrequency.Position) div 100);
end;

end.
