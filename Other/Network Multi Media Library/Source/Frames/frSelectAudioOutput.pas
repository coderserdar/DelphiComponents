(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at babikov@mail.ru

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit frSelectAudioOutput;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frSelectAudio, StdCtrls, MMSystem, ExtCtrls;

type
  TSelectAudioOutput = class(TSelectAudio)
    procedure cbWaveMapperMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  end;


implementation

{$R *.dfm}
constructor TSelectAudioOutput.Create(AOwner: TComponent);
var
  i,ndevs: integer;
  woCaps: TWaveOutCaps;
begin
  inherited;
  ndevs := waveOutGetNumDevs;
  for i:=0 to ndevs-1 do begin
    if waveOutGetDevCaps(i,@woCaps,sizeof(woCaps))=MMSYSERR_NOERROR then begin
      lbWaveDevices.Items.Add(woCaps.szPname);
    end;
  end;
  if ndevs>0 then lbWaveDevices.ItemIndex:= 0;
end;

procedure TSelectAudioOutput.cbWaveMapperMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  lbWaveDevices.Enabled:= not cbWaveMapper.Checked;
end;

end.
