(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at babikov@mail.ru

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit frSelectAudio;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, MMSystem, ExtCtrls;

type
  TSelectAudio = class(TFrame)
    Panel12: TPanel;
    Label1: TLabel;
    lbWaveDevices: TListBox;
    cbWaveMapper: TCheckBox;
    procedure cbWaveMapperClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetDeviceID(LID: Integer);
    function GetDeviceID: Integer;
  published
    property DeviceId: Integer read GetDeviceID write SetDeviceID;
  end;

implementation

{$R *.dfm}


function TSelectAudio.GetDeviceID: Integer;
begin
  if cbWaveMapper.Checked or (lbWaveDevices.Count<1) then
     result:= WAVE_MAPPER
  else
     result:= lbWaveDevices.ItemIndex;
end;

procedure TSelectAudio.SetDeviceID(LID: Integer);
begin
  lbWaveDevices.ItemIndex:= -1;
  if (LID = -1) or (LID = WAVE_MAPPER) then
     cbWaveMapper.Checked:= true
  else if (LID > -1) and (LID < lbWaveDevices.Count) then
     lbWaveDevices.ItemIndex:= LID;
end;

procedure TSelectAudio.cbWaveMapperClick(Sender: TObject);
begin
  lbWaveDevices.Enabled:= not cbWaveMapper.Checked;
end;

end.
