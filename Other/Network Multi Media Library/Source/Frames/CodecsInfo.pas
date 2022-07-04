(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at babikov@mail.ru

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit CodecsInfo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, MMSystem, MSAcm, ExtCtrls;

type
  TfrmCodecsInfo = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    memCodecDetails: TMemo;
    pnCodecDetails: TPanel;
    lbCodecs: TListBox;
    Panel4: TPanel;
    lbACMver: TLabel;
    lblCodecs: TLabel;
    Panel5: TPanel;
    Panel6: TPanel;
    procedure lbCodecsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmCodecsInfo: TfrmCodecsInfo;

implementation

uses NMMAudioCommon;

{$R *.dfm}

procedure TfrmCodecsInfo.lbCodecsClick(Sender: TObject);
var
  LPADD: TACMDriverDetails;
  LDriverID: HACMDriverID;
  LDetails: DWORD;
begin
  FillChar(LPADD,SizeOf(LPADD),0);
  LDetails:= 0;
  LPADD.cbStruct:= SizeOf(LPADD);
  try
    with lbCodecs do
      if ItemIndex >= 0 then
      begin
         AcmCheck(acmDriverDetails(HACMDRIVERID(Items.Objects[ItemIndex]),LPADD,LDetails));
         acmDriverID(Pointer(Items.Objects[ItemIndex]),LDriverID,0);
      end
      else
        exit;
  except
    exit;
  end;
  pnCodecDetails.Caption:= LPADD.szLongName + ' codec details:';
  with memCodecDetails.Lines do
  begin
    Clear;
    Add('PID='+IntToStr(LPADD.wPid));
    Add('ACM ver='+IntToStr(LPADD.vdwACM));
    Add('Driver ver='+IntToStr(LPADD.vdwDriver));


    if (ACMDRIVERDETAILS_SUPPORTF_ASYNC and LPADD.fdwSupport) = 1 then
      Add('Driver supports asynchronous conversions');
    if (ACMDRIVERDETAILS_SUPPORTF_CODEC and LPADD.fdwSupport) = 1 then
      Add('Driver supports conversion between two different format tags.');
    if (ACMDRIVERDETAILS_SUPPORTF_CONVERTER and LPADD.fdwSupport) = 1 then
      Add('Driver supports conversion between two different formats of the same format tag.');
    if (ACMDRIVERDETAILS_SUPPORTF_DISABLED and LPADD.fdwSupport) = 1 then
      Add('Driver has been disabled.');
    if (ACMDRIVERDETAILS_SUPPORTF_FILTER and LPADD.fdwSupport) = 1 then
      Add('Driver supports a filter (modification of the data without changing any of the format attributes).');
    if (ACMDRIVERDETAILS_SUPPORTF_HARDWARE and LPADD.fdwSupport) = 1 then
      Add('Driver supports hardware input, output, or both through a waveform-audio device. ');
    if (ACMDRIVERDETAILS_SUPPORTF_LOCAL and LPADD.fdwSupport) = 1 then
      Add('The driver has been installed locally with respect to the current task.');

    Add('');
    Add(StrPas(LPADD.szFeatures));
    Add(StrPas(LPADD.szLicensing));
    Add(StrPas(LPADD.szCopyright));
  end;
end;

procedure TfrmCodecsInfo.FormCreate(Sender: TObject);
var LCodecs: Integer;
begin
  lbACMver.Caption:= 'ACM version '+IntToStr(acmGetVersion);
  acmMetrics(Nil,ACM_METRIC_COUNT_CODECS,LCodecs);
  lblCodecs.Caption:= IntToStr(LCodecs)+' codecs';
  GetCodecs(lbCodecs.Items);
  if LCodecs>0 then
  begin
    lbCodecs.ItemIndex:= 0;
    lbCodecsClick(nil);
  end;
end;

end.
