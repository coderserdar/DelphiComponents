{*********************************************************}
{* General info maintenance                              *}
{*********************************************************}

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower FlashFiler
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

unit uFFSGenl;

{$I FFDEFINE.INC}

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons,
  FFSrEng, FFSrCfg;

type
  TFFGenConfigForm = class(TForm)
    grpGeneral: TGroupBox;
    lblServerName: TLabel;
    lblMaxRAM: TLabel;
    lblPriority: TLabel;
    edtServerName: TEdit;
    edtMaxRAM: TEdit;
    cbxPriority: TComboBox;
    boxEncrypt: TCheckBox;
    boxReadOnly: TCheckBox;
    boxSecurity: TCheckBox;
    boxDebugLog: TCheckBox;
    boxNoSaveCfg: TCheckBox;
    gbxStartup: TGroupBox;
    boxServerUp: TCheckBox;
    boxMinimize: TCheckBox;
    gbxKeepAlive: TGroupBox;
    lblLMInterval: TLabel;
    lblBetKeeps: TLabel;
    lblKARetries: TLabel;
    edtLastMsg: TEdit;
    edtKAInterval: TEdit;
    edtKARetries: TEdit;
    btnDiscard: TBitBtn;
    btnSave: TBitBtn;
    lblTempStoreSize: TLabel;
    edtTempStoreSize: TEdit;
    gbCollect: TGroupBox;
    boxCollectEnabled: TCheckBox;
    lblCollectFreq: TLabel;
    edtCollectFreq: TEdit;
    procedure btnSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure boxReadOnlyClick(Sender: TObject);
    procedure boxNoSaveCfgClick(Sender: TObject);
    procedure boxCollectEnabledClick(Sender: TObject);
  private
    { Private declarations }
    FEngine : TffServerEngine;
    OurGenInfo : TffGeneralInfo;

    procedure InitCtrlStates;
    procedure SetControls;
    procedure SetEngine(anEngine : TffServerEngine);

  public
    { Public declarations }
    property ServerEngine : TffServerEngine read FEngine write SetEngine;
      { The server engine being configured by this dialog. }
  end;

var
  FFGenConfigForm: TFFGenConfigForm;

implementation

uses
  FFLLComp,
  FFLLEng,
  FFLLExcp,
  FFLLBase,
  FFSRBde,
  FFLLProt;

{$R *.DFM}

{=====================================================================}
procedure TFFGenConfigForm.boxReadOnlyClick(Sender: TObject);
begin
  SetControls;
end;
{--------}
procedure TFFGenConfigForm.btnSaveClick(Sender : TObject);
const
  PortError = 'Port number should be a unique number between 1024 and 65535 inclusive';
  MaxRAMError = 'Max RAM should have a value between 1 and 2048MB';
  IntervalError = 'The interval should be a number between 1,000 and 86,400,000 milliseconds, inclusive';
  RetriesError = 'The number of retries should be a number between 1 and 100, inclusive';
  TempStorSizeError = 'The temporary storage size should have a value between 1 and 2048MB';
  CollectFreqError = 'The garbage collection frequency should be between 30,000 (30 seconds) and 3,600,000 (60 minutes) milliseconds';
var
  ec           : Integer;
  MaxRAM       : Integer;
  TempStorSize : Integer;
  CollectFreq  : Longint;
  LMInterval   : Longint;
  KAInterval   : Longint;
  KARetries    : Integer;
  errStr       : array [0..127] of char;
  aResult      : TffResult;
  OverRideRO   : Boolean;
begin
  Val(edtMaxRAM.Text, MaxRAM, ec);
  if (ec <> 0) or (MaxRAM < 1) or (MaxRam > 2048) then begin
    ActiveControl := edtMaxRAM;
    ShowMessage(MaxRAMError);
    Exit;
  end;
  Val(edtTempStoreSize.Text, TempStorSize, ec);
  if (ec <> 0) or (TempStorSize < 1) or (TempStorSize > 2048) then begin
    ActiveControl := edtTempStoreSize;
    ShowMessage(TempStorSizeError);
    Exit;
  end;
  Val(edtCollectFreq.Text, CollectFreq, ec);
  if (ec <> 0) or (CollectFreq < 30000) or (CollectFreq > 3600000) then begin
    ActiveControl := edtCollectFreq;
    ShowMessage(CollectFreqError);
    Exit;
  end;
  Val(edtLastMsg.Text, LMInterval, ec);
  if (ec <> 0) or (LMInterval < 1000) or (LMInterval > 86400000) then begin
    ActiveControl := edtLastMsg;
    ShowMessage(IntervalError);
    Exit;
  end;
  Val(edtKAInterval.Text, KAInterval, ec);
  if (ec <> 0) or (KAInterval < 1000) or (KAInterval > 86400000) then begin
    ActiveControl := edtKAInterval;
    ShowMessage(IntervalError);
    Exit;
  end;
  Val(edtKARetries.Text, KARetries, ec);
  if (ec <> 0) or (KARetries < 1) or (KARetries > 100) then begin
    ActiveControl := edtKARetries;
    ShowMessage(RetriesError);
    Exit;
  end;
  with OurGenInfo do begin
    if (edtServerName.Text <> '') then
      giServerName := edtServerName.Text;
    giMaxRAM := MaxRAM;
    {$IFDEF SecureServer}
    giAllowEncrypt := boxEncrypt.Checked;
    {$ELSE}
    giAllowEncrypt := False;                                           {!!.01}
    {$ENDIF}
    giReadOnly := boxReadOnly.Checked;
    giIsSecure := boxSecurity.Checked;
    giAutoUp := boxServerUp.Checked;
    giAutoMini := boxMinimize.Checked;
    giDebugLog := boxDebugLog.Checked;
    giLastMsgInterval := LMInterval;
    giKAInterval := KAInterval;
    giKARetries := KARetries;
    giTempStoreSize := TempStorSize;
    giCollectEnabled := boxCollectEnabled.Checked;
    giCollectFreq := CollectFreq;
    ffc_LastMsgInterval := giLastMsgInterval;
    ffc_KeepAliveInterval := giKAInterval;
    ffc_KeepAliveRetries := giKARetries;
    if (0 <= cbxPriority.ItemIndex) and
       (cbxPriority.ItemIndex <= 4) then
      giPriority := cbxPriority.ItemIndex - 2
    else
      giPriority := 0;
    giNoAutoSaveCfg := boxNoSaveCfg.Checked;
  end;
  {we have to override the ReadOnly setting if we're changing
   ReadOnly or NoAutoSaveCfg from False to True.}
  with FEngine.Configuration do begin
    OverrideRO := ((OurGenInfo.giReadOnly and
                    (not GeneralInfo^.giReadOnly)) or
                   (OurGenInfo.giNoAutoSaveCfg and
                    (not GeneralInfo^.giNoAutoSaveCfg)));
    GeneralInfo^ := OurGenInfo;
  end;
  aResult := FEngine.WriteGeneralInfo(OverrideRO);
  if aResult <> DBIERR_NONE then begin
    ffStrResBDE.GetASCIIZ(aResult, errStr, sizeof(DBIMSG));
    ShowMessage(Format('Could not save configuration: %s [$%x/%d])',
                       [strPas(errStr), aResult, aResult]));
    ModalResult := mrNone;
  end
  else
    ModalResult := mrOK;
end;
{--------}
procedure TFFGenConfigForm.FormCreate(Sender : TObject);
begin
  FEngine := nil;
end;
{--------}
procedure TFFGenConfigForm.FormShow(Sender : TObject);
begin
  if FEngine = nil then
    Exit;
  InitCtrlStates;
  if FEngine.State = ffesStarted then
    edtMaxRAM.SetFocus
  else
    edtServerName.SetFocus;
end;

{--------}
procedure TFFGenConfigForm.InitCtrlStates;
var
  ServerUp : Boolean;
begin
  ServerUp := (FEngine.State = ffesStarted);
  edtServerName.Enabled := (not ServerUp);
end;
{--------}
procedure TFFGenConfigForm.SetControls;
begin
  boxNoSaveCfg.Enabled := not boxReadOnly.Checked;
  edtCollectFreq.Enabled := boxCollectEnabled.Checked;
end;
{--------}
procedure TFFGenConfigForm.SetEngine(anEngine : TffServerEngine);
begin
  FEngine := anEngine;
  if assigned(FEngine) then begin
    OurGenInfo := FEngine.Configuration.GeneralInfo^;
    with OurGenInfo do begin
      edtServerName.Text := giServerName;
      edtMaxRAM.Text := IntToStr(giMaxRAM);
      {$IFDEF SecureServer}
      boxEncrypt.Checked := giAllowEncrypt;
      {$ELSE}
      boxEncrypt.Checked := False;                                     {!!.01}
      boxEncrypt.Enabled := False;                                     {!!.01}
      {$ENDIF}
      boxReadOnly.Checked := giReadOnly;
      boxNoSaveCfg.Checked := giNoAutoSaveCfg;
      boxSecurity.Checked := giIsSecure;
      boxServerUp.Checked := giAutoUp;
      boxMinimize.Checked := giAutoMini;
      boxDebugLog.Checked := giDebugLog;
      edtLastMsg.Text := IntToStr(giLastMsgInterval);
      edtKAInterval.Text := IntToStr(giKAInterval);
      edtKARetries.Text := IntToStr(giKARetries);
      if (giPriority < -2) or (giPriority > 2) then
        cbxPriority.ItemIndex := 2
      else
        cbxPriority.ItemIndex := giPriority + 2;
      edtTempStoreSize.Text := InttoStr(giTempStoreSize);
      boxCollectEnabled.Checked := giCollectEnabled;
      edtCollectFreq.Text := IntToStr(giCollectFreq);
    end;
  end;
end;
{=====================================================================}
procedure TFFGenConfigForm.boxNoSaveCfgClick(Sender : TObject);
begin
  SetControls;
end;

procedure TFFGenConfigForm.boxCollectEnabledClick(Sender : TObject);
begin
  SetControls;
end;

end.
