{*********************************************************}
{*                   VPWAVDLG.PAS 1.03                   *}
{*********************************************************}

{* ***** BEGIN LICENSE BLOCK *****                                            *}
{* Version: MPL 1.1                                                           *}
{*                                                                            *}
{* The contents of this file are subject to the Mozilla Public License        *}
{* Version 1.1 (the "License"); you may not use this file except in           *}
{* compliance with the License. You may obtain a copy of the License at       *}
{* http://www.mozilla.org/MPL/                                                *}
{*                                                                            *}
{* Software distributed under the License is distributed on an "AS IS" basis, *}
{* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License   *}
{* for the specific language governing rights and limitations under the       *}
{* License.                                                                   *}
{*                                                                            *}
{* The Original Code is TurboPower Visual PlanIt                              *}
{*                                                                            *}
{* The Initial Developer of the Original Code is TurboPower Software          *}
{*                                                                            *}
{* Portions created by TurboPower Software Inc. are Copyright (C) 2002        *}
{* TurboPower Software Inc. All Rights Reserved.                              *}
{*                                                                            *}
{* Contributor(s):                                                            *}
{*                                                                            *}
{* ***** END LICENSE BLOCK *****                                              *}

unit VpWavDlg;

interface

{$WARNINGS OFF} {Some of this stuff in here isn't platform friendly}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FileCtrl, StdCtrls, ExtCtrls, Buttons, VpBase, ComCtrls;

type
  TFrmSoundDialog = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    PlayButton: TSpeedButton;
    DriveComboBox1: TDriveComboBox;
    DirectoryListBox1: TDirectoryListBox;
    FileListBox1: TFileListBox;
    CBDefault: TCheckBox;
    OkBtn: TButton;
    CancelBtn: TButton;
    procedure FileListBox1Change(Sender: TObject);
    procedure PlayButtonClick(Sender: TObject);
    procedure CBDefaultClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
  public
    DingPath: string;
    ReturnCode : TVpEditorReturnCode;
    procedure Populate;
  end;

function ExecuteSoundFinder(var DingPath: string): Boolean;

implementation

uses mmSystem, VpSR;

{$R *.DFM}

function ExecuteSoundFinder(var DingPath: string): Boolean;
var
  SoundFinder: TfrmSoundDialog;
begin
  Result := false;
  Application.CreateForm(TfrmSoundDialog, SoundFinder);
  try
    SoundFinder.DingPath := DingPath;
    SoundFinder.Populate;
    SoundFinder.ShowModal;
    if SoundFinder.ReturnCode = rtCommit then begin
      if SoundFinder.CBDefault.Checked then
        DingPath := ''
      else
        DingPath := SoundFinder.FileListBox1.FileName;
      Result := true;
    end;
  finally
    SoundFinder.Release;
  end;
end;
{=====}

procedure TFrmSoundDialog.FileListBox1Change(Sender: TObject);
begin
  if FileListBox1.Items.Count > 0 then begin
    PlayButton.Enabled := true;
    DingPath := FileListBox1.FileName;
  end else begin
   PlayButton.Enabled := false;
   DingPath := '';
  end;
end;
{=====}

procedure TFrmSoundDialog.PlayButtonClick(Sender: TObject);
begin
  PlayButton.Enabled := false;
  SndPlaySound(PChar(FileListBox1.FileName), snd_Sync);
  PlayButton.Enabled := true;
end;
{=====}

procedure TFrmSoundDialog.Populate;
var
  Drive: char;
begin
  TabSheet1.Caption := RSSelectASound;
  Self.Caption := RSSoundFinder;
  CBDefault.Caption := RSDefaultSound;
  OkBtn.Caption := RSOkBtn;
  CancelBtn.Caption := RSCancelBtn;
  if DingPath = '' then begin
    CBDefault.Checked := true;
    DirectoryListBox1.Directory := ExtractFileDir(ParamStr(0));
  end else begin
    Drive := UpCase(ExtractFileDrive(DingPath)[1]);
    if FileExists(DingPath) and (Drive in ['A'..'Z']) then begin
      DriveComboBox1.Drive := Drive;
      DirectoryListBox1.Directory := ExtractFileDir(DingPath);
      FileListBox1.FileName := DingPath;
    end else begin
      DirectoryListBox1.Directory := ExtractFileDir(ParamStr(0));
    end;
  end;
end;
{=====}

procedure TFrmSoundDialog.CBDefaultClick(Sender: TObject);
begin
  DriveComboBox1.Enabled := not CBDefault.Checked;
  DirectoryListBox1.Enabled := not CBDefault.Checked;
  FileListBox1.Enabled := not CBDefault.Checked;
  PlayButton.Enabled := not CBDefault.Checked;
end;
{=====}

procedure TFrmSoundDialog.FormCreate(Sender: TObject);
begin
  ReturnCode := rtAbandon;
end;
{=====}

procedure TFrmSoundDialog.OkBtnClick(Sender: TObject);
begin
  ReturnCode := rtCommit;
  Close;
end;
{=====}

procedure TFrmSoundDialog.CancelBtnClick(Sender: TObject);
begin
  Close;
end;
{=====}

procedure TFrmSoundDialog.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
end;

end.
  
