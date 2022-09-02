{*********************************************************}
{* FlashFiler: Main form for FFRebuild210                *}
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

{$I ffdefine.inc}

unit umain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls;

type
  TRebuildState = (stIdle, stRunning);

  TfrmMain = class(TForm)
    pnlTop: TPanel;
    lvTables: TListView;
    pnlBottom: TPanel;
    prgCurrentFile: TProgressBar;
    prgAllFiles: TProgressBar;
    lblPrgFile: TLabel;
    lblPrgAllFiles: TLabel;
    lblInitialDir: TLabel;
    efInitialDir: TEdit;
    pbRebuild: TButton;
    pbClose: TButton;
    procedure FormShow(Sender: TObject);
    procedure pbCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure pbRebuildClick(Sender: TObject);
    procedure efInitialDirChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }

    { Configuration items }
    FAllowChangeDir : Boolean;
    FAutoRun : Boolean;
    FInitialDir : string;
    FOutputDir : string;

    { Status variables }
    FFirstTime : Boolean;
    FState : TRebuildState;
    FValidConfig : Boolean;

    procedure ClearTables;
    procedure GetTables;
    procedure SetCtrlStates;

  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  FileCtrl,
  ffDB,
  ffllBase,
  ffclreng,
  ffSrEng,
  uConfig, dmMain;

{$R *.DFM}

const
  csIdle = '...';
  csRebuilding = 'Rebuilding...';
  csRebuilt = 'Rebuilt successfully';

procedure TfrmMain.FormShow(Sender: TObject);
var
  Config : TFallBackConfig;
begin
  FFirstTime := True;
  FState := stIdle;
  FValidConfig := True;
  lblPrgFile.Caption := '';
  lblPrgAllFiles.Caption := '';
  dmRebuild := TdmRebuild.Create(nil);
  Config := TFallBackConfig.Create;
  try
    FAllowChangeDir := Config.AllowChangeDir;
    FAutoRun := Config.AutoRun;
    FInitialDir := Config.InitialDir;
    FOutputDir := Config.OutputDir;

    { Check requirements }
    if (FAutoRun or
        (not FAllowChangeDir)) and
       (FInitialDir = '') then begin
      FValidConfig := False;
      ShowMessage('Initial directory must be specified in configuration file.');
    end;

    if (FInitialDir <> '') and
       (not DirectoryExists(FInitialDir)) then begin
      FValidConfig := False;
      ShowMessage('Directory ' + FInitialDir + ' does not exist.');
    end;

    efInitialDir.Text := FInitialDir;
      { This line forces the list of tables to be loaded. }

  finally
    Config.Free;
  end;
end;

procedure TfrmMain.pbCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.SetCtrlStates;
var
  Running : Boolean;
begin
  Running := (FState = stRunning);
  efInitialDir.Enabled := FValidConfig and FAllowChangeDir and (not Running);

  pbRebuild.Enabled := FValidConfig and (not Running) and DirectoryExists(efInitialDir.Text);
  pbClose.Enabled := not Running;
end;

procedure TfrmMain.GetTables;
var
  Inx : Integer;
  Tables : TStringList;
  Item : TListItem;
begin
  ClearTables;

  Tables := TStringList.Create;
  try
    dmRebuild.Path := efInitialDir.Text;
    dmRebuild.GetTables(Tables);

    { Put 1 entry per table into the list view. }
    for Inx := 0 to Pred(Tables.Count) do begin
      Item := lvTables.Items.Add;
      Item.Caption := Tables[Inx];
      Item.Data := Tables.Objects[Inx];
      Item.SubItems.Add(TffTable(Tables.Objects[Inx]).FFVersion);
      Item.SubItems.Add(csIdle);
    end;
  finally
    Tables.Free;
      { We don't have to free the table objects because they are already
        attached to the items in list view. }
  end;
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  ClearTables;
  dmRebuild.Free;
end;

procedure TfrmMain.ClearTables;
var
  Inx : Integer;
begin
  for Inx := Pred(lvTables.Items.Count) downto 0 do
    TffTable(lvTables.Items[Inx].Data).Free;
  lvTables.Items.Clear;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := (FState = stIdle);
end;

type
  SrDBCracker = class(TffSrDatabase);

procedure TfrmMain.pbRebuildClick(Sender: TObject);
var
  Done : Boolean;
  Count,
  Inx : Integer;
  Item : TListItem;
  ServerDB : TffSrDatabase;
  TaskID : Longint;
  TaskStatus : TffRebuildStatus;
begin
  FState := stRunning;
  try
    SetCtrlStates;
    { Init progress bars }
    prgAllFiles.Max := lvTables.Items.Count;
    prgAllFiles.Min := 0;
    prgAllFiles.Position := 0;
    prgCurrentFile.Min := 0;
    prgCurrentFile.Max := 100;
    prgCurrentFile.Position := 0;

    { Force pack to open source table as 2_11. }
    ServerDB := dmRebuild.ServerDatabase;
    SrDBCracker(ServerDB).dbSetPackSrcTableVersion(FFVersionNumber);
      { Assumes current version is > 2_1000. }

    { Force database to create new tables as 2_10. }
    SrDBCracker(ServerDB).dbSetNewTableVersion(FFVersion2_10);

    Count := lvTables.Items.Count;
    for Inx := 0 to Pred(Count) do begin
      Item := lvTables.Items[Inx];
      Item.SubItems[1] := csRebuilding;

      lblPrgFile.Caption := Item.Caption;
      lblPrgAllFiles.Caption := Format('%d of %d', [Inx + 1, Count]);

      { Pack the table. }
      TffTable(Item.Data).PackTable(TaskID);
      { Wait until the pack is done. }
      Done := False;
      while not Done do begin
        dmRebuild.Session.GetTaskStatus(TaskID, Done, TaskStatus);
        { Update individual file progress bar }
        prgCurrentFile.Position := TaskStatus.rsPercentDone;
        Sleep(100);
        Application.ProcessMessages;
      end;

      { Update all files progress bar }
      prgAllFiles.Position := prgAllFiles.Position + 1;

      Item.SubItems[0] := TffTable(Item.Data).FFVersion;
      Item.SubItems[1] := csRebuilt;
    end;
    lblPrgFile.Caption := '';
    lblPrgAllFiles.Caption := '';
  finally
    FState := stIdle;
    SetCtrlStates;
  end;
end;

procedure TfrmMain.efInitialDirChange(Sender: TObject);
begin
  SetCtrlStates;
  if DirectoryExists(efInitialDir.Text) then
    GetTables
  else
    ClearTables;
end;

procedure TfrmMain.FormActivate(Sender: TObject);
begin
  SetCtrlStates;
  if FValidConfig and FFirstTime and FAutoRun then begin
    FFirstTime := False;
    pbRebuildClick(nil);
  end;
end;

end.
