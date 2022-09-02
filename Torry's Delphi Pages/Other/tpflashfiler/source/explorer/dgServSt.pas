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
 * Eivind Bakkestuen
 * Used with permission.
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

unit dgServSt;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Graphics,
  Forms,
  Controls,
  StdCtrls,
  Buttons,
  ExtCtrls,
  Messages,
  uConsts,
  ffdb,
  ffllbase,
  ffllprot,
  fflllgcy,
  fflllog,
  ffclreng,
  ComCtrls,
  {$IFDEF DCC4OrLater}
  ImgList,
  {$ENDIF}
  ffsrbde;

type
  TdlgServerStats = class(TForm)
    OKBtn: TButton;
    cbAutoupdate: TCheckBox;
    Label1: TLabel;
    Label3: TLabel;
    laServerVersion: TLabel;
    Bevel1: TBevel;
    btnRefresh: TButton;
    tiAutoupdate: TTimer;
    lvServers: TListView;
    Label2: TLabel;
    lvTransports: TListView;
    ilIcons: TImageList;
    Label4: TLabel;
    edFrequency: TEdit;
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure OKBtnClick(Sender: TObject);
    procedure cbAutoupdateClick(Sender: TObject);
    procedure tiAutoupdateTimer(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edFrequencyChange(Sender: TObject);
  private
    { Private declarations }
    FLog          : TffBaseLog;
    FClient       : TffClient;
    FEngine       : TffRemoteServerEngine;
    FProtocol     : TffProtocolType;
    FServerName   : TffNetAddress;
    FSession      : TFfSession;
    FUserName     : TffName;
    FPassword     : TffName;
    FTransport    : TffLegacyTransport;
    dtShown       : Boolean;
    procedure SavePreferences;
    procedure LoadPreferences;
    procedure UpdateStats;
    function ElapsedTimeToStr(T: TDateTime): string;
    procedure OpenSession;
  public
    { Public declarations }
    procedure CloseDuringShow(var Message : TMessage); message ffm_Close;
    property Protocol : TffProtocolType
      read FProtocol write FProtocol;

    property ServerName : TffNetAddress
      read FServerName write FServerName;

    property Password : TffName
      read FPassword write FPassword;

    property UserName : TffName
      read FUserName write FUserName;

    property Log : TffBaseLog
      read FLog write FLog;
  end;


implementation

{$R *.dfm}

uses
  Dialogs,
  uConfig,
  ffclbase,
  ffllcomm;


procedure TdlgServerStats.OpenSession;
var
  OldPass, OldUser : string;
begin
  OldPass := ffclPassword;
  OldUser := ffclUserName;
  try
    if FPassword <> '' then begin
      ffclPassword := FPassword;
      ffclUserName := FUserName;
    end;
    FSession.Open;
  finally
    ffclPassword := OldPass;
    ffclUserName := OldUser;
  end;
end;

procedure TdlgServerStats.FormShow(Sender: TObject);
begin
  dtShown := False;
  try
    { Set up the connection. }
    FTransport := TffLegacyTransport.Create(nil);
    with FTransport do begin
      Mode := fftmSend;
      Protocol := FProtocol;
      EventLog := FLog;
      if Assigned(FLog) then begin
        EventLogEnabled := True;
        EventLogOptions := [fftpLogErrors];
      end;
      ServerName := FServerName;
    end;

    FEngine := TffRemoteServerEngine.Create(nil);
    FEngine.Transport := FTransport;

    FClient := TffClient.Create(nil);
    FClient.ServerEngine := FEngine;
    FClient.AutoClientName := True;

    FSession := TffSession.Create(nil);
    FSession.ClientName := FClient.ClientName;
    FSession.AutoSessionName := True;
    OpenSession;

    Caption := ServerName;
    LoadPreferences;
    UpdateStats;
    dtShown := True;

  except
    on E:Exception do begin
      showMessage(E.message);
      PostMessage(Handle, ffm_Close, 0, longInt(Sender));
    end;
  end;
end;


procedure TdlgServerStats.FormDestroy(Sender: TObject);
begin
  try
    FSession.Active := False;
  finally
    FSession.Free;
  end;

  try
    FClient.Close;
  finally
    FClient.Free;
  end;

  try
    FEngine.Shutdown;
  finally
    FEngine.Free;
  end;

  try
    FTransport.Shutdown;
  finally
    FTransport.Free;
  end;
end;


procedure TdlgServerStats.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if dtShown then
    SavePreferences;
  Action := caFree;
end;


procedure TdlgServerStats.LoadPreferences;
var
  BaseSection : string;
begin
  BaseSection := ClassName + '.' + Self.Caption;
  FFEConfigGetFormPrefs(BaseSection, Self);
  cbAutoupdate.Checked := FFEConfigGetBoolean(BaseSection, 'Autoupdate', False);             {!!.07}
  tiAutoupdate.Enabled := cbAutoupdate.Checked;
  edFrequency.Text := FFEConfigGetString(BaseSection, 'TimerFreq', '1000');
  edFrequencyChange(Self);
end;

procedure TdlgServerStats.SavePreferences;
var
  BaseSection : string;
begin
  try
    BaseSection := ClassName + '.' + Self.Caption;
    FFEConfigSaveFormPrefs(BaseSection, Self);
    FFEConfigSaveBoolean(BaseSection, 'Autoupdate', cbAutoupdate.Checked);
    FFEConfigSaveString(BaseSection, 'TimerFreq', edFrequency.Text);
  except
    on E:Exception do
      ShowMessage('Error writing INI file: '+E.Message);
  end;
end;


procedure TdlgServerStats.CloseDuringShow(var Message: TMessage);
begin
  Close;
end;


procedure TdlgServerStats.OKBtnClick(Sender: TObject);
begin
  Close;
end;


function TdlgServerStats.ElapsedTimeToStr(T : TDateTime) : string;
var
  Dy : integer;
  Hr : integer;
  Mi : integer;
  Se : integer;
begin
  Dy := trunc(T);
  T := frac(T) * 24.0;
  Hr := trunc(T);
  T := frac(T) * 60.0;
  Mi := trunc(T);
  Se := trunc(frac(T) * 60.0);
  Result := Format('%d%s%.2d%s%.2d%s%.2d',
                   [
                   Dy,
                   TimeSeparator,
                   Hr,
                   TimeSeparator,
                   Mi,
                   TimeSeparator,
                   Se
                   ]);
end;


procedure TdlgServerStats.UpdateStats;
var
  aServerStats: TffServerStatistics;
  aCmdHandlerStats: TffCommandHandlerStatistics;
  aTransportStats: TffTransportStatistics;
  TransportCount,
  CmdHandlerIdx,
  TransportIdx,
  ItemIdx : Integer;
  ServerUp : Boolean;
begin
  ServerUp := FSession.GetServerStatistics(aServerStats)=DBIERR_NONE;
  laServerVersion.Caption := Format('%5.4f', [aServerStats.ssVersion / 10000.0]);
  lvServers.Items.BeginUpdate;
  lvTransports.Items.BeginUpdate;
  try
    if lvServers.Items.Count=0 then begin
      lvServers.Items.Add;
      lvServers.Items[0].ImageIndex := 0;
      for ItemIdx := 0 to 8 do
        lvServers.Items[0].SubItems.Add('');
    end;

    { update server }
    with lvServers.Items[0], aServerStats do begin
      Caption := aServerStats.ssName;
      SubItems[0] := ssState;
      SubItems[1] := FFCommaizeChL(ssClientCount, ThousandSeparator);
      SubItems[2] := FFCommaizeChL(ssSessionCount, ThousandSeparator);
      SubItems[3] := FFCommaizeChL(ssOpenDatabasesCount, ThousandSeparator);
      SubItems[4] := FFCommaizeChL(ssOpenTablesCount, ThousandSeparator);
      SubItems[5] := FFCommaizeChL(ssOpenCursorsCount, ThousandSeparator);
      SubItems[6] := FFCommaizeChL(ssRAMUsed, ThousandSeparator);
      SubItems[7] := FFCommaizeChL(ssMaxRAM, ThousandSeparator);
      SubItems[8] := ElapsedTimeToStr(ssUptimeSecs / (3600*24));
    end;
    { get transportcount }
    TransportCount := 0;
    for CmdHandlerIdx := 0 to Pred(aServerStats.ssCmdHandlerCount) do begin
      FSession.GetCommandHandlerStatistics(CmdHandlerIdx, aCmdHandlerStats);
      TransportCount := TransportCount+aCmdHandlerStats.csTransportCount;
    end;
    { adjust transportlistview if necessary }
    if TransportCount>lvTransports.Items.Count then begin
      for TransportIdx := lvTransports.Items.Count+1 to TransportCount do begin
        lvTransports.Items.Add;
        lvTransports.Items[lvTransports.Items.Count-1].ImageIndex := 1;
        for ItemIdx := 0 to 5 do
          lvTransports.Items[TransportIdx-1].SubItems.Add('');
      end;
    end
    else
    if TransportCount<lvTransports.Items.Count then
      for TransportIdx := TransportCount to lvTransports.Items.Count-1 do
        lvTransports.Items.Delete(0);
    { update transports }
    TransportCount := 0;
    for CmdHandlerIdx := 0 to Pred(aServerStats.ssCmdHandlerCount) do begin
      FSession.GetCommandHandlerStatistics(CmdHandlerIdx, aCmdHandlerStats);
      for TransportIdx := 0 to Pred(aCmdHandlerStats.csTransportCount) do begin
        FSession.GetTransportStatistics(CmdHandlerIdx, TransportIdx, aTransportStats);

        with lvTransports.Items[TransportCount],
             aTransportStats do begin
          Caption := tsName;
          SubItems[0] := tsAddress;
          SubItems[1] := tsState;
          SubItems[2] := FFCommaizeChL(tsClientCount, ThousandSeparator);
          SubItems[3] := FFCommaizeChL(tsMessageCount, ThousandSeparator);
          SubItems[4] := FormatFloat('0.####', tsMessagesPerSec);
          SubItems[5] := IntToStr(CmdHandlerIdx);
        end;
        Inc(TransportCount);
      end;
    end;
    if ServerUp then begin
      lvServers.Font.Color := clWindowText;
      lvServers.Color := clWindow;
      lvTransports.Font.Color := clWindowText;
      lvTransports.Color := clWindow;
    end
    else begin
      { warn user with some angry colours }
      lvServers.Font.Color := clWhite;
      lvServers.Color := clRed;
      lvTransports.Font.Color := clWhite;
      lvTransports.Color := clRed;
    end;
  finally
    lvServers.Items.EndUpdate;
    lvTransports.Items.EndUpdate;
  end;
end;


procedure TdlgServerStats.cbAutoupdateClick(Sender: TObject);
begin
  tiAutoupdate.Enabled := cbAutoupdate.Checked;
  edFrequency.Enabled := cbAutoupdate.Checked;
end;


procedure TdlgServerStats.tiAutoupdateTimer(Sender: TObject);
begin
  UpdateStats;
end;


procedure TdlgServerStats.btnRefreshClick(Sender: TObject);
var
  aServerStats: TffServerStatistics;
begin
  { attempt to reconnect? }
  if not (FSession.GetServerStatistics(aServerStats)=DBIERR_NONE) then begin
    FTransport.Enabled := False;
    FClient.Close;
    OpenSession;
  end;
  UpdateStats;
end;


procedure TdlgServerStats.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_ESCAPE then
    Close;
end;

procedure TdlgServerStats.edFrequencyChange(Sender: TObject);
begin
  try
    tiAutoupdate.Interval := StrToInt(edFrequency.Text);
  except
    { swallow convert error }
  end;
end;

end.
