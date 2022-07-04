program VoiceClient;

uses
  Forms,
  Main in 'Main.pas' {frmMain},
  Settings in 'Settings.pas' {frmSettings},
  frSelectAudio in '..\..\..\..\Source\Frames\frSelectAudio.pas' {SelectAudio: TFrame},
  frSelectAudioInput in '..\..\..\..\Source\Frames\frSelectAudioInput.pas' {SelectAudioInput: TFrame},
  Stat in 'Stat.pas' {frmStat},
  NoCodecDlg in '..\..\NoCodecDlg.pas' {frmNoCodec},
  Messags in 'Messags.pas' {frmMessages},
  About in '..\..\SoftPhone\About.pas' {AboutBox};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmSettings, frmSettings);
  Application.CreateForm(TfrmStat, frmStat);
  Application.CreateForm(TfrmNoCodec, frmNoCodec);
  Application.CreateForm(TfrmMessages, frmMessages);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.Run;
end.
