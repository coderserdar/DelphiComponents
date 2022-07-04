program VoiceServer;

uses
  Forms,
  Main in 'Main.pas' {frmMain},
  UserList in 'UserList.pas' {frmUsers},
  ThreadInfo in 'ThreadInfo.pas' {frmThreadInfo},
  NMMAudioSetup in '..\..\..\..\Source\Frames\NMMAudioSetup.pas',
  frSelectAudio in '..\..\..\..\Source\Frames\frSelectAudio.pas' {SelectAudio: TFrame},
  frSelectAudioInput in '..\..\..\..\Source\Frames\frSelectAudioInput.pas' {SelectAudioInput: TFrame},
  frSelectAudioOutput in '..\..\..\..\Source\Frames\frSelectAudioOutput.pas' {SelectAudioOutput: TFrame},
  CodecsInfo in '..\..\..\..\Source\Frames\CodecsInfo.pas' {frmCodecsInfo},
  NoCodecDlg in '..\..\NoCodecDlg.pas' {frmNoCodec},
  BroadcastMessage in 'BroadcastMessage.pas' {frmBroadcastMessage},
  About in 'About.pas' {AboutBox};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmUsers, frmUsers);
  Application.CreateForm(TfrmThreadInfo, frmThreadInfo);
  Application.CreateForm(TfrmCodecsInfo, frmCodecsInfo);
  Application.CreateForm(TfrmNoCodec, frmNoCodec);
  Application.CreateForm(TfrmBroadcastMessage, frmBroadcastMessage);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.Run;
end.
