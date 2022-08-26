program iOSAVAudioPlayerBackground;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FAVAudioPlayer};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFAVAudioPlayer, FAVAudioPlayer);
  Application.Run;
end.
