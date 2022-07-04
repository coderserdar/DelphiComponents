unit U_TomReg;

interface

procedure Register;

implementation
uses
  Classes,
  FileNameDialog, HeadListBox, InPanel, Pipes, PipeStatus, PJVersionInfo, TerminalBox,
  ThreadTimer, WaveList, Wavez, TrayIcon, SSStringGrid, Klawiatura;


procedure Register;
begin
  RegisterComponents('TOM', [TFileNameDialog, THeadListBox, TInPanel,
    TServerNamedPipe, TClientNamedPipe, TPipeStatusLt, TPipeStatus, TPJVersionInfo,
    TTerminalBox, TAsynTimer, TAsynEvent, TWaveList, TTomWaveFile, TTrayNotifyIcon,
    TSSStringGrid, TSSHexBinGrid, TSSEditGrid, TKlawiatura]);
end;

end.


