program TCPClientThread;

uses
  Forms,
  fMain in 'fMain.pas' {Form1},
  cSocketHostLookup in '..\..\Source\Sockets\cSocketHostLookup.pas',
  cSockets in '..\..\Source\Sockets\cSockets.pas',
  cSocketsTCPClient in '..\..\Source\Sockets\cSocketsTCPClient.pas',
  cTCPClient in '..\..\Source\Sockets\cTCPClient.pas',
  cWinSock in '..\..\Source\Sockets\cWinSock.pas',
  cTCPStream in '..\..\Source\Sockets\cTCPStream.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
