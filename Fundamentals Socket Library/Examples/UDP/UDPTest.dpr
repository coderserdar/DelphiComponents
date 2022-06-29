program UDPTest;

uses
  Forms,
  fMain in 'fMain.pas' {Form1},
  cWinSock in '..\..\Source\Sockets\cWinSock.pas',
  cSockets in '..\..\Source\Sockets\cSockets.pas',
  cSocketsUDP in '..\..\Source\Sockets\cSocketsUDP.pas',
  cSocketHostLookup in '..\..\Source\Sockets\cSocketHostLookup.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
