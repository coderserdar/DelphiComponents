{$INCLUDE ..\cDefines.inc}
unit cSocketsReg;

interface

uses
  { Delphi }
  Classes,
  { Fundamentals }
  cSocketHostLookup,
  cSocketsUDP,
  cTCPClient,
  cTCPServer;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('fnd Sockets', [
      TfndSocketHostLookup,
      TfndUDPSocket, TfndUDPClientSocket,
      TfndTCPClient, TfndTCPClientCollection, TfndSocks5Proxy, TfndHTTPTunnelProxy,
      TfndTCPServer]);
end;



end.
