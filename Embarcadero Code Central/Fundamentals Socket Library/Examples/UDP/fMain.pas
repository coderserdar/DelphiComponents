unit fMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, cSocketHostLookup, cSockets, cSocketsUDP, WinSock,
  cWindows;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    UDPSocket: TfndUDPClientSocket;
    eTx: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure UDPSocketDataAvailable(const Sender: AUDPSocket);
    procedure UDPSocketError(const Sender: ASocket);
  private
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  UDPSocket.Bind;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  UDPSocket.SendStr (eTx.Text);
end;

procedure TForm1.UDPSocketDataAvailable(const Sender: AUDPSocket);
var Buf : String;
    Address : TSockAddr;
begin
  Sender.ReadPacket (Buf, Address);
  Memo1.Lines.Add ('Packet received: ' + Buf);
end;

procedure TForm1.UDPSocketError(const Sender: ASocket);
begin
  Memo1.Lines.Add ('Error: ' + Sender.ErrorMessage);
end;

end.
