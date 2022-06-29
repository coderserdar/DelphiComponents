unit fMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, cTCPClient;

type
  TForm1 = class(TForm)
    Button1: TButton;
    fndTCPClient: TfndTCPClient;
    Memo1: TMemo;
    procedure fndTCPClientClose(const Sender: ATCPClient);
    procedure fndTCPClientConnected(const Sender: ATCPClient);
    procedure fndTCPClientConnectFailed(const Sender: ATCPClient);
    procedure fndTCPClientDataAvailable(const Sender: ATCPClient);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  cDateTime;

{$R *.dfm}

procedure TForm1.fndTCPClientClose(const Sender: ATCPClient);
begin
  Memo1.Lines.Add ('Close');
end;

procedure TForm1.fndTCPClientConnected(const Sender: ATCPClient);
begin
  Memo1.Lines.Add ('Connected');
  Sender.Socket.SendStr ('GET / HTTP/1.1'#13#10 +
                         'Host: ' + Sender.Host + #13#10 +
                         'Date: ' + NowAsRFCDateTime + #13#10 +
                         'Connection: close'#13#10 +
                         #13#10);
end;

procedure TForm1.fndTCPClientConnectFailed(const Sender: ATCPClient);
begin
  Memo1.Lines.Add ('Connect failed: ' + Sender.ErrorMsg);
end;

procedure TForm1.fndTCPClientDataAvailable(const Sender: ATCPClient);
begin
  Memo1.Lines.Add (Sender.Socket.ReadAvailable);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  fndTCPClient.Active := True;
end;

end.
