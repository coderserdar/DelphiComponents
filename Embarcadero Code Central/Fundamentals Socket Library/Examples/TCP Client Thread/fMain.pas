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
    procedure Button1Click(Sender: TObject);
    procedure fndTCPClientThreadRun(const Sender: ATCPClient);
    procedure fndTCPClientSyncConnected(const Sender: ATCPClient);
    procedure fndTCPClientSyncClose(const Sender: ATCPClient);
    procedure fndTCPClientThreadRunComplete(const Sender: ATCPClient);
  private
    Data : String;

    Procedure SyncOutput;
  public
  end;

var
  Form1: TForm1;

implementation

uses
  cDateTime;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  fndTCPClient.Active := True;
end;

Procedure TForm1.SyncOutput;
  Begin
    Memo1.Lines.Add (Data);
  End;

procedure TForm1.fndTCPClientThreadRun(const Sender: ATCPClient);
begin
  Sender.Socket.SendStr ('GET / HTTP/1.1'#13#10 +
                         'Host: ' + Sender.Host + #13#10 +
                         'Date: ' + NowAsRFCDateTime + #13#10 +
                         'Connection: close'#13#10 +
                         #13#10);
  // Read HTTP response header
  While not Application.Terminated and not Sender.Terminated and not Sender.Stream.Reader.EOF do
    begin
      Data := Sender.Stream.Reader.ExtractLine (1024);
      Sender.Synchronize (SyncOutput);
      if Data = '' then
        break;
    end;
  // Read HTTP response content
  While not Application.Terminated and not Sender.Terminated and not Sender.Stream.Reader.EOF do
    begin
      Data := Sender.Stream.Reader.ReadAvailable;
      Sender.Synchronize (SyncOutput);
      if not Sender.Stream.Wait then
        break;
    end;
end;

procedure TForm1.fndTCPClientSyncConnected(const Sender: ATCPClient);
begin
  Memo1.Lines.Add ('Connected');
end;

procedure TForm1.fndTCPClientSyncClose(const Sender: ATCPClient);
begin
  Memo1.Lines.Add ('Close');
end;

procedure TForm1.fndTCPClientThreadRunComplete(const Sender: ATCPClient);
begin
  Memo1.Lines.Add ('Complete: ' + Sender.ErrorMsg);
end;

end.
