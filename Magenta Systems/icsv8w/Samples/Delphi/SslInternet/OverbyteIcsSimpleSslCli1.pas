unit OverbyteIcsSimpleSslCli1;

{$IFNDEF USE_SSL}
  {$MESSAGE FATAL 'Define conditional define "USE_SSL" in the project options'};
{$ENDIF}
{$IF CompilerVersion < 15}
  {$MESSAGE FATAL 'This demo requires at least Delphi 7 or better'};
{$IFEND}

interface

uses
  Windows, Messages, SysUtils,
{$IFDEF COMPILER6_UP}
  Variants,
{$ENDIF}
  Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, OverbyteIcsWSocket, OverbyteIcsWndControl;

type
  TForm1 = class(TForm)
    Sock: TSslWSocket;
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    EditHost: TEdit;
    EditPort: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    EditUrl: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    Button3: TButton;
    SslContext1: TSslContext;
    procedure SockSessionConnected(Sender: TObject; ErrCode: Word);
    procedure SockDataAvailable(Sender: TObject; ErrCode: Word);
    procedure SockSslHandshakeDone(Sender: TObject; ErrCode: Word;
                                   PeerCert: TX509Base;
                                   var Disconnect: Boolean);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure SockSessionClosed(Sender: TObject; ErrCode: Word);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    RecStream : TStream;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
    RecStream := TMemoryStream.Create;
    Memo1.Clear;
    Label4.Caption := IntToStr(Memo1.Lines.Count);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
    FreeAndNil(RecStream);
end;

procedure TForm1.SockSessionConnected(Sender: TObject;
    ErrCode: Word);
begin
    if Errcode = 0 then
    begin
        Sock.SslEnable := True;
        Sock.StartSslHandshake;
        Button1.Enabled := FALSE;
        Button2.Enabled := TRUE;
    end;
end;

procedure TForm1.SockSessionClosed(Sender: TObject; ErrCode: Word);
begin
    Button1.Enabled := TRUE;
    Button2.Enabled := FALSE;
    Button3.Enabled := FALSE;
    RecStream.Seek(0, soFromBeginning);
    Memo1.Lines.LoadFromStream(RecStream);
    Label4.Caption := IntToStr(Memo1.Lines.Count);
    Memo1.Lines.Add('');
end;

procedure TForm1.SockDataAvailable(Sender: TObject; ErrCode: Word);
var
    Buf : array [0..1023] of char;
    Len : Integer;
begin
    while TRUE do begin
        Len := Sock.Receive(@Buf, SizeOf(Buf) - 1);
        if Len <= 0 then
            Exit;
        RecStream.Write(Buf[0], Len);
    end;
end;

procedure TForm1.SockSslHandshakeDone(Sender: TObject; ErrCode: Word;
    PeerCert: TX509Base; var Disconnect: Boolean);
begin
    Button3.Enabled := TRUE;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
    Sock.Addr := EditHost.Text;
    Sock.Port := EditPort.Text;
    RecStream.Size := 0;
    Memo1.Clear;
    Label4.Caption := IntToStr(Memo1.Lines.Count);
    Sock.SslEnable := FALSE;
    Sock.Connect; //-->
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
    Sock.Shutdown(1);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
    Button3.Enabled := FALSE;
    Sock.SendStr('GET ' + EditUrl.Text + ' HTTP/1.0'#13#10#13#10);
end;


end.
