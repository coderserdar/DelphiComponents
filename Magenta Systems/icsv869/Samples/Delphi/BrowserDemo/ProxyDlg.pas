unit ProxyDlg;

interface

uses
    WinTypes, WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms,
    Dialogs, StdCtrls, Buttons, ExtCtrls, OverbyteIcsWsocket, OverByteIcsSSLEAY;

type
    TProxyForm = class(TForm)
    ProxyUrlEdit: TEdit;
        Label1 : TLabel;
        OKBurron : TBitBtn;
        CancelButton : TBitBtn;
        ProxyUsername : TEdit;
        ProxyPassword : TEdit;
        Label3 : TLabel;
        Label4 : TLabel;
        Label5: TLabel;
        UserAgent: TEdit;
        lbl1: TLabel;
        lbl2: TLabel;
        SslVersionList: TComboBox;
        SslRootCertsFileEdit: TEdit;
        SslVerifyCertMode: TRadioGroup;
        SslRevokeCheck: TCheckBox;
        SslReportChain: TCheckBox;
    procedure FormCreate(Sender: TObject);
    private
        { Private declarations }
    public
        { Public declarations }
    end;

var
    ProxyForm : TProxyForm;

implementation

{$R *.DFM}

procedure TProxyForm.FormCreate(Sender: TObject);
var
    Level: TSslCliSecurity;
begin
    // V8.55 update SSL client security levels
    SslVersionList.Items.Clear;
    for Level := Low(TSslCliSecurity) to High(TSslCliSecurity) do
         SslVersionList.Items.Add (SslCliSecurityNames[Level]);
end;

end.
