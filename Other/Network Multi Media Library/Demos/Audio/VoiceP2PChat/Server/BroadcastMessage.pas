unit BroadcastMessage;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmBroadcastMessage = class(TForm)
    Label1: TLabel;
    edMsg: TEdit;
    btnSendBroadcast: TButton;
    procedure btnSendBroadcastClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmBroadcastMessage: TfrmBroadcastMessage;

implementation

uses Main;

{$R *.dfm}

procedure TfrmBroadcastMessage.btnSendBroadcastClick(Sender: TObject);
begin
  frmMain.Server.BroadcastMsg(edMsg.Text);
end;

end.
