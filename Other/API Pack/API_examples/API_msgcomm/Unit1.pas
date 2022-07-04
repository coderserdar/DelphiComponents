unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, API_msgcomm, API_grbutton, StdCtrls, API_label,
  API_edit, AppEvnts;

type
  TForm1 = class(TForm)
    API_edit2: TAPI_edit;
    API_edit3: TAPI_edit;
    API_edit4: TAPI_edit;
    API_label2: TAPI_label;
    API_label3: TAPI_label;
    API_label4: TAPI_label;
    API_grbutton1: TAPI_grbutton;
    API_label7: TAPI_label;
    API_edit5: TAPI_edit;
    API_msgcomm1: TAPI_msgcomm;
    API_label6: TAPI_label;
    Timer1: TTimer;
    CheckBox1: TCheckBox;
    API_label1: TAPI_label;
    API_edit1: TAPI_edit;
    Panel1: TPanel;
    Label3: TLabel;
    Label4: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure API_grbutton1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure API_msgcomm1Message(sender: TObject; const command,
      value: Integer; const messagetext: String);
    procedure API_edit5Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  api_edit5.Text:= api_msgcomm1.Mailbox;
  api_edit1.Text:= '0';
  api_edit2.Text:= api_msgcomm1.Mailbox;
  api_edit4.text:= 'send this by clicking button below';
end;

procedure TForm1.API_grbutton1Click(Sender: TObject);
var
  windowcaption: string;
  command: integer;
  value: integer;
  texttosend: string;
  broadcast: boolean;
begin
  windowcaption:= api_edit2.Text;
  command:= api_edit3.asInteger;
  value:= api_edit1.asInteger;
  texttosend:= api_edit4.Text;
  broadcast:= checkbox1.checked;

  if not api_msgcomm1.Send( windowcaption, command, value, texttosend, broadcast ) then
    showmessage('failed to send message!');
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  label1.Caption:= inttostr( api_msgcomm1.SentMessages );
  label2.caption:= inttostr( api_msgcomm1.ReceivedMessages );
end;

procedure TForm1.API_msgcomm1Message(sender: TObject; const command,
  value: Integer; const messagetext: String);
begin
  api_label6.caption:= inttostr(command)+' = '+inttostr(value)+' > '+messagetext;
end;

procedure TForm1.API_edit5Change(Sender: TObject);
begin
  api_msgcomm1.Mailbox:= api_edit5.Text;
  self.Caption:= 'mailbox = "'+api_msgcomm1.Mailbox+'"';
end;

end.
